(in-package :caten/ajit)
;; This paper is good to read first:
;; - https://arxiv.org/pdf/2401.06665
;; - https://www.researchgate.net/publication/320992060_Consecutivity_in_the_isl_Polyhedral_Scheduler

;; [TODO]
;; Hand updated Embedding Schedule
;; 1. Fixed the domain of T5 (10, 30, 10, 10)
;; なんか色々おかしい ~...
;; Polyhedralは全部書き直そう。。。
;; SERIALIZE=1なループを持つPolyhedralをどうにかする
;; 1 vs 1 でFuseするのを繰り返す？
;; https://github.com/Tiramisu-Compiler/tiramisu/blob/master/src/auto_scheduler/tiramisu_auto_scheduler.cpp
;; これを作るイメージ
;; https://medium.com/@zhen8838/hands-on-polyherdal-affine-loop-fusion-ffb398b0ae60
;; あ〜あとConvNDのカーネル修正も必要。。。
;; make-node suru
;; LoopCollapse, Strideが1になればAffine

(defclass PolyIR ()
  ((quasiaffine :initform nil :initarg :quasiaffine :accessor polyir-quasiaffine :type list)
   (time :initarg :time :type fixnum :accessor polyir-time)
   (body :initarg :body :type Graph :accessor polyir-body)
   (pipeline :initarg :pipeline :type hash-table :accessor polyir-pipeline)
   (render-graph :accessor polyir-render-graph)
   (read     :accessor polyir-read)
   (write    :accessor polyir-write)
   (domain   :accessor polyir-domain)
   (schedule :accessor polyir-schedule)
   (ast-option :initform :atomic :accessor polyir-ast-option))
  (:documentation "Corresponds to a single serialized loop"))

(defstruct (Cost (:constructor make-cost (metric penalty-p)))
  (metric metric :type fixnum)
  (penalty-p penalty-p :type boolean))

(defstruct (Scheduled-AST
	    (:constructor make-scheduled-ast
		(sc option kernel-rank pipeline
		 &aux
		   (sched (schedule-set-options (schedule-constraints-compute-schedule sc) option))
		   (ast (ast-build-node-from-schedule (ast-build-from-context (set-from-str "{:}")) sched))
		   (graph
		    (apply
		     #'make-graph
		     (%create-rendering-graph-nodes (parse-isl-ast (isl::ast-node-handle ast)) kernel-rank))))))
  (sc sc :type schedule-constraints)
  (sched sched :type schedule)
  (graph graph :type Graph)
  (cost (compute-memref-costs ast pipeline) :type Cost))

(defmethod print-object ((pir PolyIR) stream)
  (format stream "<PolyIR[t=~a]
{
~a
}
>"
	  (polyir-time pir)
	  (with-output-to-string (out)
	    
	    )))

(defmethod initialize-instance :after ((pir PolyIR) &key body)
  (assert body)
  (let ((input (make-hash-table)))
    (setf (gethash (polyir-time input) input) (polyir-body pir))
    (multiple-value-bind (domain read write)
	(values
	 (render-domain input :depends-on (polyir-quasiaffine pir))
	 (render-access :read input :depends-on (polyir-quasiaffine pir))
	 (render-access :write input :depends-on (polyir-quasiaffine pir)))
      (setf (polyir-domain pir) domain
	    (polyir-read pir) read
	    (polyir-write pir) write))))

(defmethod compute-memref-costs ((ast Scheduled-AST) pipeline)
  "Compute the cost of memory references in the AST to maximize the locality of data accesses.
```
cost_n = (memory_access_total_n) + (penalty_n)
```
where penalty_n = Σmax(cost) if scheduled_ast has the conditional branch.
memory_access_total_n[fixnum] is a count of memory_accesses across different loops (i.e.: DRAM Accesses)
"
  (let ((penalty-p (if (find :IF (graph-nodes (scheduled-ast-graph ast)) :key #'node-type) t nil))
	(memory-access-total 0))
    (flet ((enumerate-access (node)
	     (assert (eql (node-type node) :FUNCALL))
	     (if (memory-access-local-p (graph-nodes (scheduled-ast-graph ast)) (getattr node :idx) pipeline)
		 1
		 0)))
      (loop for node in (graph-nodes (scheduled-ast-graph ast))
	    if (eql (node-type node) :FUNCALL)
	      do (incf memory-access-total (enumerate-access node))))
    ;; memory-access-total = memory-access during different loops
    (make-cost memory-access-total penalty-p)))

(defmethod compute-schedule ((pir1 PolyIR) (pir2 PolyIR) (schedule Union-Set))
  (with-slots ((read1 read) (write1 write) (domain1 domain)) pir1
    (with-slots ((read2 read) (write2 write) (domain2 domain)) pir2
      (let* ((read (union-map-union read1 read2))
	     (write (union-map-union write1 write2))
	     (domain (union-set-union domain1 domain2))
	     (all-deps (reduce #'union-map-union (multiple-value-list (%create-dependency-graph read write schedule))))
	     (sc (schedule-constraints-on-domain domain))
	     (sc (schedule-constraints-set-coincidence sc all-deps))
	     (sc (schedule-constraints-set-validity sc all-deps))
	     ;; proximity constraints (keeps loops nested based on dependencies)
	     (sc (schedule-constraints-set-proximity sc all-deps)))
	;; we set kernel-rank as zero because the rendererd graph is not directly used to create the actual kernel.
	;; just only used to evaluate the cost.
	;; [TODO] the current implementation should be turtle slow because there is a lot of type conversations
	;; refactor to keep using isl ast objects.
	(make-scheduled-ast sc (polyir-ast-option pir1) 0 (polyir-pipeline pir1))))))

(defmethod compute-schedule ((pir1 PolyIR) (pir2 PolyIR) schedule) :failed)

(defmethod initial-schedule ((pir1 PolyIR) (pir2 PolyIR) (fuse-dim fixnum) (padding-pos fixnum))
  "Creates an initial schedule between two loops, assuming pir1 comes first, pir2 comes second.
```
[PolyIR 2]  (t=0)
    |
[PolyIR 1]  (t=1)
```
=>
```
[Union(pir1.quasiaffine), (pir2.quasiaffine)] -> {
  S1[domain] -> [fused_schedule];
  S2[domain] -> [fused_schedule];
};
```
Inputs:
- fuse-dim: 0 means no fusion, fuse_dim >= 1 means fusion at (fuse_dim-1)
- padding-pos: the function can append zero to the given dim.
"
  (union-map-from-str
   (with-output-to-string (out)
     (format out "[~(~a~)] -> " (render-list (remove-duplicates (append (polyir-quasiaffine pir1) (polyir-quasiaffine pir2)))))
     (format out "{~%")
     (with-slots ((time1 time) (graph1 body)) pir1
       (with-slots ((time2 time) (graph2 body)) pir2
	 (let ((lf1 (graph->loop-factors graph1))
	       (lf2 (graph->loop-factors graph2)))
	   ;; Check the dimension size of loops. (merged only if the size is equivalent)
	   (let ((for1 (id->value graph1 (nth fuse-dim lf1)))
		 (for2 (id->value graph2 (nth fuse-dim lf2))))
	     (symbol-macrolet ((->failed (return-from initial-schedule nil)))
	       (unless (eql (node-type for1) :IR/FOR) ->failed)
	       (unless (eql (node-type for2) :IR/FOR) ->failed)
	       ;; [TODO] Dim check
	       ;; [TODO] How to fuse [fused_polyir] and [polyir]?
	       ))
	   (flet ((fs (ls time)
		    (nconc
		     (subseq ls 0 fuse-dim)
		     (list time)
		     (subseq ls fuse-dim)))
		  (pd (ls another)
		    (nconc
		     (subseq ls 0 padding-pos)
		     (loop repeat (- (length another) (length ls)) collect 0)
		     (subseq ls padding-pos))))
	     (multiple-value-bind (fs1 fs2) (values (fs lf1 time1) (fs lf2 time2))
	       (format out "  S~a[~(~a~)] -> [~a];" time1 (render-list lf1) (render-list (pd fs1 fs2)))
	       (format out "  S~a[~(~a~)] -> [~a];" time2 (render-list lf2) (render-list (pd fs2 fs1))))))))
     (format out "}"))))

(defmethod candidate-patterns ((pir1 PolyIR) (pir2 PolyIR))
  (let* ((lf1 (graph->loop-factors (polyir-body pir1)))
	 (lf2 (graph->loop-factors (polyir-body pir2)))
	 (kernel-size (max (length lf1) (length lf2))))
    (flet ((ptn (padding-dim)
	     (loop for i upfrom 0 below kernel-size
		   collect (list i padding-dim))))
      ;; padding_dim!=0, max is not always the best solution.
      (nconc
       (ptn 0)
       (ptn kernel-size)
       (loop for i upfrom 1 below (1- kernel-size) append (ptn i))))))

(defmethod maybe-fuse-two-loops ((pir1 PolyIR) (pir2 PolyIR))
  ""
  ;; Return -> Fused Schedule
  (flet ((maybe-early-return (sast)
	   (when (and (null (cost-penalty-p (scheduled-ast-cost sast)))
		      (= 0 (cost-metric (scheduled-ast-cost sast))))
	     (return-from maybe-fuse-two-loops sast))
	   sast))
    (let* ((patterns (candidate-patterns pir1 pir2))
	   (scheduled-asts
	     (loop for p in patterns
		   for sast = (compute-schedule pir1 pir2 (initial-schedule pir1 pir2 (first p) (second p)))
		   unless (eql sast :failed)
		     collect (maybe-early-return sast))))
      scheduled-asts)))

;; loop-fusion (Poly1, Poly2)
(defstruct (Polyhedral
	    (:conc-name poly-)
	    (:constructor make-polyhedral (avm pipeline domain read write initial-schedule vm-inputs vm-outputs lex-table &key (ast-option :atomic))))
  (avm avm :type avm)
  (vm-inputs vm-inputs :type list)
  (vm-outputs vm-outputs :type list)
  (vm-io-types (infer-vm-io-types avm `(,@vm-inputs ,@vm-outputs)) :type hash-table)
  (pipeline pipeline :type hash-table)
  ;; constraints
  (domain domain :type string)
  (domain-ptr (union-set-from-str domain) :type union-set)
  (read read :type string)
  (read-ptr (union-map-from-str read) :type union-map)
  (write write :type string)
  (write-ptr (union-map-from-str write) :type union-map)
  (initial-schedule initial-schedule :type union-map)
  (schedule nil :type (or null Schedule))
  (lex-table lex-table :type hash-table)
  (ast-option ast-option :type (member :separate :atomic)))

(defun poly/io-scalar-p (poly x)
  (let ((type (gethash x (poly-vm-io-types poly))))
    (when (null type) (error "~a is not input/output" type))
    (= (buffer-nrank (car (relay-writes type))) 0)))

(defun poly/schedule-metadata (polyhedral)
  (declare (type polyhedral polyhedral))
  (when (poly-schedule polyhedral)
    (yaml:parse (schedule-to-str (poly-schedule polyhedral)))))

(defstruct (Band)
  (domain (error "") :type union-set)
  (permutable nil :type boolean)
  (coincident nil :type list))

(defun collect-bandnode (top &aux (out) (depth 0))
  (declare (type polyhedral top))
  (labels ((explore (schedule-node)
	     (let ((c (isl::%isl-schedule-node-has-children (isl::schedule-node-handle schedule-node))))
	       (when (eql c :bool-true)
		 (loop for n upfrom 0 below (isl::%isl-schedule-node-n-children (isl::schedule-node-handle schedule-node))
		       for node = (schedule-node-get-child schedule-node n)
		       do (explore node)))
	       (ecase (isl::%isl-schedule-node-get-type (isl::schedule-node-handle schedule-node))
		 (:Schedule-Node-Leaf)
		 (:Schedule-Node-Filter)
		 (:Schedule-Node-Sequence)
		 (:Schedule-Node-Band
		  (let ((dom (schedule-node-get-domain schedule-node))
			(n (isl::%isl-schedule-node-band-n-member (isl::schedule-node-handle schedule-node)))
			(shuffle-p (eql :bool-true (isl::%isl-schedule-node-band-get-permutable (isl::schedule-node-handle schedule-node)))))
		    (incf depth)
		    (push
		     (make-band
		      :domain dom
		      :permutable shuffle-p
		      :coincident
		      (loop for i upfrom 0 below n
			    collect (eql :bool-true (isl::%isl-schedule-node-band-member-get-coincident (isl::schedule-node-handle schedule-node) i))))
		     out)))
		 (:Schedule-Node-Domain)
		 (:Schedule-Node-Expansion)
		 (:Schedule-Node-Extension)
		 (:Schedule-Node-Mark)
		 (:Schedule-Node-Set)
		 (:Schedule-Node-Context)
		 (:Schedule-Node-Guard)))))
    (explore (schedule-get-root (poly-schedule top)))
    (values out depth)))

(defun finalize-polyhedral (polyhedral &aux (schedule (poly-schedule polyhedral)))
  (declare (type polyhedral polyhedral))
  (macrolet ((set-option (name level)
	       `(foreign-funcall ,(format nil "isl_options_set_~(~a~)" name)
				 :pointer (isl::context-handle isl::*context*)
				 :int ,level
				 :void)))
    (set-option "ast_build_exploit_nested_bounds" 1)
    (set-option "ast_build_detect_min_max" 1)
    (set-option "ast_build_scale_strides" 1)
    (set-option "ast_build_allow_else" 0)
    (set-option "ast_build_allow_or" 0))
  (let* ((schedule (schedule-set-options schedule (poly-ast-option polyhedral)))
	 (bands (multiple-value-list (collect-bandnode polyhedral)))
	 ;; [TODO] Better way to determine the depth (currently, 2 x {band_count})
	 (depth (* 2 (second bands)))
	 (bands (car bands))
	 (ast-build (ast-build-from-context (set-from-str "{:}")))
	 (ast-build (ast-build-set-iterators ast-build (apply #'make-id-list (map 'list #'gid (range 0 (1+ depth))))))
	 (ast-build-node (ast-build-node-from-schedule ast-build schedule)))
    (values ast-build-node bands)))

(defmethod print-polyhedral ((poly Polyhedral) stream)
  (format stream "
= [Polyhedral] ========================================================
Domain:
~a
Read:
~a
Write:
~a
Schedule:
~a
Expected Output (Scalar ops are temporarily excluded):
~a
======================================================================"
	  (poly-domain poly)
	  (poly-read poly)
	  (poly-write poly)
	  (when (poly-schedule poly)
	    (schedule-get-root (schedule-set-options (poly-schedule poly) (poly-ast-option poly))))
	  (when (poly-schedule poly)
	    (debug/render-c poly))))

(defun debug/render-c (polyhedral &aux (schedule (poly-schedule polyhedral)))
  (let* ((schedule (schedule-set-options schedule (poly-ast-option polyhedral)))
	 (build (ast-build-from-context (set-from-str "{:}")))
	 (ast   (ast-build-node-from-schedule build schedule))
	 (p     (isl::%isl-printer-to-str (isl::context-handle isl::*context*)))
	 (p     (isl::%isl-printer-set-output-format p 4)) ;; 4 == Clang
	 (q     (isl::%isl-printer-print-ast-node p (isl::ast-node-handle ast)))
	 (str   (isl::%isl-printer-get-str q)))
    str))

(defun %create-dependency-graph (read-access write-access initial-schedule)
  ;; References https://github.com/zhen8838/isl_learn/blob/main/12_schedule_program.ipynb
  (let* ((raw (union-map-intersect
	       (union-map-apply-range
		write-access
		(union-map-reverse read-access))
	       (union-map-lex-lt-union-map initial-schedule initial-schedule)))
	 (war (union-map-intersect
	       (union-map-apply-range
		read-access
		(union-map-reverse write-access))
	       (union-map-lex-lt-union-map initial-schedule initial-schedule)))
	 (waw (union-map-intersect
	       (union-map-apply-range
		write-access
		(union-map-reverse write-access))
	       (union-map-lex-lt-union-map initial-schedule initial-schedule))))
    (values raw waw war)))

;; [todo] delete
(defun create-dependency-graph (polyhedral)
  (with-slots ((domain domain-ptr) (initial-schedule initial-schedule) (read-access read-ptr) (write-access write-ptr)) polyhedral
    ;; References https://github.com/zhen8838/isl_learn/blob/main/12_schedule_program.ipynb
    (let* ((raw (union-map-intersect
		 (union-map-apply-range
		  write-access
		  (union-map-reverse read-access))
		 (union-map-lex-lt-union-map initial-schedule initial-schedule)))
	   (war (union-map-intersect
		 (union-map-apply-range
		  read-access
		  (union-map-reverse write-access))
		 (union-map-lex-lt-union-map initial-schedule initial-schedule)))
	   (waw (union-map-intersect
		 (union-map-apply-range
		  write-access
		  (union-map-reverse write-access))
		 (union-map-lex-lt-union-map initial-schedule initial-schedule))))
      (values raw waw war))))

(defun poly/make-constraints (polyhedral)
  "(2) Validty/Legality Constraints"
  (declare (type polyhedral polyhedral))
  (with-slots ((domain domain-ptr)) polyhedral
    (multiple-value-bind (raw-deps waw-deps war-deps)
	(create-dependency-graph polyhedral)
      (let* ((all-deps (union-map-union waw-deps war-deps))
	     (all-deps (union-map-union all-deps raw-deps))
	     (schedule-constraints (schedule-constraints-on-domain domain))
	     (schedule-constraints (schedule-constraints-set-coincidence schedule-constraints all-deps))
	     (schedule-constraints (schedule-constraints-set-validity schedule-constraints all-deps))
	     ;; proximity constraints (keeps loops nested based on dependencies)
	     (schedule-constraints (schedule-constraints-set-proximity schedule-constraints all-deps)))
	schedule-constraints))))

(defun poly/schedule (polyhedral &key (serialize nil))
  "
[Scheduler]
This function analyzes the read/write dependencies on the polyhedron space,
trying to apply the operator fusion as many as possible
```
for (int c0 = 0; c0 < a; c0 += 1)
  for (int c1 = 0; c1 < c; c1 += 1)
    T3(c0, c1, 0);
for (int c0 = 0; c0 < a; c0 += 1)
  for (int c1 = 0; c1 < c; c1 += 1)
    for (int c2 = 0; c2 < b; c2 += 1)
      T4(c0, c1, c2);
for (int c0 = 0; c0 < a; c0 += 1)
  for (int c1 = 0; c1 < c; c1 += 1)
    T5(c0, c1);
=>    
for (int c0 = 0; c0 < a; c0 += 1)
  for (int c1 = 0; c1 < c; c1 += 1)
    T3(c0, c1, 0);
    for (int c2 = 0; c2 < b; c2 += 1)
      T4(c0, c1, c2);
    T5(c0, c1);
```
"
  (declare (type polyhedral polyhedral))
  (macrolet ((set-option (name level)
	       `(progn
		  (foreign-funcall ,(format nil "isl_options_set_~(~a~)" name)
				   :pointer (isl::context-handle isl::*context*)
				   :int ,level
				   :void))))
    (when serialize (set-option "schedule_serialize_sccs" 1))
    (let ((n 0))
      (loop for g in (hash-table-values (poly-pipeline polyhedral))
	    do (incf n (length (graph-nodes g))))
      (set-option "schedule_outer_coincidence" 1)
      ;; (set-option "schedule_maximize_band_depth" 1)
      (set-option "schedule_treat_coalescing" 1)
      ))
  (with-slots ((domain-ptr domain-ptr) (read-ptr read-ptr) (write-ptr write-ptr)) polyhedral
    (let* ((constraints (poly/make-constraints polyhedral))
	   (schedule (schedule-constraints-compute-schedule constraints)))
      (setf (poly-schedule polyhedral) schedule)
      polyhedral)))

(defun set-schedule-options (&key (serialize nil))
  (macrolet ((set-option (name level)
	       `(progn
		  (foreign-funcall ,(format nil "isl_options_set_~(~a~)" name)
				   :pointer (isl::context-handle isl::*context*)
				   :int ,level
				   :void))))
    (when serialize (set-option "schedule_serialize_sccs" 1))
    (set-option "schedule_outer_coincidence" 1)
    ;; (set-option "schedule_maximize_band_depth" 1)
    (set-option "schedule_treat_coalescing" 1)))
