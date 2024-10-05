(in-package :caten/ajit)

;; Implements Reconfigurable Polyhedral Compiler
;; https://arxiv.org/abs/2401.06665

;; Before PG -> Optimizations are common across architectures
;; PG -> Architecture specific optimizations

;; Takes Scheduled Group as an input
;; -> Polyhedral
;; Optimized Group
;; [TODO]
;; - [Here] Tiling, Loop Collapse, Vectorize
;; - Fix Transformer Scheduling
;; - ISeq Lowring is slow
;; - Transpose+Matmul Fusion
(defclass Polyhedral-Group ()
  ((base :initarg :base :type Group :accessor polyhedral-group-base))
  (:documentation "
A Polyhedral form of the fused schedule group.
"))

(defclass Polyhedral-Auto-Scheduler (Polyhedral-Group)
  ((schedule :accessor pg-schedule)
   (domain   :accessor pg-domain)
   (dependencies :accessor pg-dependencies))
  (:documentation "groups which is subject to jit"))

(defmethod initialize-instance :after ((pg Polyhedral-Auto-Scheduler) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (multiple-value-bind (domain read write schedule) (scop (polyhedral-group-base pg))
    (setf (pg-schedule pg) schedule
          (pg-domain pg) domain)
    (let* ((access (union-access-info-from-sink read))
           (access (union-access-info-set-must-source access write))
           (access (union-access-info-set-schedule access schedule))
           (flow (union-access-info-compute-flow access))
           (RaW (union-flow-get-must-dependence flow))
           (access (union-access-info-from-sink write))
           (access (union-access-info-set-must-source access write))
           (access (union-access-info-set-may-source access read))
           (access (union-access-info-set-schedule access schedule))
           (flow   (union-access-info-compute-flow access))
           (WaW    (union-flow-get-must-dependence flow))
           (WaR    (union-flow-get-may-dependence flow))
           (dependencies
             (union-map-union
              (union-map-union WaR RaW)
              WaW)))
      (setf (pg-dependencies pg) dependencies)))
  (print "++++++BEFORE++++++")
  (print (schedule-get-root (pg-schedule pg)))
  (print "+++++NEW+++++++")
  (let ((new (schedule pg)))
    (print (debug/render-schedule new))
    (print (schedule-get-root new))))

(defmethod schedule ((pg Polyhedral-Auto-Scheduler))
  (let ((outer-coincidence 0)
        (maximize-coincidence 0)
        (treat-coalescing 0)
        (maximize-band-depth 0)
        (schedule-whole-component 1))
    (macrolet ((set-option (name level)
	         `(progn
		    (foreign-funcall ,(format nil "isl_options_set_~(~a~)" name)
				     :pointer (isl::context-handle isl::*context*)
				     :int ,level
				     :void))))
      (flet ((configure ()
               (set-option "schedule_outer_coincidence" outer-coincidence)
               (set-option "schedule_maximize_coincidence" maximize-coincidence)
               (set-option "schedule_treat_coalescing" treat-coalescing)
               (set-option "schedule_maximize_band_depth" maximize-band-depth)
               (set-option "schedule_whole_component" schedule-whole-component)))
        (configure))))
  (schedule-constraints-compute-schedule
   (schedule-constraints-set-proximity
    (schedule-constraints-set-validity
     (schedule-constraints-set-coincidence
      (schedule-constraints-on-domain (pg-domain pg))
      (pg-dependencies pg))
     (pg-dependencies pg))
    (pg-dependencies pg))))

;; https://github.com/facebookresearch/TensorComprehensions/blob/master/tc/core/polyhedral/scop.cc#L47
;; https://github.com/facebookresearch/TensorComprehensions/blob/master/tc/core/polyhedral/schedule_isl_conversion.cc
(defmethod render-domain-body-from-group ((group Group) &aux (idx2domain (make-hash-table)))
  (values
   (with-output-to-string (out)
     (format out "{~%")
     (loop with domains = nil
           for node in (graph-nodes (group-render-graph group)) do
             (case (node-type node)
               (:FOR
                (let ((by (getattr node :by)))
                  (assert (and (eql (expr-op by) :Const) (eql 1 (expr-x by)))
                          ()
                          "The group is not a scop. (TODO: Fix)"))
                (push node domains))
               (:ENDFOR (setf domains (remove (getattr node :idx) domains :key #'(lambda (x) (getattr x :idx)) :test #'equalp)))
               (:FUNCALL
                (let ((domains (reverse domains))
                      (isl     (default-device :isl-expr)))
                  (setf (gethash (getattr node :idx) idx2domain)
                        (if domains
                            (format nil "T~a[~(~a~)]" (getattr node :idx) (render-list (map 'list #'(lambda (x) (getattr x :idx)) domains)))
                            (format nil "T~a[]" (getattr node :idx))))
                  (if domains
                      (format out "  T~a[~(~a~)] : ~(~a~);~%"
                              (getattr node :idx)
                              (render-list (map 'list #'(lambda (x) (getattr x :idx)) domains))
                              (render-expr
                               isl
                               (reduce
                                #'(lambda (x y) (make-expr :AND x y))
                                (loop for dom in domains
                                      collect (make-expr :>= (make-const (getattr dom :idx) nil) (getattr dom :upfrom))
                                      collect (getattr dom :below)))))
                      (format out "  T~a[];~%" (getattr node :idx)))))
               (otherwise
                ;; :IF :ELSE :ENDIF
                (error "Group is not a scop. (Set :separate)"))))
     (format out "}"))
   idx2domain))

(defmethod render-domain-from-group ((group Group))
  (multiple-value-bind (dom dom-table) (render-domain-body-from-group group)
    (values
     (union-set-from-str
      (format nil "[~(~a~)] -> ~a" (render-list (poly-dynamic-shape (group-polyhedron group))) dom))
     dom-table)))

(defun render-domain-from-loops (node domains &aux (isl (default-device :isl-expr)))
  (if domains
      (format nil "  T~a[~(~a~)] : ~(~a~);~%"
              (getattr node :idx)
              (render-list (map 'list #'(lambda (x) (getattr x :idx)) domains))
              (render-expr
               isl
               (reduce
                #'(lambda (x y) (make-expr :AND x y))
                (loop for dom in domains
                      collect (make-expr :>= (make-const (getattr dom :idx) nil) (getattr dom :upfrom))
                      collect (getattr dom :below)))))
      (format nil "  T~a[];~%" (getattr node :idx))))
;; iranai kamo?
(defmethod remove-duplicated-task ((group group))
  ""
  (let* ((pipeline-old (poly-pipeline (group-polyhedron group)))
         (pipeline-new (make-hash-table))
         (offset (length (hash-table-keys pipeline-old)))
         (seen nil))
    (loop for node in (graph-nodes (group-render-graph group)) do
      (case (node-type node)
        (:FUNCALL
         (if (find (getattr node :idx) seen)
             (let ((copied-graph (copy-graph (gethash (getattr node :idx) pipeline-old)))
                   (idx-new (+ offset (getattr node :idx) (length seen))))
               ;; DeepCopyできてない気がする，副作用あったらすまん
               (setf (graph-nodes copied-graph) (map 'list #'copy-node (graph-nodes copied-graph))
                     (getattr node :idx) idx-new
                     (gethash idx-new pipeline-new) copied-graph))
             (progn
               (push (getattr node :idx) seen)
               (setf (gethash (getattr node :idx) pipeline-new) (gethash (getattr node :idx) pipeline-old)))))))
    pipeline-new))

(defmethod render-band-node-in-domain ((group group) area related-domains idx2domain)
  "Creates a band node(mupa):
```
for (i=0; i<10; i++)
  for (j=0; j<10; j++)
    S1[i, j] // If you were here:
    for (k=0; k<10; k++)
      S[i, j, k]
```
-> then the partial schedule is:
```
[] -> {
  S1[i, j] -> [i, j];
  S2[i, j, k] -> [i, j];
}
```
"
  (let ((related-functions
          (loop for node in area
                if (eql (node-type node) :FUNCALL)
                  collect node))
        (declared-ids (map 'list #'(lambda (x) (getattr x :idx)) related-domains)))
    (multi-union-pw-aff-from-str
     (with-output-to-string (out)
       (format out "[~(~a~)] -> {~%" (render-list (poly-dynamic-shape (group-polyhedron group))))
       (loop for funcall in related-functions
             do (format out " ~a -> [~(~a~)];~%"
                        (or (gethash (getattr funcall :idx) idx2domain) (error ""))
                        (render-list declared-ids)))
       (format out "}")))))

(defun render-access-rep (reader type-reader group idx2domain)
  (union-map-from-str
   (with-output-to-string (out)
     (format out "[~(~a~)] -> {~%" (render-list (poly-dynamic-shape (group-polyhedron group))))
     (maphash
      #'(lambda (idx dom)
          (let ((graph-in-funcall (gethash idx (poly-pipeline (group-polyhedron group)))))
            (assert graph-in-funcall)
            (dolist (node (graph-nodes graph-in-funcall))
              (loop for var in (funcall reader node)
                    for typ in (funcall type-reader (read-type-relay node))
                    do (format out "  ~a -> ~(~a~)[~(~a~)];~%"
                               dom
                               var
                               (render-isl-aref typ :indexing #'isl-access-expr-no-stride :mutate-scalar t :flatten t :use-permute nil))))))
      idx2domain)
     (format out "}"))))

(defmethod scop ((group group))
  "Formulates the Polyhedral Model from scop/
Reference: https://www.researchgate.net/publication/347152973_PET-to-MLIR_A_polyhedral_front-end_for_MLIR

1. Schedule Construction
```
     [DOMAIN_NODE]
           |
      [BAND_NODE] (= corresponds to a loop in short)
           |
     [SEQUENCE_NODE]
       /       \
  [FILTER]  [FILTER] (= corresponds to an operation in short)
      |         |
        ...
```
"
  (let ((render-nodes (graph-nodes (group-render-graph group)))
        (deps (render-list (poly-dynamic-shape (group-polyhedron group)))))
    (multiple-value-bind (domain idx2domain) (render-domain-body-from-group group)
      (labels ((explore-schedule-tree (from to
                                       &key
                                         (parent-loops)
                                       &aux
                                         (schedule :nothing)
                                         (region (subseq render-nodes from to)))
                 (loop with count = from while (< count to)
                       for node = (nth count render-nodes) do
                         (ecase (node-type node)
                           (:FUNCALL
                            ;; -> Filter Node
                            (let* ((inst-schedule
                                     (schedule-from-domain
                                      (union-set-from-str
                                       (format nil "[~(~a~)] -> { ~a }" deps (render-domain-from-loops node parent-loops))))))
                              (setf
                               schedule
                               (if (eql schedule :nothing)
                                   inst-schedule
                                   (schedule-sequence schedule inst-schedule)))
                              (incf count)))
                           (:FOR
                            ;; -> Band Node
                            (let* ((endfor
                                     (find (getattr node :idx) (nthcdr from render-nodes)
                                           :key #'(lambda (x) (and (eql (node-type x) :ENDFOR) (getattr x :idx)))
                                           :test #'equalp))
                                   (_ (when (null endfor) (error "scop: malformed rendering graph ~a" render-nodes)))
                                   (endfor-abs-position
                                     (position
                                      (node-id endfor)
                                      render-nodes
                                      :key #'node-id)))
                              (declare (ignore _))
                              ;; for (...) {
                              ;;  T0[]       }
                              ;;  ...        } dom-schedule = schedule of this area
                              ;; }
                              (let ((dom-schedule
                                      (explore-schedule-tree
                                       (1+ count) endfor-abs-position
                                       :parent-loops `(,@parent-loops ,node))))
                                (setf
                                 schedule
                                 (if (eql schedule :nothing)
                                     dom-schedule
                                     (schedule-sequence schedule dom-schedule)))
                                (setf count endfor-abs-position)
                                ;; Move next to endfor
                                (incf count))))))
                 (when (eql schedule :nothing) (error "nothing was scheduled?"))
                 (let* ((partial-schedule
                          (render-band-node-in-domain group region parent-loops idx2domain)))
                   (setf schedule (schedule-insert-partial-schedule schedule partial-schedule)))
                 schedule))

        (values
         (union-set-from-str domain)
         (render-access-rep #'node-reads #'relay-reads group idx2domain)
         (render-access-rep #'node-writes #'relay-writes group idx2domain)
         (explore-schedule-tree 0 (length render-nodes)))))))
;; ~~ Creation/Conversion ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defmethod group->polyhedral-group ((group Group))
  (make-instance
   (if (group-realize-on-vm group)
       'Polyhedral-Group
       'Polyhedral-Auto-Scheduler)
   :base group))

(defmethod polyhedral-group->group ((polyhedral-group Polyhedral-Group))
  (polyhedral-group-base polyhedral-group))
;; ~~ Auto Scheduler ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [Design] ここの最適化は，RenderGraphとPipelineを書き換える最適化にとどめる
(defmethod tile-bands ((polyhedral-group Polyhedral-Auto-Scheduler) tile-sizes)
  
  )

(defmethod unroll-bands ((polyhedral-group Polyhedral-Auto-Scheduler) unroll-factors)

  )

#|
for(int _gid0=0;(_gid0<=4);_gid0+=1) {
  for(int _gid1=0;(_gid1<=4);_gid1+=1) {
    for(int _gid2=0;(_gid2<=9);_gid2+=1) {
      float val_54 = 0.0; // T0[_gid0, _gid1, _gid2]
      for(int _gid3=0;(_gid3<=9);_gid3+=1) {
        float val_41 = _gid2; // T1[_gid0, _gid1, _gid2, _gid3]
        boolean val_48 = !(val_41!=val_36[((5*_gid0)+_gid1)]); // T2[_gid0, _gid1, _gid2, _gid3]
        float val_34 = (val_48 ? val_30[((10*_gid2)+_gid3)] : 0.0); // T3[_gid0, _gid1, _gid2, _gid3]
        val_54 = (val_54+val_34); // T4[_gid0, _gid1, _gid2]
      }
      val_58[(((50*_gid1)+(10*_gid0))+_gid2)] = val_54; // T5[_gid0, _gid1, _gid2]
    }
  }
}
|#
;; yml de parse site pprint (schedule)

(defun debug/render-schedule (schedule)
  (let* ((schedule (schedule-set-options schedule :atomic))
	 (build (ast-build-from-context (set-from-str "{:}")))
	 (ast   (ast-build-node-from-schedule build schedule))
	 (p     (isl::%isl-printer-to-str (isl::context-handle isl::*context*)))
	 (p     (isl::%isl-printer-set-output-format p 4)) ;; 4 == Clang
	 (q     (isl::%isl-printer-print-ast-node p (isl::ast-node-handle ast)))
	 (str   (isl::%isl-printer-get-str q)))
    str))

(defun hoge ()
  (let* ((schedule1
           (schedule-from-domain
            (union-set-from-str "[] -> {
S1[i, j] : 0 <= i, j <= 1024;
S2[i, j, k] : 0 <= i, j, k <= 1024;
}")))
         (schedule2
           (schedule-from-domain
            (union-set-from-str "[] -> {
S1[i, j] : 0 <= i, j <= 1024;
S2[i, j, k] : 0 <= i, j, k <= 1024;
}"))))

    (setf schedule1
          (schedule-insert-partial-schedule
           schedule1
           (multi-union-pw-aff-from-str
            "[] -> {
S1[i, j] -> [i, j];
S2[i, j, k] -> [i, j];
}")))
    (setf schedule1
          (schedule-node-insert-filter
           (schedule-get-root schedule1)
           (union-set-from-str
            "[] -> { S1[i, j] }")))
    (print schedule1)))

