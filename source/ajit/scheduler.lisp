(in-package :caten/ajit)
;; scheduler.lisp: An entry point for JIT and Polyhedral Compiler.
;; = Refenreces: (good to read first before start coding) ======================================================================
;; - https://pliss2019.github.io/albert_cohen_slides.pdf
;; - https://www.slideshare.net/slideshow/introduction-to-polyhedral-compilation/70482946
;; - https://www.researchgate.net/publication/273651704_Schedule_Trees
;; - https://www.researchgate.net/publication/317826152_Scheduling_for_PPCG
;; - https://groups-google-com.translate.goog/g/isl-development/c/2bgepkLQBhY/m/BmiDq1nDAAAJ?_x_tr_sl=en&_x_tr_tl=ja&_x_tr_hl=ja&_x_tr_pto=sc
;; - (*) https://libisl.sourceforge.io/tutorial.pdf
;; - (*) https://libisl.sourceforge.io/manual.pdf
;; - (*) https://medium.com/@zhen8838/hands-on-polyherdal-affine-loop-fusion-ffb398b0ae60
;; - (*) https://github.com/zhen8838/isl_learn/blob/main/12_schedule_program.ipynb
;; - https://arxiv.org/pdf/2401.06665
;; - https://www.researchgate.net/publication/320992060_Consecutivity_in_the_isl_Polyhedral_Scheduler
;; (*) = recommended
;; = [Overview] ================================================================================================================
;;  Data Structure  |                     Process
;; -----------------|-----------------------------------------------------------------------------------------------------------
;;  AVM (Graph)     |                 [Input (%jit)] ( scheduler.lisp is an entry point )
;;                  |                        | (preprocessing, wmma detection, lowering) (-> simplifier.lisp, scheduled-items.lisp)
;;  Scheduled-Items |                [Scheduled-Items]
;;                  |                        | (group)      (-> group.lisp)
;; Group{Submodule} |                     [Group]
;;                  |                        | (pre-fusion) (-> here)
;; Group{Submodule} |                [Group (blueprint)] (A blueprint of kernel, consisted of VM Instruction and %for %endfor)
;;                  |                        | (fusion)     (-> multiexpr.lisp)
;; Group{Submodule} |                [Group (blueprint)] (VM Instruction is mutated into :EXPR)
;;                  |                        | (Extracting Polyhedral Structure) (-> scheduler.lisp, and isl-object.lisp)
;;  Polyhedral IR   | [Polyhedral IR (Describes the dependence between *groups)]
;;                  |                        | (Solving ILP Problem) (-> polyhedral.lisp)
;;  Polyhedral IR   |                 [Optimized ISL AST]
;;                  |                        | (Extracting the graph ...) (-> isl-ast-helper.lisp)
;; Rendering-Graph  |         [Rendering-Graph + EXPR + Pipeline]
;;                  |                        | (Optimizing the memory-locality by Solving DSA) (-> memory-planner.lisp)
;; Rendering-Graph  |         [Rendering-Graph + EXPR + Pipeline]
;;                  |                        | (Post-tiling optimization) (-> transform.lisp)
;; Rendering-Graph  |         [Rendering-Graph + EXPR + Pipeline] (but funcall is mutated into packed-funcall)
;;                  |                        | (Completed the process in caten/ajit)
;; Rendering-Graph  |                   [Rendering] (-> backends/clang.lisp, user-defined backends, device.lisp)
;;                  |                        |      (Users will use only two IRs: EXPR and Rendering-Graph)
;; AVM (Compiled)   |             [Output: :JIT_KERNEL] (-> kernel-info.lisp)
;; -- [Terms] -----------------------------------------------------------------------------------------------------------------
;; *Group{Submodule} = A blueprint of Polyhedral IR, consisted of VM Instruction, %for, and %endfor.
;; *VM Instruction   = nodes defined in `aasm/attrs.lisp`
;; *Group            = A set of aIR graph whose access are the equivalent
;; Polyhedral IR     = the struct Polyhedral in ./polyhedral.lisp
;; Rendering-Graph   = Rendering-Graph + Pipeline
;;                   - Rendering-Graph = A blueprint of the final kernel which each device handle.
;;                   - Pipeline: a hash table where key and value are timestamp and vm instruction respectively.
;; Scheduled-Items   = A set of the lowest-level instruction (e.g.: :EXPR, :ADD)  (-> scheduled-items.lisp)
;; Group             = A set of Scheduled-Items (that potentially can be fused in the single kernel)
;; Polyhedral        = A polyhedral structure of Group
;; i.e.: { [Group1, Polyhedral1], {Group2, Polyhedral2}, ...}
;;          ^ Applying JIT Compilation   ^ ...
;;                  in this group
;; etc ...
;; ============================================================================================================================
(defun pipeline->submodule-tree (pipeline)
  "Creates a tree structure of submodules based on the strongly connected components."
  (declare (type hash-table pipeline))
  (maphash
   #'(lambda (ts graph)
       (declare (ignore ts))
       (setf (graph-outputs graph) (nodes-output-ids (graph-nodes graph))))
   pipeline)
  (let ((graph
	  (apply
	   #'make-graph
	   (loop for time in (hash-table-keys pipeline)
		 for graph = (gethash time pipeline)
		 collect (make-node :TIME :GRAPH (graph-outputs graph) (graph-seen graph) :id time)))))
    (setf (graph-outputs graph) (nodes-output-ids (graph-nodes graph)))
    (->fast-graph graph)))
;; [TODO] Delete
(defun pipeline->timestamp (pipeline)
  (declare (type hash-table pipeline))
  (maphash
   #'(lambda (ts graph)
       (declare (ignore ts))
       (setf (graph-outputs graph) (nodes-output-ids (graph-nodes graph))))
   pipeline)
  (let ((graph
	  (apply
	   #'make-graph
	   (loop for time in (hash-table-keys pipeline)
		 for graph = (gethash time pipeline)
		 collect (make-node :TIME :GRAPH (graph-outputs graph) (graph-seen graph) :id time))))
	(lex (make-hash-table))
	(seen))
    ;; TODO: Relocate Isolated nodes w/ the end of nodes. (when debugging, should produce a warning)
    (labels ((explore (id &key (time 0) &aux (val (id->value graph id)))
	       (when (and val (null (find `(,time ,(node-id val)) seen :test #'equal)))
		 (push (list time (node-id val)) seen)
		 (let* ((key (getattr val :id)))
		   (mapc #'(lambda (x) (explore x :time (1+ time))) (remove-duplicates (node-reads val)))
		   (if (gethash key lex)
		       (push time (gethash key lex))
		       (setf (gethash key lex) (list time)))))))
      ;; Labelling the schedule dependency w/ lexicographical order
      ;; [TODO] that should look like below, not starting with `time`?
      ;; wanna consider this when optimizing backward process; it usually has multiple outputs.
      ;; 2  2    2    ...
      ;; \  /   /      |
      ;;   1   1       4
      ;;    \ /        |
      ;;     0         3
      ;; 
      (loop for time upfrom 0
	    for id in (nodes-output-ids (graph-nodes graph))
	    do (explore id :time time))
      (assert (every #'(lambda (x) (find x seen :key #'second)) (map 'list #'node-id (graph-nodes graph))))
      (let ((tree-max-depth (apply #'max (apply #'append (hash-table-values lex)))))
	(maphash
	 #'(lambda (x y)
	     (setf (gethash x lex) (apply #'min (map 'list #'(lambda (n) (- tree-max-depth n)) y))))
	 lex)
	lex))))

(declaim (ftype (function (AVM &key (:verbose boolean)) (values list)) create-schedules-from-avm))
(defun create-schedules-from-avm (avm &key (verbose nil))
  "Step1, Creates an initial schedule.
Input: AVM
Output: Groups"
  (declare (type avm avm) (type boolean verbose))
  ;; Trace the view and dtype information.
  (let* ((type-map (run-type-infer avm)) (*recursive-find-seen* nil) (seen nil))
    (when verbose
      (format t "Verbose: Initial Computation Graph[Forward/Backward]~%")
      (uiop:symbol-call (find-package :caten) :print-avm avm))
    ;; ~~ JIT Specific Graph rewriting Processes ~~~~~~~~~~~~~~~~~~~~
    (deploy-type-infer-results avm type-map) ;; Move buffer/view nodes into :_type_relay attribtutes
    (relocate-independent-loop-bound-computation! (avm-graph avm)) ;; for (...;by+=a*b) is equivalent to for(...;by+=val_xx)
    (apply-jit-specific-simplifiers avm)     ;; Purge :view nodes, WMMA Accumlation, contiguous elimination etc...
    (when verbose
      (format t "Verbose: Simplified Graph[Forward/Backward]~%")
      (uiop:symbol-call (find-package :caten) :print-avm avm))
    ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ;; (The comment below is out-of-date. we will create more than 2 groups)
    ;; Creating a Polyhedral Compilation Group: (Group1 = Forward, Group2=Backward)
    ;; Assume there's only two groups for simplicity:
    ;;  - Forward Computation
    ;;  - Backward Computation
    ;; If we are in the mood of implementing second-order derivatives, create another group here.
    ;; So, in the early stage, we will create a list of save-for-backward, and apply multi-grouping optimization.
    ;; We also assumed that all custom kernels are scheduled as a scalar function having vector array on ISL,
    ;; there is no need to consider the situation that where a complete array used in forward, is required by another kernel
    ;; except for backward. (that's why we only create a list of save-for-backward)
    (labels ((id->buffer (graph)
	       #'(lambda (id)
		   (assert (symbolp id) () "Graph should not return a number!")
		   (let ((node (id->value graph id)))
		     (list node (car (relay-writes (read-type-relay node))) id))))
	     (make-top-schedule (group) (map 'list (compose #'make-scheduled-items (id->buffer (group-graph group))) (group-writes group)))
	     (schedule (group schedules)
	       (multiple-value-bind (sorted seen-new)
		   (schedule/resolve-isolated-ops
		    (reverse (flatten (map 'list #'(lambda (x) (recursive-find-group (group-graph group) x)) schedules)))
		    seen)
		 (setf seen (append seen-new (group-writes group)))
		 sorted))
	     (seen-in-groups (group &aux (seen-in-groups nil))
	       (if (group-sched group)
		   (loop for nth upfrom 0
			 for s in (group-sched group)
			 do (dolist (node (si-nodes s))
			      (unless (eql (node-type node) :Allocate)
				(setf seen-in-groups (append seen-in-groups (node-writes node)))))
			    (setf (si-name s) (intern (format nil "T~a" nth) "KEYWORD")))
		   (loop for node in (graph-nodes (group-graph group))
			 unless (eql (node-type node) :Allocate)
			   do (setf seen-in-groups (append seen-in-groups (node-writes node)))))
	       (remove-duplicates seen-in-groups))
	     (read-in-groups (group &aux (read-in-groups nil))
	       (if (group-sched group)
		   (loop for s in (group-sched group) do
		     (dolist (node (si-nodes s))
		       (dolist (r `(,@(node-reads node) ,@(getattr node :_loop_bound_nodes))) (when (symbolp r) (push r read-in-groups)))))
		   (loop for node in (graph-nodes (group-graph group)) do
		     (dolist (r `(,@(node-reads node) ,@(getattr node :_loop_bound_nodes))) (when (symbolp r) (push r read-in-groups)))))
	       (remove-duplicates read-in-groups)))
      (relocate-independent-allocations! (avm-graph avm))
      (let* ((groups (loop for g in (group/resolve-dependencies (split-into-subgroups (avm-graph avm)))
			   if (graph-nodes (group-graph g))
			     collect g)))
	(loop for group in groups
	      if (group-realize-on-vm group)
		do (setf seen (append seen (group-writes group)))
	      else
		collect (setf (group-sched group) (schedule group (make-top-schedule group))))
	(loop with write-deps = (map 'list #'seen-in-groups groups)
	      with read-deps = (map 'list #'read-in-groups groups)
	      for nth upfrom 1
	      for group in groups
	      for writing in write-deps
	      collect (setf (group-across-time-deps group)
			    (intersection writing `(,@(avm-fw-outputs avm)
						    ,@(avm-bw-outputs avm)
						    ,@(apply #'append (nthcdr nth read-deps))))))
	(mapc
	 #'(lambda (x)
	     (unless (group-realize-on-vm x)
	       (apply-multiexpr-grouping (group-sched x) (group-across-time-deps x))))
	 groups)

	(when verbose
	  (loop for group in groups
		for nth upfrom 0 do
		  (format t "~%= Verbose: ~ath group === ~%" nth)
		  (if (group-realize-on-vm group)
		      (print group)
		      (print-schedules (group-sched group)))))
	groups))))

(declaim (ftype (function (function AVM group &key (:verbose boolean) (:verbose-auto boolean)) (values Polyhedral)) create-polyhedron-from-group))
(defun create-polyhedron-from-group (alias-f avm group &key (verbose nil) (verbose-auto nil))
  "Step2, create a polyhedron from the scheduled items."
  (declare (type group group) (type boolean verbose))
  (let* ((submodule (map 'list #'schedule->submodule (group-sched group))) ;; Rendering :FOR and :ENDFOR
	 (pipeline (make-hash-table)))
    ;; Pipeline: Task_Idx -> FUNCALL_{IDX}(depending_args)
    ;; Task_Idx -> Submodule Graph
    (loop for nth upfrom 0
	  for s in submodule
	  do (setf (gethash nth pipeline) s))
    (when verbose
      (format t "== [Final Graph Before Applying Polyhedral Compiler] ======~%")
      (print-pipeline pipeline))
    (let ((fused-blueprints (apply-pre-fusion pipeline)))
      ;; Pre-FuseされたIRは，ISLのInitialScheduleを作成するためだけに用いる
      ;; + PreFused IR -> ISL AST -> Rendering-Graphなのを頭に入れておく
      (print "FUSED")
      (print-submodule fused-blueprints t)
      )
    
    ;; ここでPre-Fusionを挟む
    ;; とりあえずここをリファクタする方針
    (let* ((vm-inputs (avm-gather-args avm))
	   ;;(vm-input-tensors (nodes-depends-on (graph-nodes (group-graph group))))
	   (loop-sizes (loop for value being the hash-values of pipeline
			     collect (graph->loop-size value)))
	   (loop-size (apply #'append loop-sizes))
	   (dynamic-shapes (remove-duplicates `(,@vm-inputs ,@loop-size)))
	   (domain         (render-domain pipeline :depends-on dynamic-shapes))
	   ;;(dynamic-shapes (remove-duplicates `(,@dynamic-shapes ,@vm-input-tensors)))
	   (read-access  (render-access alias-f :read pipeline :depends-on dynamic-shapes))
	   (write-access (render-access alias-f :write pipeline :depends-on dynamic-shapes)))
      (multiple-value-bind (schedule lex-table) (isl-initial-schedule pipeline :depends-on dynamic-shapes)
	(when verbose-auto
	  (format t "== [Domain] ===========")
	  (format t "~%~a~%" domain)
	  (format t "== [Read Accesses] =======")
	  (format t "~%~a~%" read-access)
	  (format t "== [Write Accesses] ======")
	  (format t "~%~a~%" write-access)
	  (format t "== [Initial Scheduling domain (=domain)] ======")
	  (format t "~%~a~%" schedule))
	(make-polyhedral avm pipeline domain read-access write-access schedule vm-inputs (group-writes group) lex-table)))))

(declaim (ftype (function (Polyhedral &key (:verbose boolean) (:serialize boolean)) Polyhedral) auto-schedule!))
(defun auto-schedule! (polyhedral &key (verbose nil) (serialize nil))
  "
Step3, autoschedule polyhedron model.
Options:
- debug[boolean]:  If this option is set, this function prints the Polyhedron Model for each step of the optimization.
- serialize[boolean]: If this option is set, then all strongly connected components in the dependence
  graph are serialized as soon as they are detected. This means in particular that
  instances of statements will only appear in the same band node if these statements belong to
  the same strongly connected component at the point where the band node is constructed."
  (declare (type Polyhedral polyhedral)
	   (type boolean verbose serialize))
  (macrolet ((debug-print (step-name) `(when verbose (format t "~%[~a]~%~a~%" ,step-name (print-polyhedral polyhedral nil)))))
    (debug-print "Initial")
    ;; Loop Fusion
    (poly/schedule polyhedral :serialize serialize)
    (debug-print "Scheduled")
    polyhedral))

(declaim (ftype (function (Group keyword) graph) finalize-and-retrive-graph))
(defun finalize-and-retrive-render-graph (group backend)
  "Step4, Extract the schedule from ISL."
  (declare (type group group))
  (multiple-value-bind (ast bands) (finalize-schedule (group-polyhedron group))
    (create-rendering-graph ast bands backend (max-dimension-in-group group))))

(defstruct (Compiled-Kernel
	    (:conc-name ck-)
	    (:constructor make-compiled-kernel (name args code fcaller-list group)))
  (group group :type group)
  (name name :type keyword)
  (args args :type list)
  (code code :type string)
  (fcaller-list fcaller-list :type list))

(defun render-to-string (backend group name-prefix avm debug kernels &aux (base-name (avm-name avm)))
  "Step5, rendering the graph.
(values cffi-name body foreign-function-caller compile-function-lambda)"
  (when (group-realize-on-vm group) (return-from render-to-string (values (list group) "")))
  (assert (listp kernels))
  (let ((code ""))
    (values
     (loop for kernel in kernels
	   for nth upfrom 0
	   for name = (setf (avm-name avm) (intern (format nil "~a_~a_k~a" base-name name-prefix (kernel-renderer-nth kernel)) "KEYWORD"))
	   for body = (%render-body backend backend (apply #'make-graph (kernel-renderer-nodes kernel))
				    (group-polyhedron group) 1 (kernel-renderer-args kernel))
	   for function = (%render-function backend avm (kernel-renderer-args kernel) body)
	   collect
	   (progn
	     (setf code (format nil "~a~%~a~%" code function))
	     (make-compiled-kernel name (kernel-renderer-args kernel)
				   function (%render-function-caller backend avm (kernel-renderer-args kernel)) group)))
     (progn
       (when (>= debug 1) (format t "Compiled[~a]:~%~a" name-prefix code))
       (setf (avm-name avm) base-name)
       code))))

(defun jit->vm (backend compiled-kernels)
  "Step5, collects the related nodes."
  (loop for kernel in compiled-kernels
	append
	(etypecase kernel
	  (Compiled-Kernel
	   (list
	    (make-fused-kernel-caller (ck-name kernel) (ck-args kernel) (compile nil (ck-fcaller-list kernel))
				      (ck-fcaller-list kernel)
				      (ck-code kernel) backend (count-n-kernels (group-render-graph (ck-group kernel))))))
	  (Group (graph-nodes (group-graph kernel))))))

(defun %jit (avm
	     &key
	       (debug (ctx:getenv :JIT_DEBUG))
	       (serialize (= 1 (ctx:getenv :SERIALIZE)))
	       (backend (or (ctx:getenv :JIT_BACKEND) :clang))
	       (compile-later nil)
	       (dir nil)
	     &aux
	       (backend (if (keywordp backend)
			    (default-device backend)
			    backend))
	       (verbose-schedule (or (= debug 2) (= debug 4)))
	       (verbose-auto (or (= debug 4) (= debug 3))))
  "An entry point for JIT
Applies the jit, returning the compiled code.
DEBUG=1 to see the compiled code
DEBUG=2 to debug the scheduling process
DEBUG=3 to debug the ISL process
DEBUG=4 to debug both DEBUG=3 and DEBUG=4."
  (declare (type avm avm)
	   (type (integer 0 4) debug)
	   (type boolean serialize))
  (with-isl-context
    (let* ((groups (create-schedules-from-avm avm :verbose verbose-schedule))
	   (alias (create-reduction-alias-f (graph-nodes (avm-graph avm))))
	   (groups (loop for group in groups
			 if (group-realize-on-vm group) collect group
			   else if (group-sched group) do
			     (setf (group-polyhedron group) (create-polyhedron-from-group alias avm group :verbose verbose-schedule :verbose-auto verbose-auto))
				and collect group)))
      (mapc
       #'(lambda (x)
	   (when (group-polyhedron x)
	     (auto-schedule! (group-polyhedron x) :verbose verbose-auto :serialize serialize)
	     (funcall (compose #'remove-iteration-ir #'poly-pipeline #'group-polyhedron) x)
	     (setf (group-render-graph x) (finalize-and-retrive-render-graph x backend))))
       groups)
      (let* ((mp (make-instance 'MemoryPlanner :avm avm :groups groups :debug debug :device backend))
	     (_ (memory-plan mp))
	     (kernels (retrive-kernels mp))
	     (blueprints/codes
	       (loop for group in groups
		     for kernel in kernels
		     for nth upfrom 0
		     collect
		     (multiple-value-list (render-to-string backend group (format nil "e~a" nth) avm debug kernel))))
	     (final-code (%render-program-toplevel backend (with-output-to-string (out) (dolist (c blueprints/codes) (princ (second c) out))))))
	(declare (ignore _))
	(when (>= (ctx:getenv :JIT_DEBUG) 2)
	  (format t "Final JIT Schedule:~%")
	  (loop for nth upfrom 0
		for kr in kernels do
		  (format t "~%=== nth=~a ======" nth)
		  (if (group-p kr)
		      ;;(print (group-graph kr))
		      nil
		      (dolist (k kr) (print (kernel-renderer-nodes k))))))
	(unless compile-later (%render-compile backend avm final-code dir))
	(list
	 (map 'list #'car blueprints/codes) final-code mp
	 (loop for kr in kernels
	       if (listp kr)
		 append
		 (loop for k in kr append (kernel-renderer-args k))))))))

(defun jit (base-avm
	    &key
	      (debug (ctx:getenv :JIT_DEBUG))
	      (serialize (= 1 (ctx:getenv :SERIALIZE)))
	      (backend (or (ctx:getenv :JIT_BACKEND) :clang))
	      (dir nil)
	    &aux
	      (_ (apply-static-gensym base-avm))
	      (backend (if (keywordp backend)
			   (default-device backend)
			   backend))
	      (avm (deepcopy-avm base-avm)))
  "Apply the jit compilation to the given avm."
  (declare (type avm avm)
	   (type (integer 0 4) debug)
	   (type boolean serialize)
	   (ignore  _))
  (multiple-value-bind (compiled-kernels code mp kernel-args)
      (apply #'values (%jit avm :debug debug :serialize serialize :backend backend :compile-later nil :dir dir))
    (declare (ignore code))
    (make-avm
     (clean-up-attrs
      (optimize-non-in-place-buffers
       base-avm avm mp
       (remove-unused-allocs
	(apply
	 #'make-graph
	 (apply #'append (map 'list #'(lambda (x) (jit->vm backend x)) compiled-kernels))))
       (nodes-gather-args (graph-nodes (avm-graph avm)))
       (or (= debug 2) (= debug 4))
       kernel-args))
     (avm-name avm)
     (avm-id2tensor avm)
     (avm-fw-outputs avm)
     (avm-bw-outputs avm))))

(defun compile-isl (&key domain read write schedule (ast-option :separate))
  (let ((poly (make-polyhedral (make-avm (make-graph) :x (make-hash-table) nil nil)
			       (make-hash-table)
			       domain
			       read
			       write
			       (union-map-from-str schedule)
			       nil
			       nil
			       (make-hash-table)
			       :ast-option ast-option)))
    (auto-schedule! poly)
    (print (schedule-get-root (poly-schedule poly)))
    (print (debug/render-c poly))))

#+(or)
(compile-isl
 :domain
 "[] -> {
  T0[_gid0, _gid1] : 0 <= _gid0 <= 10 and 0 <= _gid1 <= 10;
  T1[_gid0, _gid1] : 0 <= _gid0 <= 10 and 0 <= _gid1 <= 10;
}"
 :read
 "
[] -> {
  T1[_gid0, _gid1] -> a[-1 * _gid0 + 10, -1 * _gid1 + 10]
}
"
 :write
 "
[] -> {
  T0[_gid0, _gid1] -> a[_gid0, _gid1];
}
"
 :schedule
 "
{
  T0[_gid0, _gid1] -> [0, _gid0, 0, _gid1];
  T1[_gid0, _gid1] -> [1, _gid0, 1, _gid1];
}"
 :ast-option :atomic)


;; Loop Collapse https://github.com/zhen8838/isl_learn/blob/main/10_loop_transformation.ipynb
;; (union-set-apply domain xxx)
;; [TODO] Test w/
;; Including Test
;; !softmax
;; !matmul !matmul
;; !sin (!matmul)
;; Embedding
;; Better Visualization ...

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
;; loop-fusion (Poly1, Poly2)
;; [TODO]
;; 1, Fuse Manually
;; 2, better visualization
;; 3, post-fusion
;; Implement Post Fusion
;; ISL is a tool to analyze tiling/parallelizing

;; - Prep refactor: ISL Renderer: Rendering Graphに対して実装する
;; Submodule -> Fused Graph
;; IR:FOR IR/ENDFOR -> 不要なので削除する

;; Loop-Collapseは最後に判定する
;; DomainのAccess RepとNRankでFlattenしたMapが同じならFlattenn
;; TODO List
;; - [ ] MultiExpr (Better)
;; - [ ] Loop Fusion (Ahead of poly ir)
;; - [ ] MultiExpr (Fuseされたらacross-timeの時間依存が変わるはず)
;; - [ ] CMP Ops MultiExpr?
;; - [ ] Refactor

(defmethod loop-depth ((lp Graph))
  "Returns a count of the maximum depth in the rendering graph lp."
  (loop with depth = 0
	for node in (graph-nodes lp)
	if (find (node-type node) `(:IR/FOR))
	  do (incf depth)
	else if (find (node-type node) `(:IR/ENDFOR))
	       do (decf depth)
	end
	maximize depth))

(defmethod expr-is-index-component-p ((node node))
  (or
   (eql (node-type node) :INDEX-COMPONENTS)
   (and
    (eql (node-type node) :EXPR)
    (let ((expr (getattr node :EXPR)))
      (eql (expr-op expr) :INDEX-COMPONENTS)))))

(defmethod print-submodule ((graph Graph) stream)
  (format
   stream
   "Graph{Submodule}<~%~a>"
   (with-output-to-string (out)
     (flet ((indent (n) (with-output-to-string (o) (dotimes (i n) (princ " " o)))))
       (loop with indent = 0
	     for node in (graph-nodes graph)
	     if (eql (node-type node) :IR/FOR)
	       do (format out "~afor ~(~a~)=0..~a {~%" (indent indent) (car (node-writes node)) (nth 1 (node-reads node))) (incf indent 2)
	     else if (eql (node-type node) :IR/ENDFOR)
	       do (decf indent 2) (format out "~a}~%" (indent indent))
	     else
	       do (format out "~aop[~a];~%" (indent indent) (if (eql :EXPR (node-type node)) (getattr node :expr) (node-type node))))))))
;; submodule-sequence
;;
;; for (int i 0..0) {
;;   T1 0 -> 10 affine ax+b
;; }
;;   T2 10 -> 1
;; TODO: 1 Loop 1 Reduction, and confrim the validyf ot view
;; [TODO]
;; - !mean, LHS Expand, Viewの判定: 一つのScheduleGroupは同一のViewObjectを持っていると仮定する，(%forはその情報を保持することでFusion判定が可能)
;; - [ ] 1. EmbeddingのINDEX-COMPONENTSをFuseする
;; - [ ] 2. Sumのload 0.0 (softmax, composed matmul, matmul sin)をFuseする
;; - [ ] 3. ISLにDumpしてSchedulingをする，
;; - [ ] 4. ISLがInner LoopのFusionをうまくやってくれる場合
;;    -> そのままテストを通す
;;    -> otherwise, submodule-sequenceのTODOを試す
;;
(defmethod submodule-sequence ((older Graph) (younger Graph) (offset fixnum))
  "Fuses the strongly connected two components: Loop1 and Loop2.
[older]
   |
[younger]
   |
younger is always plain
"
  (let* ((younger-deps
	   (loop for node in (graph-nodes younger)
		 if (eql (node-type node) :IR/FOR)
		   collect node))
	 (younger-instructions
	   (loop for node in (graph-nodes younger)
		 if (null (find (node-type node) `(:IR/FOR :IR/ENDFOR)))
		   collect node)))
    ;; younger-instructions must be placed after older-instruction
    (labels ((older-instructions ()
	       (loop for node in (nthcdr offset (graph-nodes older))
		     if (null (find (node-type node) `(:IR/FOR :IR/ENDFOR)))
		       collect node))
	     (find-and-insert-at-suitable-place (inst
						 &aux
						   (index-component-p
						    (or
						     (expr-is-index-component-p inst)
						     (let ((oi (older-instructions)))
						       (and
							(= (length oi) 1)
							(expr-is-index-component-p (car oi))))))    
						   (bands (make-hash-table))
						   (ops nil)
						   (changed-p nil))
	       ;; Reading the older graph from offset to an end
	       ;; Finds a suitable place to locate inst
	       ;; we refer a suitable place as:
	       ;; - Satisfies band requirement              (required _gid is defined)
	       ;; - Satisfies instruction-order requirement (inst is placed after all older-instruction)
	       (labels ((satisfy-1 ()
			  ;; Satisfies band-requirement?
			  (and
			   (= (length (hash-table-keys bands)) (length younger-deps))
			   (if index-component-p
			       ;; INDEX-COMPONENT can be located regardless of size. (consider this as just an alias for loop index, its just a scalar)
			       (every #'(lambda (x) (gethash x bands)) (map 'list (compose #'car #'node-writes) younger-deps))
			       nil)))
;;			       (every
;;				#'(lambda (x &key (id (car (node-writes x))))
;;				    (and
;;				     (gethash id bands)
;;				     (let ((lp (gethash id bands)))
;;				       ;; Band sizes are the equivalent?
;;				       (equal (node-reads lp) (node-reads x)))))
;;				younger-deps))))
			(satisfy-2 ()
			  ;; Satisfies instruction requirement?
			  (= (length ops) (length (older-instructions))))
			(satisfy-3 ()
			  (and
			   (satisfy-2)
			   ;; Satisfies the satisfies-1 except for the last dimension.
			   (= (length (hash-table-keys bands)) (1- (length younger-deps)))
			   (every
			    #'(lambda (x &key (id (car (node-writes x))))
				(and
				 (gethash id bands)
				 (or
				  (getattr x :_scalar_p)
				  (getattr (gethash id bands) :_scalar_p)
				  (equal (node-reads (gethash id bands)) (node-reads x)))))
			    (butlast younger-deps))))
			(update-size (bs &aux (tgt (gethash (car (node-writes bs)) bands)))
			  (when (and tgt (getattr tgt :_scalar_p))
			    (setf (node-reads tgt) (node-reads bs)
				  (getattr tgt :_scalar_p) (getattr bs :_scalar_p)))))
		 (values
		  (nconc
		   (subseq (graph-nodes older) 0 offset)
		   (loop for node in (nthcdr offset (graph-nodes older))
			 if (eql (node-type node) :IR/FOR)
			   do (setf (gethash (car (node-writes node)) bands) node)
			 else if (eql (node-type node) :IR/ENDFOR)
				do (remhash (car (node-reads node)) bands)
			 else
			   do (push node ops)
			 end
			 collect node
			 ;; 一旦SerializeしてISLに投げてFusionしてくれるかを確認する
			 ;; 無理だったら手動でILPを解く: ReductionとViewによってdependenceが壊れるのを防ぐのが目的
			 if (and (null changed-p) (satisfy-1) (satisfy-2))
			   do (setf changed-p t) (mapc #'update-size younger-deps) and collect inst
			 if (and (null changed-p) (satisfy-3))
			   do (setf changed-p t) (mapc #'update-size younger-deps)
			   and append `(,@(last younger-deps) ,inst ,(%endfor (car (node-writes (car (last younger-deps))))))))
		  changed-p)))
	     (create-and-merge-new-domain (inst)
	       (setf offset (length (graph-nodes older))
		     (graph-nodes older)
		     (nconc
		      (graph-nodes older)
		      `(,@younger-deps
			,inst
			,@(nreverse
			   (loop for node in younger-deps
				 collect (%endfor (car (node-writes node))))))))
	     older))
      ;; younger-instructions: each ops depends on younger-deps
      ;; INDEX-COMPONENTS can be relocated anywhere.
      ;; Find insertable location from older and insert if not found create a new loop
      (dolist (inst younger-instructions)
	(multiple-value-bind (new-graph changed-p)
	    (find-and-insert-at-suitable-place inst)
	  (if changed-p
	      (setf older (apply #'make-graph new-graph))
	      (setf older (create-and-merge-new-domain inst)))))
      (values older offset))))
;; node_id -> pipeline?
(defmethod apply-pre-fusion ((pipeline hash-table))
  (let* ((order (sort (hash-table-keys pipeline) #'<))
	 (out   (apply #'make-graph (graph-nodes (gethash (car order) pipeline))))
	 (offset 0))
    (dolist (id (cdr order))
      (multiple-value-bind (out-new offset-new) (submodule-sequence out (gethash id pipeline) offset)
	(setf out out-new offset offset-new)))
    out))
