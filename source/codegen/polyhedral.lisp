(defpackage :caten/codegen/polyhedral
  (:shadow #:set #:space)
  (:shadowing-import-from :cl :map)
  (:use :cl :caten/air :caten/aasm :caten/isl :caten/codegen/byoc :caten/common.logger :caten/common.pprinter)
  (:import-from :caten/codegen/renderer #:render-node #:Default-Renderer)
  (:export
   #:realize-node-with-autotuning
   #:make-polyhedral-from-blueprint
   #:get-blueprint-from-polyhedral)
  (:export
   #:apply-optimization
   #:NoOpt
   #:Reschedule
   #:Interchange
   #:Tile
   #:TileGPU
   #:Parallel
   #:Collapse
   #:TensorCore
   #:SplitReduce
   #:Vectorize)
  ;; GFlops Mesaurer
  (:export
   #:GFlops-Measurer
   #:GFlops-Measurer-ops
   #:GFlops-Measurer-succeed-p
   #:compute-gflops
   #:schedule-item-gflops))

(in-package :caten/codegen/polyhedral)

(defparameter *allow-compilation-error-during-beam* nil)
(defparameter *+inf* (expt 2 32))
;;; ~~~~ GFlops Measurements (Not Tested) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defstruct GFlops-Measurer
  "A helper object to compute GFlops"
  (ops (error "flops must occur") :type (or null caten/aasm/expr:Expr))
  (succeed-p t :type boolean))
(defun cannot-compute-flop () (make-gflops-measurer :ops nil :succeed-p nil))
(defmethod compute-gflops ((gfm GFlops-Measurer) elapsed params)
  (when (null (gflops-measurer-succeed-p gfm)) (return-from compute-gflops nil))
  (assert (gflops-measurer-ops gfm))
  (when (zerop elapsed) (return-from compute-gflops nil)) ;; Elapsed Time = 0.0
  (let* ((ops (apply #'caten/aasm/expr:expr-realize (gflops-measurer-ops gfm) params))
         (_ (assert (numberp (caten/runtime:buffer-value ops)) () "measure-gflpos: the result is not a number."))
         (gflops (/ (caten/runtime:buffer-value ops) (* elapsed 1e9))))
    (declare (ignore _))
    gflops))
(defmethod schedule-item-gflops (blueprint &aux (total-flops))
  (let ((ctx (make-scop-ctx-from-blueprint blueprint)))
    (loop for expr in (ctx-exprs ctx)
          for expr-graph = (caten/aasm::ast-expr-graph blueprint expr) do
            (let ((flop (caten/aasm/expr:expr-const (caten/aasm/expr::nodes-flops (graph-nodes expr-graph)) :int64))
                  (volume
                    (reduce
                     #'caten/aasm/expr:expr-mul
                     (loop for loop-info in (gethash (node-id expr) (ctx-node-to-loops ctx))
                           for loop = (getf loop-info :for-node)
                           for range = (id->value blueprint (car (node-reads loop)))
                           for size = (car (node-reads range))
                           for step = (second (node-reads range))
                           for size-expr = (id->value blueprint size)
                           for step-expr = (id->value blueprint step)
                           for size-graph = (if (numberp size) (caten/aasm/expr:expr-const size :int64) (caten/aasm/expr:expr-from-graph (car (node-reads size-expr)) blueprint))
                           for step-graph = (if (numberp step) (caten/aasm/expr:expr-const step :int64) (caten/aasm/expr:expr-from-graph (car (node-reads step-expr)) blueprint))
                           collect
                           (caten/aasm/expr:expr-div size-graph step-graph)))))
              (push (caten/aasm/expr:expr-mul flop volume) total-flops)))
    (let ((ops (reduce #'caten/aasm/expr:expr-add total-flops)))
      (make-gflops-measurer :ops ops :succeed-p t))))
;; ~~ Directive ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass Directive ()
  ((type :initarg :type :accessor directive-type)
   (amount :initarg :amount :accessor directive-amount)
   (depth :initarg :depth :accessor directive-depth)
   (visible :initarg :visible :accessor directive-visible))
  (:documentation "Directive is an instruction to schedule-node-band. This class is dumpable as a string to interoperate with ISL."))

(defun directive (type amount depth visible)
  (declare (type string type) (type fixnum amount) (type boolean visible))
  (make-instance 'Directive :type type :amount amount :depth depth :visible visible))

(defmethod print-object ((directive Directive) stream)
  (print-unreadable-object (directive stream)
    (format stream "~a" (directive->str directive))))

(defmethod directive->str ((directive Directive))
  (with-output-to-string (out)
    (format out "@DIRECTIVE(")
    (loop with slots = (c2mop:class-slots (class-of directive))
          for slot-def in slots
          for slot-name = (c2mop:slot-definition-name slot-def)
          for value     = (slot-value directive slot-name)
          for idx upfrom 0 do
            (format out "~a=~a" (string-upcase (princ-to-string slot-name)) value)
            (when (< idx (1- (length slots))) (format out ",")))
    (format out ")")))

(defmethod directive->id ((directive directive)) (isl::make-id-from-str (directive->str directive)))

(defun split-key-and-value (str)
  (let ((pos (position #\= str)))
    (assert pos)
    (let ((key (intern (subseq str 0 pos) "KEYWORD"))
          (value (subseq str (1+ pos))))
      (list
       key
       (case key
         (:TYPE value)
         ((:AMOUNT :DEPTH) (parse-integer value))
         (:VISIBLE (string= (string-upcase value) "T"))
         (otherwise value))))))

(defun split-directive-string (str)
  (let ((res '()) (start 0) (len (length str)))
    (loop for pos = (position #\, str :start start)
          do (cond
               ((null pos)
                (push (subseq str start len) res)
                (return-from split-directive-string (map 'list #'split-key-and-value (nreverse res))))
               (t
                (push (subseq str start pos) res)
                (setf start (1+ pos)))))))

(defmethod str->directive ((string string))
  ;; @DIRECTIVE(...) is a valid format.
  (unless (and (uiop:string-prefix-p "@DIRECTIVE(" string) (char= (char string (1- (length string))) #\))) (error "Invalid directive string: ~S" string))
  (let* ((content (subseq string #.(length "@DIRECTIVE(") (1- (length string)))))
    (apply #'make-instance 'Directive (apply #'append (split-directive-string content)))))
;; ~~ Polyhedral ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;;; blueprint -> polyhedral
(defclass Polyhedral-IR ()
  ((schedule :accessor poly-schedule :initarg :schedule)
   (domain   :accessor poly-domain :initarg :domain)
   (dependencies :accessor poly-dependencies :initarg :dependencies)
   (cmd-history :accessor poly-cmd-history :initform nil :initarg :history)
   (blueprint :accessor poly-blueprint :initarg :blueprint)
   (ctx :accessor poly-ctx :initarg :ctx)
   (extra-buffer-allocs :accessor poly-extra-allocs :initform nil)
   (strategy :accessor poly-strategy :initarg :strategy)
   (bp-cache :accessor poly-bp-cache)))

(defun make-polyhedral-ir (blueprint domain read write schedule ctx strategy)
  (let ((pg (make-instance 'Polyhedral-IR :ctx ctx :schedule schedule :domain domain :blueprint blueprint :strategy strategy)))
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
           (dependencies (union-map-union (union-map-union WaR RaW) WaW)))
      (setf (poly-dependencies pg) dependencies)
      pg)))

(defmethod poly-clone-for-next-generation ((pg Polyhedral-IR))
  (make-instance 'Polyhedral-IR :schedule (copy (poly-schedule pg)) :history (copy-list (poly-cmd-history pg)) :dependencies (poly-dependencies pg) :domain (poly-domain pg) :blueprint (poly-blueprint pg) :ctx (poly-ctx pg) :strategy (poly-strategy pg)))

(defmethod poly-make-schedule-constraints ((pg Polyhedral-IR))
  (let* ((sc (schedule-constraints-on-domain (poly-domain pg)))
         (sc (schedule-constraints-set-coincidence sc (poly-dependencies pg)))
         (sc (schedule-constraints-set-validity sc (poly-dependencies pg)))
         (sc (schedule-constraints-set-proximity sc (poly-dependencies pg))))
    sc))

(defmethod poly-get-rank ((pg Polyhedral-IR))
  (count :RANGE (graph-nodes (poly-blueprint pg)) :key #'node-type))

(defun gid (n) (intern (format nil "_gid_p~a" n)))

(cffi:defcallback apply-set-separate-loop :pointer
    ((schedule-node :pointer) (user :pointer))
  (declare (ignore user))
  (if (eql (isl::%isl-schedule-node-get-type schedule-node) :schedule-node-band)
      (let ((n (isl::%isl-schedule-node-band-n-member schedule-node)))
        (dotimes (i n) (setf schedule-node (isl::%isl-schedule-node-band-member-set-ast-loop-type schedule-node i 1)))
        schedule-node)
      schedule-node))

(defun schedule-set-separate (schedule)
  (isl::%%make-schedule
   (isl::%isl-schedule-map-schedule-node-bottom-up (isl::schedule-handle schedule) (cffi:callback apply-set-separate-loop) (cffi:null-pointer))))

(defun ->ast (schedule rank)
  (macrolet ((set-option (name level)
	       `(cffi:foreign-funcall ,(format nil "isl_options_set_~(~a~)" name)
				 :pointer (isl::context-handle isl::*context*)
				 :int ,level
				 :void)))
    (set-option "ast_build_atomic_upper_bound" 1)
    (set-option "ast_build_detect_min_max" 1)
    (set-option "ast_build_exploit_nested_bounds" 1)
    (set-option "ast_build_prefer_pdiv" 0)
    (set-option "ast_build_scale_strides" 1)
    (set-option "ast_build_allow_else" 0)
    (set-option "ast_build_allow_or" 0))
  (let* ((schedule (schedule-set-separate (isl:copy schedule)))
	 (ast-build (isl:ast-build-from-context (isl:set-from-str "{:}")))
         (rank (* 2 rank)) ;; rank * tile_bands * vectorizing
         (ast-build (isl:ast-build-set-iterators ast-build (apply #'isl:make-id-list (loop for i upfrom 0 below rank collect (gid i)))))
;;         (ast-build (isl:ast-build-set-options ast-build (isl:union-map-from-str "{}")))
	 (ast-build-node (isl:ast-build-node-from-schedule ast-build schedule)))
    ast-build-node))

(defmethod pg-dump-into-str ((pg Polyhedral-IR))
  (let* ((p     (isl::%isl-printer-to-str (isl::context-handle isl::*context*)))
         (ast   (->ast (poly-schedule pg) (poly-get-rank pg)))
         (p     (isl::%isl-printer-set-output-format p 4)) ;; 4 == Clang
         (q     (isl::%isl-printer-print-ast-node p (isl::ast-node-handle ast)))
         (str   (isl::%isl-printer-get-str q)))
    str))

(defmethod print-object ((pg Polyhedral-IR) stream)
  (print-unreadable-object (pg stream :type t :identity t)
    (format stream "~%~a~%  :history ~a" (pg-dump-into-str pg) (poly-cmd-history pg))))
;; ~~ SCoP ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defstruct ctx
  "Context for tracking loop structure during traversal"
  (stack nil :type list)
  (node-to-loops (make-hash-table) :type hash-table)
  (all-loops nil :type list)
  (exprs nil :type list)
  (scal->access (make-hash-table) :type hash-table))

(defun make-scop-ctx-from-blueprint (graph)
  "Traverse the blueprint graph to extract loop structure"
  (let ((ctx (make-ctx)) (visited (make-hash-table)))
    (labels ((traverse (node)
               (when (or (null node) (gethash (node-id node) visited)) (return-from traverse))
               (setf (gethash (node-id node) visited) t)
               (ecase (node-type node)
                 (:FOR
                  ;; Extract loop info from the FOR node FOR(RANGE(upfrom, below), BODY)
                  (let* ((range-id (car (node-reads node)))
                         (range-node (id->value graph range-id))
                         (idx (when range-node (getattr range-node :idx)))
                         (size (when range-node (car (node-reads range-node))))
                         (step (when range-node (cadr (node-reads range-node))))
                         (mark (getattr node :mark :allow-undefined t)))
                    (when idx
                      (let ((loop-info (list :idx idx :size size :step step :mark (or mark :noopt) :for-node node :range-node range-node)))
                        (push loop-info (ctx-all-loops ctx))
                        (push loop-info (ctx-stack ctx))))
                    ;; Traverse body
                    (traverse (id->value graph (second (node-reads node))))
                    ;; Pop loop from stack after processing body
                    (when idx (pop (ctx-stack ctx)))))
                 (:PROGN (dolist (child-id (node-reads node)) (traverse (id->value graph child-id))))
                 (:IF (when (>= (length (node-reads node)) 2) (traverse (id->value graph (second (node-reads node))))))
                 (:EXPR (push node (ctx-exprs ctx)) (setf (gethash (node-id node) (ctx-node-to-loops ctx)) (copy-list (ctx-stack ctx)))))))
      ;; Start traversal from output nodes or all nodes
      (assert (= 1 (length (graph-outputs graph))))
      (traverse (id->value graph (car (graph-outputs graph))))
      ctx)))

(defun render-expr-for-isl (id graph &aux (node (id->value graph id)))
  "Render an expression in ISL-compatible format"
  (cond
    ((numberp id) (format nil "~a" id))
    (node
     (let ((id (if (eql (node-type node) :EXPR) (car (node-reads node)) id)))
       (render-node (make-instance 'Default-Renderer :graph graph) id)))
    (t (error "The variable ~a is not defined. ~a" id node))))

(defun render-domain-for-node (blueprint node loop-info)
  "Render ISL domain string for a single node"
  (declare (type Graph blueprint) (type Node node) (type hash-table loop-info))
  (flet ((r (id) (render-expr-for-isl id blueprint)))
    (let ((loops (gethash (node-id node) loop-info)))
      (let ((constraints
              (loop for l in (reverse loops)
                    for step = (getf l :step)
                    if (= step 1)
                      collect (format nil "0 <= ~(~a~) < ~a" (getf l :idx) (r (getf l :size)))
                    else ;; [NOTE] Not Tested!!
                      collect (format nil "exists e : ~(~a~) = ~a*e and 0 <= ~(~a~) < ~a" (getf l :idx) (r step) (getf l :idx) (r (getf l :size))))))
        (format nil "~a[~{~a~^, ~}] ~a ~{~a~^ and ~}" (node-id node) (map 'list #'(lambda (l) (format nil "~(~a~)" (getf l :idx))) (reverse loops)) (if constraints ":" "") constraints)))))

(defun render-domains (ctx blueprint)
  "Create ISL domain representation from blueprint"
  (format nil "{ ~{~a~^; ~} }" (reverse (map 'list #'(lambda (x) (render-domain-for-node blueprint x (ctx-node-to-loops ctx))) (ctx-exprs ctx)))))

(defun extract-buffer-access-info (id blueprint &aux (visited (make-hash-table)) (found))
  (labels ((explore (id &aux (node (id->value blueprint id)))
             (when (or (null node) (gethash (node-id node) visited)) (return-from explore))
             (when (eql (node-type node) :BIND)
               (push (cons (getattr node :value) nil) found)
               (return-from explore))
             (when (eql (node-type node) :EXPR)
               (push (cons (car (node-writes node)) nil) found)
               (return-from explore))
             (setf (gethash (node-id node) visited) t)
             (when (eql (node-type node) :AREF)
               (let* ((p (id->value blueprint (car (node-reads node))))
                      (p (if (and p (eql (node-type p) :BIND)) (getattr p :value) (car (node-reads node)))))
                 (push (cons p (second (node-reads node))) found)
                 (return-from explore)))
             (mapc #'explore (node-reads node))))
    (explore id)
    found))

(defun render-default-isl-access (ctx bp idx loops)
  ;; Scalar Memory Access: Inherits the first configuration where the scalar was defined.
  ;; [TODO] Is it valid for all case, all kernel, all schedule? how can we prove this?
  (when (gethash idx (ctx-scal->access ctx))
    (return-from render-default-isl-access (getf (gethash idx (ctx-scal->access ctx)) :access)))
  (let* ((shape (loop for l in loops for size = (getf l :size) for expr = (id->value bp size) for node = (id->value bp (car (node-reads expr)))
                      ;; Determining the loop size from graph. (TODO: Assert RANGE(SIZE, STEM) where SIZE is always EXPR, and EXPR(LOAD(Constant)) Pattern
                      collect (progn (assert (eql (node-type node) :LOAD)) (assert (numberp (getattr node :value))) (getattr node :value))))
         (strides (caten/codegen/helpers:row-major-calc-strides shape))
         (access (format nil "~{~a~^+~}" (loop for s in strides for l in loops for idx = (getf l :idx) collect (format nil "~a*~(~a~)" s idx)))))
    (setf (gethash idx (ctx-scal->access ctx)) (list :access access :shape shape :strides strides))
    access))

(defun render-access-for-node (ctx node loops buffer index blueprint)
  "Render access relation for a single node"
  (let ((domain (format nil "~{~a~^, ~}" (map 'list #'(lambda (l) (format nil "~(~a~)" (getf l :idx))) (reverse loops)))))
    (format nil "~a[~a] -> ~a[~a]" (node-id node) domain buffer (if index (render-expr-for-isl index blueprint) (render-default-isl-access ctx blueprint buffer (reverse loops))))))

(defun extract-accesses (ctx blueprint &aux (reads) (writes))
  "Extract read and write access relations from blueprint"
  (with-slots ((node-to-loops node-to-loops) (exprs exprs)) ctx
    (loop for expr in (reverse exprs) ;; found earlier -> later
          for expr-domain = (gethash (node-id expr) node-to-loops)
          for expr-entry-point = (id->value blueprint (car (node-reads expr))) do
            (assert expr-entry-point)
            (case (node-type expr-entry-point)
              (:SETF ;; // EXPR(STORE)
               ;; SETF(AREF, EXPR)
               ;;       ^W    ^R
               (let ((write-region (extract-buffer-access-info (car (node-reads expr-entry-point)) blueprint))
                     (read-region  (extract-buffer-access-info (second (node-reads expr-entry-point)) blueprint)))
                 (dolist (w write-region)
                   (push (render-access-for-node ctx expr expr-domain (car w) (cdr w) blueprint) writes))
                 (dolist (r read-region)
                   (push (render-access-for-node ctx expr expr-domain (car r) (cdr r) blueprint) reads))))
               (otherwise ;; // EXPR
                (let ((read-region (extract-buffer-access-info (car (node-reads expr)) blueprint)))
                  (push (render-access-for-node ctx expr expr-domain (car (node-writes expr)) nil blueprint) writes)
                  (dolist (r read-region)
                    (push (render-access-for-node ctx expr expr-domain (car r) (cdr r) blueprint) reads))))))
    (cons
     (format nil "{ ~{~a~^; ~} }" (reverse reads))
     (format nil "{ ~{~a~^; ~} }" (reverse writes)))))

(defun render-band-node-in-domain (range-node related-nodes loop-info &aux (idx (getattr range-node :idx)))
  (declare (type node range-node) (type list related-nodes) (type hash-table loop-info))
  (with-output-to-string (out)
    (format out "[{")
    (loop for filter in related-nodes for nth upfrom 0
          for idxs = (map 'list #'(lambda (x) (format nil "~(~a~)" (getf x :idx))) (reverse (or (gethash (node-id filter) loop-info) (error ""))))
          do (assert (eql (node-type filter) :EXPR))
          if (not (= nth 0)) do (format out "; ")
            do (format out "~a[~{~a~^, ~}] -> [~(~a~)]" (node-id filter) idxs idx))
    (format out "}]")))

(defun rewrite-blueprint-tree->schedule-tree (ctx blueprint &aux (visited (make-hash-table)))
  "Build ISL Schedule Tree directly from blueprint structure following analyze-scop pattern"
  (declare (type Graph blueprint))
  ;; ISL Schedule starts w/ domain
  (with-slots ((loops loops) (node-to-loops node-to-loops)) ctx
    (labels ((rewrite-node (id &key (region nil) &aux (node (id->value blueprint id)))
               (declare (type symbol id))
               (when (or (null node) (gethash (node-id node) visited)) (error "Rendering for multiple times, should we allow it?"))
               (setf (gethash (node-id node) visited) t)
               (values
                (case (node-type node)
                  (:FOR
                   ;; FOR(RANGE(UPFROM, BELOW), BODY)
                   (multiple-value-bind (body-sched exprs-in-body) (rewrite-node (second (node-reads node)) :region region)
                     (let* ((range (id->value blueprint (car (node-reads node))))
                            (band (render-band-node-in-domain range exprs-in-body node-to-loops)))
                       (setf region (append region exprs-in-body))
                       (if (string= band "[{}]")
                           body-sched
                           (schedule-insert-partial-schedule body-sched (multi-union-pw-aff-from-str band))))))
                  (:IF
                   ;; [Note] How to dump :IF Node?
                   (error "not ready"))
                  ;; EXPR ==> Rewrite as a filter, and is a leaf of graph.
                  (:EXPR
                   (setf region (append region (list node)))
                   (schedule-from-domain (union-set-from-str (format nil "{ ~a }" (render-domain-for-node blueprint node node-to-loops)))))
                  (:PROGN
                    ;; [todo] you can use reduce
                    (let ((tmp-schedule :nothing))
                      (loop for item in (node-reads node) do
                        (multiple-value-bind (sched reg) (rewrite-node item)
                          (setf region (append region reg))
                          (if (eql tmp-schedule :nothing)
                              (setf tmp-schedule sched)
                              (setf tmp-schedule (schedule-sequence tmp-schedule sched)))))
                      (assert (not (eql tmp-schedule :nothing)))
                      tmp-schedule))
                  (otherwise (error "No handling case for ~a" (node-type node))))
                region)))
      (assert (= 1 (length (graph-outputs blueprint))))
      (rewrite-node (car (graph-outputs blueprint))))))

(defun make-polyhedral-from-blueprint (blueprint &key (strategy))
  "Constructs Polyhedral IR from blueprint which is a static graph.
   
   The blueprint should be a FastGraph containing nodes with the following types:
   - :RANGE - defines loop bounds
   - :FOR - marks loop entry with :mark attribute (:coincident, :reduction, :noopt)
   - :AREF - memory load operations
   - :SETF - memory store operations
   - :PROGN - sequence of operations
   
   Returns a Polyhedral-IR object."
  (declare (type Graph blueprint))
  ;; Extract domain, reads, writes
  ;; [TODO] Handler-case-bind and add a warning
  (let* ((ctx (make-scop-ctx-from-blueprint blueprint))
         (domain (union-set-from-str (render-domains ctx blueprint)))
         (schedule (rewrite-blueprint-tree->schedule-tree ctx blueprint))
         (reads/writes (extract-accesses ctx blueprint)) (reads) (writes))
    (handler-case (setf reads (union-map-from-str (car reads/writes))
                        writes (union-map-from-str (cdr reads/writes)))
      (error (c) (error "Cannot dump an access relation from the following relations:~%Reads:~%~a~%Writes:~%~a
Error:~%~a~%Is the loop affine?" (car reads/writes) (cdr reads/writes) c)))
    (make-polyhedral-ir blueprint domain reads writes schedule ctx strategy)))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;;; Polyhedral -> Blueprint
(defstruct (parse-ctx
            (:constructor make-parse-ctx (blueprint))
            (:conc-name pctx-))
  (blueprint blueprint :type Graph)
  (gid2range (make-hash-table) :type hash-table)
  (gid2offset (make-hash-table) :type hash-table)
  (variable-table (make-hash-table) :type hash-table)
  (scop-ctx (make-scop-ctx-from-blueprint blueprint) :type ctx)
  (expr2args (make-hash-table) :type hash-table)
  (band-cnt 0 :type fixnum))

(defun pctx-register-gid (pctx id range offset)
  (declare (type parse-ctx pctx) (type symbol id))
  (labels ((find-suite (i cnt)
             (if (gethash i (pctx-gid2range pctx))
                 (find-suite (intern (format nil "~a_~a" id cnt)) (1+ cnt))
                 i)))
    (let ((registered-as (find-suite id 1)) (new-ctx (copy-parse-ctx pctx)))
      (setf (gethash registered-as (pctx-gid2range new-ctx)) range
            (gethash registered-as (pctx-gid2offset new-ctx)) offset
            (pctx-variable-table new-ctx) (alexandria:copy-hash-table (pctx-variable-table new-ctx))
            (gethash id (pctx-variable-table new-ctx)) registered-as)
      (values new-ctx registered-as))))

(defun parse-isl-ast (ctx ast)
  (declare (type cffi:foreign-pointer ast))
  (let ((type (isl::%isl-ast-node-get-type ast)))
    (ecase type
      (:ast-node-error (isl::isl-error))
      (:ast-node-for   (parse-isl-ast-for ctx ast))
      (:ast-node-if    (parse-isl-ast-if ctx ast))
      (:ast-node-block (parse-isl-ast-block ctx ast))
      (:ast-node-mark  (parse-isl-ast-mark ctx ast))
      (:ast-node-user  (parse-isl-ast-user ctx ast)))))

(defun parse-isl-ast-mark (ctx ast)
  (declare (type cffi:foreign-pointer ast))
  (incf (pctx-band-cnt ctx))
  (let* ((directive (str->directive (cffi:foreign-string-to-lisp (isl::%isl-id-get-name (isl::%isl-ast-node-mark-get-id ast)))))
         (user (parse-isl-ast ctx (isl::%isl-ast-node-mark-get-node ast)))
         (depth (directive-depth directive))
         (band-id (intern (format nil "B~a" (1- (pctx-band-cnt ctx))))))
    (labels ((rec (node count)
               (declare (type node node node) (type fixnum count))
               (assert (eql (node-type node) :FOR))
               (setf (getattr node :band) band-id
                     (getattr node :directive) directive) ;; multiple directives can be applied
               (when (< count depth) (rec (id->value *ctx* (second (node-reads node))) (1+ count)))))
      (rec user 1))
    user))

(defun parse-isl-ast-block (ctx ast)
  (declare (type cffi:foreign-pointer ast))
  (let* ((children (isl::%isl-ast-node-block-get-children ast))
	 (n        (isl::%isl-ast-node-list-n-ast-node children)))
    (apply
     #'%progn
     (loop for i upfrom 0 below n
           for child = (isl::%isl-ast-node-list-get-at children i)
	   collect (parse-isl-ast ctx child)))))

(defun parse-isl-ast-if (ctx ast)
  (declare (type cffi:foreign-pointer ast))
  (let* ((condition (parse-isl-expr ctx (isl::%isl-ast-node-if-get-cond ast) :toplevel-p nil))
	 (then-node (parse-isl-ast ctx (isl::%isl-ast-node-if-get-then-node ast)))
	 (else-p (isl::%isl-ast-node-if-has-else-node ast)))
    (assert (not (eql else-p :bool-true)) () "Else statement is not allowed!")
    (%if condition then-node)))

(defun parse-isl-ast-cond (ctx ast idx)
  (declare (type cffi:foreign-pointer ast))
  (let ((type (isl::%isl-ast-expr-get-type ast)))
    (assert (eql type :ast-expr-op))
    (let* ((n-arg (isl::%isl-ast-expr-get-op-n-arg ast))
           (args (loop for nth upfrom 0 below n-arg collect (parse-isl-expr ctx (isl::%isl-ast-expr-op-get-arg ast nth) :toplevel-p nil)))
           (op-type (isl::%isl-ast-expr-op-get-type ast)))
      (multiple-value-bind (lhs rhs) (apply #'values args)
        (assert (= 2 (length args)))
        (assert (and (eql (node-type lhs) :LOAD) (eql (getattr lhs :value) idx)))
        ;; Assuming: gid < size
        (ecase op-type
          (:ast-expr-op-le (%add rhs (%iconst 1 :dtype :int64)))
          (:ast-expr-op-lt rhs))))))

(defun parse-isl-ast-for (ctx ast)
  (declare (type cffi:foreign-pointer ast))
  (let* ((iter (isl::%isl-ast-node-for-get-iterator ast))
	 (id (isl::%isl-ast-expr-get-id iter))
	 (name (cffi:foreign-string-to-lisp (isl::%isl-id-get-name id)))
	 (from (parse-isl-expr ctx (isl::%isl-ast-node-for-get-init ast) :toplevel-p nil))
	 (by (parse-isl-expr ctx (isl::%isl-ast-node-for-get-inc ast) :toplevel-p nil))
	 (to (parse-isl-ast-cond ctx (isl::%isl-ast-node-for-get-cond ast) (intern name)))
         (rid (gensym "R")))
    (multiple-value-bind (new-ctx gid) (pctx-register-gid ctx (intern name) rid from)
      ;; [TODO] by >= 1 assertion
      (let ((body (parse-isl-ast new-ctx (isl::%isl-ast-node-for-get-body ast))))
        (%range gid (%sub to from) body :step by :rid rid)))))

(defun parse-isl-expr (ctx ast &key (toplevel-p t))
  (declare (type cffi:foreign-pointer ast))
  (let* ((type (isl::%isl-ast-expr-get-type ast)))
    (funcall
     (if toplevel-p #'(lambda (x) (%expr (node->id x))) #'identity)
     (ecase type
       (:ast-expr-error (isl::isl-error))
       (:ast-expr-id
        (let* ((id (isl::%isl-ast-expr-id-get-id ast))
	       (name (intern (cffi:foreign-string-to-lisp (isl::%isl-id-get-name id))))
               (is-gid (gethash name (pctx-variable-table ctx))))
          (if is-gid
              (let ((rid (gethash is-gid (pctx-gid2range ctx)))
                    (offset (gethash is-gid (pctx-gid2offset ctx))))
                (assert (and rid offset))
                (if (eql offset 0)
                    rid
                    (%add rid (if (numberp offset) (%iconst offset :dtype :int64) offset))))
              (%iconst name :dtype :int64))))
       (:ast-expr-int
        (let* ((id (isl::%isl-ast-expr-int-get-val ast))
	       (num (isl::%isl-val-get-d id)))
	  (declare (type number num))
          (%iconst num :dtype :int64)))
       (:ast-expr-op
        (let* ((n-arg (isl::%isl-ast-expr-get-op-n-arg ast))
	       (args (loop for nth upfrom 0 below n-arg collect (parse-isl-expr ctx (isl::%isl-ast-expr-op-get-arg ast nth) :toplevel-p nil)))
	       (op-type (isl::%isl-ast-expr-op-get-type ast)))
	  (flet ((->expr (lhs rhs)
		   (assert (not (eql op-type :ast-expr-op-error)) () ":isl_ast_expr_op_error")
		   (ecase op-type
		     (:ast-expr-op-and (%and lhs rhs))
		     (:ast-expr-op-and-then (%and lhs rhs))
		     (:ast-expr-op-or (%or lhs rhs))
		     (:ast-expr-op-or-else (%or lhs rhs))
		     (:ast-expr-op-max (%max lhs rhs))
		     (:ast-expr-op-min  (%min lhs rhs))
		     (:ast-expr-op-minus (%neg lhs)) ;; (- a)
		     (:ast-expr-op-add (%add lhs rhs))
		     (:ast-expr-op-sub (%sub lhs rhs))
		     (:ast-expr-op-mul (%mul lhs rhs))
		     (:ast-expr-op-div (%idiv lhs rhs))		 
		     (:ast-expr-op-fdiv-q (%idiv lhs rhs))
		     (:ast-expr-op-pdiv-q (%idiv lhs rhs))
		     (:ast-expr-op-pdiv-r (%mod lhs rhs))
		     (:ast-expr-op-zdiv-r (%mod lhs rhs))
		     ;; (:expr-op-cond)
		     (:ast-expr-op-eq (%= nil :row lhs rhs))
                     ;; Rewrite LE to simplify the expression
		     (:ast-expr-op-le (%< nil :row lhs (%add rhs (%iconst 1 :dtype :int64))));; <=
		     (:ast-expr-op-lt (%< nil :row lhs rhs)) ;; <
		     (:ast-expr-op-ge (%not (%< nil :row lhs rhs))) ;; >=
		     (:ast-expr-op-gt (%> nil :row lhs rhs)) ;; >
		     ;; (:expr-op-call)
		     ;; (:expr-op-access)
		     ;; (:expr-op-member)
		     ;; (:expr-op-address-of)
		     (otherwise  (error "~a is not supported by caten" op-type)))))
	    (if (= (length args) 1)
	        (->expr (car args) nil)
	        (case op-type
		  (:ast-expr-op-select
		   (assert (= (length args) 3))
                   (apply #'%where args))
		  (otherwise
		   (reduce #'->expr args)))))))))))

(defun parse-isl-ast-user (ctx ast &aux (visited (make-hash-table)))
  (declare (type cffi:foreign-pointer ast))
  (let ((expr (isl::%isl-ast-node-user-get-expr ast)))
    (let* ((first-expr (isl::%isl-ast-expr-op-get-arg expr 0))
	   (n          (isl::%isl-ast-expr-get-op-n-arg expr))
	   (id         (isl::%isl-ast-expr-id-get-id first-expr))
	   (name       (cffi:foreign-string-to-lisp (isl::%isl-id-get-name id)))
	   (args       (loop for i upfrom 1 below n collect (parse-isl-expr ctx (isl::%isl-ast-expr-op-get-arg expr i) :toplevel-p nil)))
           (node (find name (graph-nodes (pctx-blueprint ctx)) :key (alexandria:compose #'symbol-name #'node-id) :test #'equalp))
           (node-to-loops (reverse (gethash (node-id node) (ctx-node-to-loops (pctx-scop-ctx ctx)))))
           (rewrite-map (make-hash-table)))
      (assert node () "The node ~a is not found from original blueprint." name)
      (assert (= (length args) (length node-to-loops)) () "Inconsistent domain loop args size")
      (loop for base-domain in node-to-loops
            for new-args in args
            do (setf (gethash (getf base-domain :idx) rewrite-map) new-args))
      ;; need a base args
      (labels ((e (id &aux (node (id->value (pctx-blueprint ctx) id)))
                 (when (or (null node) (gethash (node-id node) visited)) (return-from e id))
                 (when (eql (node-type node) :EXPR) (return-from e id))
                 ;; 3 case using gid:
                 ;; - Reference to RANGE
                 ;; - LOAD(value)
                 ;; - MUL(GID0, ...) (<- this should be deprecated)
                 (when (eql (node-type node) :RANGE)
                   (let ((new-space (gethash (getattr node :idx) rewrite-map)))
                     (assert new-space)
                     (let ((n (copy-node new-space)))
                       (assert (= 1 (length (node-writes n))))
                       (setf (node-writes n) (list id)
                             (node-id n) (gensym "NID"))
                       (emit n))
                     (return-from e id)))
                 (when (and (eql (node-type node) :LOAD) (gethash (getattr node :value) rewrite-map))
                   (let ((new-space (gethash (getattr node :value) rewrite-map)))
                     (let ((n (copy-node new-space)))
                       (assert (= 1 (length (node-writes n))))
                       (setf (node-writes n) (list id)
                             (node-id n) (gensym "NID"))
                       (emit n))
                     (return-from e id)))
                 ;; [TODO] Replace %RANGE here if exists
                 (setf (gethash (node-id node) visited) t)
                 (emit node)
                 (setf (node-reads node) (map 'list #'e (node-reads node)))
                 (car (node-writes node))))
        (setf (node-reads node) (map 'list #'e (node-reads node)))
        (emit node)
        (setf (gethash (node-id node) (pctx-expr2args ctx))
              (loop for arg in args collect (cons arg (caten/aasm::ast-make-subgraph *ctx* (car (node-writes arg))))))
        node))))

(defun verify-ast-with-context (parse-ctx ctx blueprint &aux (new-ctx (make-scop-ctx-from-blueprint blueprint)))
  ;; If there's any, rewrite val_2 -> val_2[_gid0 + gid1]
  (with-slots ((node-to-loops node-to-loops) (exprs exprs)) new-ctx
    (let ((expr-subgraphs (loop for expr in (reverse exprs) collect (cons expr (caten/aasm::ast-expr-graph blueprint expr)))))
      (labels ((lookup (node) (reverse (gethash (node-id node) node-to-loops)))
               (find-expr-from-user (user)
                 (loop for expr in expr-subgraphs
                       if (find (node-id user) (graph-nodes (cdr expr)) :key #'node-id)
                         do (return-from find-expr-from-user (car expr))))
               (get-users (id)
                 (nconc
                  (id->users blueprint id)
                  (loop for node in (graph-nodes blueprint) if (and (eql (node-type node) :BIND) (eql id (getattr node :value))) collect node)))
               (invalid-scope-p (acc-scope expr-scope)
                 (when (> (length acc-scope) (length expr-scope)) (return-from invalid-scope-p t))
                 ;; grid id is unique in the blueprint, we can use it.
                 (loop for acc in acc-scope for expr in expr-scope
                       ;; [TODO] make it idx
                       when (not (eql (node-id (getf acc :range-node)) (node-id (getf expr :range-node)))) do (return-from invalid-scope-p t))
                 nil)
               (mutate-to-tensor-p (id &aux (acc (id->value blueprint id)) (users (get-users id)) (visited (make-hash-table)))
                 ;; All users must be placed in the possible scope where acc is firstly defined.
                 ;; Otherwise, we have to allocate extra.
                 (assert (eql :EXPR (node-type acc)))
                 (loop with acc-scope = (lookup acc)
                       for user in users for expr = (find-expr-from-user user)
                       if (and expr (null (gethash (node-id expr) visited))) do
                         (setf (gethash (node-id expr) visited) t)
                         ;; Compare the scope of (lookup acc) and (lookup expr)
                         (when (invalid-scope-p acc-scope (lookup expr))
                           (return-from mutate-to-tensor-p t))) ;; if theres at least one violation
                 nil))
        (let ((rewrite-ids))
          (maphash
           #'(lambda (previously-scalar rewrite-context)
               (declare (ignore rewrite-context))
               (when (id->value blueprint previously-scalar)
                 (when (mutate-to-tensor-p previously-scalar)
                   (push previously-scalar rewrite-ids))))
           (ctx-scal->access ctx))
          (let ((extra-allocs (bp-rewrite-scalar->buffer parse-ctx ctx (list blueprint) rewrite-ids)))
            (values blueprint extra-allocs)))))))

(defun apply-directives (blueprint)
  (let ((bands (make-hash-table)))
    (loop for node in (tpsort-graph blueprint)
          if (and (eql (node-type node) :FOR) (getattr node :band) (getattr node :directive))
            do (if (gethash (getattr node :band) bands)
                   (push node (gethash (getattr node :band) bands))
                   (setf (gethash (getattr node :band) bands) (list node))))
    ;; Rewrite by each directive
    (maphash
     #'(lambda (band-id bands)
         (let ((new-bp (optrule-apply-transform-on-blueprint (intern (directive-type (getattr (car bands) :directive)) "KEYWORD") (reverse bands) blueprint)))
           (assert (graph-p new-bp) () "optrule-apply-transform-on-blueprint must return a Graph, when processing ~a, ~a" (getattr (car bands) :directive) band-id)
           (setf blueprint new-bp)))
     bands)
    (simplify-ast blueprint)
    blueprint))

(defun get-raw-bp-from-polyhedral (pctx polyhedral)
  "Convert ISL Polyhedral Representation back to blueprint graph. If loop fission was applied, generates multiple blueprint."
  (let* ((ast (isl::ast-node-handle (->ast (poly-schedule polyhedral) (poly-get-rank polyhedral))))
         (type (isl::%isl-ast-node-get-type ast)))
    (case type
      (:ast-node-error (isl::isl-error))
      ((:ast-node-for :ast-node-mark :ast-node-user) ;; they are always single kernel
       (list (with-blueprint (:noopt t) (%progn (parse-isl-ast pctx ast)))))
      (:ast-node-if (error ":ast-node-if should not be placed on the root!"))
      (:ast-node-block ;; they could be divided to multiple kernels, let's check first.
       (let* ((children (isl::%isl-ast-node-block-get-children ast))
	      (n        (isl::%isl-ast-node-list-n-ast-node children))
              (children (reverse (loop for i upfrom 0 below n collect (isl::%isl-ast-node-list-get-at children i))))
              (n-kernels 0)
              (kernels (make-hash-table)))
         ;; [TODO] Filter, Filter, TILEGPUはOKのはず。
         ;; [TODO] MultiKernelで動いてるかテストする！
         ;; TileGPU is the only trigger to generate multiple kernels
         ;; まず_gid_p0のコメントアウトしてる部分が悪い
         ;; BufferRizeの修正も合わせて考えるべき。
         (flet ((mark-is-tilegpu-p (mark)
                  (and (eql (isl::%isl-ast-node-get-type mark) :ast-node-mark)
                       (let ((d (str->directive (cffi:foreign-string-to-lisp (isl::%isl-id-get-name (isl::%isl-ast-node-mark-get-id mark))))))
                         (eql :TILEGPU (intern (directive-type d) "KEYWORD"))))))
           (loop with cannot-add-new-loop-mode = nil
                 for c in children ;; Reading from bottom
                 for type = (isl::%isl-ast-node-get-type c)
                 for nth upfrom 0
                 if (and cannot-add-new-loop-mode (find type '(:ast-node-mark :ast-node-for)))
                   do (incf n-kernels) (setf cannot-add-new-loop-mode nil)
                 if (or (mark-is-tilegpu-p c) (find type '(:ast-node-for :ast-node-mark)))
                   do (setf cannot-add-new-loop-mode t)
                 do (setf (gethash n-kernels kernels) (append (list c) (gethash n-kernels kernels))))
           (nreverse
            (loop for i upfrom 0 to n-kernels
                  for kernel-items = (gethash i kernels)
                  collect
                  (with-blueprint (:noopt t) (apply #'%progn (map 'list #'(lambda (x) (parse-isl-ast pctx x)) kernel-items)))))))))))

(defun %finalize-blueprint-from-polyhedral (polyhedral pctx kernel)
  "Convert ISL polyhedral representation back to blueprint graph"
  (declare (type Polyhedral-IR polyhedral) (type Graph kernel))
  (multiple-value-bind (new-bp extra-allocs)
      (verify-ast-with-context ;; Compare the scope of all scalar variables w/ context, if theres some changes, add them as tmp buffer.
       pctx (poly-ctx polyhedral)
       (caten/aasm::ast-simplify-expr-subgraph (caten/aasm::%simplify-ast kernel)))
    (values (apply-directives new-bp) extra-allocs)))

(defun bp-rewrite-scalar->buffer (parse-ctx ctx kernels scal-ids &aux (extra-allocs))
  (declare (type list scal-ids))
  (when (null scal-ids) (return-from bp-rewrite-scalar->buffer))
  (let* ((scal-ids (remove-duplicates scal-ids))
         (contexts (map 'list #'make-scop-ctx-from-blueprint kernels))
         (expr-subgraphs
           (loop for ctx in contexts for kernel in kernels
                 append
                 (loop for expr in (reverse (ctx-exprs ctx))
                       collect (cons expr (caten/aasm::ast-expr-graph kernel expr))))))
    (dolist (scal-id scal-ids)
      (labels ((lookup (node)
                 (loop for ctx in contexts
                       if (gethash (node-id node) (ctx-node-to-loops ctx)) do
                         (return-from lookup (gethash (node-id node) (ctx-node-to-loops ctx)))))
               (find-expr-from-user (user)
                 (loop for expr in expr-subgraphs
                       if (find (node-id user) (graph-nodes (cdr expr)) :key #'node-id)
                         do (return-from find-expr-from-user (car expr))))
               (id->value-from-kernels (id)
                 (loop for k in kernels
                       for v = (id->value k id)
                       if v do (return-from id->value-from-kernels v)))
               (id->tensor-info (id &aux (acc (id->value-from-kernels id)) (node (id->value-from-kernels (car (node-reads acc)))))
                 (assert (eql :EXPR (node-type acc)))
                 (values (tensor-relay-dtype (car (relay-writes (read-type-relay node)))) (lookup acc) acc))
               (compute-idx (acc stride load-node
                             &aux
                               (acc (or (find-expr-from-user acc) acc))
                               (load-node (or (find-expr-from-user load-node) load-node))
                               (acc-args (gethash (node-id acc) (pctx-expr2args parse-ctx)))
                               (load-args (gethash (node-id load-node) (pctx-expr2args parse-ctx))))
                 ;; [TODO] LoopInterchangeされた時，load-argsをpermuteする必要がある！
                 ;; [TODO] ↑忘れないで！！
                 (let ((args (subseq load-args 0 (length acc-args))))
                   (dolist (arg args)
                     (map 'list #'(lambda (x) (emit x)) (graph-nodes (cdr arg))))
                   (reduce
                    #'%add
                    (loop for arg in args for s in stride collect (%mul (%load (%salloc :dtype :int64) s) (car arg))))))
               (swpid (id suffix) (intern (format nil "~a_~a" id suffix)))
               (make-new-aref (id read-from acc stride load-node)
                 (with-context-nodes
                     (out (%aref read-from (compute-idx acc stride load-node) :out id))))
               (make-new-aref-bind (id bind-as base-read acc stride load-node)
                 (with-context-nodes
                     (out (%aref (emit (make-node :JIT :BIND (list (gensym)) (list base-read) :value bind-as)) (compute-idx acc stride load-node) :out id))))
               (make-new-initializer (read-from acc stride form load-node)
                 (with-context-nodes
                     (out (%expr (node->id (%setf (%aref read-from (compute-idx acc stride load-node)) form)) :out scal-id)))))
        (multiple-value-bind (dtype loops acc) (id->tensor-info scal-id)
          ;; [TODO] Get Shape/Stride
          ;; [TODO] Create extra alloc inserted to tuned runtime graph
          (assert (and dtype loops acc) () "Could not find the definition of scalar ~a" scal-id)
          (dolist (blueprint kernels)
            (let* ((rewrite-context (gethash scal-id (ctx-scal->access ctx)))
                   (shape (getf rewrite-context :shape)) (stride (getf rewrite-context :strides))
                   (argname (swpid scal-id "tmp")) (defglobal (%global argname dtype t)) (count 0))
              (assert rewrite-context)
              (push (%alloc (length shape) shape stride :dtype dtype :id argname) extra-allocs)
              ;; Rewrite the definition of scal-id if it exists in current blueprint
              (insert-nodes blueprint (list defglobal))
              (when (id->value blueprint scal-id)
                ;; Rewrite {val_2 = EXPR(0.0)} -> {val_2 = (%aref val2_tmp ...)}
                (insert-nodes blueprint (make-new-initializer argname acc stride (car (node-reads acc)) acc)))
              ;; Rewrite the user of scal-id
              (labels ((newid (node x)
                         (if (eql x scal-id)
                             (let ((id (swpid scal-id count)))
                               (incf count)
                               (insert-nodes blueprint (make-new-aref id argname acc stride node))
                               id)
                             x)))
                (loop for node in (graph-nodes blueprint)
                      ;; Case1. the user of scal-id
                      if (or (eql (node-type node) :EXPR) (not (eql (node-class node) :Render)))
                        do (let ((node (copy-node node)))
                             (setf (node-reads node) (map 'list #'(lambda (x) (newid node x)) (node-reads node)))
                             (insert-nodes blueprint (list node)))
                           ;; Case2. BIND(val_8, value=val_2) (insert the bind itself)
                           ;; Rewrite :BIND if there is no accumlator (if there is accumlator, :DEFINE-GLOBAL won't be purged)
                      if (and (eql (node-type node) :BIND) (eql scal-id (getattr node :value)))
                        do (if (id->value blueprint (car (node-reads node))) ;; two case: the reductor is defined in the same group, or
                               (insert-nodes blueprint (make-new-aref-bind (car (node-writes node)) argname (car (node-reads node)) acc stride node))
                               (insert-nodes blueprint (make-new-aref (car (node-writes node)) argname acc stride node)))))))))))
  (remove-duplicates (reverse extra-allocs) :key (alexandria:compose #'car #'node-writes)))

(defun get-blueprint-from-polyhedral (polyhedral)
  (let* ((pctx (make-parse-ctx (poly-blueprint polyhedral))) ;; Create a parse ctx from the base blueprint
         (kernels (get-raw-bp-from-polyhedral pctx polyhedral)))
    (if (= 1 (length kernels))
        (multiple-value-bind (bp allocs) (%finalize-blueprint-from-polyhedral polyhedral pctx (car kernels))
          (values (list bp) allocs))
        (let* ((all-nodes (apply #'append (map 'list #'graph-nodes kernels)))
               (all-nodes (loop for n in all-nodes if (not (eql (node-class n) :Render)) collect n))
               (common-buffer-among-kernels ;; a list of buffers which must be mutated into :DEFINE-GLOBAL
                 (loop for kernel in kernels
                       append (graph-get-undefined-variables kernel)))
               (common-buffer-among-kernels
                 ;; If the symbol was used as :BIND, replace them w/ :value
                 (loop for c in common-buffer-among-kernels
                       for user = (find c all-nodes :test #'find :key #'node-reads)
                       do (assert user)
                       if (eql (node-type user) :BIND) collect (getattr user :value) else collect c)))
          ;; Bufferizeは，Skipするケースへ分岐する。この分岐が正しく動けばOK
          ;; Bufferize
          (let ((extra-allocs (bp-rewrite-scalar->buffer pctx (poly-ctx polyhedral) kernels common-buffer-among-kernels)))
            (values
             (loop for kernel in kernels
                   collect
                   (multiple-value-bind (k alcs) (%finalize-blueprint-from-polyhedral polyhedral pctx kernel)
                     (dolist (a alcs) (push a extra-allocs))
                     k))
             ;; Loop Fissionすると，完全に無意味なMOVEが生成されたりする。これがあったら，カーネルを削除する。
             (remove-duplicates extra-allocs :key (alexandria:compose #'car #'node-writes))))))))
;; ~~ OptimizeRule ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass OptimizationRule ()
  ((axis :initarg :axis :accessor optrule-axis :initform nil)
   (band :initarg :band :accessor optrule-band :initform nil)))

(defmethod print-object ((obj OptimizationRule) stream)
  (print-unreadable-object (obj stream :type t)
    (dolist (slot-def (closer-mop:class-slots (class-of obj)))
      (let ((name  (closer-mop:slot-definition-name slot-def))
            (value (slot-value obj (closer-mop:slot-definition-name slot-def))))
        (when (null (find name `(band)))
          (format stream " :~a ~S" name value))))))

(defgeneric optrule-generate-search-space (polyhedral bands optrule-trigger))
(defgeneric optrule-apply-transform-on-polyhedral (polyhedral optrule)) ;; Insert Directive
(defgeneric optrule-apply-transform-on-blueprint (directive-id bands blueprint))  ;; Directive Parse

(defun apply-optimization (polyhedral optrule)
  (declare (type Polyhedral-IR polyhedral) (type OptimizationRule optrule))
  (let ((polyhedral (poly-clone-for-next-generation polyhedral)))
    (push optrule (poly-cmd-history polyhedral))
    (optrule-apply-transform-on-polyhedral polyhedral optrule)
    polyhedral))
;; ~~ Verifiers ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun map-schedule-node-children (f schedule-node)
  (declare (type function f) (type isl::schedule-node schedule-node))
  (let* ((node schedule-node) (next-nodes) (outputs))
    (loop named map-search
          for n-children = (isl::%isl-schedule-node-n-children (isl::schedule-node-handle node))
          while (>= n-children 0) do
            (loop for nth upfrom 0 below n-children
                  for mark = (when (eql (schedule-node-get-type node) :schedule-node-mark)
                               (cffi:foreign-string-to-lisp
                                (isl::%isl-id-get-name
                                 (isl::%isl-schedule-node-mark-get-id
                                  (isl::schedule-node-handle node)))))
                  for band = (schedule-node-get-child node nth)
                  for type = (schedule-node-get-type band) do
                    (let ((out (funcall f type band mark))) (when out (push out outputs)))
                    (push band next-nodes))
            (when (= (length next-nodes) 0) (return-from map-search))
            (setf node (pop next-nodes)))
    (nreverse outputs)))

(defun schedule-node-get-undernearth-bands (schedule-node)
  (declare (type isl::schedule-node schedule-node))
  (map-schedule-node-children
   #'(lambda (type band mark)
       (when (eql type :schedule-node-band)
         (when (or (null mark) (directive-visible (str->directive mark))) ;; Only visible bands are gathered
           band)))
   schedule-node))

(defun schedule-node-get-band-from-relative-idx (schedule-node idx)
  (declare (type isl::schedule-node schedule-node) (type fixnum idx))
  (nth idx (schedule-node-get-undernearth-bands schedule-node)))

(defun get-zeros-on-union-set (delta-uset)
  (declare (type isl::union-set delta-uset))
  (let* ((delta-set (set-from-union-set delta-uset))
         (ma (multi-aff-zero (set-get-space delta-set))))
    (union-set-from-set (set-from-multi-aff ma))))

(defun check-legality-parallel (node dep)
  "
```
(check-legality-parallel node dep)
```
Returns T if the band node is legal to be parallelized with respect to the dep.
Reference: https://github.com/hikettei/tadashi/blob/main/src/legality.c#L91-L122"
  (declare (type isl::schedule-node node) (type isl::union-map dep))
  (when (union-map-is-empty dep) (return-from check-legality-parallel t))
  (let* ((map (schedule-node-band-get-partial-schedule-union-map node))
         (domain (union-map-apply-range (union-map-apply-domain dep map) map))
         (delta (union-map-deltas domain))
         (_ (when (union-set-is-empty delta) (return-from check-legality-parallel t)))
         (zeros (get-zeros-on-union-set delta))
         (cmp (union-set-lex-lt-union-set delta zeros))
         (retval (union-set-is-empty cmp))
         (cmp (union-set-lex-gt-union-set delta zeros)))
    (declare (ignore _))
    (and retval (union-set-is-empty cmp))))

(defun check-legality (schedule dep)
  "
```
(check-legality schedule dep)
```
Returns T if the current schedule does not break any dependences in dep."
  (declare (type isl::schedule schedule) (type isl::union-map dep))
  (when (union-map-is-empty dep) (return-from check-legality t))
  (let* ((map (schedule-get-map schedule))
         (domain (union-map-apply-domain dep map))
         (domain (union-map-apply-range domain map))
         (delta (union-map-deltas domain))
         (zeros (get-zeros-on-union-set delta))
         (le (union-set-lex-le-union-set delta zeros))
         (retval (union-set-is-empty le)))
    retval))

(defmethod verify-polyhedral-ir ((pg Polyhedral-IR)) (check-legality (poly-schedule pg) (poly-dependencies pg)))
;; ~~ Implementations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Note: This is hackable by users (as intended)
(defclass NoOpt (OptimizationRule) nil)
(defmethod optrule-generate-search-space (poly bands (id (eql :NoOpt))) (list (make-instance 'NoOpt)))
(defmethod optrule-apply-transform-on-polyhedral (poly (optrule NoOpt)) poly)

(defclass Reschedule (OptimizationRule)
  ((outer-coincidence :initarg :outer-coincidence :initform 0)
   (maximize-coincidence :initarg :maximize-coincidence :initform 0)
   (treat-coalescing :initarg :treat-coalescing :initform 0)
   (maximize-band-depth :initarg :maximize-band-depth :initform 0)
   (schedule-whole-component :initarg :schedule-whole-component :initform 0)
   (serialize-sccs :initarg :serialize-sccs :initform 0)
   (max-coefficient :initarg :max-coefficient :initform 1) ;; always set to 1 to keep simplicy!
   (max-constant-term :initarg :max-constant-term :initform 0))) ;; always set to 0 to keep simplicity!

(defmethod optrule-generate-search-space (poly bands (id (eql :Reschedule)))
  ;; Reschedule can be placed on the top of scheduling commands.
  (when (null (some #'(lambda (x) (typep x 'Reschedule)) (poly-cmd-history poly)))
    (list
     ;; [TODO] Isn't there more to search configurations?
     ;; [TODO] proximity/validity/coincidence, what is constraints?
     ;; [TODO] More Patterns!
     ;(make-instance 'Reschedule :serialize-sccs 1) ;; Loop Fission
     (make-instance 'Reschedule :outer-coincidence 0 :maximize-coincidence 1 :treat-coalescing 0 :maximize-band-depth 0 :schedule-whole-component 0)
     (make-instance 'Reschedule :outer-coincidence 0 :maximize-coincidence 0 :treat-coalescing 0 :maximize-band-depth 1 :schedule-whole-component 0)
     (make-instance 'Reschedule :outer-coincidence 1 :maximize-coincidence 1 :treat-coalescing 1 :maximize-band-depth 0 :schedule-whole-component 0))))

(defmethod optrule-apply-transform-on-polyhedral (poly (optrule Reschedule))
  (macrolet ((set-option (name slot)
	       `(cffi:foreign-funcall
                 ,(format nil "isl_options_set_~(~a~)" name)
                 :pointer (isl::context-handle isl::*context*)
                 :int (slot-value optrule ',slot)
		 :void)))
    (set-option "schedule_serialize_sccs" serialize-sccs)
    (set-option "schedule_max_constant_term" max-constant-term)
    (set-option "schedule_max_coefficient" max-coefficient)
    (set-option "schedule_outer_coincidence" outer-coincidence)
    (set-option "schedule_maximize_coincidence" maximize-coincidence)
    (set-option "schedule_treat_coalescing" treat-coalescing)
    (set-option "schedule_maximize_band_depth" maximize-band-depth)
    (set-option "schedule_whole_component" schedule-whole-component))
  (setf (poly-schedule poly) (schedule-constraints-compute-schedule (poly-make-schedule-constraints poly))))

(defun schedule-node-get-band-depth (band) (space-dim (schedule-node-band-get-space band) 3))

(defun permutations (lst)
  (if (null lst) (list nil)
      (mapcan (lambda (x) (mapcar (lambda (y) (cons x y)) (permutations (remove x lst :count 1)))) lst)))

(defclass Interchange (OptimizationRule)
  ((order :initarg :order :accessor interchange-order :type list)))

(defun reorder-mupa-string (mupa-str order)
  (declare (type string mupa-str) (type list order))
  (assert (and (>= (length mupa-str) 4)
               (string= (subseq mupa-str 0 2) "[{")
               (string= (subseq mupa-str (- (length mupa-str) 2) (length mupa-str)) "}]")))
  (let* ((inner (subseq mupa-str 2 (1- (length mupa-str))))
         (raw-chunks (cl-ppcre:split "\\}, *\\{" inner))
         (parts (mapcar (lambda (s) (string-trim " {}" s)) raw-chunks)))
    (assert (= (length parts) (length order)))
    (with-output-to-string (out)
      (format out "[")
      (loop for i from 0 below (length order)
            for idx = (nth i order)
            do (format out "{ ~a }" (nth idx parts))
               (when (< i (1- (length order)))
                 (format out ", ")))
      (format out "]"))))

(defmethod optrule-generate-search-space (poly bands (id (eql :Interchange)))
  (when nil ;; unable to run
  (loop for band in bands for nth upfrom 0
        if (eql :bool-true (isl::%isl-schedule-node-band-get-permutable (isl::schedule-node-handle band)))
          append
          (loop with default-perm = (caten/codegen/helpers:range 0 (schedule-node-get-band-depth band))
                with permutations = (permutations default-perm)
                for perm in permutations
                when (not (equal perm default-perm))
                  collect (make-instance 'Interchange :axis nth :band band :order perm)))))

(defmethod optrule-apply-transform-on-polyhedral (poly (opt Interchange)) nil) ;; [TODO]

(defun tiling-size (band size)
  (declare (type fixnum size))
  (let* ((band-space (schedule-node-band-get-space band))
         (dim (space-dim band-space 3)))
    (multi-val-from-val-list
     band-space(apply #'make-value-list (loop for i upfrom 0 below dim collect size)))))

(defclass Tile (OptimizationRule) ((size :initarg :size :accessor tile-size)))

(defmethod optrule-generate-search-space (poly bands (id (eql :Tile)))
  (loop for band in bands for nth upfrom 0
        append
        (loop for size in (slot-value (poly-strategy poly) 'caten/codegen/byoc::tile-search-space)
              do (assert (and (integerp size) (>= size 1)) () "tile-search-space must be a list of fixnum greater than zero!")
              collect
              (make-instance 'Tile :size size :band band :axis nth))))

(defmethod optrule-apply-transform-on-polyhedral (poly (opt Tile))
  (setf
   (poly-schedule poly)
   (schedule-node-get-schedule
    (schedule-node-band-tile (optrule-band opt) (tiling-size (optrule-band opt) (tile-size opt))))))

(defclass TileGPU (OptimizationRule)
  ((local-size :initarg :local-size :accessor tile-gpu-local-size)
   (band-split-at :initarg :band-split-at :accessor tile-gpu-band-split-at)))

(defun schedule-node-band-get-coincident (band)
  (loop for i upfrom 0 below (schedule-node-get-band-depth band)
        if (eql :bool-true (isl::%isl-schedule-node-band-member-get-coincident (isl::schedule-node-handle band) i))
          collect 1 else collect 0))

(defun schedule-node-band-no-directive-p (band name)
  (declare (type string name))
  (labels ((explore (node)
             (when (eql :bool-false (isl::%isl-schedule-node-has-parent (isl::schedule-node-handle node)))
               (return-from schedule-node-band-no-directive-p t))
             (let ((parent (isl::schedule-node-parent node)))
               (case (schedule-node-get-type parent)
                 (:schedule-node-domain (explore parent))
                 (:schedule-node-mark
                  (let ((id (str->directive (cffi:foreign-string-to-lisp (isl::%isl-id-get-name (isl::%isl-schedule-node-mark-get-id (isl::schedule-node-handle parent)))))))
                    (if (equalp (directive-type id) name)
                        (return-from schedule-node-band-no-directive-p nil)
                        (explore parent))))
                 (otherwise (explore parent))))))
    (explore band)
    t))

(defmethod optrule-generate-search-space (poly bands (id (eql :TileGPU)))
  ;; TileGPU Can be applied at once
  (when (>= (slot-value (poly-strategy poly) 'caten/codegen/byoc::ptile-max-rank) 2)
    (loop with max-threads = (slot-value (poly-strategy poly) 'caten/codegen/byoc::local-max)
          for band in bands for nth upfrom 0
          for valid-p = (schedule-node-band-no-directive-p band "TILEGPU")
          for coincident = (schedule-node-band-get-coincident band)
          for split-at-base = (or (position 0 coincident) (length coincident))
          for split-at = (min split-at-base (slot-value (poly-strategy poly) 'caten/codegen/byoc::ptile-max-rank))
          if (and valid-p (> split-at 0) (every #'(lambda (x) (= x 1)) (subseq coincident 0 split-at)))
            append
            (loop for size in (slot-value (poly-strategy poly) 'caten/codegen/byoc::ptile-search-space)
                  do (assert (and (integerp size) (>= size 1)) () "ptile-search-space must be a list of fixnum greater than zero!")
                     if (or (null max-threads) (<= (expt size split-at) max-threads))
                       collect
                       (make-instance 'TileGPU :local-size size :band-split-at (if (= (length coincident) split-at) nil split-at) :band band :axis nth)))))

(defmethod optrule-apply-transform-on-polyhedral (poly (opt TileGPU))
  (let* ((depth (or (tile-gpu-band-split-at opt) (schedule-node-get-band-depth (optrule-band opt))))
         (band (schedule-node-insert-mark
                (optrule-band opt)
                (directive->id (directive "TILEGPU" (tile-gpu-local-size opt) depth nil))))
         (band (if (tile-gpu-band-split-at opt)
                   (schedule-node-band-split (schedule-node-get-child band 0) (tile-gpu-band-split-at opt))
                   band)))
    (setf
     (poly-schedule poly)
     (schedule-node-get-schedule band))))

(defmethod optrule-apply-transform-on-blueprint ((directive-id (eql :TileGPU)) bands blueprint)
  "Applies the tile and mapping them into blockIdx/threadIdx in CUDA. The inner tile is always further fused for memory locality.
e.g.
for (int i=0; i<32; i+=2)
  for (int j=0; j<32; j+=2)
    for (int ii=0; ii<2; ii++)
      for (int jj=0; jj<2; jj++)
        A[i+ii, j+jj]
=>
for (int i=0; i<32; i+=2)
  for (int j=0; j<32; j+=2)
    for (int ii=0; ii<2*2; ii++)
        A[i+(ii/2), j+(jj%2)]
=>

"
  ;; [TODO] Loop FissionされたBlueprintにTILEを適用すると，RANGE expects IDX ... で失敗する。
  (assert (= (length bands) (directive-depth (getattr (car bands) :directive))))
  (let* ((new-bp (ast-band-tile-gpu blueprint (car (last bands)) (loop for b in bands collect (directive-amount (getattr (car bands) :directive)))))
         (innerbands (loop for node in (graph-nodes new-bp)
                           if (and (eql (node-type node) :SPACE) (eql (getattr node :level) :thread))
                             collect node))
         (innerbands (sort innerbands #'< :key #'(lambda (x) (getattr x :rank))))
         (blocksize (directive-amount (getattr (car bands) :directive))))
    (case (length innerbands)
      (2
       (let* ((thread (%lid 0 (caten/aasm/expr:expr-mul (getattr (car innerbands) :size) (getattr (second innerbands) :size))))
              (x (with-context-nodes (_ (%idiv thread (%load (%salloc :dtype :int64) blocksize) :id (car (node-writes (nth 0 innerbands)))))))
              (y (with-context-nodes (_ (%mod  thread (%load (%salloc :dtype :int64) blocksize) :id (car (node-writes (nth 1 innerbands))))))))
         (insert-nodes new-bp (append (list thread) x y))))
      (3
       (let* ((thread (%lid 0 (caten/aasm/expr:expr-mul (getattr (nth 0 innerbands) :size) (getattr (nth 1 innerbands) :size) (getattr (nth 2 innerbands) :size))))
              (x (with-context-nodes (_ (%idiv thread (%load (%salloc :dtype :int64) (* blocksize blocksize)) :id (car (node-writes (nth 0 innerbands)))))))
              (y (with-context-nodes (_ (%mod (%idiv thread (%load (%salloc :dtype :int64) blocksize)) (%load (%salloc :dtype :int64) blocksize) :id (car (node-writes (nth 1 innerbands)))))))
              (z      (with-context-nodes (_ (%mod thread (%load (%salloc :dtype :int64) blocksize) :id (car (node-writes (nth 2 innerbands))))))))
         (insert-nodes new-bp (append (list thread) x y z)))))
    new-bp))

(defclass Parallel (OptimizationRule) ((depth :initarg :depth :accessor parallel-depth))) ;; [TODO] CPU Parallel Using OpenMP
(defmethod optrule-generate-search-space (poly bands (id (eql :Parallel)))
  (when (= (slot-value (poly-strategy poly) 'caten/codegen/byoc::ptile-max-rank) 1)
    (loop for band in bands for nth upfrom 0
          for valid-p = (schedule-node-band-no-directive-p band "PARALLEL")
          for coincident = (schedule-node-band-get-coincident band)
          for split-at = (or (position 0 coincident) (length coincident))
          if (and (> split-at 0) (every #'(lambda (x) (= x 1)) (subseq coincident 0 split-at)))
            collect (make-instance 'Parallel :depth (if (= (length coincident) split-at) nil split-at) :band band :axis nth))))

(defmethod optrule-apply-transform-on-polyhedral (poly (opt Parallel))
  (let* ((depth (or (parallel-depth opt) (schedule-node-get-band-depth (optrule-band opt))))
         (band (schedule-node-insert-mark
                (optrule-band opt)
                (directive->id (directive "PARALLEL" 0 depth nil))))
         (band (if (parallel-depth opt)
                   (schedule-node-band-split (schedule-node-get-child band 0) (parallel-depth opt))
                   band)))
    (setf
     (poly-schedule poly)
     (schedule-node-get-schedule band))))

(defmethod optrule-apply-transform-on-blueprint ((id (eql :PARALLEL)) bands blueprint)
  (setf blueprint (caten/aasm::ast-band-collapse blueprint (reverse bands) :parallel 1))
  blueprint)
;; [TODO] Caten Level Loop Collapse
;; [TODO] Auto Scheduler Loop Collapse (aasm transformation rule!) これ1DになってSimplifyできたら面白そうじゃね?
(defclass Collapse (OptimizationRule) nil)

;; [TODO] FlashAttention, This will rewrite a graph
(defclass FuseWithParent (OptimizationRule) nil)
(defclass TensorCore (OptimizationRule) nil) ;; TODO
(defclass SplitReduce (OptimizationRule)
  ;; :mark :reductionを使用するようにしたい。 (TODO: It has two mode, :warp level and :block level)
  nil)

(defclass Vectorize (OptimizationRule) ((width :initarg :width :accessor vectorize-width)))
(defmethod optrule-generate-search-space (poly bands (id (eql :Vectorize)))
  (loop for band in bands for nth upfrom 0
        append
        (loop for size in (slot-value (poly-strategy poly) 'caten/codegen/byoc::vectorize-search-space)
              do (assert (and (integerp size) (>= size 1)) () "vectorize-search-space must be a list of fixnum greater than zero!")
              collect
              (make-instance 'Vectorize :width size :band band :axis nth))))

(defmethod optrule-apply-transform-on-polyhedral (poly (opt Vectorize))
  (let* ((child (schedule-node-insert-mark (optrule-band opt) (directive->id (directive "VECTORIZE" (vectorize-width opt) 1 NIL)))))
    (setf (poly-schedule poly) (schedule-node-get-schedule child))))

(defmethod optrule-apply-transform-on-blueprint ((directive-id (eql :VECTORIZE)) bands blueprint)
  (warn "WIP: Vectorize Rewrite")
  (let ((d (getattr (car bands) :directive)))
;;    (caten/codegen/blueprint:print-blueprint blueprint t)
    (loop for band in bands do
      (setf
       blueprint
       (caten/aasm::ast-band-unroll blueprint band (list (directive-amount d)) :rewriter #'caten/aasm::ast-unroll-body)))
    (simplify-ast blueprint) (simplify-ast blueprint)
;;    (caten/codegen/blueprint:print-blueprint blueprint t)
    blueprint))
;; RootがReschedule->Reorderなら...的な話かも
;; うまく言語化できないけど，最初にReorder -> Tileとかで，求めるOptimalに到達する可能性があるから，やっぱり木構造で順番に
;; Apply Optsしていく探索空間をイメージするのでうまくいくんじゃないかな
;; PPRINTを充実させるか，とっととParser作ってもろて
;; - poly-ir-schedule-node: これを追加するべきか？
;; - [TODO] Reductionのval_2 = ...のScalar, Write, これをMatrixにする
;; ~~ AutoScheduler Implementation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defparameter *search-space* ;; (n-generation . Candidates)
  '((0 . (:NoOpt :Reschedule))  ;; Solve ILP with multiple strategy (Detect Band/Coincidence, Loop Fussion at early stage)
    ;; (1 . (:NoOpt :Interchange)) ;; Shuffle the memory order for finding the best candidate!
    (t . (:NoOpt :Parallel :TileGPU))))  ;; Recursively optimize things ... ;; :TILE, :VECTORIZE

(defmethod get-next-optimization-rules ((polyhedral Polyhedral-IR))
  (let ((n-generation (length (poly-cmd-history polyhedral)))
        (bands (schedule-node-get-undernearth-bands (schedule-get-root (poly-schedule polyhedral)))))
    (loop for space in (cdr (or (find n-generation *search-space* :key #'car) (find t *search-space* :key #'car) (error "No *search-space* configuration for t")))
          append (optrule-generate-search-space polyhedral bands space))))

(defmethod polyhedral-ir-mutate-for-children ((polyhedral Polyhedral-IR))
  (let* ((space (get-next-optimization-rules polyhedral))
         (next-generations
           (remove-duplicates
            (loop for opt in space collect (apply-optimization polyhedral opt)) ;; [TODO] This should be lowered first.
            :test #'string= :key #'pg-dump-into-str)))
    (loop for gen in next-generations
          if (verify-polyhedral-ir gen) collect gen)))

(defun make-kernel-from-blueprint (base-node kernel-cls base-kernel blueprint nth dep)
  (let* ((args
           (loop for b in (graph-nodes blueprint)
                 if (eql (node-type b) :DEFINE-GLOBAL)
                   collect b))
         (read-args
           (loop for arg in args
                 if (find (car (node-writes arg)) (node-writes base-node))
                   collect (intern (format nil "~a_dst" (car (node-writes arg))))
                 else
                   collect (car (node-writes arg)))))
    ;; [TODO] Determine write-to, how to do this?
    ($kernel dep read-args
             (make-instance
              kernel-cls
              :name (intern (format nil "~a_~a" (kernel-name base-kernel) nth))
              :blueprint blueprint
              :args args
              :flops (kernel-flops base-kernel))
             :optimized-p t
             :out (car (node-writes base-node)))))

(defmethod polyhedral-ir-evaluate ((polyhedral Polyhedral-IR) runtime node abstract-kernel args n base-name base-args)
  (let ((renderer (make-instance (caten/codegen/byoc:get-backend-renderer (ctx:getenv :BACKEND)))))
    (multiple-value-bind (generated-kernels extra-allocs) (get-blueprint-from-polyhedral polyhedral)
      (let ((kernels
              (loop with dep = (subseq (node-reads node) 0 (getattr node :n-kernel-args))
                    for kernel in generated-kernels for nth upfrom 0
                    collect
                    (let ((kernel (make-kernel-from-blueprint
                                   node
                                   (class-name (class-of abstract-kernel))
                                   abstract-kernel kernel nth dep)))
                      (setf dep (list (node->id kernel)))
                      kernel))))
        (setf (node-writes (car (last kernels))) (copy-list (node-writes node)))
        ;; Save the result for when the polyhedral was selected as a best kernel
        (setf (poly-bp-cache polyhedral) kernels
              (poly-extra-allocs polyhedral) extra-allocs)
        (when (>= (ctx:getenv :JIT_DEBUG) 2)
          (lformat "== [Evaluation] ====================================~%```~%")
          (loop for nth upfrom 0 for blueprint in kernels do
            (lformat "// ~ath kernel~%" nth)
            (caten/codegen/blueprint::print-blueprint (kernel-blueprint (getattr blueprint :kernel-info)) nil))
          (lformat "~%```")
          (lformat "~%[Polyhedral]:~%~a~%" (pprint-isl-schedule (poly-schedule polyhedral)))
          (lformat "[Schedules]:~%")
          (dolist (s (reverse (poly-cmd-history polyhedral)))
            (lformat "~a~%" s)))
        (loop for kernel in kernels do
          (caten/codegen/byoc:%render-kernel renderer (getattr kernel :kernel-info)))
        (handler-case
            (caten/codegen/byoc:%compile-kernel renderer (map 'list #'(lambda (x) (getattr x :kernel-info)) kernels) nil)
          (error (c)
            (funcall (if *allow-compilation-error-during-beam* #'warn #'error) "Failed compilation due to ~a" c)
            (return-from polyhedral-ir-evaluate *+inf*)))
        (let* ((extra-args
                 (loop for arg in (poly-extra-allocs polyhedral)
                       collect (cons (car (node-writes arg)) (uiop:symbol-call :caten/runtime/runtime :realize-node :Allocate runtime arg (node-reads arg)))))
               (total 0.0))
          (flet ((getvar (id)
                   (if (find id extra-args :key #'car)
                       (cdr (find id extra-args :key #'car))
                       (if (numberp id) id (uiop:symbol-call :caten/runtime/runtime :runtime-getvar runtime id)))))
            (dotimes (i n)
              (dolist (node kernels)
                (let ((arg-symbols (subseq (node-reads node) (getattr node :n-kernel-args))))
                  (incf total (kernel-call (getattr node :kernel-info) runtime node (map 'list #'getvar arg-symbols)))))))
          ;; 任意の条件を満たさないカーネルは実行するまでもなく+Inf時間でいいように思える
          (map 'list #'(lambda (x) (uiop:symbol-call :caten/runtime/buffer :close-buffer runtime (cdr x))) extra-args)
          (when (>= (ctx:getenv :JIT_DEBUG) 1)
            (lformat "[CostFunction]: ~a(s) ~aGFLOps~%" total (compute-gflops (kernel-flops (getattr (car kernels) :kernel-info)) (/ total n) nil)))
          total)))))

(defun realize-node-with-autotuning (runtime node args
                                     &aux
                                       (base-args (kernel-args (getattr node :kernel-info)))
                                       (base-name (kernel-name (getattr node :kernel-info)))
                                       (beam-width (ctx:getenv :BEAM))
                                       (threshold 1e-5) ;; 改善率で計測すべきでは
                                       (auto-scheduler (make-instance (get-backend-auto-scheduler (ctx:getenv :BACKEND))))
                                       (strategy (auto-scheduler-strategy auto-scheduler))
                                       (spos (length (format nil "~a : [SEARCH] " (caten/common.logger::timestamp)))))
  (when (getattr node :optimized-p) (return-from realize-node-with-autotuning t))
  (when (>= (ctx:getenv :JIT_DEBUG) 2)
    (separate/print-info spos "[SEARCH] ┃ Autotuning the kernel ~a" (kernel-name (getattr node :kernel-info))))
  (with-slots ((n caten/codegen/byoc::n-profile) (per-band-optrules caten/codegen/byoc::per-band-optrules)) strategy
    (with-isl-context
      (labels ((make-candidate (polyhedral-ir)
                 (declare (type Polyhedral-IR polyhedral-ir))
                 (cons polyhedral-ir (polyhedral-ir-evaluate polyhedral-ir runtime node (caten/air:getattr node :kernel-info) args n base-name base-args))))
        (let* ((band-count (count :RANGE (graph-nodes (kernel-blueprint (getattr node :kernel-info))) :key #'node-type))
               (max-iters (+ 2 (* band-count per-band-optrules)))
               (origin (make-polyhedral-from-blueprint (kernel-blueprint (caten/air:getattr node :kernel-info)) :strategy strategy))
               (beam (list (cons origin *+inf*))))
          ;; Print Info
          (when (>= (ctx:getenv :JIT_DEBUG) 2)
            (lformat "Strategy: max_iters=~a, band_count=~a, threshold=~a~%" max-iters band-count threshold))
          (loop named beam for iter upfrom 0 below max-iters for candidates = nil do
            (when (>= (ctx:getenv :JIT_DEBUG) 2) (print-info "[~ath BEAM n=~a]:~%" iter (length beam)))
            (loop for (kernel . score) in beam do
              (dolist (new-kernel (polyhedral-ir-mutate-for-children kernel))
                (push (make-candidate new-kernel) candidates)))
            (when (null candidates) (return-from beam)) ;; no new candidates -> exit
            (setf candidates (sort candidates #'< :key #'cdr))
            (let ((new-beam (subseq candidates 0 (min (length candidates) beam-width))))
              (when (< (abs (- (cdar beam) (cdar new-beam))) threshold)
                (setf beam new-beam)
                (return-from beam))
              (setf beam new-beam)))
          (let ((best-kernel (car beam)))
            (when (>= (ctx:getenv :JIT_DEBUG) 1)
              (lformat "[BestKernel]:~%")
              (lformat "~a" (car best-kernel))
              (lformat "~%Evaluation: ~a(s)" (cdr best-kernel)))
            (loop for extra-arg in (append (poly-bp-cache (car best-kernel)) (poly-extra-allocs (car best-kernel)))
                  if (eql (node-type extra-arg) :Allocate) do (setf (getattr extra-arg :pool) nil)
                  do (uiop:symbol-call :caten/codegen/jit :register-autotune-node extra-arg))
            ;; [TODO] Copy the initial results? to avoid overflow? or for sparse optimizations?
            t))))))
;; [TODO]
;; BEAM Enhancements
;; - [ ] Transform More Things on ISL
;;  - [ ] Unroll
;;    - [ ] Scalarifyが完全に邪魔
;;    - [ ] Vectorizeも
;;    - [ ] TensorCore検索
;;  - [ ] TileGPU
;;  - [ ] Coalesce
;;  - [ ] Shared Memory, ReduceSplit
;;  - [ ] Interchange
;; - [ ] More Beautiful Logger
;; - [ ] Support Symbolics


;; [TODO] 戻ったらやること
;; - [x] RuntimeGraphのカーネル呼び出しの仕様を変える。_dstは気持ち悪い。
;;   - [x] Kernel(Kernel(X), Kernel(Y, tensors), tensors) みたいにする。KERNEL((DEPEND_KERNELS), DEPEND_TENSORS)
;;   - [x] RuntimeGraph作れるように。
;; - [x] TileGPUの付与について -> ScheduleTreeをRootからTraverseして探索する方法に変える
;;   - [x] これによって，複数のTileGPUが付与される。
;; - [x] 探索空間下に戻す
;; - [x] Measure the score based on GFLOPs
;; - [ ] Parallel+TILE is not working
;; - [ ] LoopCollapse Standalone
;; - [ ] Implement Float4(Upcast) Workload
;;  - [ ] val_2のIndexingで悩むが，これはUnrollする範囲にEXPR Definitionがあるかどうかで決めれば良い？(Scalar Expr == Let in Common Lisp)
;;  - [ ] 先に!sumとかの展開でFailするのを直す (1. EXPR ... is not found?, 2. A should be EXPR but getting)
;;    - [ ] これはLoop Fissionをサポートしていないのが悪い。(FOR(EXPR, ))を満たさないのは。
;;    - [ ] !sigmoid -> TypeInference
;;    - [ ] Range Repro ->
;;    - [ ] !sum :axis t looks slow ... they canot use tilegpu? 
;;  - [ ] TypeInference+Unrollを再利用することで実装
;;  - [ ] Upcast*Upcast -> TensorCore Mappingを考える
;;  - [ ] TileGPU, VISIBLE=Tに変更する (so further vectorized)
                                        ;
                                        ; [TODO]
;; - [x] Bring Back Metal Renderer
;; - [x] Bring Back Lisp Renderer (BEAM is too slow on my mac)
;; - [x] Define AutoSchedulerConfig
;; - [x] TileGPU -> use render-ops.lisp feature and insert mark
;;  - [x] Provide the directive class, and parse utils
;;  - [x] TileGPU is just splitting the band w/ coincidence parts
;;  - [ ] Optimization on Reduction
;;  - [ ] TensorCore, SIMD, i.e., float4
;;  - [ ] Support Loop Fission, and post loop collapse.
;;  - [ ] Unroll is applied automatically, there should be a threshold for applying this
;;  - [ ] How to implement loop coalescing to the band tile?
;;  - [ ] float4, unroll!
;; - Then all have to do is to get optimal kernel!
;; - カーネルの分割/融合を正しくサポートする
;; - Loop Interchange is REQUIRED
;; - 4. fix a bug in threefry2x32
;; - 5. ループの途中でincf挿入するやつやりたい?
;; - 6. BEAM Cacheを実装する
;; - 7. Symbolic Kernelに対して，探索したSchedule Commandsを適用する？
;; - 8. val_2がSeparateされたとき，追加も一時領域Bufferを作成する (そんな難しくないという認識)
;;  - 1. DetectSeparateScheduledを実装
;;  - 2. Extractするときに，ISLに登録した通りにBufferを登録する。Argsは増えることになる。
;; - 9. Support Symbolics. I think it is doable.

;; Paper: https://arxiv.org/pdf/2410.03210
;; [TODO] Implement Polyhedral-Guided, Customizable AutoScheduler Engine
;; https://chatgpt.com/c/6870e59d-2970-8005-abda-1b62c5808111?model=o3-pro
;; o3-pro proposed the following:
;; 1. reduce dependencies are not handled
;; 2. scalar is not a scalar
"
| 名前                             | 意味・対象                          | 主な効果 (GPU 観点)                                     |
| ------------------------------ | ------------------------------ | ------------------------------------------------- |
| **reorder\_ikj**               | ループ順序交換                        | メモリアクセス連続化，依存を壊さずに L/S 帯域↑。                       |
| **tile\_block\_ij{M×N}**       | CTA (block) タイル化               | グローバル→SMEM 転送の削減。CTA 数で並列度を制御。                    |
| **tile\_warp\_ij{m×n}**        | Warp タイル化                      | Tensor Core／simdgroup 演算単位と一致させ，LD/ST を coalesce。 |
| **tile\_lane\_vec{w}**         | Lane 内 SIMD (vec2/4/8)         | ld.v4/st.v4 等のベクトル命令で帯域効率↑。                       |
| **split\_k\_reduce{p}**        | k 軸並列 + 反復還元                   | CTA/SM 並列度拡大。`p` は分割数。                            |
| **cache\_read\_shared\_{A/B}** | 行列タイルの SMEM 読み込み               | L2⇔SMEM 帯域と再利用回数の最適化。                             |
| **cache\_write\_shared**       | 出力タイルを SMEM 経由で書き戻し            | 書き込みコアレッシング・衝突回避。                                 |
| **sw\_pipeline\_stage{n}**     | 二重/三重バッファリング                   | データ転送と演算をオーバラップし隠蔽。                               |
| **tensorcore\_mma**            | 専用 FMA (16×16×16 等)            | FLOPs/clk を劇的に向上。Metal は `simdgroup_mad`.         |
| **vector\_fma**                | 汎用 SIMD FMA                    | Tensor Core が無い GPU で使用。                          |
| **unroll\_k{u}**               | 内部ループ展開                        | ループ制御除去と ILP 向上。                                  |
| **async\_copy**                | 非同期 cp.async / simdgroup\_copy | 転送レイテンシ隠蔽と帯域最大化。                                  |
| **rfactor\_axis{k}**           | 還元因子分割                         | prefix‑sum, attention‐score reduce 等で並列度↑。        |
| **fuse\_{i,j}**                | ループ／演算子融合                      | 中間テンソル排除，DRAM 往復削減。                               |
| **recompute\_small**           | 小演算の再計算でメモリ節約                  | O(N²)→O(N) メモリモデルで有効。                             |
| **auto\_vector\_width**        | 動的 SIMD 幅決定                    | GPU 世代差・データ型差を隠蔽。                                 |


def softmax(input[1280, 1280], output[1280, 1280]):
  allocate temp[1280, 1280]
  for n in parallel(1280):
    val_11 = 0.0
    val_2 = -Inf
    for m in range(1280):
      val_2 = max(val_2, input[1280*n+m]
    for m in range(1280):
      temp[_gid0*n+m] = exp2((input[1280*n+m]-val_2)*1.442695)
      val_11 = val_11 + temp[_gid0*n+m]
    for m in range(1280):
      output[n*1280+m] = val_9[n*1280+m]/val_11
//=============================================================================
// softmax_stored_locally_multi_dim の “warpOnly” 完全インライン化版
//=============================================================================
def softmax_stored_locally_multi_dim(input, output, m, n):
    num_packs = ceil((n/4) / blockDim.x)
    parallel for block_x in range(gridDim.x):
      parallel for thread_y in range(blockDim.y):
        parallel for tid in range(blockDim.x):
          for row in range(block_x*blockDim.y + thread_y, m, gridDim.x*blockDim.y):
            row_offset = row * (n>>2)
            row_x = input  + row_offset
            row_y = output + row_offset

            //── ローカル読み込み＋最大値
            buf = allocate float4[num_packs]
            local_max = -Inf
            for pack_id in range(num_packs):
              col_base = pack_id*blockDim.x + tid
              vectorize(4):
                if col_base + lane < n/4:
                  h4 = row_x[col_base]
                  buf[pack_id][lane] = half2float(h4.component[lane])
                else:
                  buf[pack_id][lane] = -Inf
              local_max = max(local_max,
                              max(max(buf[pack_id].x, buf[pack_id].y),
                                  max(buf[pack_id].z, buf[pack_id].w)))

            //── ワープ内最大値還元
            for mask in [blockDim.x/2, blockDim.x/4, …, 1]:
              local_max = max(local_max,
                              __shfl_xor_sync(0xffffffff,
                                              local_max,
                                              mask,
                                              32))

            //── exp＋ワープ内和還元
            local_sum = 0.0
            for i in range(num_packs):
              vectorize(4):
                buf[i][lane] = exp(buf[i][lane] - local_max)
                local_sum += buf[i][lane]
            for mask in [blockDim.x/2, blockDim.x/4, …, 1]:
              local_sum += __shfl_xor_sync(0xffffffff,
                                           local_sum,
                                           mask,
                                           32)

            //── 書き戻し
            for i in range(num_packs):
              col_base = i*blockDim.x + tid
              vectorize(4):
                if col_base + lane < n/4:
                  row_y[col_base].component[lane] = buf[i][lane] / local_sum

def matmul(X[128, 128], Y[128, 128], OUT[128, 128]):
  for i in range(128):
    for j in range(128):
      acc = 0.0f
      for k in range(128):
          acc = acc + X[128*i+k] * Y[128*k + j]
      OUT[128*i+j] = acc

// CTA タイル = 64×64，Warp タイル = 16×16，lane = vec4
for B_i in blockIdx.y parallel tile_block_ij:64  // 0..1
  for B_j in blockIdx.x parallel tile_block_ij:64
    // 共有メモリキャッシュ
    shared Xs[64][64] @cache_read_shared_X
    shared Ys[64][64] @cache_read_shared_Y

    // Warp グリッド (=4×4 Warps per CTA)
    for W_i in threadIdx.y parallel tile_warp_ij:16  // 0..3
      for W_j in threadIdx.x/32 parallel tile_warp_ij:16  // 0..3
        // レジスタ蓄積
        reg_C[16][16] = 0

        //--------- k 軸分割（split_k_reduce=2）-----------
        for k_outer in range(0, 128, 64):
          // 二重バッファで Xs, Ys を先読み (sw_pipeline_stage2 + async_copy)
          prefetch X, Y tiles → Xs, Ys

          //---------------- 主演算 ------------------------
          for k_inner in unroll_k8 range(0, 64, 8):
            // lane = vec4 で 4 要素ロード
            vectorize(4):
              reg_A = ld4(Xs, W_i*16 + lane, k_inner + s)    // s=0..7
              reg_B = ld4(Ys, k_inner + s, W_j*16 + lane)
            // Tensor Core (16×16×8 FMA) または simdgroup_mad
            tensorcore_mma(reg_C, reg_A, reg_B)
          //------------------------------------------------

        //--------- 出力 (vec4 store) ---------------------
        vectorize(4):
          st4(OUT,
              (B_i*64 + W_i*16 + lane),
              (B_j*64 + W_j*16 + vec4-id),
              reg_C[lane][:])


"

;; One week is enough to run FlashAttention for me ...
;; [Workload]
;; - コストモデル, 測定方法はどうでもいい。
;; ASTに対する
;;   - 探索方法(DFS)
;;   - How to transform the loop?
;; We already have a naive and fused FlashAttention Kernel.
;; We need:
;; - Loop Tiling               (Unroll, Vectorize, )
;; - Evaluate the cacheability (The number of alloc reuse)
;;  - 早い指標でTop Kを決めて, 最後にまとめてCompileしてTop1を決める，はできるかもしれない
;; これかが決定すれば自動スケジューラーが完成する
;; - Load from VRAM -> SRAM -> Register. Polyhedralでどう表現しますか
;; - optimal_kernel = apply(base_kernel, [] in SUBSET)
;;   - What is SUBSET?
;; - Ops.TRANSFER的なのを実装する (n-layered memory caching arch)
;;   - [ ] SIMD/Warp
;;   - [ ]

;; Optimization is applied into:
;; - 最適なループ形状 + :AREF={CACHE, NOOPT}の空間を探索？
;; - :FOR Level (Everything is ND TILING)
;;   - Loop Tiling
;;   - Loop Unrolling (i.e.: VECTORIZE, PARALLELIZE)
;; - :AREF Level
;;  - (ast-apply-cache的なsomethingが必要) VRAM -> DRAM -> SRAM
;;  -
;; The ft of Tensor Compiler is POLYHEDRAL Compiler.
;; - Believe Polyhedral Compiler, implement something like polyhedral compiler.
;; e.g.: Caten can create both of:
;;   - LU Decomposing
;;   - 

;; TODO
;; - 1. Introduce Ops.FUNCTION
;;   - 2. Hand-writtern Kernelを記述するMacro, 構文を実装する
;; - 2. Thinking the minimal Softmax Transformation
;; - 3.

;; [SearchSpace] => {Action} => [SearchSpace]

;; (defkernel xxx (...) (:policy `(d . 128))) <- specify OptimizationPolicy

;; Policy:
;; - GraphRewriteでRuntimeGraphが常にStaticであることを保証して，進めていく
;; - いずれにせよグラフの形状がわからないと進められない。
;; ScheduleCommands:
;; 1. new_graph, hi, ho = apply_tile(graph, ...)
;;
;; Workload
;; LoopInterchange is actually what we need:
;; - Implement Polyhedral
;; Schedule + RaW Accessing, 同じフォーマットである必要？
;; - なぜループが0から始まらないのか？
