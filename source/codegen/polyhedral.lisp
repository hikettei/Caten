(defpackage :caten/codegen/polyhedral
  (:shadow #:set #:space)
  (:shadowing-import-from :cl :map)
  (:use :cl :caten/air :caten/aasm :caten/isl :caten/codegen/byoc)
  (:import-from :caten/codegen/renderer #:render-node #:Default-Renderer)
  (:export
   #:realize-node-with-autotuning
   #:make-polyhedral-from-blueprint
   #:get-blueprint-from-polyhedral))

(in-package :caten/codegen/polyhedral)

(defparameter *+inf* (expt 2 32))
;; ~~ Polyhedral ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;;; blueprint -> polyhedral
(defclass Polyhedral-IR ()
  ((schedule :accessor poly-schedule :initarg :schedule)
   (domain   :accessor poly-domain :initarg :domain)
   (dependencies :accessor poly-dependencies :initarg :dependencies)
   (cmd-history :accessor poly-cmd-history :initform nil :initarg :history)
   (blueprint :accessor poly-blueprint :initarg :blueprint)))

(defun make-polyhedral-ir (blueprint domain read write schedule)
  (let ((pg (make-instance 'Polyhedral-IR)))
    (setf (poly-schedule pg) schedule (poly-domain pg) domain (poly-blueprint pg) blueprint)
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
  (make-instance 'Polyhedral-IR :schedule (copy (poly-schedule pg)) :history (copy-list (poly-cmd-history pg)) :dependencies (poly-dependencies pg) :domain (poly-domain pg) :blueprint (poly-blueprint pg)))

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
    (return-from render-default-isl-access (gethash idx (ctx-scal->access ctx))))
  (let* ((shape (loop for l in loops for size = (getf l :size) for expr = (id->value bp size) for node = (id->value bp (car (node-reads expr)))
                      ;; Determining the loop size from graph. (TODO: Assert RANGE(SIZE, STEM) where SIZE is always EXPR, and EXPR(LOAD(Constant)) Pattern
                      collect (progn (assert (eql (node-type node) :LOAD)) (assert (numberp (getattr node :value))) (getattr node :value))))
         (strides (caten/codegen/helpers:row-major-calc-strides shape))
         (access (format nil "~{~a~^+~}" (loop for s in strides for l in loops for idx = (getf l :idx) collect (format nil "~a*~(~a~)" s idx)))))
    (setf (gethash idx (ctx-scal->access ctx)) access)
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

(defun make-polyhedral-from-blueprint (blueprint)
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
  (let* ((ctx (make-scop-ctx-from-blueprint blueprint))
         (domain (union-set-from-str (render-domains ctx blueprint)))
         (schedule (rewrite-blueprint-tree->schedule-tree ctx blueprint))
         (reads/writes (extract-accesses ctx blueprint)))
    (make-polyhedral-ir blueprint domain (union-map-from-str (car reads/writes)) (union-map-from-str (cdr reads/writes)) schedule)))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;;; Polyhedral -> Blueprint
(defstruct (parse-ctx
            (:constructor make-parse-ctx (blueprint))
            (:conc-name pctx-))
  (blueprint blueprint :type Graph)
  (gid2range (make-hash-table) :type hash-table)
  (gid2offset (make-hash-table) :type hash-table)
  (variable-table (make-hash-table) :type hash-table)
  (scop-ctx (make-scop-ctx-from-blueprint blueprint) :type ctx))

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
                 (when (or (null node) (gethash (node-id node) visited)) (return-from e))
                 (when (eql (node-type node) :EXPR) (return-from e))
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
                     (return-from e)))
                 (when (and (eql (node-type node) :LOAD) (gethash (getattr node :value) rewrite-map))
                   (let ((new-space (gethash (getattr node :value) rewrite-map)))
                     (let ((n (copy-node new-space)))
                       (assert (= 1 (length (node-writes n))))
                       (setf (node-writes n) (list id)
                             (node-id n) (gensym "NID"))
                       (emit n))
                     (return-from e)))
                 ;; [TODO] Replace %RANGE here if exists
                 (setf (gethash (node-id node) visited) t)
                 (emit node)
                 (mapc #'e (node-reads node))))
        (mapc #'e (node-reads node))
        (emit node)))))

(defun get-blueprint-from-polyhedral (polyhedral)
  "Convert ISL polyhedral representation back to blueprint graph"
  (declare (type Polyhedral-IR polyhedral))
  (let ((ast (->ast (poly-schedule polyhedral) (poly-get-rank polyhedral))))
    (declare (type isl::ast-node ast))
    (caten/aasm::ast-simplify-expr-subgraph
     (with-blueprint () (parse-isl-ast (make-parse-ctx (poly-blueprint polyhedral)) (isl::ast-node-handle ast))))))
;; ~~ OptimizeRule ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass OptimizationRule ()
  ((axis :initarg :axis :accessor optrule-axis :initform nil)
   (band :initarg :band :accessor optrule-band :initform nil)))

(defmethod print-object ((obj OptimizationRule) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream ":axis ~a" (optrule-axis obj))))

(defgeneric optrule-generate-search-space (polyhedral bands optrule-trigger))
(defgeneric optrule-apply-transform-on-polyhedral (polyhedral optrule)) ;; Insert Directive
(defgeneric optrule-apply-transform-on-blueprint (polyhedral optrule))  ;; Directive Parse

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
                  for mark = (when (eql (schedule-node-get-type node) :schedule-node-mark) (identifier-name (schedule-node-mark-get-id node)))
                  for band = (schedule-node-get-child node nth)
                  for type = (schedule-node-get-type band) do
                    (let ((out (funcall f type band mark))) (when out (push out outputs)))
                    (push band next-nodes))
            (when (= (length next-nodes) 0) (return-from map-search))
            (setf node (pop next-nodes)))
    (nreverse outputs)))

(defun schedule-node-get-undernearth-bands (schedule-node)
  (declare (type isl::schedule-node schedule-node))
  (map-schedule-node-children #'(lambda (type band mark) (declare (ignore mark)) (when (eql type :schedule-node-band) band)) schedule-node))

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
(defmethod optrule-apply-transform-on-blueprint (poly (optrule NoOpt)) nil)

(defclass Rewrite/FuseLoadReduceStore (OptimizationRule) nil)
(defmethod optrule-generate-search-space (poly bands (id (eql :Rewrite/FuseLoadReduceStore)))
  ;; [TODO] If there's reduction
  ;; [TODO] Rewrite the base blueprint to have:
  ;; - Remove LOAD (Separated Loop)
  ;; - Add extra buffer
  ;; - e.g.:
  ;; val = 0.0
  ;;   val += ...
  ;; out[...] = val
  ;; is rewrittern as
  ;; for ...
  ;;   out[...] = initial_value
  ;; OUT[...] += ...
  ;; separate activation
  ;; OR, MAKE Post-Tile-Fusion DOABLE!!!
  ;; [Original] -> [NoOpt, FuseLoadReduceStore] -> [Reschedule1, Reschedule2, Reschedule3] ... -> {SKETCH!}
  ;; No Need to add this right?
  nil)

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
  ;; Reschedule can be placed on the top of commands.
  (when (null (some #'(lambda (x) (typep x 'Reschedule)) (poly-cmd-history poly)))
    (list
     ;; [TODO] Isn't there more to search configurations?
     ;; [TODO] proximity/validity/coincidence, what is constraints?
     ;; [TODO] More Patterns!
     (make-instance 'Reschedule :serialize-sccs 1) ;; Loop Fission
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

(defmethod optrule-apply-transform-on-blueprint (poly (optrule Reschedule)) nil)

(defclass Interchange (OptimizationRule)
  ((idx :initarg :idx :accessor interchange-idx)))

(defmethod optrule-generate-search-space (poly bands (id (eql :Interchange)))
  (loop for band in bands for nth upfrom 0
        append
        (loop for c upfrom 1 below (isl::%isl-schedule-node-n-children (isl::schedule-node-handle band))
              collect (make-instance 'Interchange :axis nth :band band :idx c))))

(defmethod optrule-apply-transform-on-polyhedral (poly (opt Interchange))
  (let* ((mupa (schedule-node-band-get-partial-schedule (optrule-band opt)))
         (node (schedule-node-delete (optrule-band opt)))
         (n-child (isl::%isl-schedule-node-n-children (isl::schedule-node-handle node)))
         (_ (when (= 0 n-child) (error "cannot apply interchange")))
         (node (schedule-node-get-band-from-relative-idx node (interchange-idx opt)))
         (__ (assert node () "IDX=~a does not exists in the schedule:~%~A" (interchange-idx opt) (optrule-band opt)))
         (node (schedule-node-insert-partial-schedule node mupa)))
    (declare (ignore _ __))
    (when (check-legality (schedule-node-get-schedule node) (poly-dependencies poly))
      ;;(schedule-node-insert-mark node (directive->id (directive "INTERCHANGE" idx t)))
      (setf (poly-schedule poly) (schedule-node-get-schedule node)))))

(defmethod optrule-apply-transform-on-blueprint (poly (opt Interchange))

  )
;; [TODO] FlashAttention
(defclass FuseWithParent (OptimizationRule)
  nil)
;; [TODO] SIMD
(defclass TensorCore (OptimizationRule)
  nil)

(defun tiling-sizes (band &key (size-default 32) (dims))
  (declare (type list dims) (type fixnum size-default))
  (let* ((band-space (schedule-node-band-get-space band))
         (dim (space-dim band-space 3)))
    (multi-val-from-val-list
     band-space
     (apply #'make-value-list (loop for i upfrom 0 below dim collect (or (nth i dims) size-default))))))

(defclass Tile (OptimizationRule)
  ((size :initarg :size :accessor tile-size)))

(defmethod optrule-generate-search-space (poly bands (id (eql :Tile)))
  (loop for band in bands for nth upfrom 0
        append
        (loop for size in `(2 4 8 16 32)
              collect
              (make-instance 'Tile :size size :band band :axis nth))))

(defmethod optrule-apply-transform-on-polyhedral (poly (opt Tile))
  (setf
   (poly-schedule poly)
   (schedule-node-get-schedule
    (schedule-node-band-tile (optrule-band opt) (tiling-sizes (optrule-band opt) :size-default (tile-size opt))))))

(defmethod optrule-apply-transform-on-blueprint (poly (opt Tile)))

(defclass ParallelTile (OptimizationRule) ;; CPU will use this!
  nil)

(defclass SplitReduce (OptimizationRule)
  ;; TODO: Mode = :warp :block
  nil)


;; RootがReschedule->Reorderなら...的な話かも
;; うまく言語化できないけど，最初にReorder -> Tileとかで，求めるOptimalに到達する可能性があるから，やっぱり木構造で順番に
;; Apply Optsしていく探索空間をイメージするのでうまくいくんじゃないかな
;; PPRINTを充実させるか，とっととParser作ってもろて
;; - poly-ir-schedule-node: これを追加するべきか？
;; - [TODO] Reductionのval_2 = ...のScalar, Write, これをMatrixにする
;; ~~ AutoScheduler Implementation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defparameter *search-space* ;; (n-generation . Candidates)
  '((0 . (:NoOpt :Reschedule))  ;; Solve ILP with multiple strategy (Detect Band/Coincidence, Loop Fussion at early stage)
    (1 . (:NoOpt :Interchange)) ;; Shuffle the memory order for finding the best candidate!
    (t . (:NoOpt :Tile))))      ;; Recursively optimize things ...

(defmethod get-next-optimization-rules ((polyhedral Polyhedral-IR))
  (let ((n-generation (length (poly-cmd-history polyhedral)))
        (bands (schedule-node-get-undernearth-bands (schedule-get-root (poly-schedule polyhedral)))))
    (loop for space in (cdr (or (find n-generation *search-space* :key #'car) (find t *search-space* :key #'car) (error "No *search-space* configuration for t")))
          append (optrule-generate-search-space polyhedral bands space))))

(defmethod polyhedral-ir-mutate-for-children ((polyhedral Polyhedral-IR))
  (let* ((space (get-next-optimization-rules polyhedral))
         (next-generations
           (remove-duplicates
            (loop for opt in space collect (apply-optimization polyhedral opt))
            :test #'string= :key #'pg-dump-into-str)))
    (loop for gen in next-generations
          if (verify-polyhedral-ir gen) collect gen)))

(defmethod polyhedral-ir-evaluate ((polyhedral Polyhedral-IR) abstract-kernel args n)
  ;; [TODO] Recompile it and run as an kernel
  (format t "~%[Kernel]:~%==========~%~a~%" (caten/codegen/blueprint::print-blueprint (get-blueprint-from-polyhedral polyhedral) nil))
  (print (reverse (poly-cmd-history polyhedral)))
  (format t "~%===========~%")
  (* n (random 1.0)))

(defun realize-node-with-autotuning (runtime node args &aux (beam-width 10) (max-iters 1) (n 10) (threshold 1e-5))
  ;; BEAM Search
  ;; Parameters:
  ;;  - n
  ;;  - beam_width
  ;;  - max_iters
  (labels ((make-candidate (polyhedral-ir)
             (declare (type Polyhedral-IR polyhedral-ir))
             (cons polyhedral-ir (polyhedral-ir-evaluate polyhedral-ir (caten/air:getattr node :kernel-info) args n))))
    (let* ((origin (caten/codegen/polyhedral:make-polyhedral-from-blueprint (kernel-blueprint (caten/air:getattr node :kernel-info))))
           (beam (list (cons origin *+inf*))))
      (loop named beam for iter upfrom 0 below max-iters for candidates = nil do
        (format t "= [~ath BEAM n=~a] ==~%" iter (length beam))
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
        (print "BEST KERNEL IS")
        (print best-kernel)
        ;; (setf (caten/air:getattr node :kernel-info) (cdr (sort searched #'< :key #'car)))
        ;; [TODO] Copy the initial results? to avoid overflow? or for sparse optimizations?
        (apply #'values (subseq args 0 (length (caten/air:node-writes node))))))))

;; [TODO]
;; Band, Interchange is REQUIREDDD
;; - Why the indexing is so messed around? We have to fix this FIRST.
;; - Two Things I should fix:
;;  - 1. Schedule, won't zero start. (... Arefの話はこのままでいい気がしてきた。)
;;  - 2. Indexing is flatten, should we allow it?
;; - 1. Replace IDX
;; - 2. Simplifier, More Powerful Symbolic Simplification Patterns
;; - 3. Flexible reduction accumlator
;; - 4. fix a bug in threefry2x32
;; - 5. ループの途中でincf挿入するやつやりたい?
;; - 6. BEAM Cacheを実装する
;; - 7. Symbolic Kernelに対して，探索したSchedule Commandsを適用する？
;; - 8. val_2がSeparateされたとき，追加も一時領域Bufferを作成する (そんな難しくないという認識)
;;  - 1. DetectSeparateScheduledを実装
;;  - 2. Extractするときに，ISLに登録した通りにBufferを登録する。Argsは増えることになる。

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
;; - Polyhedral Modelが正しくない気がする
;; - SearchSpace

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
