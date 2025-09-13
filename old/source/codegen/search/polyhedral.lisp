(defpackage :caten/codegen/search/polyhedral
  (:use :cl :caten/air :caten/codegen/renderer :caten/codegen/search/schedule)
  (:export
   #:Global-Lex-Order
   #:Global-Lex-Order-Dict
   #:Make-Global-Lex-Order
   #:Global-Lex-Order-Session-Id
   #:global-lex-order-dim
   #:polyaref-on-global-lex-order
   #:*+inf*
   #:Polyhedral-Schedule-Item
   #:theta #:psi-theta
   #:dependency-graph #:psi-dependency-graph #:psi-domain #:psi-strategy
   #:psi-read-union-map #:psi-write-union-map #:psi-global-lex-order
   #:opt-history #:psi-opt-history
   #:psi-evaluation
   #:%make-polyhedral-schedule-item
   #:make-polyhedral-schedule-item
   #:ctx #:ctx-node-to-loops #:ctx-all-loops #:ctx-exprs #:ctx-scal->access
   #:node-to-loops #:all-loops #:exprs #:scal->access
   #:make-scop-ctx-from-blueprint
   #:psi-clone-for-next-generation
   #:psi-verify-legality
   #:psi.
   #:extract-accesses))

(in-package :caten/codegen/search/polyhedral)

(defstruct Global-Lex-Order
  "Global-Lex-Order stores a session-scoped dictionary for lexicographic
orderings of multi-dimensional indices and their corresponding linear (1-D)
indices.
- Provide a canonical, session-wide mapping from a lexicographic coordinate
  S(i0, i1, ..., ik) to its linearized index `idx`.
- Serve as a common reference when lowering schedules, validating affine
  constraints, and generating code.
- We denote a lexicographic point as:
    S(_gid1, 0, 0)
  where each component (e.g., `_gid1`, `0`, `0`) is an index expression.
- Row-major linearization with stride vector:
    DIM | s0 | s1 | ... | sk |
    --------------------------
    IDX | i0 | i1 | ... | ik |
  The linear index is:
    idx = s0*i0 + s1*i1 + ... + sk*ik
  For the concrete 3-D illustration below:
    DIM | N*M | M | 1 |
    ------------------- 
    IDX | _gid1 | 0 | 0 |
  Hence:
    idx = (N*M)*_gid1 + N*0 + 1*0
- Each index component (e.g., `_gid1`, `0`, `0`) MUST be an affine expression
  over loop iterators and symbolic parameters. Non-affine terms are not allowed.
- Each dim components MUST be an constant term."
  (session-id nil :type symbol)
  (quasiaffine nil :type list)
  (dict (make-hash-table) :type hash-table))

(defun global-lex-order-dim (glo)
  (declare (type Global-Lex-Order glo))
  (length (alexandria:hash-table-keys (global-lex-order-dict glo))))

(defun polyaref-on-global-lex-order (global-lex-order polyaref graph)
  (declare (type Global-Lex-Order global-lex-order) (type node polyaref))
  (assert (eql (node-type polyaref) :PolyAref))
  (let ((rank (getattr polyaref :nrank))
        (renderer (make-instance 'Default-Renderer :graph graph :render-expr->expr t))
        (schedule-dims (make-list (global-lex-order-dim global-lex-order) :initial-element (list 0))))
    (loop for i upfrom 0 below rank
          for dim = (nth (1+ i) (node-reads polyaref))
          for idx = (nth (+ 1 rank i) (node-reads polyaref))
          for plc = (gethash dim (global-lex-order-dict global-lex-order)) do
            (assert plc () "polyaref-on-lex-order: The dimension ~a is not exist in scheduling space: ~a" dim (alexandria:hash-table-keys (global-lex-order-dict global-lex-order)))
            (push idx (nth plc schedule-dims)))
    (flet ((r (items)
             (format nil "~{~a~^+~}" (map 'list #'(lambda (x) (render-node renderer x)) items))))
      (format nil "~{~a~^, ~}" (map 'list #'r schedule-dims)))))

(defparameter *+inf* (coerce (expt 2 32) 'double-float))
(defclass Polyhedral-Schedule-Item ()
  ((theta :accessor psi-theta :initarg :initial-theta)
   (domain :accessor psi-domain :initarg :domain)
   
   (read-union-map :accessor psi-read-union-map :initarg :read)
   (write-union-map :accessor psi-write-union-map :initarg :write)
   (dependency-graph :accessor psi-dependency-graph :initarg :dependency-graph)

   (strategy :accessor psi-strategy :initarg :strategy)
   (global-lex-order :accessor psi-global-lex-order :initarg :global-lex-order :initform nil)
   
   (opt-history :accessor psi-opt-history :initform nil :initarg :opt-history)
   (evaluation :accessor psi-evaluation :initform *+inf* :type double-float))
  (:documentation "
Class `Polyhedral-Schedule-Item` is a wrapper around a Blueprint.

It stores the memory-dependence graph—analyzed from the original computation graph—together with its scheduling information.

Conceptually:
```
DependencyGraph, θ_0 = CreatePolyhedral(Blueprint)
θ_n+1 = ApplyTransformation(θ_n, OptimizationRule_n)
```

After applying a series of loop transformations to θ₀ an optimized Blueprint can be produced:

```
Blueprint_optimized = MakeBlueprintFromPolyhedral(Blueprint, θ_0) s.t.: ScheduleIsValid(DependencyGraph)
```

In other words, this class encapsulates both the analysis results and the scheduling state, enabling generation of a valid, optimized Blueprint from the polyhedral representation.

During the optimization, auto scheduler tries to minimize the floating value of psi-evaluation."))

(defmethod psi-verify-legality ((poly Polyhedral-Schedule-Item))
  (schedule-is-legal-p (psi-theta poly) (psi-dependency-graph poly)))

(defun psi-clone-for-next-generation (psi)
  (declare (type Polyhedral-Schedule-Item psi))
  (make-instance 'Polyhedral-Schedule-Item
                 :dependency-graph (psi-dependency-graph psi) :domain (psi-domain psi)
                 :read (psi-read-union-map psi) :write (psi-write-union-map psi) :strategy (psi-strategy psi)
                 :initial-theta (psi-theta psi) :opt-history (copy-list (psi-opt-history psi))
                 :global-lex-order (psi-global-lex-order psi)))
;; ~~ SCoP ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defstruct ctx
  "Context for tracking loop structure during traversal"
  (glo nil :type (or null Global-Lex-Order))
  (stack nil :type list)
  (node-to-loops (make-hash-table) :type hash-table)
  (all-loops nil :type list)
  (exprs nil :type list)
  (scal->access (make-hash-table) :type hash-table))

(defun make-scop-ctx-from-blueprint (graph &key (allow-if nil) (glo nil))
  "Traverse the blueprint graph to extract loop structure"
  (let ((ctx (make-ctx :glo glo)) (visited (make-hash-table)))
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
                      (let ((loop-info (list :type :loop :idx idx :size size :step step :mark (or mark :noopt) :for-node node :range-node range-node)))
                        (push loop-info (ctx-all-loops ctx))
                        (push loop-info (ctx-stack ctx))))
                    ;; Traverse body
                    (traverse (id->value graph (second (node-reads node))))
                    ;; Pop loop from stack after processing body
                    (when idx (pop (ctx-stack ctx)))))
                 (:PROGN (dolist (child-id (node-reads node)) (traverse (id->value graph child-id))))
                 (:DEFINE-LOCAL)
                 (:IF
                  (when allow-if (push (list :type :if :if-node node) (ctx-stack ctx)))
                  (traverse (id->value graph (second (node-reads node))))
                  (when allow-if (pop (ctx-stack ctx))))
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
       (render-node (make-instance 'Default-Renderer :graph graph :render-expr->expr t) id)))
    (t (error "The variable ~a is not defined. ~a" id node))))

(defun render-domain-for-node (blueprint node loop-info)
  "Render ISL domain string for a single node"
  (declare (type Graph blueprint) (type Node node) (type hash-table loop-info))
  (flet ((r (id) (render-expr-for-isl id blueprint)))
    (let ((loops (gethash (node-id node) loop-info)))
      (let ((constraints
              (loop for l in (reverse loops)
                    for step = (getf l :step)
                    if (string= (r step) "1")
                      collect (format nil "0 <= ~(~a~) < ~a" (getf l :idx) (r (getf l :size)))
                    else ;; [NOTE] Not Tested!!
                      collect (format nil "exists e : ~(~a~) = ~a*e and 0 <= ~(~a~) < ~a" (getf l :idx) (r step) (getf l :idx) (r (getf l :size))))))
        (format nil "~a[~{~a~^, ~}] ~a ~{~a~^ and ~}" (node-id node) (map 'list #'(lambda (l) (format nil "~(~a~)" (getf l :idx))) (reverse loops)) (if constraints ":" "") constraints)))))

(defun render-domains (ctx blueprint)
  "Create ISL domain representation from blueprint"
  (format nil "{ ~{~a~^; ~} }" (reverse (map 'list #'(lambda (x) (render-domain-for-node blueprint x (ctx-node-to-loops ctx))) (ctx-exprs ctx)))))

(defun extract-buffer-access-info (id blueprint &aux (visited (make-hash-table)) (found))
  ;; Return: a list of (cons (cons visible_name graph_id) access_id)
  (labels ((explore (id &aux (node (id->value blueprint id)))
             (when (or (null node) (gethash (node-id node) visited)) (return-from explore))
             (when (eql (node-type node) :BIND)
               (push (cons (cons (getattr node :value) (getattr node :value)) nil) found)
               (return-from explore))
             (when (eql (node-type node) :EXPR)
               (push (cons (cons (car (node-writes node)) (car (node-writes node))) nil) found)
               (return-from explore))
             (setf (gethash (node-id node) visited) t)
             (when (eql (node-type node) :Aref)
               (error "extract-buffer-access-info: Cannot extract polyhedral model from given blueprint. Replace all :AREF with :PolyAref first."))
             (when (eql (node-type node) :PolyAref)
               (let* ((p (id->value blueprint (car (node-reads node))))
                      (v (if (and p (eql (node-type p) :BIND)) (getattr p :value) (car (node-reads node))))
                      (p (if (and p (eql (node-type p) :BIND)) (getattr p :value) (car (node-reads node)))))
                 (push (cons (cons p v) (cdr (node-reads node))) found)
                 (return-from explore)))
             (mapc #'explore (node-reads node))))
    (explore id)
    found))

(defun render-default-isl-access (ctx bp idxs loops &key (scal->array t))
  "Renders a default memory accessing expr for scalar values."
  (declare (type cons idxs))
  (unless scal->array
    (return-from render-default-isl-access "0"))
  (multiple-value-bind (visible-name graph-id) (values (car idxs) (cdr idxs))
    (declare (ignore graph-id))
    ;; Scalar Memory Access: Inherits the first configuration where the scalar was defined.
    ;; [TODO] Is it valid for all case, all kernel, all schedule? how can we prove this?
    (when (gethash visible-name (ctx-scal->access ctx))
      (return-from render-default-isl-access (getf (gethash visible-name (ctx-scal->access ctx)) :access)))
    (let* ((shape (loop for l in loops for size = (getf l :size) for expr = (id->value bp size) for node = (id->value bp (car (node-reads expr)))
                        ;; Determining the loop size from graph. (TODO: Assert RANGE(SIZE, STEM) where SIZE is always EXPR, and EXPR(LOAD(Constant)) Pattern
                        collect (progn (assert (eql (node-type node) :LOAD)) (assert (numberp (getattr node :value))) (getattr node :value))))
           (strides (caten/codegen/helpers:row-major-calc-strides shape))
           (access (format nil "~{~a~^+~}" (loop for s in strides for l in loops for idx = (getf l :idx) collect (format nil "~a*~(~a~)" s idx)))))
      (setf (gethash visible-name (ctx-scal->access ctx)) (list :access access :shape shape :strides strides))
      access)))
;; - [ ] Remove scal->array option? or leave it for calculating parallelism? it depends on how beam should be
;; - implemented
;; - [ ] QuasiAffine
(defun render-index-for-isl (ctx index blueprint)
  (declare (type ctx ctx) (type list index) (type FastGraph blueprint))
  (assert (ctx-glo ctx) () "render-index-for-isl: Cannot render index space w/o providing Global-Lex-Order")
  (polyaref-on-global-lex-order (ctx-glo ctx) (caten/aasm:%polyaref 'tmp (subseq index 0 (/ (length index) 2)) (subseq index (/ (length index) 2))) blueprint))

(defun render-access-for-node (ctx node loops buffers index blueprint &key (scal->array t) (getlisp))
  "Render access relation for a single node"
  (multiple-value-bind (visible-id graph-id) (values (car buffers) (cdr buffers))
    (declare (ignore visible-id))
    (let ((domain (format nil "~{~a~^, ~}" (map 'list #'(lambda (l) (format nil "~(~a~)" (getf l :idx))) (reverse loops)))))
      (if getlisp
          (list (node-id node) graph-id (render-index-for-isl ctx index blueprint))
          (format nil "~a[~a] -> ~a[~a]" (node-id node) domain graph-id (render-index-for-isl ctx index blueprint))))))

(defun extract-accesses (ctx blueprint &key (scal->array t) (getlisp) &aux (reads) (writes))
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
                 (assert (= 1 (length write-region)))
                 (dolist (w write-region)
                   (let ((macc (cons (caar w) (caar w)))) ;; visible as (caar w) but internally expr.writes[0]
                     (push (render-access-for-node ctx expr expr-domain macc (cdr w) blueprint :scal->array scal->array :getlisp getlisp) writes)))
                 (dolist (r read-region)
                   (push (render-access-for-node ctx expr expr-domain (car r) (cdr r) blueprint :scal->array scal->array :getlisp getlisp) reads))))
               (otherwise ;; // EXPR
                (let ((read-region (extract-buffer-access-info (car (node-reads expr)) blueprint)))
                  (push
                   (render-access-for-node
                    ctx expr expr-domain
                    (cons (car (node-writes expr)) (car (node-writes expr))) nil blueprint
                    :scal->array scal->array :getlisp getlisp)
                   writes)
                  (dolist (r read-region)
                    (push (render-access-for-node ctx expr expr-domain (car r) (cdr r) blueprint :scal->array scal->array :getlisp getlisp) reads))))))
    (if getlisp
        (cons reads writes)
        (cons
         (format nil "{ ~{~a~^; ~} }" (reverse reads))
         (format nil "{ ~{~a~^; ~} }" (reverse writes))))))

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

(defun rewrite-blueprint-tree->schedule-tree (ctx blueprint &key (scal->array t) &aux (visited (make-hash-table)))
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
                           (isl:schedule-insert-partial-schedule (isl:! body-sched) (isl:! (isl:multi-union-pw-aff-from-str band)))))))
                  (:IF
                   ;; [Note] How to dump :IF Node?
                   (error "not ready"))
                  ;; EXPR ==> Rewrite as a filter, and is a leaf of graph.
                  (:EXPR
                   (setf region (append region (list node)))
                   (caten/isl:schedule-from-domain (isl:! (caten/isl:union-set-from-str (format nil "{ ~a }" (render-domain-for-node blueprint node node-to-loops))))))
                  (:PROGN
                    ;; [todo] you can use reduce
                    (let ((tmp-schedule :nothing))
                      (loop for item in (node-reads node) do
                        (multiple-value-bind (sched reg) (rewrite-node item)
                          (setf region (append region reg))
                          (if (eql tmp-schedule :nothing)
                              (setf tmp-schedule sched)
                              (setf tmp-schedule (caten/isl:schedule-sequence (isl:! tmp-schedule) (isl:! sched))))))
                      (assert (not (eql tmp-schedule :nothing)))
                      tmp-schedule))
                  (otherwise (error "No handling case for ~a" (node-type node))))
                region)))
      (assert (= 1 (length (graph-outputs blueprint))))
      (rewrite-node (car (graph-outputs blueprint))))))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun %make-polyhedral-schedule-item (domain schedule reads writes &key (strategy) (global-lex-order))
  (declare (type isl::union-set domain) (type isl::union-map reads writes) (type isl::schedule schedule))
  (make-instance 'Polyhedral-Schedule-Item
                 :dependency-graph (compute-dependence-relation reads writes schedule)
                 :initial-theta schedule :read reads :write writes
                 :domain domain :strategy strategy :opt-history nil
                 :global-lex-order global-lex-order))

(defun make-polyhedral-schedule-item (blueprint &key (scal->array t) (strategy) (opt-history) (global-lex-order))
  "
- scal->array[bool]
  - If set to T, scalar values are rendered as tensor (to maximize parallelism)
  - Otherwise, scalar values are renderered as val[0] (to maximize locality, detect illegal permutation)"
  (declare (type FastGraph blueprint) (type boolean scal->array))
  (let* ((ctx (make-scop-ctx-from-blueprint blueprint))
         (domain (isl:union-set-from-str (render-domains ctx blueprint)))
         (schedule (rewrite-blueprint-tree->schedule-tree ctx blueprint :scal->array scal->array))
         (reads/writes (extract-accesses ctx blueprint :scal->array scal->array)) (reads) (writes))
    (handler-case (setf reads (isl:union-map-from-str (car reads/writes))
                        writes (isl:union-map-from-str (cdr reads/writes)))
      (error (c) (error "Cannot dump an access relation from the following relations:~%Reads:~%~a~%Writes:~%~a
Error:~%~a~%Is the loop affine?" (car reads/writes) (cdr reads/writes) c)))
    (make-instance 'Polyhedral-Schedule-Item
                   :dependency-graph (compute-dependence-relation reads writes schedule)
                   :initial-theta schedule :read reads :write writes
                   :domain domain :strategy strategy :opt-history opt-history
                   :global-lex-order global-lex-order)))

(defun psi. (psi1-before psi2-after)
  "Merges two schedule into a single schedule"
  (assert (and (psi-global-lex-order psi1-before) (psi-global-lex-order psi2-after)))
  (assert (eql (global-lex-order-session-id (psi-global-lex-order psi1-before))
               (global-lex-order-session-id (psi-global-lex-order psi2-after)))
          ()
          "psi.: psi1-before and psi2-after must live in the same session!")
  (let* ((new-read (isl:union-map-union (psi-read-union-map psi1-before) (psi-read-union-map psi2-after)))
         (new-write (isl:union-map-union (psi-write-union-map psi1-before) (psi-write-union-map psi2-after)))
         (new-domain (isl:union-set-union (psi-domain psi1-before) (psi-domain psi2-after)))
         (new-schedule (isl:schedule-sequence (psi-theta psi1-before) (psi-theta psi2-after))))
    (make-instance 'Polyhedral-Schedule-Item
                   :dependency-graph (compute-dependence-relation new-read new-write new-schedule)
                   :initial-theta new-schedule :read new-read :write new-write
                   :domain new-domain :strategy (psi-strategy psi1-before)
                   :opt-history (append (psi-opt-history psi1-before) (psi-opt-history psi2-after))
                   :global-lex-order (psi-global-lex-order psi1-before))))
