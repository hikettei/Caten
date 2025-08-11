(defpackage :caten/codegen/search/polyhedral
  (:use :cl :caten/air :caten/codegen/renderer :caten/codegen/search/schedule)
  (:export
   #:*+inf*
   #:Polyhedral-Schedule-Item
   #:theta #:psi-theta
   #:dependency-graph #:psi-dependency-graph #:psi-domain
   #:opt-history #:psi-opt-history
   #:psi-evaluation
   #:make-polyhedral-schedule-item
   #:ctx #:ctx-node-to-loops #:ctx-all-loops #:ctx-exprs #:ctx-scal->access
   #:node-to-loops #:all-loops #:exprs #:scal->access
   #:make-scop-ctx-from-blueprint
   #:psi-clone-for-next-generation
   ))
(in-package :caten/codegen/search/polyhedral)

(defparameter *+inf* (coerce (expt 2 32) 'double-float))
(defclass Polyhedral-Schedule-Item ()
  ((theta :accessor psi-theta :initarg :initial-theta)
   (domain :accessor psi-domain :initarg :domain)
   (dependency-graph :accessor psi-dependency-graph :initarg :dependency-graph)
   (opt-history :accessor psi-opt-history :initform nil :initarg :opt-history)
   (evaluation :accessor psi-evaluation :initform *+inf* :type double-float)
   ;; ctx?
   ;; during transformation blueprint should not be used
   )
   
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

(defun psi-clone-for-next-generation (psi)
  (declare (type Polyhedral-Schedule-Item psi))
  (make-instance 'Polyhedral-Schedule-Item
                 :dependency-graph (psi-dependency-graph psi) :domain (psi-domain psi)
                 :initial-theta (psi-theta psi) :opt-history (copy-list (psi-opt-history psi))))
;; ~~ SCoP ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defstruct ctx
  "Context for tracking loop structure during traversal"
  (stack nil :type list)
  (node-to-loops (make-hash-table) :type hash-table)
  (all-loops nil :type list)
  (exprs nil :type list)
  (scal->access (make-hash-table) :type hash-table))

(defun make-scop-ctx-from-blueprint (graph &key (allow-if nil))
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
  ;; Return: a list of (cons (cons visible_name graph_id) access_id)
  (labels ((explore (id &aux (node (id->value blueprint id)))
             (when (or (null node) (gethash (node-id node) visited)) (return-from explore))
             (when (eql (node-type node) :BIND)
               (push (cons (cons (getattr node :value) (car (node-reads node))) nil) found)
               (return-from explore))
             (when (eql (node-type node) :EXPR)
               (push (cons (cons (car (node-writes node)) (car (node-writes node))) nil) found)
               (return-from explore))
             (setf (gethash (node-id node) visited) t)
             (when (eql (node-type node) :AREF)
               (let* ((p (id->value blueprint (car (node-reads node))))
                      (v (if (and p (eql (node-type p) :BIND)) (car (node-reads p)) (car (node-reads node))))
                      (p (if (and p (eql (node-type p) :BIND)) (getattr p :value) (car (node-reads node)))))
                 (push (cons (cons p v) (second (node-reads node))) found)
                 (return-from explore)))
             (mapc #'explore (node-reads node))))
    (explore id)
    found))

(defun render-default-isl-access (ctx bp idxs loops)
  (declare (type cons idxs))
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

(defun render-access-for-node (ctx node loops buffers index blueprint)
  "Render access relation for a single node"
  (multiple-value-bind (visible-id graph-id) (values (car buffers) (cdr buffers))
    (declare (ignore visible-id))
    (let ((domain (format nil "~{~a~^, ~}" (map 'list #'(lambda (l) (format nil "~(~a~)" (getf l :idx))) (reverse loops)))))
      (format nil "~a[~a] -> ~a[~a]" (node-id node) domain graph-id
              (if index (render-expr-for-isl index blueprint) (render-default-isl-access ctx blueprint buffers (reverse loops)))))))

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
                 (assert (= 1 (length write-region)))
                 (dolist (w write-region)
                   (let ((macc (cons (caar w) (car (node-writes expr))))) ;; visible as (caar w) but internally expr.writes[0]
                     (push (render-access-for-node ctx expr expr-domain macc (cdr w) blueprint) writes)))
                 (dolist (r read-region)
                   (push (render-access-for-node ctx expr expr-domain (car r) (cdr r) blueprint) reads))))
               (otherwise ;; // EXPR
                (let ((read-region (extract-buffer-access-info (car (node-reads expr)) blueprint)))
                  (push
                   (render-access-for-node
                    ctx expr expr-domain
                    (cons (car (node-writes expr)) (car (node-writes expr))) nil blueprint)
                   writes)
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
                           (caten/isl:schedule-insert-partial-schedule body-sched (caten/isl:multi-union-pw-aff-from-str band))))))
                  (:IF
                   ;; [Note] How to dump :IF Node?
                   (error "not ready"))
                  ;; EXPR ==> Rewrite as a filter, and is a leaf of graph.
                  (:EXPR
                   (setf region (append region (list node)))
                   (caten/isl:schedule-from-domain (caten/isl:union-set-from-str (format nil "{ ~a }" (render-domain-for-node blueprint node node-to-loops)))))
                  (:PROGN
                    ;; [todo] you can use reduce
                    (let ((tmp-schedule :nothing))
                      (loop for item in (node-reads node) do
                        (multiple-value-bind (sched reg) (rewrite-node item)
                          (setf region (append region reg))
                          (if (eql tmp-schedule :nothing)
                              (setf tmp-schedule sched)
                              (setf tmp-schedule (caten/isl:schedule-sequence tmp-schedule sched)))))
                      (assert (not (eql tmp-schedule :nothing)))
                      tmp-schedule))
                  (otherwise (error "No handling case for ~a" (node-type node))))
                region)))
      (assert (= 1 (length (graph-outputs blueprint))))
      (rewrite-node (car (graph-outputs blueprint))))))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun make-polyhedral-schedule-item (blueprint)
  (declare (type FastGraph blueprint))
  (let* ((ctx (make-scop-ctx-from-blueprint blueprint))
         (domain (caten/isl:union-set-from-str (render-domains ctx blueprint)))
         (schedule (rewrite-blueprint-tree->schedule-tree ctx blueprint))
         (reads/writes (extract-accesses ctx blueprint)) (reads) (writes))
    (handler-case (setf reads (caten/isl:union-map-from-str (car reads/writes))
                        writes (caten/isl:union-map-from-str (cdr reads/writes)))
      (error (c) (error "Cannot dump an access relation from the following relations:~%Reads:~%~a~%Writes:~%~a
Error:~%~a~%Is the loop affine?" (car reads/writes) (cdr reads/writes) c)))
    (make-instance 'Polyhedral-Schedule-Item
                   :dependency-graph (compute-dependence-relation reads writes schedule)
                   :initial-theta schedule
                   :domain domain)))
