(defpackage :caten/codegen/polyhedral
  (:shadow #:set #:space)
  (:shadowing-import-from :cl :map)
  (:use :cl :caten/air :caten/aasm :caten/isl)
  (:import-from :caten/codegen/renderer #:render-expr #:Default-Renderer)
  (:export
   #:make-polyhedral-from-blueprint
   #:get-blueprint-from-polyhedral))

(in-package :caten/codegen/polyhedral)
;; ~~ Polyhedral ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
  (make-instance 'Polyhedral-IR :schedule (copy (poly-schedule pg)) :history (copy-list (poly-cmd-history pg))
                                :dependencies (poly-dependencies pg) :domain (poly-domain pg) :blueprint (poly-blueprint pg)))

(defun gid (n) (intern (format nil "_gid~a" n)))
(defun ->ast (schedule rank)
  (macrolet ((set-option (name level)
	       `(cffi:foreign-funcall ,(format nil "isl_options_set_~(~a~)" name)
				 :pointer (isl::context-handle isl::*context*)
				 :int ,level
				 :void)))
    (set-option "ast_build_atomic_upper_bound" 1)
    (set-option "ast_build_detect_min_max" 1)
    (set-option "ast_build_exploit_nested_bounds" 1)
    (set-option "ast_build_scale_strides" 1)
    (set-option "ast_build_allow_else" 0)
    (set-option "ast_build_allow_or" 0))
  (let* ((schedule (isl:copy schedule))
	 (ast-build (isl:ast-build-from-context (isl:set-from-str "{:}")))
         (rank (* 2 rank)) ;; rank * tile_bands * vectorizing
         (ast-build (isl:ast-build-set-iterators ast-build (apply #'isl:make-id-list (loop for i upfrom 0 below rank collect (gid i)))))
         (ast-build (isl:ast-build-set-options ast-build (isl:union-map-from-str "{}")))
	 (ast-build-node (isl:ast-build-node-from-schedule ast-build schedule)))
    ast-build-node))

(defmethod debug-render-to-clang ((pg Polyhedral-IR))
  (let* ((p     (isl::%isl-printer-to-str (isl::context-handle isl::*context*)))
         (ast   (->ast (poly-schedule pg) 0))
         (p     (isl::%isl-printer-set-output-format p 4)) ;; 4 == Clang
         (q     (isl::%isl-printer-print-ast-node p (isl::ast-node-handle ast)))
         (str   (isl::%isl-printer-get-str q)))
    str))
;; ~~ SCoP ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defstruct ctx
  "Context for tracking loop structure during traversal"
  (stack nil :type list)
  (node-to-loops (make-hash-table) :type hash-table)
  (all-loops nil :type list)
  (exprs nil :type list))

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
             (when (eql (node-type node) :EXPR) (return-from explore))
             (setf (gethash (node-id node) visited) t)
             (when (eql (node-type node) :AREF)
               (push (cons (car (node-reads node)) (second (node-reads node))) found))
             (mapc #'explore (node-reads node))))
    (explore id)
    found))

(defun render-access-for-node (node loops buffer index blueprint)
  "Render access relation for a single node"
  (format nil "~a[~{~a~^, ~}] -> ~a[~a]"
          (node-id node)
          (map 'list #'(lambda (l) (format nil "~(~a~)" (getf l :idx))) (reverse loops))
          buffer
          (if index (render-expr-for-isl index blueprint) "0")))

(defun extract-accesses (ctx blueprint &aux (reads) (writes))
  "Extract read and write access relations from blueprint"
  (with-slots ((node-to-loops node-to-loops) (exprs exprs)) ctx
    (loop for expr in exprs
          for expr-domain = (gethash (node-id expr) node-to-loops)
          for expr-entry-point = (id->value blueprint (car (node-reads expr))) do
            (assert expr-entry-point)
            (case (node-type expr-entry-point)
              (:SETF
               ;; SETF(AREF, EXPR)
               ;;       ^W    ^R
               (let ((write-region (extract-buffer-access-info (car (node-reads expr-entry-point)) blueprint))
                     (read-region  (extract-buffer-access-info (second (node-reads expr-entry-point)) blueprint)))
                 (dolist (w write-region)
                   (push (render-access-for-node expr expr-domain (car w) (cdr w) blueprint) writes))
                 (dolist (r read-region)
                   (push (render-access-for-node expr expr-domain (car r) (cdr r) blueprint) reads))))
               (otherwise
                (let ((read-region (extract-buffer-access-info (car (node-reads expr)) blueprint)))
                  (push (render-access-for-node expr expr-domain (car (node-writes expr)) nil blueprint) writes)
                  (dolist (r read-region)
                    (push (render-access-for-node expr expr-domain (car r) (cdr r) blueprint) reads))))))
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
                   ;; [Not] How to express :IF?
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
(defun get-blueprint-from-polyhedral (polyhedral)
  "Convert ISL polyhedral representation back to blueprint graph"
  ;; Entry point for:
  ;; - @directive parsing, getting blueprint from Polyhedral.
  ;; - 
  ;;
  ;; This would require parsing the ISL AST and reconstructing the graph
  ;; For now, this is a placeholder that returns the input for compatibility
  (declare (ignore polyhedral))
  (warn "get-blueprint-from-polyhedral: Not fully implemented yet")
  nil);; [TODO] ↓のAccess Relations, ScalarはFissionできるように記述したい
;; ~~ OptimizeRule ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass OptimizationRule ()
  nil)

(defgeneric optrule-generate-search-space (polyhedral optrule))
(defgeneric optrule-apply-transform-on-polyhedral (polyhedral optrule)) ;; Insert Directive
(defgeneric optrule-apply-transform-on-blueprint (polyhedral optrule))  ;; Directive Parse

(defun apply-optimization (polyhedral optrule)
  (declare (type Polyhedral-IR polyhedral) (type OptimizationRule optrule))
  (let ((polyhedral (poly-clone-for-next-generation polyhedral)))
    (push optrule (poly-cmd-history polyhedral))
    (optrule-apply-transform-on-polyhedral polyhedral optrule)
    polyhedral))
;; ~~ Implementations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass Reschedule (OptimizationRule)
  nil)
