(defpackage :caten/codegen/polyhedral
  (:shadow #:set #:space)
  (:shadowing-import-from :cl :map)
  (:use :cl :caten/air :caten/aasm :caten/isl)
  (:import-from :caten/codegen/renderer #:render-expr #:Default-Renderer)
  (:export
   #:make-polyhedral-from-blueprint
   #:get-blueprint-from-polyhedral))

(in-package :caten/codegen/polyhedral)

(defun get-blueprint-from-polyhedral (polyhedral)
  "Convert ISL polyhedral representation back to blueprint graph"
  ;; This would require parsing the ISL AST and reconstructing the graph
  ;; For now, this is a placeholder that returns the input for compatibility
  (declare (ignore polyhedral))
  (warn "get-blueprint-from-polyhedral: Not fully implemented yet")
  nil)

(defstruct loop-context
  "Context for tracking loop structure during traversal"
  (stack nil :type list)
  (node-to-loops (make-hash-table) :type hash-table)
  (all-loops nil :type list))

(defun traverse-blueprint-for-loops (graph &optional (start-nodes nil))
  "Traverse the blueprint graph to extract loop structure"
  (let ((ctx (make-loop-context))
        (visited (make-hash-table)))
    (labels ((traverse (node)
               (when (or (null node) (gethash (node-id node) visited))
                 (return-from traverse))
               (setf (gethash (node-id node) visited) t)
               (case (node-type node)
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
                        (push loop-info (loop-context-all-loops ctx))
                        (push loop-info (loop-context-stack ctx))))
                    ;; Traverse body
                    (when (>= (length (node-reads node)) 2)
                      (traverse (id->value graph (cadr (node-reads node)))))
                    ;; Pop loop from stack after processing body
                    (when idx
                      (pop (loop-context-stack ctx)))))
                 (:PROGN
                  ;; Process children in order
                  (dolist (child-id (node-reads node))
                    (traverse (id->value graph child-id))))
                 (:IF
                  ;; [TODO] Support :IF Node?
                  ;; Process condition and then branch
                  (when (>= (length (node-reads node)) 2)
                    (traverse (id->value graph (cadr (node-reads node))))))
                 (otherwise
                  ;; Regular computation nodes
                  (unless (member (node-type node) '(:RANGE))
                    (when (loop-context-stack ctx)
                      (setf (gethash (node-id node) (loop-context-node-to-loops ctx))
                            (copy-list (loop-context-stack ctx)))))
                  ;; Continue traversal through reads
                  (dolist (read-id (node-reads node))
                    (when (symbolp read-id)
                      (traverse (id->value graph read-id))))))))
      ;; Start traversal from output nodes or all nodes
      (if start-nodes
          (dolist (node start-nodes) (traverse node))
          (dolist (node (graph-nodes graph)) (traverse node))))
    
    (values (loop-context-all-loops ctx) 
            (loop-context-node-to-loops ctx))))

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
      (assert loops)
      (let ((constraints
              (loop for l in (reverse loops)
                    for step = (getf l :step)
                    if (= step 1)
                      collect (format nil "0 <= ~(~a~) < ~a" (getf l :idx) (r (getf l :size)))
                    else
                      ;; [NOTE] Not Tested!
                      ;; Handle non-unit strides with existential quantifier
                      collect (format nil "exists e : ~(~a~) = ~a*e and 0 <= ~(~a~) < ~a" (getf l :idx) (r step) (getf l :idx) (r (getf l :size))))))
        (format nil "~a[~{~a~^, ~}] : ~{~a~^ and ~}" (node-id node) (map 'list #'(lambda (l) (format nil "~(~a~)" (getf l :idx))) (reverse loops)) constraints)))))

(defun render-domains (blueprint)
  "Create ISL domain representation from blueprint"
  (multiple-value-bind (loops node-to-loops) (traverse-blueprint-for-loops blueprint)
    (declare (ignore loops))
    (let ((domains nil))
      (dolist (node (graph-nodes blueprint))
        (when (eql (node-type node) :EXPR) ;(unless (member (node-type node) '(:RANGE :FOR :PROGN :IF))
          (let ((domain-str (render-domain-for-node blueprint node node-to-loops)))
            (when (> (length domain-str) 0)
              (push domain-str domains)))))
      (assert domains)
      (print (format nil "{ ~{~a~^; ~} }" (reverse domains))))))

(defun render-access-relation (node buffer-var indices params)
  "Render access relation for a buffer access"
  (format nil "[~{~a~^, ~}] -> ~a[~{~a~^, ~}]"
          params
          buffer-var
          indices))

(defun extract-buffer-access-info (node graph)
  "Extract buffer name and indices from an access node"
  (case (node-type node)
    (:AREF
     (let ((buffer (car (node-reads node)))
           (index (cadr (node-reads node))))
       (values buffer index)))
    (:SETF
     ;; For SETF, we need to look at the AREF it's writing to
     (let ((aref-node (id->value graph (car (node-reads node)))))
       (when (and aref-node (eql (node-type aref-node) :AREF))
         (extract-buffer-access-info aref-node graph))))
    (otherwise nil)))

(defun render-access-for-node (node loops buffer index blueprint)
  "Render access relation for a single node"
  (format nil "~a[~{~a~^, ~}] -> ~a[~a]"
          (node-id node)
          (map 'list #'(lambda (l) (format nil "~(~a~)" (getf l :idx))) (reverse loops))
          buffer
          (render-expr-for-isl index blueprint)))

(defun extract-accesses (blueprint)
  "Extract read and write access relations from blueprint"
  (multiple-value-bind (loops node-to-loops) (traverse-blueprint-for-loops blueprint)
    (declare (ignore loops))
    (let ((reads nil) (writes nil))
      (dolist (node (graph-nodes blueprint))
        (let ((loops (gethash (node-id node) node-to-loops)))
          ;; [TODO] この修正必要
          (when loops
            (case (node-type node)
              (:AREF
               ;; This is a read operation
               (multiple-value-bind (buffer index) 
                   (extract-buffer-access-info node blueprint)
                 (when buffer
                   (push (render-access-for-node node loops buffer index blueprint)
                         reads))))
              (:SETF
               ;; This is a write operation
               (multiple-value-bind (buffer index)
                   (extract-buffer-access-info node blueprint)
                 (when buffer
                   (push (render-access-for-node node loops buffer index blueprint) writes))))))))
      (values 
       (format nil "{ ~{~a~^; ~} }" (reverse reads))
       (format nil "{ ~{~a~^; ~} }" (reverse writes))))))

(defun render-schedule (blueprint)
  "Create ISL schedule representation from blueprint execution order"
  (multiple-value-bind (loops node-to-loops) (traverse-blueprint-for-loops blueprint)
    (declare (ignore loops))
    (let ((schedule-items nil)
          (position 0))
      (dolist (node (graph-nodes blueprint))
        (when (eql (node-type node) :EXPR)
          (let ((loops (gethash (node-id node) node-to-loops)))
            (when loops
              (push (format nil "~a[~{~a~^, ~}] -> [~{~a, ~}~a]"
                            (node-id node)
                            (map 'list #'(lambda (l) (format nil "~(~a~)" (getf l :idx))) (reverse loops))
                            (map 'list #'(lambda (l) (format nil "~(~a~)" (getf l :idx))) (reverse loops))
                            position)
                    schedule-items)
              (incf position)))))
      (if schedule-items
          (format nil "{ ~{~a~^; ~} }" (reverse schedule-items))
          (format nil "{ }")))))

(defun extract-parameters (blueprint)
  "Extract parameter symbols from the blueprint"
  (let ((params (make-hash-table))
        (param-list nil))
    ;; Collect all symbols used in loop bounds and buffer accesses
    (dolist (node (graph-nodes blueprint))
      (case (node-type node)
        (:RANGE
         ;; Extract symbols from loop bounds
         (let ((size (car (node-reads node))))
           (when (and (symbolp size) 
                      (not (member size '(nil t))))
             (setf (gethash size params) t))))
        (:DEFINE-GLOBAL
         ;; Buffer declarations
         (setf (gethash (node-id node) params) t))))
    
    ;; Convert to list of strings for ISL
    (maphash #'(lambda (k v) 
                 (declare (ignore v))
                 (push (format nil "~a" k) param-list)) 
             params)
    (or param-list '("N"))))

(defun render-band-schedule (loops node-to-loops)
  "Create band node schedule for reduction loops"
  (let ((band-schedules nil))
    ;; Group nodes by their reduction loops
    (maphash
     #'(lambda (node-id node-loops)
         (let ((reduction-loops 
                 (remove-if-not #'(lambda (l) (eql (getf l :mark) :reduction))
                                node-loops)))
           (when reduction-loops
             ;; Create partial schedule for reduction loops
             (push (format nil "~a[~{~a~^, ~}] -> [~{~a~^, ~}]"
                           node-id
                           (map 'list #'(lambda (l) (format nil "~(~a~)" (getf l :idx))) (reverse node-loops))
                           (map 'list #'(lambda (l) (format nil "~(~a~)" (getf l :idx))) (reverse reduction-loops)))
                   band-schedules))))
     node-to-loops)
    (when band-schedules
      (format nil "{ ~{~a~^; ~} }" band-schedules))))

(defclass Polyhedral-IR ()
  ((schedule :accessor poly-schedule)
   (domain   :accessor poly-domain)
   (dependencies :accessor poly-dependencies)))

(defun make-polyhedral-ir (domain read write schedule)
  (let ((pg (make-instance 'Polyhedral-IR)))
    (setf (poly-schedule pg) schedule
          (poly-domain pg) domain)
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
      (setf (poly-dependencies pg) dependencies)
      pg)))
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

(defun make-polyhedral-from-blueprint (blueprint)
  "Constructs Polyhedral IR from blueprint which is a static graph.
   
   The blueprint should be a FastGraph containing nodes with the following types:
   - :RANGE - defines loop bounds
   - :FOR - marks loop entry with :mark attribute (:coincident, :reduction, :noopt)
   - :AREF - memory load operations
   - :SETF - memory store operations
   - :PROGN - sequence of operations
   
   Returns a plist with :domain, :reads, :writes, and :schedule ISL objects."
  (declare (type Graph blueprint))
  (let* ((domain-str (render-domains blueprint))
         (domain (union-set-from-str domain-str)))
    (when (>= (ctx:getenv :JIT_DEBUG) 3)
      (format t "[Polyhedral] Domain: ~a~%" domain-str))

    (multiple-value-bind (read-str write-str) (extract-accesses blueprint)
      (when (>= (ctx:getenv :JIT_DEBUG) 3)
        (format t "[Polyhedral] Reads: ~a~%" read-str)
        (format t "[Polyhedral] Writes: ~a~%" write-str))
      
      (let ((reads (union-map-from-str read-str))
            (writes (union-map-from-str write-str))
            (schedule-str (render-schedule blueprint))
            (schedule (schedule-from-domain domain)))
        
        (when (>= (ctx:getenv :JIT_DEBUG) 3)
          (format t "[Polyhedral] Schedule: ~a~%" schedule-str))
        
        ;; Create schedule from domain if we have schedule items
        ;;(let ((sched-map (multi-union-pw-aff-from-str schedule-str)))
        ;;  (setf schedule (schedule-insert-partial-schedule schedule sched-map)))
        
        ;; Handle band nodes for reduction loops
        (print schedule)
        (multiple-value-bind (loops node-to-loops) (traverse-blueprint-for-loops blueprint)
          (let ((band-str (render-band-schedule loops node-to-loops)))
            (when (and band-str (>= (ctx:getenv :JIT_DEBUG) 3))
              (format t "[Polyhedral] Band schedule: ~a~%" band-str))
            (when band-str
              (print band-str)
              (let ((band-map (multi-union-pw-aff-from-str band-str)))
                (setf schedule (schedule-insert-partial-schedule schedule band-map))))))
        
        ;; Return polyhedral IR structure
        (print (debug-render-to-clang (make-polyhedral-ir domain reads writes schedule)))))))
