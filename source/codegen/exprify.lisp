(defpackage :caten/codegen/exprify
  (:documentation "Implements various transforming patterns on the AST")
  (:use :cl :caten/air :caten/codegen/expr)
  (:import-from
   :caten/codegen/shape-inference
   #:read-type-relay
   #:relay-reads
   #:relay-writes
   #:relay-read-iters
   #:relay-write-iters)
  (:import-from
   :caten/avm
   #:Buffer
   #:Buffer-depend-idx-list
   #:buffer-nrank
   #:buffer-dtype)
  (:import-from
   :caten/codegen/renderer
   #:make-aref)
  (:export
   #:graph-scalarify
   #:graph-exprify
   #:graph-propagete-reduction))

(in-package :caten/codegen/exprify)

(defun force-realize-p (node schedule-graph)
  (when (null (getattr node :reduction :allow-undefined t))
    (return-from force-realize-p nil))
  (let ((parent (id->value schedule-graph (car (node-reads node)))))
    (when (null parent) (return-from force-realize-p nil))
    (when (not (getattr parent :allocate-p))
      (return-from force-realize-p nil))
    (let ((allocate (car (getattr parent :Items))))
      (assert (eql (node-type allocate) :Allocate))
      (if (getattr allocate :from) t nil))))

(defun blueprint-tmp-buffers (blueprints schedule-graph &key (except-for nil))
  (let ((ids) (seen))
    (dolist (b blueprints)
      (let ((out (get-output-to b)))
        (when (and out (symbolp out))
          (push out ids)))
      (dolist (r (node-reads b))
        (when (symbolp r)
          (push r seen)))
      (dolist (w (node-writes b))
        (when (and (null (find w seen)) (not (force-realize-p b schedule-graph)))
          (push w ids))))
    (loop for e in (remove-duplicates ids)
          if (null (find e except-for))
            collect e)))

(defun memory-access-local-p (blueprint id)
  (declare (optimize (speed 3))
           (type list blueprint)
           (type symbol id))
  (let ((search-key
          (loop for bp in blueprint
                for nth fixnum upfrom 0
                if (and (not (eql (node-class bp) :Render)) (or (find id (node-reads bp)) (find id (node-writes bp))))
                  collect nth)))
    (assert search-key () "The id ~a is not used in the bp." id)
    (loop with start fixnum = (apply #'min search-key)
          with end fixnum = (apply #'max search-key)
          with depth fixnum = 0
          for nth upfrom start to end
          for ir = (nth nth blueprint)
          if (find (node-type ir) `(:FOR :IF))
            do (incf depth)
          else if (find (node-type ir) `(:ENDFOR :ENDIF))
                 do (decf depth)
          end
          if (< depth 0) do (return-from memory-access-local-p nil))
    t))

(defmethod rewrite-as-scalar ((buffer buffer) wi suffix)
  (setf (buffer-nrank buffer) -1))

(defun schedule-outputs (graph)
  (let ((outs))
    (loop for n in (graph-nodes graph)
          if (null (getattr n :allocate-p))
            do (setf outs (append outs (node-writes n))))
    (remove-duplicates outs)))

(defmethod graph-scalarify (blueprint (node Node) (schedule-graph Graph))
  "Rewrites the buffer as scalar as many as possible"
  (declare (type list blueprint))
  (let* ((ids (blueprint-tmp-buffers blueprint schedule-graph :except-for (schedule-outputs schedule-graph)))
         (replaceable (loop for i in ids if (memory-access-local-p blueprint i) collect i))
         (suffix))
    (loop for b in blueprint
          if (eql (node-type b) :FOR) do (push (getattr b :idx) suffix)
          if (eql (node-type b) :ENDFOR) do (setf suffix (remove (getattr b :idx) suffix))
          if (not (eql (node-class b) :Render)) do
            (loop for r in (node-reads b)
                  for rt in (relay-reads (read-type-relay b))
                  for ri in (relay-read-iters (read-type-relay b))
                  if (and (symbolp r) rt ri (find r replaceable))
                    do (rewrite-as-scalar rt ri (reverse suffix)))
            (assert (= (length (node-writes b)) 1) () "graph-scalarify excepts only one write node. (please update the loop below...)")
            (loop for w in (node-writes b)
                  for wt in (relay-writes (read-type-relay b))
                  for wi in (relay-write-iters (read-type-relay b))
                  if (and (symbolp w) wt wi (find w replaceable))
                    do (rewrite-as-scalar wt wi (reverse suffix))
                       (setf (getattr b :declare-type) (list t)
                             (node-reads node) (remove w (node-reads node)))))
    blueprint))
;; [TODO] Update Polyhedral
;; [TODO] Transform INDEX-COMPONENTS (use procedure)
(defmethod exprify ((node Node))
  (make-node :JIT :EXPR (copy-list (node-writes node)) (copy-list (node-reads node))
             :reduction (getattr node :reduction :allow-undefined t)
             :_type_relay (read-type-relay node)
             :expr (make-expr
                    :graph
                    (apply
                     #'make-graph
                     (append
                      (loop for r in (node-reads node)
                            for rt in (relay-reads (read-type-relay node))
                            for ri in (relay-read-iters (read-type-relay node))
                            if (symbolp r)
                              append
                              (if (= 0 (buffer-nrank rt))
                                  (let ((nodes (graph-nodes (expr-graph (expr-const r (buffer-dtype rt))))))
                                    (assert (eql (node-type (car (last nodes))) :LOAD))
                                    (setf (node-writes (car (last nodes))) (list r))
                                    nodes)
                                  (list (make-aref r rt ri))))
                      (list node)))
                    :out node)))

(defmethod graph-exprify (blueprint (node Node) (schedule-graph Graph))
  (declare (type list blueprint))
  (let* ((ids (blueprint-tmp-buffers blueprint schedule-graph :except-for (schedule-outputs schedule-graph)))
         (replaceable (loop for i in ids if (memory-access-local-p blueprint i) collect i)))
    (let ((new-bp
            (loop for bp in blueprint
                  if (eql (node-class bp) :Render) collect bp
                    else collect (exprify bp))))
      (labels ((group->expr-group (group)
                 ;; (print "GROUP!")
                 ;; (print group)
                 ;; [todo] compose exprs
                 group)
               (rewriter (from to &aux (group) (new-region :nothing))
                 (loop with count = from while (< count to)
                       for node = (nth count new-bp) do
                         (ecase (node-type node)
                           (:FOR
                            (let* ((endfor
                                     (find (getattr node :Idx) (nthcdr count new-bp)
                                           :key #'(lambda (x) (and (eql (node-type x) :ENDFOR) (getattr x :idx)))))
                                   (_ (when (null endfor) (error "malformed bp ~a" blueprint)))
                                   (endfor-abs-position (position (node-id endfor) new-bp :key #'node-id)))
                              (declare (ignore _))
                              (assert (>= endfor-abs-position count) () "malformed bp ~a" blueprint)
                              (assert (eql (node-type (nth endfor-abs-position new-bp)) :ENDFOR))
                              (setf new-region
                                    `(,@(when (not (eql new-region :nothing))
                                          new-region)
                                      ,@(group->expr-group (reverse group))
                                      ,node
                                      ,@(rewriter (1+ count) endfor-abs-position)
                                      ,(nth endfor-abs-position new-bp))
                                    group nil)
                              (setf count endfor-abs-position)
                              (incf count)))
                           (:ENDFOR
                            (error "malformed bp ~a" blueprint))
                           (:EXPR
                            (push node group)
                            (incf count))))
                 (when group
                   (setf new-region
                         `(,@(when (not (eql new-region :nothing))
                               new-region)
                           ,@(group->expr-group (reverse group)))))
                 (if (eql new-region :nothing)
                     nil
                     new-region)))
        ;; graph-propagete-function:
        ;; Shifting to the let binding based IR from DAG
        ;; C <- A + B
        ;; =>
        ;; A += B
        (graph-propagete-reduction (rewriter 0 (length new-bp)) replaceable)))))

(defun rewrite-expr-aref (expr replace)
  (declare (type graph expr))
  (dolist (n (graph-nodes expr))
    (if (eql (node-type n) :AREF)
        (multiple-value-bind (id type is) (funcall replace (car (node-writes n)) (getattr n :buffer) (getattr n :space))
          (setf (node-writes n) (list id)
                (getattr n :buffer) type
                (getattr n :space) is))
        (setf (node-reads n)
              (map
               'list
               #'(lambda (x)
                   (let ((id (funcall replace x nil nil)))
                     (or id x)))
               (node-reads n))))))

(defmethod graph-propagete-reduction (blueprint replaceable)
  (let ((id->tgt (make-hash-table))) ;; id -> (list new_id new_type new_is)
    (loop for bp in blueprint
          if (and (eql (node-type bp) :EXPR) (getattr bp :reduction) (find (car (node-reads bp)) replaceable))
            do (setf (gethash (car (node-writes bp)) id->tgt)
                     (list (car (node-reads bp)) (car (relay-reads (read-type-relay bp))) (car (relay-read-iters (read-type-relay bp))))))
    (labels ((final-new-id (id)
               (if (gethash id id->tgt)
                   (or (final-new-id (car (gethash id id->tgt))) (gethash id id->tgt))
                   (gethash id id->tgt)))
             (new (id type is)
               (let ((new-id (final-new-id id)))
                 (if (null new-id)
                     (values id type is)
                     (apply #'values new-id)))))
      (macrolet ((updt (expr &key (reader) (ireader) (treader))
                   `(loop for nth upfrom 0
                          for read in (,reader ,expr)
                          for is in (,ireader (read-type-relay ,expr))
                          for type in (,treader (read-type-relay ,expr))
                          if (and read is type (symbolp read)) do
                            (multiple-value-bind (newid new-type new-is) (new read type is)
                              (assert (and newid new-type new-is))
                              (setf (nth nth (,reader ,expr)) newid
                                    (nth nth (,ireader (read-type-relay ,expr))) new-is
                                    (nth nth (,treader (read-type-relay ,expr))) new-type)))))
        (loop for bp in blueprint
              if (eql (node-type bp) :EXPR)
                do (updt bp :reader node-reads :ireader relay-read-iters :treader relay-reads)
                   (updt bp :reader node-writes :ireader relay-write-iters :treader relay-writes)
                   (rewrite-expr-aref (expr-graph (getattr bp :expr)) #'new))
        blueprint))))

;; [TODO]
;; - [ ] Non JITAble standalone
;; - [ ] Complete Expr Grouping
;; - [ ] Fix randn, softmax (scalar load)
;; - [ ] Implement Renderer
;; - [ ] Fix poly (a*b) is not an affine
;; - [ ] Tiling/Parallelizing
;; - [ ] Vectorize