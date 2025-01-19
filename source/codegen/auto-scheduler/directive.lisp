(defpackage :caten/codegen/directive
  (:documentation "Provides various hand-written blueprint rewriting rule for corresponding @directive.")
  (:use :cl :caten/codegen/expr :caten/codegen/polyhedral-ast :caten/air :caten/codegen/shape-inference)
  (:export
   #:make-unrolled-body
   #:make-packed-body
   #:compute-reminder-for-unroll
   #:unroll-expr
   #:astfor-mutate-global
   #:astfor-mutate-local
   #:astfor-mutate-reminder-global))

(in-package :caten/codegen/directive)
;; ~~~ UNROLL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun make-unrolled-body (user body n-unroll)
  "Creates a copy of ASTFor body with the idx is set to nth."
  (declare (type ASTFor body) (type fixnum n-unroll))
  (unroll-ast (astfor-body body) (astfor-idx body) (astfor-idx user) n-unroll))

(defun make-packed-body (user body n-pack)
  (packing-ast (astfor-body body) (astfor-idx body) (astfor-idx user) n-pack))

(defmethod astfor-compute-reminder ((astfor ASTFor) unroll-by)
  (let ((below (expr-detach-loop-bound (astfor-to astfor)))
        (incremental (astfor-by astfor)))
    (assert (eql :Load (node-type (expr-out incremental))) () "The AstFor is not an SCOP.")
    ;; TODO(hikettei): wanna assert below > from
    (values (expr-mod (expr-sub below (astfor-from astfor)) incremental) below)))

(defun compute-reminder-for-unroll (user body n-unroll)
  "Creates an AstFor for the reminder part of the unrolled directive."
  (declare (type ASTFor body))
  (multiple-value-bind (reminder size) (astfor-compute-reminder user n-unroll)
    (setf (astfor-to user) (expr-< (expr-const (intern (astfor-idx user)) :int64) (expr-add (astfor-from body) (expr-sub size reminder))))
    (make-for
     (astfor-idx body)
     (expr-add (astfor-from body) (expr-sub size reminder))
     (expr-< (expr-const (intern (astfor-idx body)) :int64) (expr-add (astfor-from body) size))
     (expr-const 1 :int64)
     (copy-and-assign-ast-tree (astfor-body body) (astfor-idx user) 0))))

(defun make-suffix (space user &key (unroll (user-unroll user)))
  (flet ((s (idx &aux (val (find idx unroll :key #'car :test #'string=)))
           (if val (cdr val) nil)))
    (let ((suffix
            (apply #'concatenate 'string
                   (butlast (loop for s in space
                                  for id = (s s)
                                  if id append (list (princ-to-string id) "_"))))))
      (if (string= suffix "")
          ""
          (format nil "_~a" suffix)))))

(defun make-suffix-for-is (name iteration-space user max-size)
  (let* ((prefixes (user-unroll-prefix user))
         (space (loop for stride in (iteration-space-strides iteration-space)
                      for nth upfrom 0
                      for p = (nth nth prefixes)
                      if (or (null p) (expr-equal-to stride 0))
                        collect 0
                      else
                        collect p))
         (space (loop for i upfrom 0 below max-size
                      collect (or (nth i space) 0))))
    (intern
     (string-downcase
      (format nil "~a_~a" name (apply #'concatenate 'string (butlast (loop for s in space append (list (princ-to-string s) "_")))))))))

(defun make-padding-suffix (name max-size)
  (intern
   (string-downcase
    (format nil "~a_~a" name (apply #'concatenate 'string (butlast (loop repeat max-size append (list "0" "_"))))))))

(defun unroll-node (node user maximum-rank ds)
  (when (eql (node-type node) :Aref)
    (when (= -1 (caten/runtime:buffer-nrank (getattr node :buffer)))
      (setf (getattr node :storage-id) (make-suffix-for-is (getattr node :storage-id) (getattr node :space) user maximum-rank)))
    (return-from unroll-node node))
  (when (eql (node-type node) :LOAD)
    (when (symbolp (getattr node :value))
      (when (and (= 0 (caten/runtime:buffer-nrank (car (relay-reads (read-type-relay node)))))
                 (null (gethash (string-upcase (princ-to-string (getattr node :value))) ds))) ;; not defined as a dynamic shape
        (setf (getattr node :value) (make-padding-suffix (getattr node :value) maximum-rank))))
    (return-from unroll-node node))
  (when (null (getattr node :_type_relay :allow-undefined t))
    (return-from unroll-node node))
  (loop for write in (relay-writes (read-type-relay node))
        for w in (node-writes node)
        for wi in (relay-write-iters (read-type-relay node))
        for nth upfrom 0
        if (= (caten/runtime:buffer-nrank write) -1)
          do (setf (nth nth (node-writes node)) (make-suffix-for-is w wi user maximum-rank))
        if (null wi)
          do (setf (nth nth (node-writes node)) (make-padding-suffix w maximum-rank)))
  (loop for read in (relay-reads (read-type-relay node))
        for r in (node-reads node)
        for ri in (relay-read-iters (read-type-relay node))
        for nth upfrom 0
        if (and read ri (= 0 (caten/runtime:buffer-nrank read)))
          do (setf (nth nth (node-reads node)) (make-suffix-for-is r ri user maximum-rank))
        else if (and (symbolp r) (null ri))
          do (setf (nth nth (node-reads node)) (make-padding-suffix r maximum-rank)))
  node)

(defun unroll-expr (space expr user maximum-rank ds)
  "Unrolls the expr who belongs to the User"
  (declare (type node expr) (type User user) (type hash-table ds))
  (assert (eql (node-type expr) :EXPR))
  (assert (null (getattr expr :unroll-history)))
  (assert (null (getattr expr :parent-node-id)))
  (setf (getattr expr :unroll-history) (user-unroll user) (getattr expr :parent-node-id) (node-id expr))
  (let ((suffix (make-suffix space user)))
    (setf (getattr expr :EXPR)
          (make-expr :graph (apply #'make-graph (map 'list #'copy-node (graph-nodes (expr-graph (getattr expr :EXPR)))))
                     :out (copy-node (expr-out (getattr expr :EXPR))))
          (node-id expr) (intern (string-upcase (format nil "~a~a" (node-id expr) suffix))))
    (unroll-node expr user maximum-rank ds)
    (map 'list #'(lambda (node) (when (find (node-type node) `(:AREF :LOAD)) (unroll-node node user maximum-rank ds))) (graph-nodes (expr-graph (getattr expr :EXPR))))
    expr))
;; ~~~ Block/Thread Parallelism ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun astfor-mutate-global (astfor depth local-size callback &key (dtype :int64))
  (assert (expr-equal-to (astfor-from astfor) 0) () "AstFor-Mutate-Global: The Global should start from 0.")
  (let* ((bind (astfor-idx astfor)) (to (astfor-to astfor)) (by (astfor-by astfor))
         (upper-bound (expr-detach-loop-bound to))
         ;; EXPR passed to the GPU Driver
         (gs (expr-ceiling (expr-div (expr-cast upper-bound :float32) (expr-cast (expr-const local-size dtype) :float32)) :float32))
         (global (expr-grid :block depth dtype gs))
         (idx (expr-mul global by))
         (type-relay (make-inferred-type nil (list (caten/runtime:make-buffer nil nil dtype nil :device 'caten/codegen/shape-inference::RelayBuffer))))
         (bind-name (intern (string-upcase (princ-to-string bind))))
         (meta nil)) ;; TODO: Update ExprGrid
    (setf (relay-write-iters type-relay) (list (make-iteration-space)))
    (expr-infer-type idx)
    (make-block
     (list
      (make-astexpr (make-node :JIT :EXPR (list bind-name) nil :declare-type `(t) :EXPR idx :_type_relay type-relay :meta meta) t)
      (funcall callback (astfor-body astfor))))))

(defun astfor-mutate-local (astfor parent depth local-size callback &key (dtype :int64))
  (assert (expr-equal-to (astfor-from astfor) 0) () "AstFor-Mutate-Local: The Local should start from 0.")
  (let* ((bind (astfor-idx astfor)) (by (astfor-by astfor))
         (ls (expr-grid :thread depth dtype local-size))
         (idx (expr-mul ls by)) ;; [TODO] Replace this expr to implement memory coalescing.
         (type-relay (make-inferred-type nil (list (caten/runtime:make-buffer nil nil dtype nil :device 'caten/codegen/shape-inference::RelayBuffer))))
         (bind-name (intern (string-upcase (princ-to-string bind))))
         (meta nil) ;; [TODO]
         ;; reminder computation
         (parent-id (expr-const (intern (string-upcase (astfor-idx parent))) dtype))
         (loop-size (expr-detach-loop-bound (astfor-to parent))))
    (setf (relay-write-iters type-relay) (list (make-iteration-space)))
    (expr-infer-type idx)
    (make-block
     (list
      (make-astexpr (make-node :JIT :EXPR (list bind-name) nil :declare-type `(t) :EXPR idx :_type_relay type-relay :meta meta) t)
      (make-if
       (expr-< (expr-add (expr-const bind-name dtype) parent-id) loop-size)
       (funcall callback (astfor-body astfor))
       nil)))))
