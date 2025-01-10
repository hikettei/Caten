(defpackage :caten/codegen/directive
  (:documentation "Provides various hand-written blueprint rewriting rule for corresponding @directive.")
  (:use :cl :caten/codegen/expr :caten/codegen/polyhedral-ast :caten/air :caten/codegen/shape-inference)
  (:export
    #:make-unrolled-body
    #:compute-reminder-for-unroll
    #:unroll-expr
    #:astfor-mutate-global
    #:astfor-mutate-reminder-global))

(in-package :caten/codegen/directive)
;; ~~~ UNROLL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun make-unrolled-body (user body n-unroll)
  "Creates a copy of ASTFor body with the idx is set to nth."
  (declare (type ASTFor body) (type fixnum n-unroll))
  (unroll-ast (astfor-body body) (astfor-idx body) (astfor-idx user) n-unroll))

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

(defun make-suffix (space user)
  (flet ((s (idx &aux (val (find idx (user-unroll user) :key #'car :test #'string=)))
           (if val (cdr val) nil)))
    (let ((suffix
            (apply #'concatenate 'string
                   (butlast (loop for s in space
                                  for id = (s s)
                                  if id append (list (princ-to-string id) "_"))))))
      (if (string= suffix "")
          ""
          (format nil "_~a" suffix)))))

(defun make-suffix-for-is (name iteration-space space user)
  (let ((space (loop for s in space
                     for stride in (iteration-space-strides iteration-space)
                     unless (expr-equal-to stride 0) collect s)))
    (intern (string-upcase (format nil "~a~a" name (make-suffix space user))))))

(defun unroll-node (node space user)
  (when (eql (node-type node) :Aref)
    (when (= -1 (caten/runtime:buffer-nrank (getattr node :buffer)))
      (setf (getattr node :storage-id) (make-suffix-for-is (getattr node :storage-id) (getattr node :space) space user)))
    (return-from unroll-node node))
  (when (null (getattr node :_type_relay :allow-undefined t))
    (return-from unroll-node node))
  (loop for write in (relay-writes (read-type-relay node))
        for w in (node-writes node)
        for wi in (relay-write-iters (read-type-relay node))
        for nth upfrom 0
        if (= (caten/runtime:buffer-nrank write) -1)
          do (setf (nth nth (node-writes node)) (make-suffix-for-is w wi space user)))
  (loop for read in (relay-reads (read-type-relay node))
        for r in (node-reads node)
        for ri in (relay-read-iters (read-type-relay node))
        for nth upfrom 0
        if (and read ri (= 0 (caten/runtime:buffer-nrank read)))
          do (setf (nth nth (node-reads node)) (make-suffix-for-is r ri space user)))
  node)

(defun unroll-expr (space expr user)
  "Unrolls the expr who belongs to the User"
  (declare (type node expr) (type User user))
  (when (null (user-unroll user))
    (return-from unroll-expr expr))
  (assert (eql (node-type expr) :EXPR))
  (assert (null (getattr expr :unroll-history)))
  (assert (null (getattr expr :parent-node-id)))
  (setf (getattr expr :unroll-history) (user-unroll user) (getattr expr :parent-node-id) (node-id expr))
  (let ((suffix (make-suffix space user)))
    (setf (getattr expr :EXPR)
          (make-expr :graph (apply #'make-graph (map 'list #'copy-node (graph-nodes (expr-graph (getattr expr :EXPR)))))
                     :out (copy-node (expr-out (getattr expr :EXPR))))
          (node-id expr) (intern (string-upcase (format nil "~a~a" (node-id expr) suffix))))
    (unroll-node expr space user)
    (map 'list #'(lambda (node) (when (eql (node-type node) :AREF) (unroll-node node space user))) (graph-nodes (expr-graph (getattr expr :EXPR))))
    expr))
;; ~~~ Block/Thread Parallelism ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun astfor-mutate-global (astfor depth local-size &key (dtype :int64))
  "Mutates the ASTFor to use global/local indexing."
  (declare (type ASTFor astfor))
  (let* ((bind (astfor-idx astfor)) (from (astfor-from astfor)) (to (astfor-to astfor)) (by (astfor-by astfor))
         (upper-bound (expr-detach-loop-bound to :allow-failed t))
         (size (expr-ceiling (expr-div (expr-cast (expr-sub upper-bound from) :float32) (expr-cast by :float32)) :float32))
         (gs (expr-cast (expr-div size (expr-const local-size :float32)) dtype))
         (global (expr-grid :block depth dtype gs))
         (local  (expr-grid :thread depth dtype (expr-const local-size dtype)))
         (idx (expr-add (expr-mul global (expr-const local-size dtype)) local))
         (idx (expr-add from (expr-mul idx by)))
         (type-relay (make-inferred-type nil (list (caten/runtime:make-buffer nil nil dtype nil :device 'caten/codegen/shape-inference::RelayBuffer))))
         (bind-name (intern (string-upcase (princ-to-string bind))))
         (meta (make-instance 'Exprgrid :rank depth :global-size gs :local-size (expr-const local-size dtype))))
    (setf (relay-write-iters type-relay) (list (make-iteration-space)))
    (assert upper-bound () "astfor-mutate-global: The ASTFor is not an SCOP: ~a" astfor)
    (expr-infer-type idx)
    (make-block
     (list
      (make-astexpr (make-node :JIT :EXPR (list bind-name) nil :declare-type `(t) :EXPR idx :_type_relay type-relay :meta meta) t)
      (make-if
       (expr-< (expr-const bind-name dtype) upper-bound)
       (astfor-body astfor)
       nil)))))

(defun astfor-mutate-reminder-global (parent-idx astfor &key (dtype :int64))
  "This function receives the reminder part of ASTFor, returning the new ASTFor using global indexing."
  (declare (type ASTFor astfor))
  ;; (IF IDX == UPFROM)
  ;; ASTFOR
  (let* ((parent-bind (intern (string-upcase (princ-to-string parent-idx))))
         (from (astfor-from astfor)))
    (make-if (expr-= (expr-const parent-bind dtype) from) astfor nil)))
