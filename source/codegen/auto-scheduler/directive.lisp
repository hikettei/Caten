(defpackage :caten/codegen/directive
  (:documentation "Provides various hand-written blueprint rewriting rule for marks.")
  (:use :cl :caten/codegen/expr :caten/codegen/polyhedral-ast :caten/codegen/unroll :caten/air
        :caten/codegen/shape-inference)
  (:import-from :caten/codegen/scop #:expr-detach-loop-bound)
  (:export
   #:make-unrolled-body
   #:compute-reminder-for-unroll
   #:unroll-expr))

(in-package :caten/codegen/directive)

(defun make-unrolled-body (user body nth)
  "Creates a copy of ASTFor body with the idx is set to nth."
  (declare (type ASTFor body) (type fixnum nth))
  (copy-and-assign-ast-tree (astfor-body body) (astfor-idx body) nth :unroll-at (astfor-idx user)))

(defmethod astfor-compute-reminder ((astfor ASTFor))
  (let ((below (expr-detach-loop-bound (astfor-to astfor)))
        (incremental (astfor-by astfor)))
    (assert (eql :Load (node-type (expr-out incremental))) () "The AstFor is not an SCOP.")
    ;; TODO(hikettei): wanna assert below > from
    (values (expr-mod (expr-sub below (astfor-from astfor)) incremental) below)))

(defun compute-reminder-for-unroll (user body)
  "Creates an AstFor for the reminder part of the unrolled directive."
  (declare (type ASTFor body))
  (multiple-value-bind (reminder size) (astfor-compute-reminder user)
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
    (when (= -1 (caten/avm:buffer-nrank (getattr node :buffer)))
      (setf (getattr node :storage-id) (make-suffix-for-is (getattr node :storage-id) (getattr node :space) space user)))
    (return-from unroll-node node))
  (when (null (getattr node :_type_relay :allow-undefined t))
    (return-from unroll-node node))
  (loop for write in (relay-writes (read-type-relay node))
        for w in (node-writes node)
        for wi in (relay-write-iters (read-type-relay node))
        for nth upfrom 0
        if (= (caten/avm:buffer-nrank write) -1)
          do (setf (nth nth (node-writes node)) (make-suffix-for-is w wi space user)))
  (loop for read in (relay-reads (read-type-relay node))
        for r in (node-reads node)
        for ri in (relay-read-iters (read-type-relay node))
        for nth upfrom 0
        if (and read ri (= 0 (caten/avm:buffer-nrank read)))
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
