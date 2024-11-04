(defpackage :caten/codegen/expr-cache
  (:documentation "The `expr-cache` is a system designed to forcibly convert Non-Affine Shapes into Affine forms, allowing them to be processed by the ISL Scheduler. (e.g.,` a * b` -> `_expr_id_0`)

- `with-expr-cache` to create a new expr-cache during the scope.
- `stash-expr` to make the same expr to be the same named symbol.
- `restore-expr` to retrive the original expr.
[TODO] Use this to optimize expr-scalar-equivalent-p
")
  (:use :cl :caten/codegen/expr :caten/codegen/renderer)
  (:export
   #:Expr-Cache
   #:*expr-cache*
   #:with-expr-cache
   #:stash-expr
   #:restore-expr
   #:expr-cache-reduce-alias
   #:cache-pointer-map
   #:read-newid
   #:read-ptrid))

(in-package :caten/codegen/expr-cache)

(defvar *expr-cache*)

(defclass Expr-Cache ()
  ((cache :type hash-table :initform (make-hash-table :test 'equal) :accessor cache-table)
   (id2expr :type hash-table :initform (make-hash-table :test 'equal) :accessor id2expr-table)
   (global-counter :initform 0 :type fixnum :accessor global-counter)
   (pointer-map :type hash-table :accessor cache-pointer-map :initarg :pointer-map)
   (global-reduce-alias :type hash-table :initform (make-hash-table :test 'equal) :accessor expr-cache-reduce-alias))
  (:documentation "Creates a cached object for (scalar) EXPR graph."))

(defmacro with-expr-cache ((&key (pointer-map (make-hash-table))) &body body)
  `(let ((*expr-cache* (make-instance 'Expr-Cache :pointer-map ,pointer-map)))
     ,@body))

(defun expr-id (num) (format nil "_expr_id_~a" num))
(defun expr-hash-key (expr) (render-expr 'Default-Renderer expr))

(defmethod %stash-expr ((expr-cache Expr-Cache) (expr Expr))
  (let ((key (expr-hash-key expr)))
    (if (gethash key (cache-table expr-cache))
        (values (expr-id (gethash key (cache-table expr-cache))) nil)
        (progn
          (incf (global-counter expr-cache))
          (setf (gethash key (cache-table expr-cache)) (global-counter expr-cache)
                (gethash (expr-id (global-counter expr-cache)) (id2expr-table expr-cache)) expr)
          (values (expr-id (global-counter expr-cache)) t)))))

(declaim (ftype (function (Expr) (values string boolean &optional)) stash-expr))
(defun stash-expr (expr)
  "Return: (values expr_id is_new)"
  (declare (type Expr expr))
  (assert *expr-cache* () "*expr-cache* is not set.")
  (%stash-expr *expr-cache* expr))

(declaim (ftype (function (string) (or Expr null)) restore-expr))
(defun restore-expr (expr-id)
  (declare (type string expr-id))
  (assert *expr-cache* () "*expr-cache* is not set.")
  (let ((key (format nil "~(~a~)" expr-id)))
    (when (or (<= (length key) 9)
              (not (string= "_expr_id_" (subseq key 0 9))))
      (return-from restore-expr nil))
    (or
     (gethash key (id2expr-table *expr-cache*))
     (error "Undefined EXPR ID ~a (or do not use the symbol starting with _expr_id)" key))))

(defun read-newid (id)
  (assert *expr-cache*)
  (or (gethash id (expr-cache-reduce-alias *expr-cache*)) id))

(defun read-ptrid (id)
  (assert *expr-cache*)
  (or (gethash id (cache-pointer-map *expr-cache*)) id))
