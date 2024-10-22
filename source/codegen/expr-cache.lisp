(defpackage :caten/codegen/expr-cache
  (:documentation "stash-expr to make the same expr to be the same named symbol. restore-expr to retrive the original expr.")
  (:use :cl :caten/codegen/expr :caten/codegen/renderer)
  (:export
   #:Expr-Cache
   #:with-expr-cache
   #:stash-expr
   #:restore-expr))

(in-package :caten/codegen/expr-cache)
;; [TODO] Use this to optimize expr-scalar-equivalent-p
(defvar *expr-cache*)

(defclass Expr-Cache ()
  ((cache :type hash-table :initform (make-hash-table :test 'equal) :accessor cache-table)
   (id2expr :type hash-table :initform (make-hash-table :test 'equal) :accessor id2expr-table)
   (global-counter :initform 0 :type fixnum :accessor global-counter))
  (:documentation "Creates a cached object for (scalar) EXPR graph."))

(defmacro with-expr-cache (() &body body)
  `(let ((*expr-cache* (make-instance 'Expr-Cache)))
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