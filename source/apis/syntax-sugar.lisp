(in-package :caten/apis)
;;(defpackage :caten/ops)
;; todo: define a new package which overloads cl:+ cl:- ...
;; this may decrease the performance but the entire code should looks pretty good
;; Experimental: ShapeTransformer (what feature is required for the comprehension of the shape operations?)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %parse-tf (tf)
    "Tf: Either of (~ A B C) or (~ A B C -> A B C)"
    (declare (type list tf))
    (let* ((pos (position "->" tf :test #'string= :key #'(lambda (x) (format nil "~a" x))))
	   (cnt (count "->" tf :test #'string= :key #'(lambda (x) (format nil "~a" x)))))
      (assert (<= cnt 1) () "Failed to compile the shape transformer:
Too many arrows (->).
Follow the either of:
  - (~ A B C)
  - (~ A B C -> A B C)")
      (values (subseq tf 0 pos) (when pos (subseq tf (1+ pos))))))

  (defun %->transform (before after)
    (with-gensyms (thing)
      `(lambda (,thing)
	 (declare (type list ,thing))
	 (match ,thing
	   ((list ,@before) ,@after)
	   (_ (error "Transform"))))))
  
  (defun %->shape (before)
    (with-gensyms (thing count)
      `(lambda (,thing)
	 (declare (type list ,thing))
	 (loop for ,count upfrom 0 below (length ,thing)
	       collect (or (nth ,count ',before) (nth ,count ,thing)))))))

(defstruct Transform (before nil :type list) (after nil :type list) (caller (error "caller should occur")))
(defmethod apply-transform ((op transform) list) (funcall (transform-caller op) list))
(defmacro ~ (&rest transformation)
  "TODO: Docs
(!reshape x (~ A B C -> (!* A B C)))
(!view x (~ 0))"
  (multiple-value-bind (before after) (%parse-tf transformation)
    `(make-transform :before ',before :after ',after :caller ,(if after (%->transform before after) (%->shape before)))))
