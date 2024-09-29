(in-package :caten/apis)
;;(defpackage :caten/ops)
;; TODO: Doing an operator overloading at `c::+` and `c::-` (is it a good idea?)
;; Note that doing this in caten/apis package will decrease the performance even the generid methods are inlined.

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Transform: Experimental Notation for manipulating the symbolic shape.
  ;; (WIP)
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
;; The code below is out-of-date and not supported. but i think the idea is really good as proven in cl-waffe2 ...
(defmacro ~ (&rest transformation)
  "TODO: Docs
(!reshape x (~ A B C -> (!* A B C)))
(!view x (~ 0))"
  (multiple-value-bind (before after) (%parse-tf transformation)
    `(make-transform :before ',before :after ',after :caller ,(if after (%->transform before after) (%->shape before)))))

;; Einsum
(eval-when (:compile-toplevel :load-toplevel :execute)

  )
