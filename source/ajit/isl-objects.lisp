(in-package :caten/ajit)

(deftype integer-t () `(or number symbol))
(defmacro define-isl-object (print-name docstring ((&rest args) &rest slots) &body body)
  (declare (type string print-name))
  (let* ((name (intern (string-upcase print-name)))
	 (constructor (symb 'make- name)))
    `(progn
       (defstruct (,name
		   (:constructor ,constructor (,@args)))
	 ,docstring
	 ,@slots)
       ;; [TODO] Confirm no memory-leak here...
       (defmethod form ((c ,name)) ,@body)
       (defmethod print-object ((c ,name) stream) (format stream "~a: ~a" ,print-name (form c))))))

(define-isl-object "IConstraint"
    "Equivalent to `upfrom <= var < below`"
    ((var upfrom below)
     (var var :type symbol)
     (upfrom upfrom :type integer-t)
     (below below :type integer-t))
  (format nil "~(~a~) <= ~(~a~) < ~(~a~)"
	  (iconstraint-upfrom c)
	  (iconstraint-var c)
	  (iconstraint-below c)))

(define-isl-object "IUnion"
    "Union: [m] where m = alpha * index + beta"
    ((indices &optional (alphas '(1)) (betas '(0)))
     (indices indices :type list)
     (alphas alphas :type list)
     (betas betas :type list))
  (with-output-to-string (out)
    (with-slots ((indices indices) (alphas alphas) (betas betas)) c
      (format out "[ ")
      (loop for index in indices for alpha in alphas for beta in betas do
	(cond
	  ((every #'numberp `(,index ,alpha ,beta))
	   (format out "~a" (+ (* index alpha) beta)))
	  ((and (numberp alpha) (numberp beta) (= alpha 1) (= beta 0))
	   (format out "~(~a~)" index))
	  ((and (numberp alpha) (numberp beta) (= 0 beta alpha))
	   (format out "0"))
	  ((and (numberp beta) (= beta 0))
	   (format out "~(~a~)*~(~a~)" index alpha))
	  ((and (numberp alpha) (= alpha 0))
	   (format out "~(~a~)" beta))
	  (T
	   (format out "(~(~a~)*~(~a~)+~(~a~))" index alpha beta)))
	(format out "+"))
      (format out "0 ]"))))

(define-isl-object "IMap"
    "IMap: { Union -> Union }"
    ((union-read union-write)
     (union-read union-read :type IUnion)
     (union-map union-write :type IUnion))
  (with-slots ((read union-read) (write union-write)) c
    (format nil "{ ~a -> ~a }" (form read) (form write))))

(define-isl-object "CUnion"
    "CUnion (Constrainted Union): { Union : Constraint }"
    ((union constraint)
     (union union :type IUnion)
     (constraint constraint :type IConstraint))
  (with-slots ((u union) (cnst constraint)) c
    (format nil "{ ~a : ~a }" (form u) (form cnst))))

