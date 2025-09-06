(in-package :caten/isl)

(defun parse-isl-args (args)
  (loop for arg in args
        for type = (car arg)
        collect
        (ecase type
          (:keep `(:keep ,(second arg) ,(gensym (symbol-name (second arg)))))
          (:parm arg)
          (:take `(:take ,(second arg) ,(gensym (symbol-name (second arg))))))))

(defun infer-result-wrapper (result-type)
  (cond ((isl-object-name-p result-type)
         (isl-object-%make result-type))
        ((eql result-type 'boolean)
         'lispify-isl-bool)
        ((eql result-type 'size)
         'lispify-isl-size)
        (t 'identity)))

(defmacro define-isl-function (name primitive result &rest args)
  (check-type name symbol)
  (check-type primitive symbol)
  (let ((args (parse-isl-args args)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (export ',name)
       (declaim
        (ftype
         (function
          (,@(loop for arg in args
                   if (not (eql (car arg) :parm))
                     collect (second arg)))
          (values ,(second result)))
         ,name))
       (defun ,name (,@(loop for arg in args
                             if (not (eql (car arg) :parm))
                               collect (third arg)))
         (declare
          ,@(loop for arg in args
                  if (not (eql (car arg) :parm))
                    collect `(type ,@(cdr arg))))
         (,(infer-result-wrapper (second result))
          (,primitive
           ,@(loop for arg in args
                   for type = (car arg) for lisp-type = (second arg) for value = (third arg)
                   collect
                   (ecase type
                     (:keep
                      (if (isl-object-name-p lisp-type)
                          `(isl-object-handle (__isl_keep ,value))
                          value))
                     (:parm
                      (if (isl-object-name-p lisp-type)
                          `(isl-object-handle (the ,lisp-type ,value))
                          `(the ,lisp-type ,(third arg))))
                     (:take
                      (if (isl-object-name-p lisp-type)
                          `(isl-object-handle (__isl_take ,value))
                          value))
                     (:null (error "Arguments with :null qualifier are not allowed."))))))))))
