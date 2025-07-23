(in-package :caten/lang)

(defstruct (directive-context)
  "A structure for storing the result of `read-caten-directive function.`
```lisp
@caten.<feat_name>(params){
  CODE
}
```
"
  (params nil :type list)
  (code "" :type string))

(defparameter *feature-docstring-table* (make-hash-table))

(defgeneric get-feature-expander-macro (dispatcher-id)
  (:documentation "The method `get-feature-expander-macro` return a lambda function which returns a s-expression for the feature"))

(defmethod get-feature-expander-macro (dispatcher-id)
  (if (next-method-p)
      (call-next-method)
      (error "@caten macro parse error: The feature @caten.~(~a~) is not defined.~%
Currently following features are available:~%~a"
             dispatcher-id
             (with-output-to-string (out)
               (maphash #'(lambda (x y) (format out "@caten.~(~a~)~%```~%~a~%```~%" x y)) *feature-docstring-table*)))))

(defmacro define-caten-feature ((name &key (docstring "No docstring is provided")) ((&rest args) &body body))
  "Defines a caten feature macro which is expanded by ```@caten.<name>(...) { ...} syntax```
The expander function is called as (lambda (directive-ctx &rest args) body) The body must return a s-expression
which is evaluated in the compilation time."
  (declare (type symbol name) (type string docstring))
  (alexandria:with-gensyms (dispatcher-id)
    (let ((idx (intern (symbol-name name) "KEYWORD")))
      `(progn
         (setf (gethash ,idx *feature-docstring-table*) ,docstring)
         (defmethod get-feature-expander-macro ((,dispatcher-id (eql ,idx)))
           #'(lambda (,@args) ,@body))))))
