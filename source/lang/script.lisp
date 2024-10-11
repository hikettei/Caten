(in-package :caten/lang)

(eval-when (:compile-toplevel :load-toplevel :execute)

(defparameter *action-macro-features* (make-hash-table :test #'equal)
  "*action-macro-features* is a hash-table that maps a symbol to macro-function[lambda].
This table is used to to expand a macro in the parse-action-body function.
New macro can be added by using the macro a/defmacro.
")

(defparameter *action-function-features* (make-hash-table :test #'equal)
  "*action-function-features* is a hash-table that maps a symbol to builtin function.
This table is used to create a parsed object in the parse-action-body function.
New function can be added by using the macro a/defun.")

(defmacro a/defmacro (name lambda-list documentation &body body)
  "
```
(a/defmacro name lambda-list documentation &body body)
```

Defines a macro only used in action-body.

Internally, `name` is processed as a string to simplify the management of symbols.
"
  (assert (and documentation (stringp documentation)) () "a/defmacro: documentation is required!")
  `(progn
     (when (gethash ,(symbol-name name) *action-macro-features*)
       (warn "Redefining the macro ~a" ',name))
     (when (gethash ,(symbol-name name) *action-macro-features*)
       (warn "The function ~a was already defined as a macro." ',name))
     (setf (gethash ,(symbol-name name) *action-macro-features*)
           (lambda ,lambda-list ,@body))))

(defmacro a/defun (name lambda-list documentation &body body)
  "
```
(a/defun name lambda-list documentation &body body)
```
Defines a build-in function only used in action-body.

Internally, `name` is processed as a string to simplify the management of symbols.
"
  (assert (and documentation (stringp documentation)) () "a/defun: documentation is required!")
  `(progn
     (when (gethash ,(symbol-name name) *action-function-features*)
       (warn "Redefining the function ~a" ',name))
     (when (gethash ,(symbol-name name) *action-macro-features*)
       (warn "The function ~a was already defined as a macro." ',name))
     (setf (gethash ,(symbol-name name) *action-function-features*)
           (lambda ,lambda-list ,@body))))

(defmacro assert-syntax-error (form (message &rest args) body position)
  `(when (not ,form)
     (error 'Action-Syntax-Error
            :message (format nil ,message ,@args)
            :form ,body
            :position ,position)))

(defun a/macroexpand-all (body)
  (match body
    ((guard x (numberp x)) x)
    ((guard x (symbolp x))
     ;; [TODO] Defvar check
     x)
    ((guard x (stringp x)) x)
    ((list* _)
     (multiple-value-bind (car cdr) (values (car body) (cdr body))
       (assert-syntax-error
           (symbolp car) ("Invaild Syntax: car should be a symbol, but got ~a." car)
           body 0)
       (let ((macro-feature (gethash (symbol-name car) *action-macro-features*))
             (function-feature (gethash (symbol-name car) *action-function-features*)))
         (assert-syntax-error
             (or macro-feature function-feature)
             ("Invaild Syntax: The macro or function ~a is not defined." car)
             body 0)
         (assert (not (and macro-feature function-feature)) () "a/defun and a/defmacro are conflicted for ~a." car)
         (if macro-feature
             (handler-bind ((error #'(lambda (cond) (assert-syntax-error nil (" caught by the error when parsing the macro ~a:~% ~a" car cond) body 0))))
               (a/macroexpand-all (apply macro-feature cdr)))
             `(,car ,@(map 'list #'a/macroexpand-all cdr))))))
    (_
     (assert-syntax-error
         nil
         ("Invaild Syntax: The form is not supported.")
         body 0))))
;; ParseActionBody -> Return:
;; 
;;
;;

;; {
;;   int i;みたいにすることを想定している SCOPE ENDSCOPE (Optional)
;; }

(defclass ActionBody ()
  ((graph :type Graph :reader actionbody-graph :initarg :graph)
   (pipeline :type hash-table :reader actionbody-pipeline :initarg :pipeline))
  (:documentation "ActionBody is a parsed action-body form.
graph is a render-graph.
pipeline is a hash-table that maps an index of FUNCALL to a graph.
"))

;; やっぱFunctionにしたほうがいいかも
(defmacro action-body ((&rest arguments) &body body)
  "
- arguments[list] a list of symbols that is already defined in the scope
- body[form] script
"
  (a/macroexpand-all `(progn ,@body)))

#|
(action-body (n transformer)
  (if (< n 10)
      (run transformer)))

(action-body ()
  (let ((tokens (map 'vector (lambda (c) (position c vocabulary :test #'string=)) text)))
    (loop named outer
	  for best-score = -1e10
	  for best-id = -1
	  for best-index = -1
	  do (loop for i below (1- (length tokens))
		   for string = (concatenate
                                 'string
	                         (aref vocabulary (aref tokens i))
	                         (aref vocabulary (aref tokens (1+ i))))
		   for id = (position string vocabulary :test #'string=)
		   if (and id (> (aref scores id) best-score)) ;This merge pair exists in vocabulary
		     do (setf best-score (aref scores id)
			      best-id id
			      best-index i))
	     (if (= best-index -1) (return-from outer tokens))
	     (setf (aref tokens best-index) best-id
		   tokens
                   (concatenate 'vector
                                (subseq tokens 0 (1+ best-index))
(subseq tokens (+ 2 best-index)))))))

|#

)
