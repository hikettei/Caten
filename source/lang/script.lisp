(in-package :caten/lang)

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
  (let ((tmp-name (intern (format nil "~a_FORM" name))))
    `(progn
       (when (gethash ,(symbol-name name) *action-macro-features*)
         (warn "Redefining the macro ~a" ',name))
       (when (gethash ,(symbol-name name) *action-macro-features*)
         (warn "The function ~a was already defined as a macro." ',name))
       (setf (gethash ,(symbol-name name) *action-macro-features*)
             (flet ((,tmp-name ,lambda-list ,@body)) #',tmp-name)))))

(defmacro a/defun (name (ctx-bind &rest lambda-list) documentation &body body)
  "
```
(a/defun name lambda-list documentation &body body)
```
Defines a build-in function only used in action-body.

Internally, `name` is processed as a string to simplify the management of symbols.

The function defined by this macro receives the class `Context` as the first argument. So, the lambda-list should be defined as `(context &rest args)`.

The function should return a `Parsed-Form` object.

The function will receive arguments as a `Parsed-Form` object.
"
  (assert (and documentation (stringp documentation)) () "a/defun: documentation is required!")
  (let ((tmp-name (intern (format nil "~a_FORM" name))))
  `(progn
     (when (gethash ,(symbol-name name) *action-function-features*)
       (warn "Redefining the function ~a" ',name))
     (when (gethash ,(symbol-name name) *action-macro-features*)
       (warn "The function ~a was already defined as a macro." ',name))
     (setf (gethash ,(symbol-name name) *action-function-features*)
           (flet ((,tmp-name (,ctx-bind ,@lambda-list) ,@body))
             #',tmp-name)))))

(defmacro assert-syntax-error (form (message &rest args) body position)
  `(when (not ,form)
     (error 'Action-Syntax-Error
            :message (format nil ,message ,@args)
            :form ,body
            :position ,position)))

(defun a/macroexpand-all (body)
  (match body
    ((guard x (numberp x)) x)
    ((guard x (symbolp x)) x)
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
               ;; Bottom-up
               (a/macroexpand-all (apply macro-feature cdr)))
             `(,car ,@(map 'list #'a/macroexpand-all cdr))))))
    (_
     (assert-syntax-error
         nil
         ("Invaild Syntax: The form is not supported.")
         body 0))))

(defun a/parse-form (context body)
  (match body
    ((guard x (numberp x))
     (make-parsed-form
      nil
      (caten/ajit:make-expr :Const x (make-const-buffer (lisp-type->dtype (type-of x))))
      (make-const-buffer (lisp-type->dtype (type-of x)))))
    ((guard x (symbolp x))
     (make-parsed-form
      nil
      (caten/ajit:make-expr :Const x (ctx-get-variable-type context x))
      (ctx-get-variable-type context x)))
    ((list* _)
     (multiple-value-bind (car cdr) (values (car body) (cdr body))
       (assert-syntax-error
           (symbolp car) ("Invaild Syntax: car should be a symbol, but got ~a." car)
           body 0)
       (let ((macro-feature (gethash (symbol-name car) *action-macro-features*))
             (function-feature (gethash (symbol-name car) *action-function-features*)))
         (assert-syntax-error
             (null macro-feature)
             ("Invaild Syntax: ~a should be a function, not a macro" car)
             body 0)
         (assert-syntax-error function-feature ("The function ~a is not defined." car) body 0)
         (handler-bind ((error #'(lambda (cond) (assert-syntax-error nil (" caught by the error when parsing the function ~a:~% ~a" car cond) body 0))))
           ;; Top-down
           (flet ((is-parsed-form? (form)
                    (assert (parsed-form-p form))
                    form))
             (let ((output (apply function-feature context (map 'list #'is-parsed-form? (map 'list #'(lambda (x) (a/parse-form context x)) cdr)))))
               (assert (typep output 'Parsed-Form) () "The function ~a should return a parsed-form." car)
               output))))))
    (_
     (assert-syntax-error
         nil
         ("Invaild Syntax: The form is not supported.")
         body 0))))

(defclass Context ()
  ((name :type symbol :initarg :name :accessor ctx-name)
   (parsed-form :type Parsed-Form :accessor ctx-parsed-form)
   (var2type :type hash-table :reader ctx-var2type :initform (make-hash-table :test #'equal))
   (pipeline :type hash-table :reader ctx-pipeline :initarg :pipeline))
  (:documentation "Context is a class that is used to manage the state of the action-body.
Graph is a render-graph.
pipeline is a hash-table that maps an index of FUNCALL to a graph.
"))

(defmethod print-object ((ctx Context) stream)
  (print-unreadable-object (ctx stream :type t :identity t)
    (format stream "~a
 :parsed-form  ~a
 :pipeline ~a"
            (ctx-name ctx)
            (ctx-parsed-form ctx)
            (ctx-pipeline ctx))))

(defstruct (Parsed-Form
            (:constructor make-parsed-form (nodes expr type)))
  "Parsed-Form is an object where:
- `nodes` represents for the render-graph.
- the form returns a type of `type`.
- If you evaluate the form, it will return `expr`."
  (nodes nodes :type list)
  (expr expr :type caten/ajit:Expr)
  (type type :type caten/avm:Buffer))

(defmethod ctx-define-and-make-funcall-from-expr ((ctx Context) (expr caten/ajit:Expr) write type decl)
  (let ((name (gensym "CALL")))
    (setf (gethash name (ctx-pipeline ctx))
          (make-graph
           (make-node :JIT :EXPR (list write) nil :expr expr :reduction nil
                      :_type_relay (caten/ajit:make-inferred-type nil (list type))
                      :declare-type decl)))
    (caten/ajit:r/funcall-string name)))

(defmethod ctx-declare-local-var ((ctx Context) place dtype)
  (declare (type symbol place)
           (type keyword dtype))
  (let ((name (gensym "TMP")))
    (setf (gethash name (ctx-pipeline ctx))
          (make-graph
           (make-node :Buffer :Allocate (list place) nil :dtype dtype :nrank 0
                      :_type_relay (caten/ajit:make-inferred-type nil (list (make-const-buffer dtype))))))
    (caten/ajit:r/funcall-string name)))

(defmethod ctx-register-variable ((ctx Context) place type)
  (declare (type symbol place)
           (type caten/avm:Buffer type))
  (when (gethash (symbol-name place) (ctx-var2type ctx))
    (warn "The variable ~a was already defined." place))
  (setf (gethash (symbol-name place) (ctx-var2type ctx)) type))

(defmethod parsed-form-output-to ((form Parsed-Form))
  (let ((form (parsed-form-expr form)))
    (assert (eql :Const (caten/ajit::expr-op form)) () "The form should be a constant.")
    (caten/ajit:expr-x form)))

(defmethod ctx-remove-variable ((ctx Context) place)
  (declare (type symbol place))
  (remhash (symbol-name place) (ctx-var2type ctx)))

(defmethod ctx-get-variable-type ((ctx Context) place)
  (declare (type symbol place))
  (or (gethash (symbol-name place) (ctx-var2type ctx))
      (error "The variable ~a is not defined here." place)))

(defmethod ctx-render ((ctx Context) (device caten/ajit:Device))
  (caten/ajit:%render-body
   device device
   (apply #'make-graph (parsed-form-nodes (ctx-parsed-form ctx)))
   (ctx-pipeline ctx) 1 nil))

(defun make-context-from-list (&rest body)
  "
```
(make-context-from-list &rest body)
```

- arguments[list] a list of symbols that is already defined in the scope
- body[form] script
"
  (let ((context (make-instance 'Context :name (gensym "BLOCK") :pipeline (make-hash-table))))
    (setf (ctx-parsed-form context)
          (a/parse-form context (a/macroexpand-all `(progn ,@body))))
    context))

;; Export to Cを実装
;; 
;; let
;; {
;;   int i;みたいにすることを想定している SCOPE ENDSCOPE (Optional)
;; }

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
