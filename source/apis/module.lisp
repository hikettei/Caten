(in-package :caten/apis)

(defclass Module (Func)
  ((outputs :initform nil :accessor module-outputs)
   (lower-outputs :initform nil :accessor module-lower-outputs)
   (impl-iseq :initform nil :accessor module-impl-iseq)
   (attrs :initform nil :initarg :attrs :accessor module-attrs)
   (sv4bw :initform nil :accessor module-sv4bws))
  (:documentation "See: defmodule"))
(defgeneric impl (module &rest inputs) (:documentation "See: defmodule"))
(defmethod forward :around ((module Module) &rest inputs)
  (declare (ignore inputs))
  (let ((outputs (multiple-value-list (call-next-method))))
    (setf (module-outputs module) outputs)
    (loop for o in outputs
          for nth upfrom 0
          do (setf (tensor-nth-output o) nth))
    (apply #'values outputs)))
(defmethod impl :around ((module Module) &rest inputs)
  (declare (ignore inputs))
  (let ((outputs (multiple-value-list (call-next-method))))
    (setf (module-lower-outputs module) outputs)
    (when (and (every #'(lambda (x) (every #'numberp (shape x))) (module-outputs module))
	       (every #'(lambda (x) (every #'numberp (shape x))) (module-lower-outputs module)))
      (when (not (every #'(lambda (x y) (equal (shape x) (shape y))) (module-outputs module) (module-lower-outputs module)))
	(warn "Detected the inconsistent shape-inference during lowering ~a.~%Please resolve this first before investigating runtime-error.~%Expected(Forward):~%~a~%Lowered:~%~a"
	      module
	      (module-outputs module)
	      (module-lower-outputs module))))
    (apply #'values outputs)))
(defmacro defmodule ((name ((&rest constructor-args) &rest attrs) &key (where nil) (direct-superclasses nil))
		     (&rest slots)
		     &key (documentation "") (impl nil) (forward nil) (backward nil))
  "
```
(defmodule (name ((&rest constructor-args) &rest attrs) &key (where nil) (direct-superclasses nil))
  (&rest slots)
  &key (documentation \"\") (impl nil) (forward nil) (backward nil))
```

Define a module named `name`.

In Caten, `Module` is a CLOS class that represents a set of Funcs and is defined as a subclass of `Func` itself. It is used to represent computational nodes that can be expressed through compositions of Funcs. Consequently, as a subclass of Func, Module utilizes the following three methods for manipulation:

#### [method] impl

`(impl (op Module) &rest tensors)`

- tensors[list] a list of the input tensor.

In the `impl` method, please describe the process for constructing the computational graph of the `Module` using a combination of `Func` and `Module`.
The computational graph must begin with the inputs.
If there are multiple outputs, bind them with `cl:values`.
If you need to record Tensors for the backward process, now is the time to do so.

#### [method] forward

`(forward (op Module) &rest tensors)`

- tensors[List] a list of the input tensor.

In the forward method, describe the operation to create a Tensor after computation.
Be mindful of its lazy evaluation nature; do not perform the actual computation at this stage.
The `st` macro in ShapeTracker is quite useful for creating the Tensor after the operation. If necessary, you may also include checks for additional attributes or slots here.
If you specify ShapeTracker in `:where`, the defmodule macro will automatically generate the forward.
Therefore, you must describe either `:where` or `:forward`.

#### [method] backward (optional)

`(backward (op Module) prev-grad) -> (values input_1.grad input_2.grad ...)`

- prev-grad[Tensor]

In the `backward` method, describe the gradient computation for the Module using a combination of `Func` and `Module`.
The arguments are fixed as `(op prev-grad)`, where op = module instance, prev-grad is a tensor.
If you need the value of the Tensor at the input stage for the gradient calculation, temporarily store it using the `module-sv4bws` accessor while calling the `impl` method.
The compiler does not depend on `module-sv4bws`, so you are free to choose how to store the Tensor.
In Caten, since save-for-backward is automatically determined, there is no need to be concerned about in-place operations.

Note that `backward` is **optional**. If it is not provided, AD will be applied based on the computational graph from `impl`.

#### [method] lower

The lower method is automatically written by the `defmodule`, so there is no need to consider it when describing the module.
However, it is necessary to understand how it is lowered for when defining simplifiers for the `Module`.

`lower` produces the following node:
`(make-node :Graph (intern (symbol-name (symb 'graph/ name)) \"KEYWORD\") outputs inputs &rest attrs)`

Nodes whose class is `:Graph` are completely eliminated during lower by `impl`.

#### Syntax

`forward`, `backward`, `impl` are described in one of the following format.

```
forward := ((op &rest args) &body body)
forward := (lambda (&rest args) &body body)
forward := fname
```

#### Effects

- it defines a class named `name`.
- it defines a function named `name`. it works as a constructor.

#### Arguments

- name[symbol] the name of module
- constructor-args[list] arguments for the constructor
- attrs[list] define attrs for the lowered graph based on the constructor-args variables using the following format: `(:key1 value1 :key1 value2 ...)`.
- slots[list] slots for the defined class.
- where[nil or string] ShapeTracker
- documentation[string] documentation

#### Notes

- The methods are called in the order of `forward->impl->backward` during compilation
- `impl` is performed recursively, so modules must not be co-dependent within the `impl` method. (e.g.: do not define a module `A` that depends on `B` that depends on `A` ...)
"
  (assert (or (stringp where) forward) () "defmodule: Provide ShapeTracker to :where, or provide :forward.")
  (assert impl () "defmodule: :impl is required to lower a module.")
  (labels ((assert-bw-args (args rest)
	     (assert (= (length rest) 1) () "defmodule: The argument for backward is fixed as `(op dout)`~%but got ~a." args))
	   (impl-form (method form bw-p)
	     (with-gensyms (op-bind inputs-bind)
	       (match form
		 ((list* (list* op rest) body)
		  (when bw-p (assert-bw-args `(,op @rest) rest))
		  `(defmethod ,method ((,op ,name) &rest ,inputs-bind)
		     (assert (= (length ,inputs-bind) ,(length rest))
			     ()
			     "Error in the ~a ~a, the number of inputs does not match the declared one.~%decl=~a" ',method ',name ',rest)
		     (multiple-value-bind (,@rest) (apply #'values ,inputs-bind) ,@body)))
		 ((list* 'cl:lambda (list* rest) body)
		  (when bw-p (assert-bw-args rest rest))
		  `(defmethod ,method ((,op-bind ,name) &rest ,inputs-bind)
		     (apply #'(lambda (,@rest) ,@body) ,op-bind ,inputs-bind)))
		 ((guard x (symbolp x))
		  `(defmethod ,method ((,op-bind ,name) &rest ,inputs-bind)
		     (apply #',x ,op-bind ,inputs-bind)))
		 (_ (error "defmodule: The ~(~a~) method should be implemented in one of the following patterns:
  - :~(~a~) := ((op &rest args) &body body)
  - :~(~a~) := (lambda (&rest args) &body body)
  - :~(~a~) := function-name
The provided form does not match any of them:~%~a" method method method method form))))))
    `(progn
       (defclass ,name (Module ,@direct-superclasses) ,slots (:documentation ,documentation))
       ,(if forward
	    (impl-form 'forward forward nil)
	    `(defmethod forward ((op ,name) &rest inputs) (st ,where (inputs))))
       ,(if backward
	    (impl-form 'backward backward t)
	    `(defmethod backward ((op ,name) &optional prev-grad) (declare (ignore prev-grad)) :module/skip-bw))
       ,(impl-form 'impl impl nil)
       (defnode (:Module ,(intern (symbol-name (symb 'graph/ name)) "KEYWORD")) () ""
		:slots (,@(loop for attr in attrs
				for nth upfrom 0
				if (and (= 0 (mod nth 2)) (keywordp attr))
				  collect (list (intern (symbol-name attr))))
			(metadata :type ,name)))
       (defmethod print-object ((module ,name) stream)
         (print-unreadable-object (module stream :type t :identity t)
           (format stream "(~a)"
                   (with-output-to-string (out)
                     (loop for nth upfrom 0 below (length (module-attrs module)) by 2
                           for key = (nth nth (module-attrs module))
                           for value = (nth (1+ nth) (module-attrs module))
                           do (format out ":~(~a~) ~a" key value)
                           if (not (= nth (- (length (module-attrs module)) 2))) do (format out " "))))))
       (defmethod lower ((op ,name) &rest inputs)
	 (make-graph
	  (apply #'make-node :Module (intern (symbol-name (symb 'graph/ ',name)) "KEYWORD")
		 (map 'list #'tensor-id (module-outputs op))
		 (map 'list #'node->id inputs) (append (module-attrs op) (list :metadata op)))))
       (defun ,name (,@constructor-args) (make-instance ',name :attrs (list ,@attrs))))))
;; ~~ State Dict ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defstruct State-Dict
  "
A structure representing the state dictionary of a neural network model.

- Entry[Hash-Table]: A hash table containing the parameters of the model, where:
  - Key[string]: A string representing the parameter name, following a naming convention that reflects the model's architecture.
  - Value[Tensor]: A tensor containing the parameter values associated with the key.

This hash table stores all the parameters of the model, enabling easy saving, loading, and manipulation of model weights.
"
  (entry (make-hash-table :test #'equal) :type hash-table))

(defmethod print-object ((obj State-Dict) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~%  {~%")
    (let* ((keys (map 'list #'princ-to-string (hash-table-keys (state-dict-entry obj))))
           (maxlen (if keys (apply #'max (map 'list #'length keys)) 0)))
      (flet ((print-key (key)
               (let ((len (length key)))
                 (with-output-to-string (out)
                   (princ key out)
                   (dotimes (i (- maxlen len)) (princ " " out))))))
        (maphash
         #'(lambda (key value)
             (format stream "    ~a -> ~a~%" (print-key key) (tensor-shape value)))
         (state-dict-entry obj))))
    (format stream "  }")))

(defmethod state-dict-keys ((state-dict state-dict))
  (hash-table-keys (state-dict-entry state-dict)))

(defmethod state-dict-values ((state-dict state-dict))
  (hash-table-values (state-dict-entry state-dict)))

(defgeneric ->state-dict (module parents) (:documentation "
```
(->state-dict module parents)
```

Generates a list of cons cells containing the keys and values for creating a `State-Dict` from any given `Module/Class`.

Return: A list of cons cells in the form `(coms (list module_names ...) . tensor)`, representing parameters and their corresponding tensors. module_names are the list of symbols representing the path to the parameter slot from the root module.

TL;DR. this function traverses all slots of the Module/Class and recognizes the following objects as parameters:

- Tensor
- Module/Class
- List of Tensors
- List of Modules/Classes

By default, the keys are created according to the following rules:

- Tensor
  - If a slot contains a Tensor, create a cons cell with the key as `(append parent (list slot_name))` and the value as the Tensor.
- Module
  - If a slot contains a Module, recursively apply `(->state-dict slot_value parent)` to that Module with setting parent = `(append parent (list slot_name))` as the new Parent.
- List of Tensors/Modules
  - If a slot contains a list of Tensors or Modules, apply the above rules to each element.
  - Keys are created by appending the slot name and the index (e.g., `slot_name 0`, `slot_name 1`, ...) to the Parent.

To recognize other objects as parameters, please extend this method by adding methods for the desired classes.
"))

(defmethod ->state-dict ((module Module) parents)
  (let ((slots (map 'list #'c2mop:slot-definition-name (c2mop:class-slots (class-of module)))))
    (loop for slot in slots
          for value = (slot-value module slot)
          ;; A single tensor
          if (tensor-p value)
            collect (cons (append parents (list slot)) value)
          ;; A list of tensor: parents.0, parents.1, ...
          if (and value (listp value) (every #'tensor-p value))
            append
            (loop for nth upfrom 0
                  for v in value
                  collect (cons (append parents (list slot nth)) v))
            ;; A list of Module:
          if (and value (listp value) (every #'(lambda (x) (typep x 'Module)) value))
            append
            (loop for nth upfrom 0
                  for m in value
                  append (->state-dict m (append parents (list slot nth))))
          if (typep value 'Module)
            append (->state-dict value (append parents (list slot))))))

(defun pytorch-style-dict-key (list)
  (let ((key (apply #'concatenate 'string (butlast (loop for l in list append (list (format nil "~(~a~)" l) "."))))))
    (cl-ppcre:regex-replace-all "-" key "_")))

(defun get-state-dict (module &key (key-mapper #'pytorch-style-dict-key))
  "
```
(get-state-dict module &key (key-mapper #'pytorch-style-dict-key))
```

Constructs a `State-Dict` by recursively exploring all paramters of the given Module/Class.

- Module[Module/Class] The module from which to extract parameters.
- Key-Mapper[Function] A function that takes a list of names (the first element of the cons cell) and returns a string to be used as the key in the StateDict. Defaults to `pytorch-style-dict-key`. which must be `#'(lambda (x) ...)`

Returns: `State-Dict`"
  (declare (type Module module))
  (let ((state-dict-base (->state-dict module nil))
        (state-dict (make-state-dict)))
    (assert (every #'consp state-dict-base) () "get-state-dict: The state-dict-base must be a list of cons.")
    (loop for (key . value) in state-dict-base
          do (assert (tensor-p value) () "get-state-dict: The value for ~a must be a tensor, getting ~a" key value)
             (setf (gethash (funcall key-mapper key) (state-dict-entry state-dict)) value))
    state-dict))

(declaim (ftype (function (Module State-Dict &key (:silent boolean) (:key-mapper function)) Module) load-state-dict))
(defun load-state-dict (module state-dict &key (silent nil) (key-mapper #'pytorch-style-dict-key))
  "
```
(load-state-dict module state-dict &key (silent nil) (key-mapper #'pytorch-style-dict-key))
```

Loads the parameters from the given state-dict into the module, returning the given module.
- silent[boolean] If set to t, suppresses warnings about unused keys, dtype mismatches, shape mismatches, and uninitialized tensors.
- key-mapper[function] A function used to map the keys in the state-dict to the keys in the module. Defaults to `pytorch-style-dict-key`. (see: get-state-dict)
"
  (declare (type State-Dict state-dict) (type module module))
  (let ((model-state-dict (get-state-dict module :key-mapper key-mapper)))
    (when (and (null silent) (> (length (state-dict-keys state-dict)) (length (state-dict-keys model-state-dict))))
      (let ((not-found (set-difference (state-dict-keys state-dict) (state-dict-keys model-state-dict) :test #'string=)))
        (warn "load-state-dict: Found unused keys in the state-dict: ~a" not-found)))
    (maphash
     #'(lambda (k v)
         (let ((replacement (gethash k (state-dict-entry state-dict))))
           (when (and (null silent) (null replacement))
             (warn "load-state-dict: not loading ~a" k))
           (when replacement
             (when (null silent)
               (when (not (eql (tensor-dtype v) (tensor-dtype replacement)))
                 (warn "load-state-dict: dtype mismatch for ~a: place: ~a -> replacement: ~a" k (tensor-dtype v) (tensor-dtype replacement)))
               (when (not (equal (shape v) (shape replacement)))
                 (warn "load-state-dict: shape mismatch for ~a: place: ~a -> replacement: ~a" k (shape v) (shape replacement)))
               (when (null (tensor-buffer replacement))
                 (warn "load-state-dict: loading to an uninitialized tensor ~a" k)))
             (setf (tensor-buffer v) (tensor-buffer replacement)))))
     (state-dict-entry model-state-dict))
    module))
