(in-package :caten)
;; Module = the higher level graph of 
;;
;;
(defclass Module (Func)
  ((outputs :initform nil :accessor module-outputs)
   (attrs :initform nil :initarg :attrs :accessor module-attrs))
  (:documentation "## [class] Module"))
(defgeneric impl (module &rest inputs) (:documentation "Lowers the module into functions"))
(defmethod forward :around ((module Module) &rest inputs)
  (declare (ignore inputs))
  (let ((outputs (multiple-value-list (call-next-method))))
    (setf (module-outputs module) outputs)
    (apply #'values outputs)))
;; TODO: Improve assertions
(defmacro defmodule ((name ((&rest constructor-args) &rest attrs) &key (where nil)) (&rest slots) &key (documentation "") (impl nil) (forward nil) (backward nil))
  "## [macro] defmodule
forward := ((op &rest args) &body body)
forward := (lambda (&rest args) &body body)
forward := #'fname"
  (assert (or (stringp where) forward) () "defmodule: Provide st in :where, or supercede :forward")
  (assert impl ())
  (labels ((assert-bw-args (rest)
	     (assert (= (length rest) 1) () "backward(op, dout)"))
	   (impl-form (method form bw-p)
	     (with-gensyms (op-bind inputs-bind)
	       (match form
		 ((list* (list* op rest) body)
		  (when bw-p (assert-bw-args rest))
		  `(defmethod ,method ((,op ,name) &rest ,inputs-bind)
		     (assert (= (length ,inputs-bind) ,(length rest)) ())
		     (multiple-value-bind (,@rest) (apply #'values ,inputs-bind) ,@body)))
		 ((list* 'cl:lambda (list* rest) body)
		  (when bw-p (assert-bw-args rest))
		  `(defmethod ,method ((,op-bind ,name) &rest ,inputs-bind)
		     (apply #'(lambda (,@rest) ,@body) ,op-bind ,inputs-bind)))
		 ((guard x (functionp x))
		  `(defmethod ,method ((,op-bind ,name) &rest ,inputs-bind)
		     (apply ,x ,op-bind ,inputs-bind)))
		 (_ (error "defmodule: ..."))))))
    `(progn
       (defclass ,name (Module) ,slots (:documentation ,documentation))
       ,(if forward
	    (impl-form 'forward forward nil)
	    `(defmethod forward ((op ,name) &rest inputs) (st ,where (inputs))))
       ,(if backward
	    (impl-form 'backward backward t))
       ,(impl-form 'impl impl nil)
       (defmethod lower ((op ,name) &rest inputs)
	 (make-graph
	  (apply #'make-node :Module (intern (symbol-name ',name) "KEYWORD")
		 (map 'list #'tensor-id (module-outputs op))
		 (map 'list #'node->id inputs) (module-attrs op))))
       (defun ,name (,@constructor-args) (make-instance ',name :attrs (list ,@attrs))))))

(defmodule (Sum ((&key (axis 0)) :axis axis) :where "A[~] -> A[~]")
    ()
    :documentation "Sum tensors along axis"
    :impl ((sum x)
	   ;; tmp
	   (!add x x)))

(defmodule (Sigmoid (()) :where "A[~] -> A[~]")
    ()
    :documentation "Implements a sigmoid function"
    :impl ((sigmoid x)
	   ;; temporary
	   (!neg (!neg x))))


;; Keep In Mind: Module Graph Levelでの最適化, aIRでimpl
#|
(defclass Sigmoid (Module) nil)
(defmethod forward ((op Sigmoid) &rest inputs) (st "A[~] -> A[~]" (inputs)))
(defmethod backward ((op Sigmoid) dout) )

(defmethod lower ((op Sigmoid) &rest inputs) (make-node :Graph :type 

							(
|#
