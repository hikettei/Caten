(in-package :caten/ajit)
;; Perform the following inference given the graph by direcly running it in :relay-checker VM.
;; - Shape/View/Stride Information
;; - (Plus) Permute information.
;;   - The idea of "permute" and permutation inference is an afterthought.
;;     - because there were no plans to implement Permute in the initial JIT.
;;     - The initial JIT implementation plan was to treat all internal Tensors as one-dimensional, but now they are multidimensional in order to express the accesing relations in an affine function for symbolic compilation.
;;     - This is a trade-off, Symbolic Compilation is possible instead of being able to hack the stride by rewriting it to whatever value you want. Functions that should be implemented with the former should be implemented by introducing another API.
;;   - Plus, we refer to "permute" as:
;;     - How the original shape/view/stride was shuffled to obtain the current shape/view/stride.
;;     - Therefore, when shuffling the shape/stride/view in the aIR level, you must to add :permute attribute in the VIEW node.
;;     - We have no plan for refactoring this, as "permute inference" is a still simple solution, and arrays are one-dimensional anyway when rendering.
(defparameter *type-reporter* nil)
(defstruct (Type-Reporter
	    (:conc-name rp-)
	    (:constructor make-type-reporter ()))
  (id2buffer (make-hash-table :test #'eql))
  (seen nil :type list))

(defun map/type-of (type-reporter id)
  ;; Return: Buffer or number
  (declare (type type-reporter type-reporter)
	   (type (or number symbol) id))
  (if (numberp id)
      id
      (or (gethash id (rp-id2buffer type-reporter)) (error "map/type-of: ~a cannot be inferred from the graph" id))))

(defstruct (FakeArray
	    (:constructor make-fakearray (shape dtype initial-element)))
  (shape shape :type list)
  (dtype dtype :type dtype-t)
  (initial-element initial-element))

(defmethod %vm/allocate-buffer ((device-id (eql :relay-checker)) buffer)
  (let ((initial-value (if (eql (buffer-dtype buffer) :bool)
			   nil
			   (coerce 0 (dtype->lisp (buffer-dtype buffer))))))
    (if (= (buffer-nrank buffer) 0)
	(setf (buffer-value buffer) (make-fakearray nil (buffer-dtype buffer) initial-value))
	(setf (buffer-value buffer) (make-fakearray (buffer-shape buffer) (buffer-dtype buffer) initial-value)))
    buffer))

(defmethod %impl :around ((device-id (eql :relay-checker)) op graph node args)
  (let ((out-buffer (multiple-value-list (if (next-method-p) (call-next-method) (car args)))))
    (when *type-reporter*
      (loop for n in (node-writes node)
	    for o in out-buffer
	    do (assert (and (buffer-p o) (fakearray-p (buffer-value o)))
		       ()
		       "relay-checker: ~a should return a buffer whose value is fake-array, but got ~a" op o)
	       (setf (gethash n (rp-id2buffer *type-reporter*)) o)))
    (apply #'values out-buffer)))
(defmethod %impl ((device-id (eql :relay-checker)) op graph node args)
  (if (next-method-p)
      (call-next-method)
      (let ((buff (make-buffer (buffer-nrank (car args)) (buffer-shape (car args)) (buffer-stride (car args)) (buffer-dtype (car args)) (buffer-views (car args)))))
	(setf (buffer-value buff) (make-fakearray (buffer-shape buff) (buffer-dtype buff) (car (node-writes node)))
	      (buffer-inferred-permute buff) (buffer-inferred-permute (car args)) ;; (buffer-inferred-permute buff)
	      (buffer-orig-buffer-shape buff) (buffer-orig-buffer-shape (car args)))
	buff)))
(defmethod %impl ((device-id (eql :relay-checker)) (op (eql :Allocate)) graph node args)
  (multiple-value-bind (shape stride) (parse-allocate-node node args)
    (realize-buffer graph (node->id node) :shape1 shape :stride1 stride)))
;; The same algorithm in function.lisp (class Permute)
(defun permute-list (order list) (loop for nth in order collect (nth nth list)))
(defmethod %impl ((device-id (eql :relay-checker)) (op (eql :view)) graph node args)
  (multiple-value-bind (shape v1 v2 v3 stride bc)
      (parse-view-node node args)
    (let ((buffer (copy-buffer (car args))))
      (setf (buffer-shape buffer) (map 'list #'reveal-buffer shape)
	    (buffer-stride buffer) (map 'list #'reveal-buffer stride)
	    (buffer-views buffer)
	    (loop for i upfrom 0 below (length v1)
		  collect (list (reveal-buffer (nth i v1)) (reveal-buffer (nth i v2)) (reveal-buffer (nth i v3)) (nth i bc)))
	    (buffer-nrank buffer) (length shape)
	    (buffer-inferred-permute buffer) (if (and (buffer-inferred-permute buffer) (getattr node :permute))
						 (permute-list (getattr node :permute) (buffer-inferred-permute buffer))
						 (or
						  (buffer-inferred-permute buffer)
						  (getattr node :permute)))
	    (buffer-orig-buffer-shape buffer) (map 'list #'reveal-buffer (or (buffer-orig-buffer-shape (car args)) (buffer-shape (car args)))))
      buffer)))

(defmethod %impl ((device-id (eql :relay-checker)) (op (eql :Load)) graph node args)
  (let* ((tgt (car args))
	 (val (getattr node :value)))
    (let ((out (copy-buffer tgt)))
      (setf (buffer-value out) (make-fakearray nil (buffer-dtype out) val))
      out)))

(defmethod %impl ((device-id (eql :relay-checker)) (op (eql :WHERE)) graph node args)
  (let ((buff (copy-buffer (second args))))
    (setf (buffer-value buff) (make-fakearray (buffer-shape buff) (buffer-dtype buff) (car (node-writes node))))
    buff))

(declaim (ftype (function (AVM) Type-Reporter) run-type-infer))
(defun run-type-infer (avm)
  "Run the shape-inference given AVM, returning Type-Reporter"
  (declare (type avm avm))
  (let ((*device* :relay-checker) (*type-reporter* (make-type-reporter)))
    (setf (avm-tape-length avm) (length (graph-nodes (avm-graph avm))))
    (vm/forward avm) (vm/backward avm)
    *type-reporter*))

(defstruct (Inferred-Type
	    (:conc-name relay-)
	    (:constructor make-inferred-type (reads writes)))
  (reads reads :type list)
  (writes writes :type list))

(defmethod print-object ((type Inferred-type) stream)
  (format stream "<OK>"))

(defun read-type-relay (node)
  (declare (type node node))
  (or (getattr node :_type_relay) (error "Failed to infer the type of ~a" node)))

(defun deploy-type-infer-results (avm type-map &key (allow-overwrite nil))
  "Writes the result of type-infer to :_type_relay"
  (flet ((->type (id) (when (symbolp id) (map/type-of type-map id))))
    (loop for n in (graph-nodes (avm-graph avm)) do
      (let ((type (make-inferred-type
		   (map 'list #'->type (node-reads n))
		   (map 'list #'->type (node-writes n)))))
	(when (null allow-overwrite)
	  (assert (null (getattr n :_type_relay)) () ":_type_relay should be a nil!~%%safely-purge-views-from-graph was previously applied?~%- do not override the attr :_type_relay."))
	(when (null (getattr n :_type_relay))
	  (setf (getattr n :_type_relay) type))))))
