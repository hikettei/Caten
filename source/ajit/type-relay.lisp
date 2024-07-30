(in-package :caten/ajit)

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
	(setf (buffer-value buffer) (make-fakearray (buffer-shape buffer) (buffer-dtype buffer) initial-value)))))

(defmethod %impl :around ((device-id (eql :relay-checker)) op graph node args)
  ;; TODO: Record outputs to hash-table
  ;; NODE_ID <-> FakeAray corresponds one-by-one
  ;; using such that information, we can schedule the graph
  )

(defmethod %impl ((device-id (eql :relay-checker)) (op (eql :Allocate)) graph node args)
  (multiple-value-bind (shape stride) (parse-allocate-node node args)
    (realize-buffer graph (node->id node) :shape1 shape :stride1 stride)))

(defmethod %impl ((device-id (eql :relay-checker)) (op (eql :Load)) graph node args)
  (let* ((tgt (car args))
	 (val (getattr node :value)))
    (if (= (buffer-nrank (car args)) 0)
	(let ((out (copy-buffer tgt)))
	  (setf (buffer-value out) val)
	  out)
	;; [TODO] map-view
	)))


(defmethod %impl ((device-id (eql :relay-checker)) (op (eql :store)) graph node args)
  
  )

(defmethod %impl ((device-id (eql :relay-checker)) (op (eql :Index-Components)) graph node args)

  )

(macrolet ((impl (kw)  `(defmethod %impl ((device-id (eql :relay-checker)) (op (eql ,kw)) graph node args) (car args))))
  (impl :add)  
  )

(defmethod %impl ((device-id (eql :relay-checker)) (op (eql :view)) graph node args)

  )
