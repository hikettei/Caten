(in-package :caten/avm)
;; TODO: magicl backend
(defmethod %vm/allocate-buffer ((device-id (eql :lisp)) buffer)
  (let ((initial-value (if (eql (buffer-dtype buffer) :bool)
			   nil
			   (coerce 0 (dtype->lisp (buffer-dtype buffer))))))
    (if (= (buffer-nrank buffer) 0)
	(setf (buffer-value buffer) initial-value)
	(setf (buffer-value buffer)
	      (make-array (apply #'* (buffer-shape buffer))
			  :element-type (dtype->lisp (buffer-dtype buffer))
			  :initial-element initial-value)))
    buffer))

(defmethod %vm/read-index ((device-id (eql :lisp)) buffer nth) (aref (buffer-value buffer) nth))
(defun index-components ())
(defun map-into/buffer (result op &rest buffers)
  (declare (type buffer result)
	   (type function op)
	   (type list buffers))
  (when (eql op #'index-components)
    (assert (not (eql (buffer-dtype (car buffers)) :bool))
	    ()
	    "Assertion Failed: IndexComponents(x: Bool) is not supported now."))
  (assert (every #'(lambda (x) (or (= (buffer-nrank x) 0)  (= (buffer-nrank x) (buffer-nrank result)))) buffers)
	  ()
	  "Assertion Failed: All buffers should have the same rank, getting ~a." (map 'list #'buffer-nrank buffers))
  (let* ((nrank (buffer-nrank (car buffers)))
	 (index-components-p (eql op #'index-components))
	 (index-components (when index-components-p (coerce 0 (dtype->lisp (buffer-dtype (car buffers))))))
	 (offsets (make-list (1+ (length buffers)) :initial-element 0)))
    (labels ((bref (buffer idx)
	       (if (= (buffer-nrank buffer) 0)
		   (buffer-value buffer)
		   (aref (buffer-value buffer) idx)))
	     (explore (dim offsets)
	       (let ((size (if (some (compose #'identity #'(lambda (x) (nth dim x)) #'buffer-views) buffers)
			       (let ((view (nth dim (buffer-views (find-if (compose #'identity #'(lambda (x) (nth dim x)) #'buffer-views) buffers)))))
				 (abs (/ (- (car view) (second view)) (third view))))
			       (nth dim (buffer-shape result)))))
		 ;; initial offset
		 (loop for n upfrom 0
		       for buff in `(,result ,@buffers)
		       for view = (nth dim (buffer-views buff))
		       if view
			 do (incf (nth n offsets) (* (nth dim (buffer-stride buff)) (car view))))
		 (loop for n upfrom 0 below size
		       do (if (= (1+ dim) nrank)
			      (if index-components-p
				  (progn
				    (setf (aref (buffer-value result) (car offsets)) index-components)
				    (incf index-components))
				  (if (= (buffer-nrank result) 0)
				      (setf (buffer-value result) (apply op (map 'list #'bref buffers (cdr offsets))))
				      (setf (aref (buffer-value result) (car offsets))
					    (apply op (map 'list #'bref buffers (cdr offsets))))))
			      (explore (1+ dim) (copy-list offsets)))
			  (loop for n upfrom 0
				for buff in `(,result ,@buffers)
				for view = (nth dim (buffer-views buff))
				for stride = (nth dim (buffer-stride buff))
				if (and stride view)
				  do (if (fourth view)
					 nil ;; broadcast
					 (incf (nth n offsets) (* (third view) stride)))
				else if stride do (incf (nth n offsets) stride))))))
      (explore 0 offsets))))

(defun map-view (reduction-p op &rest buffers)
  "Note: In a special case where op is #'index-components, map-view writes (car buffer) <- index-component."
  (let ((out (copy-buffer (car buffers))))
    (if (= 0 (buffer-nrank (car buffers)))
	(setf (buffer-value out) (apply op (map 'list #'buffer-value buffers)))
	(progn
	  (setf (buffer-value out) (copy-seq (buffer-value out)))
	  (if reduction-p
	      (apply #'map-into/buffer out op `(,out ,@(cdr buffers)))
	      (apply #'map-into/buffer out op buffers))))
    out))

(defmethod %impl ((device-id (eql :lisp)) (op (eql :Allocate)) graph node args)
  (multiple-value-bind (shape stride)
      (parse-allocate-node node args)
    (flet ((->number (x) (if (buffer-p x) (buffer-value x) x)))
      (realize-buffer graph (node->id node)
		      :shape1 (map 'list #'->number shape)
		      :stride1 (map 'list #'->number stride)))))

(defmethod %impl ((device-id (eql :lisp)) (op (eql :Load)) graph node args)
  (let* ((tgt (car args))
	 (val (getattr node :value))
	 (val (reveal-buffer (if (numberp val) val (vm/readvar *vm* val))))
	 (val (dtype/cast val (buffer-dtype tgt))))
    (if (= (buffer-nrank (car args)) 0)
	(let ((out (copy-buffer tgt)))
	  (setf (buffer-value out) val)
	  out)
	(map-view nil #'(lambda (x) x val) (car args)))))

(defmethod %impl ((device-id (eql :lisp)) (op (eql :store)) graph node args)
  (let* ((to (copy-buffer (car args))))
    (setf (buffer-value to) (buffer-value (second args)))
    to))

(defmethod %impl ((device-id (eql :lisp)) (op (eql :Index-Components)) graph node args)
  (map-view nil #'index-components (car args)))

(macrolet ((impl (kw op)
	     `(defmethod %impl ((device-id (eql :lisp)) (op (eql ,kw)) graph node args &aux (max (caten/common.dtype:dtype/max (buffer-dtype (car args)))) (wrap-around (getattr node :wrap-around)))
		(declare (ignorable max wrap-around))
		(apply #'map-view (getattr node :reduction) ,op args))))
  (impl :add #'(lambda (&rest args &aux (out (apply #'+ args))) (if wrap-around (mod out max) out)))
  (impl :mul #'(lambda (&rest args &aux (out (apply #'* args))) (if wrap-around (mod out max) out)))
  (impl :move #'(lambda (x y) x y))
  (impl :and #'(lambda (x y) (if (and (numberp x) (numberp y)) (logand x y) (and x y))))
  (impl :or #'(lambda (x y) (if (and (numberp x) (numberp y)) (logior x y) (or x y))))
  (impl :xor #'(lambda (x y) (if (and (numberp x) (numberp y)) (logxor x y) (xor x y))))
  (impl :gcd #'gcd)
  (impl :max #'max)  
  (impl :sqrt  #'sqrt)
  (impl :neg   #'-)
  (impl :recip #'/)
  (impl :SIN #'sin)
  (impl :EXP2 #'(lambda (x) (expt 2 x)))
  (impl :LOG2 #'(lambda (x) (log x 2)))
  (impl :not #'(lambda (x) (if (numberp x) (lognot x) (not x))))
  (impl :cast #'(lambda (m x)
		  (declare (ignore m))
		  (dtype/cast x (getattr node :dtype))))
  (impl :NEQ #'(lambda (_ x y) _ (not (= x y)))) ;; input is a boolean
  (impl :LT #'(lambda (_ x y) _ (< x y)))
  (impl :WHERE #'(lambda (c x y) (if c x y))))

(defmethod %impl ((device-id (eql :lisp)) (op (eql :view)) graph node args)
  (multiple-value-bind (shape v1 v2 v3 stride bc)
      (parse-view-node node args)
    (flet ((->number (x) (if (buffer-p x) (buffer-value x) x)))
      (let ((buffer (copy-buffer (car args))))
	(setf (buffer-shape buffer) (map 'list #'->number shape)
	      (buffer-stride buffer) (map 'list #'->number stride)
	      (buffer-views buffer)
	      (loop for i upfrom 0 below (length v1)
		    collect (list (->number (nth i v1)) (->number (nth i v2)) (->number (nth i v3)) (nth i bc)))
	      (buffer-nrank buffer) (length shape))
	buffer))))
