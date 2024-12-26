(in-package :caten/avm)

(defparameter *vm* nil "Binds to the AVM object currently running.")

(defclass AVM ()
  ((graph :initarg :graph :accessor avm-graph)
   (name :initarg :name :accessor avm-name)
   (fw-outputs :initarg :fw-outputs :accessor avm-fw-outputs)
   (bw-outputs :initarg :bw-outputs :accessor avm-bw-outputs)
   (id2tensor :initarg :id2tensor :accessor avm-id2tensor) ;; Node ID -> Tensor
   (tape-length :initarg :tape-length :accessor avm-tape-length)
   (pc :initform 0 :accessor avm-pc)
   (variables :initform (make-hash-table) :initarg :variables :accessor avm-variables)
   (params-to-optimize :initform nil :accessor avm-params-to-optimize)
   (dumped :initform nil :initarg :dumped :accessor avm-dumped)
   (device :initform 'AVM :accessor avm-device)))

(defclass Relay-Checker (AVM) nil)

(defun make-avm (graph name id2tensor fw-outputs bw-outputs &optional params (dumped nil) (device 'AVM))
  (when (null (graph-outputs graph))
    (setf (graph-outputs graph) (append fw-outputs bw-outputs)))
  (make-instance
   device
   :graph graph :name name :id2tensor (or id2tensor (make-hash-table))
   :fw-outputs fw-outputs :bw-outputs bw-outputs
   :tape-length (length (graph-nodes graph))
   :variables (make-hash-table-from-params params)
   :dumped dumped))

(defmethod avm-gather-args ((avm avm))
  (remove-duplicates
   (loop for node in (graph-nodes (avm-graph avm))
	 if (and (eql (node-type node) :Load) (getattr node :value) (symbolp (getattr node :value)))
	   collect (getattr node :value)
         else if (and (eql (node-type node) :Allocate) (getattr node :from) (symbolp (getattr node :from)))
                collect (getattr node :from))))

(defmethod print-object ((avm AVM) stream &aux (n-indent 4))
  (print-unreadable-object (avm stream :type t)
    (format stream "{~a: ~(~a~) -> (~(~a~), ~(~a~))}~%" (avm-name avm) (avm-gather-args avm) (avm-fw-outputs avm) (avm-bw-outputs avm))
    (macrolet ((indent (n) `(with-output-to-string (out) (dotimes (i ,n) (princ " " out)))))
      (loop for node in (graph-nodes (avm-graph avm))
            if (eql (Node-type node) :Allocate) do
              (let ((nrank (getattr node :nrank)))
                (format stream "~a~(~a~) = ~(~a~)(shape=(~a), stride=(~a)~a);~%" (indent n-indent) (render-list (node-writes node))
			(node-type node) (render-list (subseq (node-reads node) 0 nrank)) (render-list (subseq (node-reads node) nrank))
                        (if (getattr node :from)
                            (if (symbolp (getattr node :from)) (format nil ", from=~a" (getattr node :from)) (format nil ", from=<Realized Buffer>"))
                            "")))
 	    else if (eql (Node-type node) :View) do
	      (let ((nrank (getattr node :nrank)))
		(flet ((subseq1p (x y z) (subseq x (1+ y) (1+ z))))
		  (format stream "~a~(~a~) = ~(~a~)(~(~a~), shape=(~a), views=(~a), stride=(~a)~a);~%"
			  (indent n-indent)
			  (render-list (node-writes node))
			  (node-type node)
			  (car (node-reads node))
			  (render-list (subseq1p (node-reads node) 0 nrank))
			  (let ((upfrom (subseq1p (node-reads node) nrank (* 2 nrank)))
				(below (subseq1p (node-reads node) (* 2 nrank) (* 3 nrank)))
				(by (subseq1p (node-reads node) (* 3 nrank) (* 4 nrank)))
				(bc (getattr node :broadcast)))
			    (render-list
			     (map 'list #'(lambda (x y z l) (format nil "(~a)" (render-list (list x y z l)))) upfrom below by bc)))
			  (render-list (subseq1p (node-reads node) (* 4 nrank) (* 5 nrank)))
			  (if (getattr node :permute)
			      (format nil ", permute=~a" (getattr node :permute))
			      ""))))
	    else
	      do (format stream "~a~(~a~)~a~(~a~)(~(~a~));~%" (indent n-indent) (render-list (node-writes node)) (if (node-writes node) " = " "")
                         (if (eql (node-type node) :JIT_KERNEL)
                             (uiop:symbol-call :caten/codegen/jit :compiled-kernel-name (getattr node :kernel-info))
                             (node-type node))
                         (render-list (node-reads node)))))))
;; [TODO] Remove AOT
(defmethod make-load-form ((avm AVM) &optional env)
  (declare (ignore env))
  `(make-avm
    (let ((graph (make-graph ,@(map 'list #'identity (graph-nodes (avm-graph avm))))))
      (setf (graph-seen graph) ',(graph-seen (avm-graph avm))
	    (graph-outputs graph) ',(graph-outputs (avm-graph avm)))
      graph)
    ,(avm-name avm)
    ,(avm-id2tensor avm)
    ',(avm-fw-outputs avm)
    ',(avm-bw-outputs avm)
    nil t ',(avm-device avm)))

(defmethod avm-reset ((avm AVM))
  (setf (avm-pc avm) 0
        (avm-tape-length avm) (length (graph-nodes (avm-graph avm)))
        (avm-variables avm) (make-hash-table)))

(defun vm/readvar (avm id)
  (declare (type avm avm)
	   (type symbol id))
  (let ((out (gethash id (avm-variables avm))))
    (if (eql out :nil)
	nil
	(or out (error "AVM Runtime Error: ~a is not defined in ~a" id avm)))))

(defun vm/setvar (avm id value)
  (declare (type avm avm)
	   (type symbol id)
	   (type Buffer value))
  (setf (gethash id (avm-variables avm)) value))

(defun vm/step (avm &aux (*vm* avm))
  (declare (type avm avm))
  (let ((node (nth (avm-pc avm) (graph-nodes (avm-graph avm)))))
    (declare (type node node))
    (flet ((->real (x)
	     (if (symbolp x)
		 (vm/readvar avm x)
		 x)))
      (let ((type   (node-type node))
	    (writes (node-writes node))
	    (reads  (node-reads node)))
	(when (eql type :Pause/backward)
	  (loop for read in reads
		for write in writes
		do (vm/setvar avm write (vm/readvar avm read)))
	  (return-from vm/step))	  
        (let ((t1 (get-internal-real-time))
              (out (multiple-value-list
		    (handler-bind ((error #'(lambda (cond) (error 'avm-runtime-error :avm avm :cond cond))))
		      (%impl *device* type (avm-graph avm) node (map 'list #'->real reads)))))
              (t2 (get-internal-real-time)))
	  (assert (or (null writes) (= (length out) (length writes))) () "The length of output ~a does not match ~a" out node)
          (when (= 1 (ctx:getenv :PROFILE))
            (let ((sec (float (/ (- t2 t1) internal-time-units-per-second))))
              (case (node-type node)
                (:Allocate (incf *allocate-time* sec))
                (:JIT_KERNEL nil)
                (otherwise
                 (incf *vm-time* sec)
                 (format t "~a |    VM    | ~,6fs | ~a ~%" (render-avm-position avm) sec (node-type node))))))
	  (loop for real in out
		for place in writes
		do (vm/setvar avm place real))
	  ;; Move to the next tape
	  (incf (avm-pc avm))))))
  t)

(defun vm/forward (avm &aux (*jit-time* 0.0) (*vm-time* 0.0) (*allocate-time* 0.0))
  (declare (type avm avm))
  (setf (avm-pc avm) 0)
  (start-profile)
  (flet ((finish ()
           (report-profile-result)
	   (return-from vm/forward (apply #'values (map 'list #'(lambda (x) (vm/readvar avm x)) (avm-fw-outputs avm))))))
    (loop while (< (avm-pc avm) (avm-tape-length avm)) do
      (unless (vm/step avm) (finish)))
    (finish)))

(defun vm/backward (avm &aux (*jit-time* 0.0) (*vm-time* 0.0) (*allocate-time* 0.0))
  (declare (type avm avm))
  (let ((current-tape (nth (avm-pc avm) (graph-nodes (avm-graph avm)))))
    (when (null current-tape) (return-from vm/backward))
    (assert (eql (node-type current-tape) :Pause/backward) ()
	    "vm/backward: invaild pc counter. Before doing (backward avm), you should run the forward pass first.")
    (incf (avm-pc avm)))
  (start-profile)
  (loop while (< (avm-pc avm) (avm-tape-length avm)) do (vm/step avm))
  (report-profile-result)
  t)

(defun vm/set-params (avm params)
  (loop for (k . v) in params
	do (assert (and (symbolp k) (or (buffer-p v) (numberp v)))
		   ()
		   "vm/set-params: Invaild params (~a . ~a)~%Params should be a cons of (symbol . number) or (symbol . buffer)." k v)
	   (setf (gethash k (avm-variables avm)) v)))

(defun %realize (graph)
  (declare (type graph graph))
  (let ((out (node-writes (car (last (graph-nodes graph))))))
    (vm/forward (make-avm graph :test nil out nil))))
