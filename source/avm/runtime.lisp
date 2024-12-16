(in-package :caten/avm)
(defparameter *vm* nil)
(defgeneric %impl (device op graph node args) (:documentation "Peforms the corresponding nodes"))

(defun make-hash-table-from-params (params)
  (declare (type list params))
  (let ((out (make-hash-table :test #'eql)))
    (loop for (k . v) in params
	  do (setf (gethash k out) v))
    ;; Constants
    (setf (gethash t out) t
	  (gethash nil out) :nil)
    out))
(defstruct (AVM
	    (:constructor make-avm (graph name id2tensor fw-outputs bw-outputs &optional params (dumped nil)
                                          &aux
                                          (id2tensor (or id2tensor (make-hash-table)))
                                            (_
                                             (when (null (graph-outputs graph))
                                               (setf (graph-outputs graph) (append fw-outputs bw-outputs)))))))
  "Tape based iseq executor"
  (graph graph :type graph)
  (name name :type keyword)
  (fw-outputs fw-outputs :type list)
  (bw-outputs bw-outputs :type list)
  (id2tensor id2tensor :type hash-table)
  (tape-length (length (graph-nodes graph)) :type fixnum)
  (pc 0 :type fixnum)
  (variables (make-hash-table-from-params params) :type hash-table)
  (params-to-optimize nil :type list)
  (dumped dumped :type boolean)) ;; If dumped: CLOS objects are removed which is unnecessary to reconstruct the executable graph.

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
    nil t))

(defun deepcopy-avm (avm &aux (avm (copy-avm avm)))
  (declare (type avm avm))
  (setf (avm-graph avm) (copy-graph (avm-graph avm)))
  (setf (graph-nodes (avm-graph avm)) (map 'list #'copy-node (graph-nodes (avm-graph avm))))
  avm)

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
