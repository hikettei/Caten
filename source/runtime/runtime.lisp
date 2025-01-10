(defpackage :caten/runtime/runtime
  (:documentation "
This package provides GraphRuntime, which is a class to run an air graph.
")
  (:use :cl :caten/air :caten/aasm :caten/runtime/buffer :caten/runtime/profile :caten/common.dtype)
  (:import-from :alexandria :compose)
  (:export
   #:*supress-allocate-mode*
   #:GraphRuntime
   #:runtime-graph
   #:runtime-id2tensor
   #:runtime-fw-outputs
   #:runtime-bw-outputs
   #:runtime-pc
   #:runtime-variables
   #:runtime-params
   #:runtime-renderer
   
   #:realize-graph
   #:make-runtime
   #:free-runtime
   #:runtime-gather-args
   #:runtime-setvar
   #:runtime-getvar
   #:runtime-buffer-type
   #:runtime-step
   #:realize-node
   #:runtime-forward
   #:runtime-backward
   #:parse-allocate-node
   #:parse-view-node
   #:runtime-invoke-jit-kernel
   #:runtime-error))

(in-package :caten/runtime/runtime)

(defparameter *supress-allocate-mode* nil "Set T to supress the allocation of the buffers in the realize-node. (Useful for tracing the JIT graph)")

(defclass GraphRuntime ()
  ((graph :accessor runtime-graph :initarg :graph :type Graph)
   (id2tensor :accessor runtime-id2tensor :initarg :id2tensor)
   (fw-outputs :initarg :fw-outputs :accessor runtime-fw-outputs :initform nil)
   (bw-outputs :initarg :bw-outputs :accessor runtime-bw-outputs :initform nil)
   (pc :initform 0 :accessor runtime-pc)
   (variables :initform nil :accessor runtime-variables :initarg :variables)
   (params :initform nil :accessor runtime-params :initarg :params)
   (buffer-type :initarg :buffer-type :initform 'AbstractBuffer :accessor runtime-buffer-type)
   (renderer :initarg :renderer :accessor runtime-renderer))
  (:documentation ""))

(defgeneric runtime-invoke-jit-kernel (runtime kernel-info node args))

(defgeneric realize-node (node-type runtime node args)
  (:documentation ""))

(defun make-runtime (graph &key (fw-outputs nil) (bw-outputs nil) (variables (make-hash-table)) (params nil) (id2tensor (make-hash-table)) (runtime 'GraphRuntime) (buffer-type 'AbstractBuffer) (renderer nil))
  (when (null (graph-outputs graph))
    (setf (graph-outputs graph) (append fw-outputs bw-outputs)))
  (make-instance runtime :graph graph :fw-outputs fw-outputs :bw-outputs bw-outputs :variables variables :params params :id2tensor id2tensor :buffer-type buffer-type :renderer renderer))

(defmethod free-runtime ((runtime GraphRuntime))
  "Frees all allocations in the runtime"
  (mapc #'(lambda (x) (close-buffer runtime x)) (alexandria:hash-table-values (runtime-variables runtime))))

(defmethod runtime-gather-args ((runtime GraphRuntime))
  (remove-duplicates
   (loop for node in (graph-nodes (runtime-graph runtime))
	 if (and (eql (node-type node) :Load) (getattr node :value) (symbolp (getattr node :value)))
	   collect (getattr node :value)
         else if (and (eql (node-type node) :Allocate) (getattr node :from) (symbolp (getattr node :from)))
                collect (getattr node :from))))

(defmethod runtime-setvar ((runtime GraphRuntime) var value)
  (check-type var symbol)
  (assert (or (numberp value) (find value `(t :nil)) (typep value 'AbstractBuffer)) () "Runtime: the value ~a is not a one of AbstractBuffer, number, boolean.." value)
  (setf (gethash var (runtime-variables runtime)) value))

(defmethod runtime-getvar ((runtime GraphRuntime) var)
  (check-type var symbol)
  (let ((val (gethash var (runtime-variables runtime))))
    (if (eql val :nil) nil (or val (error "Runtime: the variable ~a is not defined in the graph." var)))))

;; ~~~~ Runner ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(define-condition runtime-error ()
  ((runtime :initarg :runtime)
   (cond :initarg :cond))
  (:report
   (lambda (c s)
     (with-slots ((runtime runtime) (cond cond)) c
       (format s "Runtime-Error: Encountered the runtime error at the ~a instruction.
condition:
  ~a
disassemble:
~a"
	       (caten/common.logger:make-ordinal-suffix (runtime-pc runtime))
	       cond
	       (with-output-to-string (out)
		 (loop for nth upfrom 0
		       for node in (graph-nodes (runtime-graph runtime))
		       do (format out "~a| ~a~%"
				  (if (= nth (runtime-pc runtime))
				      (format nil "*~a " nth)
				      (format nil " ~a " nth))
				  node))))))))

(defmethod runtime-step ((runtime GraphRuntime))
  (let ((node (nth (runtime-pc runtime) (graph-nodes (runtime-graph runtime)))))
    (declare (type Node node))
    (flet ((preprocess-argument (x)
             (if (numberp x) x (runtime-getvar runtime x))))
      (when (eql (node-type node) :Pause/Backward) ;; A special node to stop the execution
        (loop for read in (node-reads node) for write in (node-writes node)
              do (runtime-setvar runtime write (runtime-getvar runtime read)))
        (return-from runtime-step))
      (let ((t1 (get-internal-real-time))
            (out (multiple-value-list
                  (handler-bind ((error #'(lambda (c) (error 'runtime-error :runtime runtime :cond c))))
                    (realize-node (node-type node) runtime node (map 'list #'preprocess-argument (node-reads node))))))
            (t2 (get-internal-real-time)))
        (assert (or (null (node-writes node)) (= (length out) (length (node-writes node)))) () "Runtime: the number of outputs does not match the number of writes.")
        (when (= 1 (ctx:getenv :PROFILE))
          (let ((sec (float (/ (- t2 t1) internal-time-units-per-second))))
            (case (node-type node)
              (:Allocate (incf *allocate-time* sec))
              (:JIT_KERNEL nil)
              (otherwise
               (incf *vm-time* sec)
               (format t "~a |    VM    | ~,6fs | ~a ~%" (render-runtime-position runtime) sec (node-type node))))))
        (loop for value in out for place in (node-writes node)
              do (runtime-setvar runtime place value))
        (incf (runtime-pc runtime))))
    t))

(defmethod runtime-forward ((runtime GraphRuntime) &aux (*jit-time* 0.0) (*vm-time* 0.0) (*allocate-time* 0.0))
  (setf (runtime-pc runtime) 0)
  ;; Two constants: T=T, NIL=:NIL (replaced as nil by the runtime-getvar function)
  (runtime-setvar runtime t t)
  (runtime-setvar runtime nil :nil)
  (start-profile)
  (flet ((finish ()
           (report-profile-result)
           (return-from runtime-forward (apply #'values (map 'list #'(lambda (x) (runtime-getvar runtime x)) (runtime-fw-outputs runtime))))))
    (loop while (< (runtime-pc runtime) (length (graph-nodes (runtime-graph runtime))))
          do (unless (runtime-step runtime) (finish)))
    (finish)))

(defmethod runtime-backward ((runtime GraphRuntime) &aux (*jit-time* 0.0) (*vm-time* 0.0) (*allocate-time* 0.0))
  (let ((current-tape (nth (runtime-pc runtime) (graph-nodes (runtime-graph runtime)))))
    (when (null current-tape) (return-from runtime-backward))
    (assert (eql (node-type current-tape) :Pause/Backward) () "runtime-backward: invaild pc counter. Before doing (backward runtime), you should run the forward pass first.")
    (incf (runtime-pc runtime))
    (start-profile)
    (loop while (< (runtime-pc runtime) (length (graph-nodes (runtime-graph runtime))))
          do (runtime-step runtime))
    (report-profile-result)
    t))

(defun realize-graph (graph &key (outs nil) (runtime 'GraphRuntime) (buffer-type 'AbstractBuffer))
  (let ((outs (or outs (node-writes (car (last (graph-nodes graph)))))))
    (runtime-forward (make-runtime graph :fw-outputs outs :runtime runtime :buffer-type buffer-type))))
;; ~~~~ print-object ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun render-list (list) (apply #'concatenate 'string (butlast (loop for n in list append (list (format nil "~a" n) ", ")))))
(defun render-comment (node)
  (case (node-type node)
    (:LOAD (format nil " // :value = ~a" (getattr node :value)))
    (:JIT_KERNEL (format nil " // JIT: ~a" (getattr node :kernel-info)))
    (otherwise "")))
(defmethod print-object ((runtime GraphRuntime) stream &aux (n-indent 4))
  (print-unreadable-object (runtime stream :type t)
    (format stream "{~(~a~) -> (~(~a~), ~(~a~))}~%" (runtime-gather-args runtime) (runtime-fw-outputs runtime) (runtime-bw-outputs runtime))
    (macrolet ((indent (n) `(with-output-to-string (out) (dotimes (i ,n) (princ " " out)))))
      (loop for node in (graph-nodes (runtime-graph runtime))
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
	      do (format stream "~a~(~a~)~a~(~a~)(~(~a~));~a~%" (indent n-indent) (render-list (node-writes node)) (if (node-writes node) " = " "")
                         (if (eql (node-type node) :JIT_KERNEL)
                             (uiop:symbol-call :caten/codegen/jit :compiled-kernel-name (getattr node :kernel-info))
                             (node-type node))
                         (render-list (node-reads node))
                         (render-comment node))))))
;; ~~~~ Helpers ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun parse-allocate-node (alloc-node args)
  "Return: (values shape stride)"
  (declare (type node alloc-node))
  (assert (eql (node-type alloc-node) :allocate))
  (let ((nrank (getattr alloc-node :nrank)))
    (values (subseq args 0 nrank) (subseq args nrank))))

(defun parse-view-node (view-node args)
  (declare (type node view-node))
  (assert (eql (node-type view-node) :view))
  (flet ((subseq1p (list from to) (subseq list (1+ from) (1+ to))))
    (let ((nrank (getattr view-node :nrank)))
      (values (subseq1p args 0 nrank) ;; shape
	      (subseq1p args nrank (* 2 nrank)) ;;view1
	      (subseq1p args (* 2 nrank) (* 3 nrank)) ;;view2
	      (subseq1p args (* 3 nrank) (* 4 nrank)) ;;view3
	      (subseq1p args (* 4 nrank) (* 5 nrank)) ;; stride
	      (getattr view-node :broadcast))))) ;; broadcast
;; ~~~~ Default Implementations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; VMOPs
;; Special treatments for :Allocate with (getattr node :from)
(defmethod realize-node :around ((node-type (eql :Allocate)) runtime node args)
  (let ((from (getattr node :from)))
    ;; [TOOD] Insert transfer-buffer if the buffers on the different devices.
    (if (or (null from) *supress-allocate-mode*)
        (call-next-method) ;; Tmp buffer allocation etc
        (typecase from
          (symbol
           (let ((val (runtime-getvar runtime from)))
             (assert (buffer-p val) () "The :from attribute in the ~a node should be a buffer or a symbol." node)
             (report-allocation runtime t (buffer-dtype val) (buffer-shape val))
             val))             
          (AbstractBuffer
           (report-allocation runtime t (buffer-dtype from) (buffer-shape from))
           from)
          (otherwise
           (error "The :from attribute in the ~a node should be a buffer or a symbol." node))))))

(defmethod realize-node ((node-id (eql :Allocate)) (runtime GraphRuntime) node args)
  (multiple-value-bind (shape stride) (parse-allocate-node node args)
    (flet ((->number (x) (if (buffer-p x) (buffer-value x) x)))
      (let ((memory-pool (getattr node :pool))) ;; the second run of :Allocation?
        (when (and (buffer-p memory-pool) shape)
          (when (equal (map 'list #'->number shape) (buffer-shape memory-pool)) ;; dynamic shape can changed the demanded size.
             (report-allocation runtime t (buffer-dtype memory-pool) (buffer-shape memory-pool))
             (return-from realize-node memory-pool))
          ;; Overwritting the old memory pool
          (when (buffer-p memory-pool) (close-buffer runtime memory-pool))))
      (let ((buffer (make-buffer (map 'list #'->number shape) (map 'list #'->number stride) (getattr node :dtype) nil :device (runtime-buffer-type runtime))))
        (open-buffer runtime buffer)
        (setf (getattr node :pool) buffer)
        (report-allocation runtime nil (buffer-dtype (getattr node :pool)) (buffer-shape (getattr node :pool)))
        (getattr node :pool)))))

(defmethod realize-node ((node-id (eql :View)) (runtime GraphRuntime) node args)
  (multiple-value-bind (shape v1 v2 v3 stride bc)
      (parse-view-node node args)
    (flet ((->number (x) (if (typep x 'abstractbuffer) (buffer-value x) x)))
      (let* ((buffer (copy-buffer (car args)))
             (scalar-to-tensor-p (and (or (typep (buffer-value buffer) 'boolean) (numberp (buffer-value buffer))) (> (getattr node :nrank) 0))))
	(setf (buffer-shape buffer) (map 'list #'->number shape)
	      (buffer-stride buffer)
              (map 'list #'->number stride)
	      (buffer-views buffer)
	      (loop for i upfrom 0 below (length v1)
		    collect (list (->number (nth i v1)) (->number (nth i v2)) (->number (nth i v3)) (nth i bc)))
	      (buffer-nrank buffer) (length shape))
        ;; Casting from scalar -> array
        (when scalar-to-tensor-p
          (let ((initial-elements (buffer-value buffer)))
            (setf (buffer-value buffer) (open-buffer runtime buffer))
            (transfer-from-array runtime buffer
                                 (make-array (apply #'* (loop for b in (getattr node :broadcast)
                                                              for s in shape
                                                              if b collect 1 else collect (->number s)))
	                                     :element-type (dtype->lisp (buffer-dtype buffer))
	                                     :initial-element initial-elements))))
	buffer))))
;; ~~~ Iterator Utils ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun index-components ())
(defun map-into/buffer (result op &rest buffers)
  (declare (type AbstractBuffer result)
	   (type function op)
	   (type list buffers))
  (when (eql op #'index-components)
    (assert (not (eql (buffer-dtype (car buffers)) :bool))
	    ()
	    "Assertion Failed: IndexComponents(x: Bool) is not supported now."))
  (assert (every #'(lambda (x) (or (= (buffer-nrank x) 0) (= (buffer-nrank x) (buffer-nrank result)))) buffers)
	  ()
	  "Assertion Failed: All buffers should have the same rank, getting ~a." (map 'list #'buffer-nrank buffers))
  (let* ((nrank (buffer-nrank (car buffers)))
	 (index-components-p (eql op #'index-components))
	 (index-components (when index-components-p (coerce 0 (dtype->lisp (buffer-dtype (car buffers))))))
	 (offsets (make-list (1+ (length buffers)) :initial-element 0)))
    (labels ((bref (buffer idx)
	       (if (= (buffer-nrank buffer) 0)
		   (buffer-value buffer)
                   (if (>= idx 0)
		       (aref (buffer-value buffer) idx)
                       nil))) ;; prevent to aref it because !padding may expect this behaviour
	     (explore (dim offsets)
               ;; TODO(hikettei): Always use (nth dim (buffer-shape result)), currently caten/avm.test have an old %view.
	       (let ((size (if (some (compose #'identity #'(lambda (x) (nth dim x)) #'buffer-views) buffers)
			       (let ((view (nth dim (buffer-views (find-if (compose #'identity #'(lambda (x) (nth dim x)) #'buffer-views) buffers)))))
                                 (/ (- (second view) (car view)) (third view)))
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
					 nil
					 (incf (nth n offsets) (* (third view) stride)))
				else if stride do (incf (nth n offsets) stride))))))
      (explore 0 offsets))))
;; ~~~ Helpers ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun column-major-calc-strides (shape)
  (declare (type list shape))
  (let* ((num-dims (length shape))
         (strides (make-list num-dims :initial-element 1)))
    (loop for i from 1 to (- num-dims 1) do
      (setf (nth i strides) (* (nth (- i 1) strides) (nth (- i 1) shape))))
    strides))

(defun row-major-calc-strides (shape)
  (declare (type list shape))
  (let* ((num-dims (length shape))
         (strides (make-list num-dims :initial-element 1)))
    (loop for i downfrom (- num-dims 2) to 0 do
      (setf (nth i strides) (* (nth (+ i 1) strides) (nth (+ i 1) shape))))
    strides))

(defun compute-strides (shape)
  (ecase *default-order*
    (:row (row-major-calc-strides shape))
    (:column (column-major-calc-strides shape))))

(defun make-contiguous-buffer (runtime base-buffer)
  (setf (buffer-stride base-buffer) (compute-strides (buffer-shape base-buffer))
        (buffer-views base-buffer) (loop for i upfrom 0 below (buffer-nrank base-buffer) collect nil))
  (close-buffer runtime base-buffer)
  (open-buffer runtime base-buffer)
  base-buffer)
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun map-view (runtime reduction-p op &rest buffers)
  "Note: In a special case where op is #'index-components, map-view writes (car buffer) <- index-component."
  (let ((out (copy-buffer (car buffers))))
    (if (= 0 (buffer-nrank (car buffers)))
	(setf (buffer-value out) (apply op (map 'list #'buffer-value buffers)))
	(if reduction-p
	    (progn
              (setf (buffer-value out) (copy-seq (buffer-value out)))
	      (apply #'map-into/buffer out op `(,out ,@(cdr buffers)))
              (let ((base-elms (buffer-value (car buffers))))
                (dotimes (i (array-total-size base-elms)) ;; synchronize the reduction to the original buffer.
                  (setf (aref base-elms i) (aref (buffer-value out) i)))))
            (progn
              ;; [TODO] BACKEND=LISP specific so we should only consider for the case where BACKEND=LISP
              ;; If not reduced and the `out` is broadcasted?
              ;; In that case the output tensor should be a contiguous. (e.g: out[10, 10] = x[1] + y[10, 10])
              (if (and (some #'identity (buffer-views out))
                       (arrayp (buffer-value out))
                       ;; But when all buffers have the same sized array -> no need to make contiguous.
                       (some #'(lambda (x) (when (arrayp (buffer-value x)) (> (array-total-size (buffer-value x)) (array-total-size (buffer-value out))))) (cdr buffers)))
                  (setf out (make-contiguous-buffer runtime out)) ;; Initializing a new contiguous array for the output
                  (setf (buffer-value out) (copy-seq (buffer-value out))))
	      (apply #'map-into/buffer out op buffers))))
    out))
;; ~~~~ Implementations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun reveal-buffer (obj) (if (buffer-p obj) (buffer-value obj) obj))
(defmethod realize-node ((node-id (eql :Load)) (runtime GraphRuntime) node args)
  (let* ((tgt (car args))
	 (val (getattr node :value))
	 (val (reveal-buffer (if (numberp val) val (runtime-getvar runtime val))))
	 (val (dtype/cast val (buffer-dtype tgt))))
    (if (= (buffer-nrank (car args)) 0)
	(let ((out (copy-buffer tgt)))
	  (setf (buffer-value out) val)
	  out)
	(map-view runtime nil #'(lambda (x) x val) (car args)))))

(defmethod realize-node ((node-id (eql :Store)) (runtime GraphRuntime) node args)
  (let ((to (copy-buffer (car args))))
    (setf (buffer-value to) (buffer-value (second args)))
    to))

(defmethod realize-node ((node-id (eql :Index-Components)) (runtime GraphRuntime) node args)
  (map-view runtime nil #'index-components (car args)))

(defun wrap-around (x max min)
  (if (= min 0)
      (mod x (1+ max))
      ;; [TODO] Wrap-around for the underflow?
      (mod x (1+ max))))

(macrolet ((impl (kw op)
             `(defmethod realize-node ((node-id (eql ,kw)) (runtime GraphRuntime) node args)
                (let ((min (dtype/min (buffer-dtype (car args))))
                      (max (dtype/max (buffer-dtype (car args))))
                      (wrap-around (getattr node :wrap-around :allow-undefined t)))
                  (declare (ignorable min max wrap-around))
                  (apply #'map-view runtime (getattr node :reduction :allow-undefined t) ,op args)))))
  (impl :add #'(lambda (&rest args &aux (out (apply #'+ args))) (if wrap-around (wrap-around out max min) out)))
  (impl :mul #'(lambda (&rest args &aux (out (apply #'* args))) (if wrap-around (wrap-around out max min) out)))
  (impl :idiv #'floor)
  (impl :mod #'mod)
  (impl :move #'(lambda (x y) x y))
  (impl :and #'(lambda (x y) (if (and (numberp x) (numberp y)) (logand x y) (and x y))))
  (impl :or #'(lambda (x y) (if (and (numberp x) (numberp y)) (logior x y) (or x y))))
  (impl :xor #'(lambda (x y) (if (and (numberp x) (numberp y)) (logxor x y) (alexandria:xor x y))))
  (impl :gcd #'gcd)
  (impl :max #'max)  
  (impl :sqrt #'sqrt)
  (impl :neg #'-)
  (impl :recip #'/)
  (impl :SIN #'sin)
  (impl :EXP2 #'(lambda (x) (expt 2 x)))
  (impl :LOG2 #'(lambda (x) (log x 2)))
  (impl :not #'(lambda (x) (if (numberp x) (lognot x) (not x))))
  (impl :cast #'(lambda (m x)
		  (declare (ignore m))
		  (dtype/cast x (getattr node :dtype))))
  (impl :!= #'(lambda (_ x y) _ (not (= x y)))) ;; input is a boolean
  (impl :< #'(lambda (_ x y) _ (< x y))))

(defmethod realize-node ((node-id (eql :Where)) (runtime GraphRuntime) node args)
  (map-view runtime (getattr node :reduction :allow-undefined t) #'(lambda (x c y) (if c x y)) (nth 1 args) (nth 0 args) (nth 2 args)))
