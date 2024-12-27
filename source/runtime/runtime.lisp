(defpackage :caten/runtime/runtime
  (:documentation "
This package provides GraphRuntime, which is a class to run an air graph.
")
  (:use :cl :caten/air :caten/aasm :caten/runtime/buffer :caten/runtime/profile)
  (:export
   #:*supress-allocate-mode*
   #:GraphRuntime
   #:make-runtime
   #:free-runtime
   #:runtime-gather-args
   #:runtime-setvar
   #:runtime-getvar
   #:runtime-step
   #:realize-node
   ))

(in-package :caten/runtime/runtime)

(defparameter *supress-allocate-mode* nil "Set T to supress the allocation of the buffers in the realize-node. (Useful for tracing the JIT graph)")

(defclass GraphRuntime ()
  ((graph :accessor runtime-graph :type Graph)
   (fw-outputs :initarg :fw-outputs :accessor runtime-fw-outputs :initform nil)
   (bw-outputs :initarg :bw-outputs :accessor runtime-bw-outputs :initform nil)
   (pc :initform 0 :accessor runtime-pc)
   (variables :initform nil :accessor runtime-variables)
   (params :initform nil :accessor runtime-params))
  (:documentation ""))

(defgeneric realize-node (node-type runtime node args)
  (:documentation ""))

(defun make-runtime (graph &key (fw-outputs nil) (bw-outputs nil) (variables (make-hash-table)) (params (make-hash-table)) (runtime 'GraphRuntime))
  (make-instance runtime :graph graph :fw-outputs fw-outputs :bw-outputs bw-outputs :variables variables :params params))

(defmethod free-runtime ((runtime GraphRuntime))
  "Frees all allocations in the runtime"
  (mapc #'close-buffer (alexandria:hash-table-values (runtime-variables runtime))))

(defmethod runtime-gather-args ((runtime GraphRuntime))
  (remove-duplicates
   (loop for node in (graph-nodes (runtime-graph runtime))
	 if (and (eql (node-type node) :Load) (getattr node :value) (symbolp (getattr node :value)))
	   collect (getattr node :value)
         else if (and (eql (node-type node) :Allocate) (getattr node :from) (symbolp (getattr node :from)))
                collect (getattr node :from))))

(defmethod runtime-setvar ((runtime GraphRuntime) var value)
  (check-type var symbol)
  (assert (typep value 'AbstractBuffer) () "Runtime: the value ~a is not a buffer." value)
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
       (format s "Runtime-Error: Encountered the runtime error at ~ath instruction.
condition:
  ~a
disassemble:
~a"
	       (runtime-pc runtime)
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
                    (apply #'realize-node (node-type node) runtime node (map 'list #'preprocess-argument (node-reads node))))))
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
        (incf (runtime-pc runtime))))))
;; ~~~~ print-object ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun render-list (list) (apply #'concatenate 'string (butlast (loop for n in list append (list (format nil "~a" n) ", ")))))
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
	      do (format stream "~a~(~a~)~a~(~a~)(~(~a~));~%" (indent n-indent) (render-list (node-writes node)) (if (node-writes node) " = " "")
                         (if (eql (node-type node) :JIT_KERNEL)
                             (uiop:symbol-call :caten/codegen/jit :compiled-kernel-name (getattr node :kernel-info))
                             (node-type node))
                         (render-list (node-reads node)))))))
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
;; ~~~~ Default Implementations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Special treatments for :Allocate with (getattr node :from)
(defmethod realize-node :around ((node-type (eql :Allocate)) runtime node args)
  (let ((from (getattr node :from)))
    ;; [TODO] caching treatments
    (if (or (null from) *supress-allocate-mode*)
        (call-next-method) ;; Tmp buffer allocation etc
        ;; [TODO]
        (typecase from
          (symbol
           
           )
          (AbstractBuffer
           ;; Insert the transfer
           )
          (otherwise
           
           )))))        
