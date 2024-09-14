(defpackage :caten/metal
  (:use :cl :caten/ajit :caten/air :caten/avm :cffi :cl-metal)
  (:import-from
   :caten/common.dtype
   #:dtype/cast)
  (:export
   #:Metal))

(in-package :caten/metal)

(defclass Metal (Device)
  ((device-id :initform 0 :initarg :id :accessor metal-device-id)))
(defmethod initialize-instance :after ((metal Metal) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (use-device (metal-device-id metal)))
(defmethod device-parallel-depth ((id Metal)) 3)
(defmethod default-device ((lang (eql :metal)))
  (make-instance 'Metal :id 0))
(defmethod device-packed-by ((id Metal)) 4)
(defun Metal (&key (gpu 0)) (make-instance 'Metal :id gpu))

;; bfloat16/float16
;; Update this line: https://github.com/hikettei/cl-metal/blob/main/lib/Sources/CLMetal/cl-metal.swift#L333
;; Reading: https://dl.acm.org/doi/pdf/10.1145/2400682.2400713
;; https://developer.apple.com/documentation/metal/compute_passes/creating_threads_and_threadgroups
(defparameter *access* nil)
(defparameter *args* nil)
(defun args-p (id) (if (stringp id) (find (intern id) *args*) (find id *args*)))
;; tensor_cores = [TensorCore(dims=(8,8,8), threads=[(0,2),(1,4),(0,2),(1,2)], dtype_in=di, dtype_out=do) for (di, do) in [(dtypes.float, dtypes.float), (dtypes.half, dtypes.float), (dtypes.half, dtypes.half)]] # noqa: E501
;;
(defun render-to-c (obj)
  (if (typep obj 'double-float)
      (cl-ppcre:regex-replace "d" (format nil "~a" obj) "e")
      (let ((obj (format nil "~(~a~)" obj)))
	(if (string= obj "t")
	    "1"
	    (if (string= obj "nil")
		"0"
		obj)))))

(defun dtype->mtype (dtype)
  (ecase dtype
    (:bool 'boolean)
    (:float64 'double)
    (:float32 'float)
    (:bfloat16 'bfloat)
    (:uint64 'uint64-t)
    (:int64 'int64-t)
    (:int32 'int32-t)
    (:uint32 'uint32-t)
    (:int16 'int16-t)
    (:uint16 'uint16-t)
    (:uint8 'uint8-t)
    (:int8 'int8-t)))

(defun io->metal (io) (ecase io (:input :in) (:output :out) (:io :io)))
(defun maybe-buffer-value (x)
  (if (buffer-p x)
      (buffer-value x)
      x))
(defmethod %render-compile ((lang Metal) avm function dir) (eval (read-from-string function)))
(defmethod %render-function-caller ((lang Metal) avm args)
  `(lambda (&rest args)
     (%funcall-metal
      (get-kernel ,(avm-name avm))
      :args (map 'list #'maybe-buffer-value args)
      :global-size `(1 1 1)
      :local-size `(1 1 1))))

;; ~~ EXPRS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(macrolet ((unary (name render)
	     `(defmethod %render-expr ((lang Metal) (op (eql ,name)) lhs rhs z)
		(assert (null rhs)) (assert (null z))
		(format nil "~a(~(~a~))" ,render (render-expr lang lhs)))))
  (unary :NEG "-")
  (unary :SIN "sin")
  (unary :RECIP "1/")
  (unary :SQRT "sqrt")
  (unary :LOG2 "log2")
  (unary :EXP2 "exp2"))

(defmethod %render-expr ((lang Metal) (op (eql :Const)) lhs rhs z)
  (assert (or (stringp lhs) (symbolp lhs) (numberp lhs)))
  (assert (null z))
  (if (args-p lhs)
      (format nil "(*~(~a~))" (render-to-c lhs))
      (format nil "~(~a~)" (render-to-c lhs))))

(defmethod %render-expr ((lang Metal) (op (eql :MAX)) lhs rhs z)
  (assert (and lhs rhs))
  (if z
      (format nil "max(~a, max(~a, ~a))" (render-expr lang lhs) (render-expr lang rhs) (render-expr lang z))
      (format nil "max(~a, ~a)" (render-expr lang lhs) (render-expr lang rhs))))

(defmethod %render-expr ((lang Metal) (op (eql :CAST)) lhs rhs z)
  (assert (null z))
  (format nil "(~a)~a" (->cdtype rhs) (render-expr lang lhs)))

(defmethod %render-expr ((lang Metal) (op (eql :MIN)) lhs rhs z)
  (assert (and lhs rhs))
  (assert (null z))
  (format nil "min(~a, ~a)" (render-expr lang lhs) (render-expr lang rhs)))

(defmethod %render-expr ((lang Metal) (op (eql :Aref)) lhs rhs z)
  (assert (null z))
  (assert (and lhs rhs))
  (let ((ref (render-isl-aref rhs :genid #'(lambda (x) (nth x *access*)))))
    (if (string= ref "")
	(if (args-p lhs)
	    (format nil "(*~(~a~))" lhs)
	    (format nil "~(~a~)" lhs))
	(format nil "~(~a~)[~(~a~)]" lhs ref))))

(defmethod %render-expr ((lang Metal) (op (eql :INDEX-COMPONENTS)) lhs rhs z)
  (assert (buffer-p (expr-y lhs)))
  (assert (null z))
  (let ((strides (map 'list #'(lambda (x) (render-expr lang x)) rhs)))
    (format nil "(~a)" (render-isl-aref (expr-y lhs) :genid #'(lambda (x) (intern (or (nth x *access*) (car *access*)))) :strides strides))))

(defmethod %render-expr ((lang Metal) (op (eql :NOT)) lhs rhs z)
  (assert (and lhs (null rhs) (null z)))
  (format nil "!~a" (render-expr lang lhs)))

(defmethod %render-expr ((lang Metal) op lhs rhs z)
  (assert (and lhs rhs) () "~a is not implemented?" op)
  (assert (null z))
  (format nil "(~a~(~a~)~a)"
	  (render-expr lang lhs)
	  (ecase op
	    (:+ :+) (:- :-) (:* :*) (:/ :/)
	    (:ADD :+) (:MUL :*)
	    (:AND :&) (:OR "|") (:!= :!=) (:EQ :=)
	    (:XOR "^")
	    (:% :%) (:equal :==) (:<= :<=) (:>= :>=) (:< :<) (:> :>))
	  (render-expr lang rhs)))

(defmethod %render-expr ((lang Metal) (op (eql :WHERE)) x y z)
  (assert (and x y z))
  (format nil "(~(~a~) ? ~(~a~) : ~(~a~))" (render-expr lang x) (render-expr lang y) (render-expr lang z)))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#|
float2 __WMMA_8_8_8_float_float(float2 m, float2 n, float2 o) {
  simdgroup_float8x8 a,b,c; a.thread_elements()[0] = m.x; a.thread_elements()[1] = m.y; b.thread_elements()[0] = n.x;
  b.thread_elements()[1] = n.y; c.thread_elements()[0] = o.x; c.thread_elements()[1] = o.y; simdgroup_multiply_accumulate(c, a, b, c);
  return float2(c.thread_elements()[0], c.thread_elements()[1]);
}
|#

(defmethod %render-program-toplevel ((lang Metal) body) body)
(defmethod %render-function ((lang Metal) avm args body)
  (with-output-to-string (out)
    (prin1
     `(define-kernel (,(avm-name avm) :threadgroup-position-in-grid gid :thread-position-in-threadgroup lid :style :metal :stream ,(>= (ctx:getenv :JIT_DEBUG) 1))
	  (void (,@(loop for arg in args
			 collect `(,(intern (string-upcase (format nil "~a~a" (argument-name arg) (if (argument-pointer-p arg) "*" ""))))
				   ,(dtype->mtype (argument-dtype arg))
				   ,(io->metal (argument-io arg))))))
	  ,body)
     out)))

(defmethod %render-body ((lang Metal) kernel-lang jit-graph polyhedral indent args)
  (declare (type graph jit-graph) (type polyhedral polyhedral) (type fixnum indent))
  (let* ((global-loops
	   (loop for node in (graph-nodes jit-graph)
		 if (and (eql (node-type node) :FOR) (eql (getattr node :scope) :global))
		   collect node))
	 (global-loops
	   (if (>= (length global-loops) 3)
	       (subseq global-loops 0 3)
	       global-loops))
	 (*args* (loop for arg in args if (argument-pointer-p arg) collect (caten/ajit:argument-name arg))))
    (with-output-to-string (out)
      (macrolet ((line (designator &rest args)
		   `(progn
		      (dotimes (i (* 2 indent)) (princ " " out))
		      (format out ,designator ,@args)
		      (format out "~%")))
		 (r (obj) `(render-expr lang ,obj))
		 (global-p (node) `(find (getattr ,node :idx) global-loops :key #'(lambda (x) (getattr x :idx)) :test #'string=)))
	(loop with nth = 0
	      for node in (graph-nodes jit-graph)
	      for type = (node-type node) do
		(assert (eql :Render (node-class node)))
		(ecase type
		  (:FOR
		   (multiple-value-bind (idx upfrom below by)
		       (values (getattr node :idx) (getattr node :upfrom) (getattr node :below) (getattr node :by))
		     (assert (and idx upfrom below by) () "Missing ~a" (list idx upfrom below by))
		     (if (global-p node)
			 (progn
			   (line "uint ~a = lid.~a;" idx (ecase nth (0 "x") (1 "y") (2 "z"))))
			 (progn
			   (line "for(int ~(~a~)=~a;~a;~a+=~a) {" (r idx) (r upfrom) (r below) (r idx) (r by))
			   (incf indent)))
		     (incf nth)))
		  (:ENDFOR
		   (decf indent)
		   (if (global-p node)
		       nil
		       (line "}")))
		  (:IF
		   (let ((c (getattr node :condition)))
		     (assert c () "Missing condition")
		     (line "if ~a {" (r c))
		     (incf indent)))
		  (:ELSE
		   (decf indent)
		   (line "} else {")
		   (incf indent))
		  (:ENDIF
		   (decf indent)
		   (line "}"))
		  (:FUNCALL
		   (let ((idx (getattr node :idx))
			 (args (map 'list #'(lambda (x) (r x)) (getattr node :args))))
		     (princ (%render-nodes kernel-lang (gethash idx (poly-pipeline polyhedral)) args indent) out)))))))))

(defmethod %render-nodes ((lang Metal) graph access indent)
  (with-output-to-string (out)
    (macrolet ((line (designator &rest args)
		 `(progn
		    (dotimes (i (* 2 indent)) (princ " " out))
		    (format out ,designator ,@args)
		    (format out "~%"))))
      (labels ((render-aref (id type)
		 (let ((ref (render-isl-aref type :genid #'(lambda (x) (nth x access)))))
		   (if (string= ref "")
		       (if (args-p id)
			   (format nil "(*~(~a~))" id)
			   (format nil "~(~a~)" id))
		       (format nil "~(~a~)[~(~a~)]" id ref)))))
	(loop with *access* = access
	      for node in (graph-nodes graph)
	      for type = (read-type-relay node) do		
		(ecase (node-type node)
		  (:ALLOCATE
		   (line "~(~a~) ~(~a~)~a;"
			 (->cdtype (getattr node :dtype)) (car (node-writes node))
			 (let ((nrank (getattr node :nrank)))
			   (if (= nrank 0)
			       ""
			       (format
				nil
				"[~(~a~)]"
				(apply
				 #'concatenate
				 'string
				 (butlast
				  (loop for x in (subseq (node-reads node) 0 nrank)
					append (list (format nil "~a" x) "*")))))))))
		  (:WMMA
		   (multiple-value-bind (c a b) (apply #'values (node-reads node))
		     (multiple-value-bind (ct at bt) (apply #'values (relay-reads type))
		       (line "~(~a~) += ~(~a~) * ~(~a~);" (render-aref c ct) (render-aref a at) (render-aref b bt)))))
		  (:EXPR
		   (multiple-value-bind (at) (apply #'values (relay-writes type))
		     (line "~(~a~) = ~(~a~);" (render-aref (car (node-writes node)) at) (render-expr lang (getattr node :EXPR)))))))))))
