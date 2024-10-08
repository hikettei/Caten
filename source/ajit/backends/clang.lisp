(defpackage :caten/ajit.backends.clang
  (:use :cl :caten/ajit :caten/air :caten/avm :cffi)
  (:import-from
   :caten/common.dtype
   #:dtype/cast))
(in-package :caten/ajit.backends.clang)
;; ~~~ CLANG ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass Clang (Device) nil)
(defmethod default-device ((id (eql :clang))) (make-instance 'clang))
(defmethod device-parallel-depth ((id Clang)) 0)
(defmethod device-packed-by ((id Clang)) 4)
(defparameter *access* nil)
(defparameter *args* nil)
(defparameter *suffix* nil)
(defun args-p (id) (if (stringp id) (find (intern id) *args*) (find id *args*)))

(defun load-foreign-function (source &key (compiler "gcc") (lang "c") (compiler-flags) (dir nil))
  (declare (type string source compiler))
  (uiop:with-temporary-file (:pathname sharedlib :type "so" :keep t :directory dir)
    nil
    :close-stream
    (let* ((cmd
	     ;; gcc -shared -o sharedlib
	     (append
	      (list
	       compiler "-shared"
	       "-x" lang)
	      compiler-flags
	      (list "-o" (uiop:native-namestring sharedlib) "-")))
	   (process-info (uiop:launch-program
			  cmd
			  :input :stream
			  :error-output :stream))
	   (input (uiop:process-info-input process-info))
	   (error-output (uiop:process-info-error-output process-info)))
      (unwind-protect (princ source input)
	(close input))
      (unless (zerop (uiop:wait-process process-info))
	(error "Caten[Clang]: Failed to compile a shared library:~%~a~%

Compiled with: ~a"
	       (alexandria:read-stream-content-into-string error-output)
	       (with-output-to-string (out)
		 (dolist (c cmd) (princ c out) (princ " " out))))))
    (cffi:load-foreign-library sharedlib)))

(defmethod %render-compile ((lang Clang) avm function dir)
  (load-foreign-function function :compiler (ctx:getenv :CC) :lang "c" :compiler-flags '("-O3") :dir dir))

(defun bool->bit (x)
  (declare (type buffer x))
  (if (eql (buffer-dtype x) :bool)
      (make-array (array-total-size (buffer-value x)) :element-type 'bit :initial-contents (map 'list #'(lambda (x) (if x 1 0)) (buffer-value x)))
      (buffer-value x)))

(defun maybe-buffer-value (x) (if (buffer-p x) (buffer-value x) x))
(defmethod %render-function-caller ((lang Clang) avm args &aux (tmps))
  (labels ((expand (rest-forms body)
             (if rest-forms
		 (if (= 0 (buffer-nrank (argument-metadata (car rest-forms))))
		     (if (not (argument-pointer-p (car rest-forms)))
			 (expand (cdr rest-forms) body)
			 (let ((node (car rest-forms))
			       (tmp (gensym)))
			   (push (cons tmp node) tmps)
			   `(let ((,tmp ,(caten/ajit:argument-name (car rest-forms))))
			      (with-foreign-object (,(caten/ajit:argument-name node) ,(->cffi-dtype (argument-dtype node)))
				(setf (mem-ref ,(caten/ajit:argument-name (car rest-forms)) ,(->cffi-dtype (argument-dtype node))) (buffer-value ,tmp))
				,(expand (cdr rest-forms) body)))))
		     `(with-pointer-to-vector-data
			  (,(caten/ajit:argument-name (car rest-forms)) (bool->bit ,(caten/ajit:argument-name (car rest-forms))))
			,(expand (cdr rest-forms) body)))
		 `(progn
		    ,@body
		    ,@(loop for (buffer . node) in tmps
			    for cffi = (caten/ajit:argument-name node)
			    for type = (->cffi-dtype (argument-dtype node))
			    collect `(setf (buffer-value ,buffer) (mem-ref ,cffi ,type)))))))
    `(lambda (,@(map 'list #'caten/ajit:argument-name args))
       (declare (optimize (compilation-speed 3)))
       ,(expand
	 args
	 `((cffi:foreign-funcall
            ,(format nil "~(~a~)" (avm-name avm))
            ,@(loop for arg in args
		    for is-pointer = (argument-pointer-p arg)
		    if (not is-pointer)
		      append `(,(->cffi-dtype (argument-dtype arg)) (maybe-buffer-value ,(caten/ajit:argument-name arg)))
		    else
		      append `(:pointer ,(caten/ajit:argument-name arg)))
            :void))))))

(defun render-to-c (obj)
  (declare (type (or string symbol number) obj))
  (if (typep obj 'double-float)
      (cl-ppcre:regex-replace "d" (format nil "~a" obj) "e")
      (ecase (if (numberp obj)
                 (uiop:symbol-call :caten/apis :float-type-of obj)
                 t)
        (:inf "_INFINITY")
        (:-inf "_NEGATIVE_INFINITY")
        (:nan "_nan")
        ('t
         (let ((obj (format nil "~(~a~)" obj)))
           (if (string= obj "t")
	       "1"
	       (if (string= obj "nil")
                   "0"
		   obj)))))))

(defmethod %render-program-toplevel ((lang Clang) body)
  (format nil "~%#include <math.h>
#include <stdint.h>
~a
#define boolean _Bool
#define _infinity INFINITY
#define _negative_infinity -INFINITY
#define _nan NAN
#define min(a, b) ((a) < (b) ? (a) : (b))~%#define max(a, b) ((a) > (b) ? (a) : (b))
~a"
	  (if (= 1 (ctx:getenv :OMP))
	      "#include <omp.h>"
	      "")
	  body))

(defmethod %render-function ((lang Clang) avm args body)
  (let ((header
	  (format nil "void ~(~a~)(~a)"
		  (avm-name avm)
		  (apply
		   #'concatenate
		   'string
		   (butlast
		    (loop for arg in args
			  append
			  (list
			   (format nil "~a~a ~(~a~)"
				   (->cdtype (argument-dtype arg))
				   (if (= (buffer-nrank (argument-metadata arg)) 0)
				       (if (argument-pointer-p arg) "*" "")
				       "*")
				   (caten/ajit:argument-name arg))
			   ", "))))))
	(shapes
	  (format nil "/*~%Arrays:~%~a*/~%"
		  (with-output-to-string (out)
		    (loop for arg in args
			  for metadata = (argument-metadata arg)
			  do (format out "  - ~a[~(~a~)]: ~a~a~%" (caten/ajit:argument-name arg) (buffer-dtype metadata) (buffer-shape metadata)
				     (format nil " // ~a, ~a" (argument-io arg) (argument-type arg))))))))				
    (format nil "~a~a;~%~a {~%~a}" shapes header header body)))	  

(macrolet ((unary (name render)
	     `(defmethod %render-expr ((lang Clang) (op (eql ,name)) lhs rhs z)
		(assert (null rhs)) (assert (null z))
		(format nil "~a(~(~a~))" ,render (render-expr lang lhs)))))
  (unary :NEG "-")
  (unary :SIN "sin")
  (unary :RECIP "1/")
  (unary :SQRT "sqrt")
  (unary :LOG2 "log2")
  (unary :EXP2 "exp2"))

(defmethod %render-expr ((lang Clang) (op (eql :Const)) lhs rhs z)
  (assert (or (stringp lhs) (symbolp lhs) (numberp lhs)))
  (assert (null z))
  (if (args-p lhs)
      (format nil "(*~(~a~))" (render-to-c lhs))
      (format nil "~(~a~)" (render-to-c lhs))))

(defmethod %render-expr ((lang Clang) (op (eql :MAX)) lhs rhs z)
  (assert (and lhs rhs))
  (if z
      (format nil "max(~a, max(~a, ~a))" (render-expr lang lhs) (render-expr lang rhs) (render-expr lang z))
      (format nil "max(~a, ~a)" (render-expr lang lhs) (render-expr lang rhs))))

(defmethod %render-expr ((lang Clang) (op (eql :CAST)) lhs rhs z)
  (assert (null z))
  (format nil "(~a)~a" (->cdtype rhs) (render-expr lang lhs)))

(defmethod %render-expr ((lang Clang) (op (eql :MIN)) lhs rhs z)
  (assert (and lhs rhs))
  (assert (null z))
  (format nil "min(~a, ~a)" (render-expr lang lhs) (render-expr lang rhs)))

(defmethod %render-expr ((lang Clang) (op (eql :Aref)) lhs rhs z)
  (assert (null z))
  (assert (and lhs rhs))
  (let ((ref (render-aref lang rhs :genid #'(lambda (x) (nth x *access*)))))
    (if (string= "0" ref)
	(if (args-p lhs)
	    (format nil "(*~(~a~)~a)" lhs (unroll-suffix rhs *suffix*))
	    (format nil "~(~a~)~a" lhs (unroll-suffix rhs *suffix*)))
	(format nil "~(~a~)[~(~a~)]" lhs ref))))

(defmethod %render-expr ((lang Clang) (op (eql :INDEX-COMPONENTS)) lhs rhs z)
  (assert (buffer-p (expr-y lhs)))
  (assert (null z))
  (format nil "~a" (render-index-components lang lhs rhs *access*)))

(defmethod %render-expr ((lang Clang) (op (eql :NOT)) lhs rhs z)
  (assert (and lhs (null rhs) (null z)))
  (format nil "!~a" (render-expr lang lhs)))

(defmethod %render-expr ((lang Clang) op lhs rhs z)
  (assert (and lhs rhs) () "~a is not implemented?" op)
  (assert (null z))
  (format nil "(~a~(~a~)~a)"
	  (render-expr lang lhs)
	  (ecase op
	    (:+ :+) (:- :-) (:* :*) (:/ :/)
	    (:ADD :+) (:MUL :*) (:IDIV "/")
	    (:AND :&) (:OR "|") (:!= :!=) (:EQ :=)
	    (:XOR "^")
	    (:% :%) (:equal :==) (:<= :<=) (:>= :>=) (:< :<) (:> :>))
	  (render-expr lang rhs)))

(defmethod %render-expr ((lang Clang) (op (eql :WHERE)) x y z)
  (assert (and x y z))
  (format nil "(~(~a~) ? ~(~a~) : ~(~a~))" (render-expr lang x) (render-expr lang y) (render-expr lang z)))

(defmethod %render-body ((lang Clang) kernel-lang jit-graph polyhedral indent args)
  (declare (type graph jit-graph)
	   (type polyhedral polyhedral)
	   (type fixnum indent))
  (let ((*args* (loop for arg in args if (argument-pointer-p arg) collect (argument-name arg))))
    (with-output-to-string (out)
      (macrolet ((line (designator &rest args)
		   `(progn
		      (dotimes (i (* 2 indent)) (princ " " out))
		      (format out ,designator ,@args)
		      (format out "~%")))
		 (r (obj) `(render-expr lang ,obj)))
	(loop with nth = 0
	      for node in (graph-nodes jit-graph)
	      for type = (node-type node)
	      for outermost-p = (= nth 0) do
		(assert (eql :Render (node-class node)))
		(ecase type
		  (:FOR
		   (multiple-value-bind (idx upfrom below by)
		       (values (getattr node :idx) (getattr node :upfrom) (getattr node :below) (getattr node :by))
		     (assert (and idx upfrom below by) () "Missing ~a" (list idx upfrom below by))
		     (when (and outermost-p (eql :global (getattr node :scope)) (= 1 (ctx:getenv :OMP)))
		       (line "#pragma omp parallel for"))
		     (line "for(int ~(~a~)=~a;~a;~a+=~a) {" (r idx) (r upfrom) (r below) (r idx) (r by))
		     (incf indent))
		   (incf nth))
		  (:ENDFOR
		   (decf indent)
		   (line "}"))
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
		   (dolist (node (if (getattr node :_packed)
				     (getattr node :_unrolled)
				     (list node)))
		     (let ((idx (getattr node :idx))
			   (args (map 'list #'(lambda (x) (r x)) (getattr node :args)))
			   (*suffix* (getattr node :unroll-offsets)))
		       (princ (%render-nodes kernel-lang (gethash idx (poly-pipeline polyhedral)) args indent) out))))))))))

(defun ->cdtype (dtype)
  (ecase dtype
    (:bool "boolean")
    (:float64 "double")
    (:float32 "float")
    (:uint64 "uint64_t")
    (:int64 "int64_t")
    (:int32 "int32_t")
    (:uint32 "uint32_t")
    (:int16 "int16_t")
    (:uint16 "uint16_t")
    (:uint8 "uint8_t")
    (:int8 "int8_t")))

(defun ->cffi-dtype (dtype)
  (ecase dtype
    (:bool :bool)
    (:float64 :double)
    (:float32 :float)
    (:uint64 :uint64)
    (:int64 :int64)
    (:int32 :int32)
    (:uint32 :uint32)
    (:int16 :int16)
    (:uint16 :uint16)
    (:uint8 :uint8)
    (:int8 :int8)))

(defmethod %render-nodes ((lang Clang) graph access indent)
  (with-output-to-string (out)
    (macrolet ((line (designator &rest args)
		 `(progn
		    (dotimes (i (* 2 indent)) (princ " " out))
		    (format out ,designator ,@args)
		    (format out "~%"))))
      (labels ((%render-aref (id type)
		 (let ((ref (render-aref lang type :genid #'(lambda (x) (nth x access)))))
		   (if (string= ref "0")
		       (if (args-p id)
			   (format nil "(*~(~a~)~a)" id (unroll-suffix type *suffix*))
			   (format nil "~(~a~)~a" id (unroll-suffix type *suffix*)))
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
		       (line "~(~a~)~(~a~) += ~(~a~) * ~(~a~);"
			     (if (car (getattr node :declare-type))
				 (format nil "~a " (->cdtype (buffer-dtype ct)))
				 "")
			     (%render-aref c ct) (%render-aref a at) (%render-aref b bt)))))
		  (:EXPR
		   (multiple-value-bind (at) (apply #'values (relay-writes type))
		     (line "~(~a~)~(~a~) = ~(~a~);"
			   (if (car (getattr node :declare-type))
			       (format nil "~a " (->cdtype (buffer-dtype at)))
			       "")
			   (%render-aref (car (node-writes node)) at) (render-expr lang (getattr node :EXPR)))))))))))
