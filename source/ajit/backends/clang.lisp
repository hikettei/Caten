(defpackage :caten/ajit.backends.clang
  (:use :cl :caten/ajit :caten/air :caten/avm :cffi)
    (:import-from
     :caten/common.dtype
     #:dtype/cast))
(in-package :caten/ajit.backends.clang)
;; ~~~ CLANG ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defparameter *access* nil)
(defparameter *args* nil)
(defun args-p (id) (find id *args*))

(defun load-foreign-function (source &key (compiler "gcc") (lang "c") (compiler-flags))
  (declare (type string source compiler))
  (uiop:with-temporary-file (:pathname sharedlib :type "so" :keep t)
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

(defmethod %render-compile ((lang (eql :clang)) avm allocs function)
  (load-foreign-function function :compiler (ctx:getenv :CC) :lang "c" :compiler-flags '("-O3")))

(defmethod %render-function-caller ((lang (eql :clang)) avm allocs &aux (tmps))
  (labels ((expand (rest-forms body)
             (if rest-forms
		 (if (= 0 (getattr (car rest-forms) :nrank))
		     (if (null (getattr (car rest-forms) :_pointer))
			 (expand (cdr rest-forms) body)
			 (let ((node (car rest-forms))
			       (tmp (gensym)))
			   (push (cons tmp node) tmps)
			   `(let ((,tmp ,@(node-writes (car rest-forms))))
			      (with-foreign-object (,@(node-writes (car rest-forms)) ,(->cffi-dtype (getattr node :dtype)))
				(setf (mem-ref ,@(node-writes (car rest-forms)) ,(->cffi-dtype (getattr node :dtype))) (buffer-value ,tmp))
				,(expand (cdr rest-forms) body)))))
		     `(with-pointer-to-vector-data
			  (,@(node-writes (car rest-forms)) (buffer-value ,@(node-writes (car rest-forms))))
			,(expand (cdr rest-forms) body)))
		 `(progn
		    ,@body
		    ,@(loop for (buffer . node) in tmps
			    for cffi = (car (node-writes node))
			    for type = (->cffi-dtype (getattr node :dtype))
			    collect `(setf (buffer-value ,buffer) (mem-ref ,cffi ,type)))))))
    `(lambda (,@(apply #'append (map 'list #'node-writes allocs)))
       (declare (optimize (compilation-speed 3)))
       ,(expand
	 allocs
	 `((cffi:foreign-funcall
            ,(format nil "~(~a~)" (avm-name avm))
            ,@(loop for node in allocs
		    for is-pointer = (getattr node :_pointer)
		    if (null is-pointer)
		      append `(,(->cffi-dtype (getattr node :dtype)) (buffer-value ,(car (node-writes node))))
		    else
		      append `(:pointer ,(car (node-writes node))))
            :void))))))

(defun render-to-c (obj)
  (let ((obj (format nil "~(~a~)" obj)))
    (if (string= obj "t")
	"1"
	(if (string= obj "nil")
	    "0"
	    obj))))

(defmethod %render-program-toplevel ((lang (eql :clang)) body)
  (format nil "~%#include <math.h>
#include <stdint.h>
#define boolean _Bool
#define min(a, b) ((a) < (b) ? (a) : (b))~%#define max(a, b) ((a) > (b) ? (a) : (b))
~a" body))

(defmethod %render-function ((lang (eql :clang)) avm allocs body)
  (let ((header
	  (format nil "void ~(~a~)(~a)"
		  (avm-name avm)
		  (apply
		   #'concatenate
		   'string
		   (butlast
		    (loop for node in allocs
			  append
			  (list
			   (format nil "~a~a ~(~a~)"
				   (->cdtype (getattr node :dtype))
				   (if (= (getattr node :nrank) 0)
				       (if (getattr node :_pointer) "*" "")
				       "*")
				   (car (node-writes node)))
			   ", "))))))
	(shapes
	  (format nil "/*~%Arrays:~%~a*/~%"
		  (with-output-to-string (out)
		    (loop for node in allocs
			  for nrank = (getattr node :nrank)
			  do (format out "  - ~a[~(~a~)]: ~a~a~%" (car (node-writes node)) (getattr node :dtype) (subseq (node-reads node) 0 nrank)
				     (if (getattr node :_not_a_input)
					 " // Tmp"
					 "")))))))
    (format nil "~a~a;~%~a {~%~a}" shapes header header body)))	  

(macrolet ((unary (name render)
	     `(defmethod %render-expr ((lang (eql :clang)) (op (eql ,name)) lhs rhs z)
		(assert (null rhs)) (assert (null z))
		(format nil "~a(~(~a~))" ,render (render-expr lang lhs)))))
  (unary :NEG "-")
  (unary :SIN "sin")
  (unary :RECIP "1/")
  (unary :SQRT "sqrt")
  (unary :LOG2 "log2")
  (unary :EXP2 "exp2"))

(defmethod %render-expr ((lang (eql :clang)) (op (eql :Const)) lhs rhs z)
  (assert (or (stringp lhs) (symbolp lhs) (numberp lhs)))
  (assert (null z))
  (assert (null rhs))
  (if (args-p lhs)
      (format nil "(*~(~a~))" (render-to-c lhs))
      (format nil "~(~a~)" (render-to-c lhs))))

(defmethod %render-expr ((lang (eql :clang)) (op (eql :MAX)) lhs rhs z)
  (assert (and lhs rhs))
  (assert (null z))
  (format nil "max(~a, ~a)" (render-expr lang lhs) (render-expr lang rhs)))

(defmethod %render-expr ((lang (eql :clang)) (op (eql :CAST)) lhs rhs z)
  (assert (null z))
  (format nil "(~a)~a" (->cdtype rhs) (render-expr lang lhs)))

(defmethod %render-expr ((lang (eql :clang)) (op (eql :MIN)) lhs rhs z)
  (assert (and lhs rhs))
  (assert (null z))
  (format nil "min(~a, ~a)" (render-expr lang lhs) (render-expr lang rhs)))

(defmethod %render-expr ((lang (eql :clang)) (op (eql :Aref)) lhs rhs z)
  (assert (null z))
  (assert (and lhs rhs))
  (let ((ref (render-isl-aref rhs :genid #'(lambda (x) (nth x *access*)))))
    (if (string= ref "")
	(if (args-p lhs)
	    (format nil "(*~(~a~))" lhs)
	    (format nil "~(~a~)" lhs))
	(format nil "~(~a~)[~(~a~)]" lhs ref))))

(defmethod %render-expr ((lang (eql :clang)) (op (eql :INDEX-COMPONENTS)) lhs rhs z)
  (assert (buffer-p (expr-y lhs)))
  (assert (null z))
  (let ((strides (map 'list #'(lambda (x) (render-expr lang x)) rhs)))
    (format nil "(~a)" (render-isl-aref (expr-y lhs) :genid #'(lambda (x) (intern (nth x *access*))) :strides strides))))

(defmethod %render-expr ((lang (eql :clang)) (op (eql :NOT)) lhs rhs z)
  (assert (and lhs (null rhs) (null z)))
  (format nil "!~a" (render-expr lang lhs)))

(defmethod %render-expr ((lang (eql :clang)) op lhs rhs z)
  (assert (and lhs rhs) () "~a is not implemented?" op)
  (assert (null z))
  (format nil "(~a~(~a~)~a)"
	  (render-expr lang lhs)
	  (ecase op
	    (:+ :+) (:- :-) (:* :*) (:/ :/)
	    (:ADD :+) (:MUL :*)
	    (:AND :&&) (:OR "||") (:NEQ :!=) (:EQ :=)
	    (:% :%) (:equal :==) (:<= :<=) (:>= :>=) (:< :<) (:> :>))
	  (render-expr lang rhs)))

(defmethod %render-expr ((lang (eql :clang)) (op (eql :WHERE)) x y z)
  (assert (and x y z))
  (format nil "(~(~a~) ? ~(~a~) : ~(~a~))" (render-expr lang x) (render-expr lang y) (render-expr lang z)))

(defmethod %render-body ((lang (eql :clang)) kernel-lang jit-graph polyhedral indent allocs)
  (declare (type graph jit-graph)
	   (type polyhedral polyhedral)
	   (type fixnum indent))
  (let ((*args* (loop for alloc in allocs if (getattr alloc :_pointer) collect (car (node-writes alloc)))))
    (with-output-to-string (out)
      (macrolet ((line (designator &rest args)
		   `(progn
		      (dotimes (i (* 2 indent)) (princ " " out))
		      (format out ,designator ,@args)
		      (format out "~%")))
		 (r (obj) `(render-expr lang ,obj)))
	(loop for node in (graph-nodes jit-graph)
	      for type = (node-type node) do
		(assert (eql :Render (node-class node)))
		(ecase type
		  (:FOR
		   (multiple-value-bind (idx upfrom below by)
		       (values (getattr node :idx) (getattr node :upfrom) (getattr node :below) (getattr node :by))
		     (assert (and idx upfrom below by) () "Missing ~a" (list idx upfrom below by))
		     (line "for(int ~(~a~)=~a;~a;~a+=~a) {" (r idx) (r upfrom) (r below) (r idx) (r by))
		     (incf indent)))
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
		   (let ((idx (getattr node :idx))
			 (args (map 'list #'(lambda (x) (r x)) (getattr node :args))))
		     (princ (%render-nodes kernel-lang (gethash idx (poly-pipeline polyhedral)) args indent) out)))))))))

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

(defmethod %render-nodes ((lang (eql :clang)) graph access indent)
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
