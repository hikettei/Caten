(defpackage :caten/ajit.backends.clang
  (:use :cl :caten/ajit :caten/air :caten/avm :cffi))
(in-package :caten/ajit.backends.clang)
;; ~~~ CLANG ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

(defmethod %render-function-caller ((lang (eql :clang)) avm allocs function)
  (labels ((expand (rest-forms body)
	     (if rest-forms
		 (if (= 0 (getattr (car rest-forms) :nrank))
		     (expand (cdr rest-forms) body)
		     `(with-pointer-to-vector-data
			  (,@(node-writes (car rest-forms)) (buffer-value ,@(node-writes (car rest-forms))))
			,(expand (cdr rest-forms) body)))
		 `(progn ,@body))))
    (compile
     nil
     `(lambda (,@(apply #'append (map 'list #'node-writes allocs)))
	(declare (optimize (compilation-speed 3)))
	,(expand
	  allocs
	  `((cffi:foreign-funcall
	     ,(format nil "~(~a~)" (avm-name avm))
	     ,@(loop for node in allocs
		     for type = (intern (string-upcase (->cdtype (getattr node :dtype))) "KEYWORD")
		     if (= (getattr node :nrank) 0)
		       append `(,type (buffer-value ,(car (node-writes node))))
		     else
		       append `(:pointer ,(car (node-writes node))))
	     :void)))))))

(defmethod %render-program-toplevel ((lang (eql :clang)) body)
  (format nil "~%#include <math.h>
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
				   (if (= 0 (getattr node :nrank))
				       ""
				       "*")
				   (car (node-writes node)))
			   ", ")))))))
    (format nil "~a;~%~a {~%~a}" header header body)))	  

(defmethod %render-expr ((lang (eql :clang)) (op (eql :NEG)) lhs rhs)
  (assert (null rhs))
  (format nil "-~(~a~)" (render-expr lang lhs)))

(defmethod %render-expr ((lang (eql :clang)) (op (eql :Const)) lhs rhs)
  (assert (or (stringp lhs) (numberp lhs)))
  (assert (null rhs))
  (format nil "~(~a~)" lhs))

(defmethod %render-expr ((lang (eql :clang)) (op (eql :MAX)) lhs rhs)
  (assert (and lhs rhs))
  (format nil "max(~a, ~a)" (render-expr lang lhs) (render-expr lang rhs)))

(defmethod %render-expr ((lang (eql :clang)) (op (eql :MIN)) lhs rhs)
  (assert (and lhs rhs))
  (format nil "min(~a, ~a)" (render-expr lang lhs) (render-expr lang rhs)))

(defmethod %render-expr ((lang (eql :clang)) op lhs rhs)
  (assert (and lhs rhs))
  (format nil "(~a~(~a~)~a)"
	  (render-expr lang lhs)
	  (ecase op
	    (:+ :+) (:- :-) (:* :*) (:/ :/)
	    (:AND :&&) (:OR "||")
	    (:% :%) (:equal :==) (:<= :<=) (:>= :>=) (:< :<) (:> :>))
	  (render-expr lang rhs)))

(defmethod %render-body ((lang (eql :clang)) kernel-lang jit-graph polyhedral indent)
  (declare (type graph jit-graph)
	   (type polyhedral polyhedral)
	   (type fixnum indent))
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
		   (princ (%render-nodes kernel-lang (gethash idx (poly-pipeline polyhedral)) args indent) out))))))))

(defun ->cdtype (dtype)
  (ecase dtype
    (:float64 "double")
    (:float32 "float")
    (:int32 "int")
    (:uint32 "int")))

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
		       (format nil "~(~a~)" id)
		       (format nil "~(~a~)[~(~a~)]" id ref)))))
	(loop for node in (graph-nodes graph)
	      for type = (read-type-relay node) do		
		(case (node-type node)
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
		  (:LOAD
		   (let ((value (getattr node :value)))
		     (line "~(~a~) = ~a;" (render-aref (car (node-reads node)) (car (relay-reads type))) value)))
		  (:WMMA
		   (multiple-value-bind (c a b) (apply #'values (node-reads node))
		     (multiple-value-bind (ct at bt) (apply #'values (relay-reads type))
		       (line "~(~a~) += ~(~a~) * ~(~a~);" (render-aref c ct) (render-aref a at) (render-aref b bt)))))
		  (:STORE
		   (multiple-value-bind (a b) (apply #'values (node-reads node))
		     (multiple-value-bind (at bt) (apply #'values (relay-reads type))
		       (when (not (equal a b))
			 (line "~(~a~) = ~(~a~);" (render-aref a at) (render-aref b bt))))))
		  (:SIN
		   (multiple-value-bind (a at) (values (car (node-reads node)) (car (relay-reads type)))
		     (line "~(~a~) = sin(~(~a~));" (render-aref a at) (render-aref a at))))
		  (:INDEX-COMPONENTS
		   (line "~(~a~) = ~(~a~);" (render-aref (car (node-reads node)) (car (relay-reads type))) (render-isl-aref (car (relay-reads type)) :genid #'(lambda (x) (intern (format nil "c~a" x))))))
		  (:MOVE
		   (multiple-value-bind (a at) (values (car (node-reads node)) (car (relay-reads type)))
		     (multiple-value-bind (b bt) (values (second (node-reads node)) (second (relay-reads type)))
		       (line "~(~a~) = ~(~a~);" (render-aref a at) (render-aref b bt)))))
		  (otherwise
		   (case (node-class node)
		     (:BinaryOps
		      (let* ((r (getattr node :reduction))
			     (op (case (node-type node)
				   (:ADD (if r "+=" "+")) (:MUL (if r "*=" "=")))))
			(if op
			    (if r
				(line "~(~a~) ~a ~(~a~);" (render-aref (car (node-reads node)) (car (relay-reads type))) op (render-aref (second (node-reads node)) (second (relay-reads type))))
				(line "~(~a~) = ~(~a~);" (render-aref (car (node-reads node)) (car (relay-reads type)))
				      (apply
				       #'concatenate
				       'string
				       (butlast
					(loop for r in (node-reads node)
					      for rt in (relay-reads type)
					      append (list (render-aref r rt) op))))))
			    (let ((op (ecase (node-type node)
					(:MAX "max"))))
			      (line "~(~a~) = ~a(~a);" (render-aref (car (node-reads node)) (car (relay-reads type))) op
				    (apply
				     #'concatenate
				     'string
				     (butlast
				      (loop for r in (node-reads node)
					    for rt in (relay-reads type)
					    append (list (render-aref r rt) ", ")))))))))				    
		     (otherwise
		      (error "Renderer for ~a is not implemented yet." node))))))))))
