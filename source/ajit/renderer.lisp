(in-package :caten/ajit)

;; ~~ Abstraction ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defgeneric %render-subroutine (lang kernel-lang jit-graph polyhedral indent type-map)
  (:documentation
   "IRs used in the jit-graph:
(TODO: Docs)
- FOR
- ENDFOR
- FUNCALL
- IF
- ELSE
- ENDIF
"))

(defgeneric %render-expr (lang op lhs rhs)
  (:documentation "
TODO: Deftype
OP :=
:CONST(value, nil)
:AND
:OR
:MAX
:MIN
:+ :- :* :/
:NEG(value, nil)
:% (mod)
:equal
:<=
:>=
:<
:>
"))

(defgeneric %render-nodes (lang graph args indent type-map))

(defun render-expr (lang expr)
  "Render-expr"
  (declare (type keyword lang))
  (if (expr-p expr)
      (%render-expr lang (expr-op expr) (expr-x expr) (expr-y expr))
      (%render-expr lang :Const expr nil)))

;; ~~~ CLANG ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defmethod %render-expr ((lang (eql :clang)) (op (eql :NEG)) lhs rhs)
  (assert (null rhs))
  (format nil "-~(~a~)" (render-expr lang lhs)))

(defmethod %render-expr ((lang (eql :clang)) (op (eql :Const)) lhs rhs)
  (assert (or (stringp lhs) (numberp lhs)))
  (assert (null rhs))
  (format nil "~(~a~)" lhs))

(defmethod %render-expr ((lang (eql :clang)) (op (eql :MAX)) lhs rhs)
  (assert (and lhs rhs))
  (format nil "MAX(~a, ~a)" (render-expr lang lhs) (render-expr lang rhs)))

(defmethod %render-expr ((lang (eql :clang)) (op (eql :MIN)) lhs rhs)
  (assert (and lhs rhs))
  (format nil "MIN(~a, ~a)" (render-expr lang lhs) (render-expr lang rhs)))

(defmethod %render-expr ((lang (eql :clang)) op lhs rhs)
  (assert (and lhs rhs))
  (format nil "(~a~(~a~)~a)"
	  (render-expr lang lhs)
	  (ecase op
	    (:+ :+) (:- :-) (:* :*) (:/ :/)
	    (:AND :&&) (:OR "||")
	    (:% :%) (:equal :==) (:<= :<=) (:>= :>=) (:< :<) (:> :>))
	  (render-expr lang rhs)))

;; memo: こいつがHeaderのRenderingに対応。SCHEDULE=任意のグラフでIfやForを表現できる
(defmethod %render-subroutine ((lang (eql :clang)) kernel-lang jit-graph polyhedral indent type-map)
  (declare (type graph jit-graph)
	   (type polyhedral polyhedral)
	   (type fixnum indent))
  (with-output-to-string (out)
    (macrolet ((line (designator &rest args)
		 `(progn
		    (dotimes (i (* 4 indent)) (princ " " out))
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
		   (princ (%render-nodes kernel-lang (gethash idx (poly-pipeline polyhedral)) args indent type-map) out))))))))

(defun ->cdtype (dtype)
  (ecase dtype
    (:float64 "double")
    (:float32 "float")))

(defmethod %render-nodes ((lang (eql :clang)) graph args indent type-map)
  (with-output-to-string (out)
    (macrolet ((line (designator &rest args)
		 `(progn
		    (dotimes (i (* 4 indent)) (princ " " out))
		    (format out ,designator ,@args)
		    (format out "~%"))))
      (labels ((render-aref (id)
		 (let ((ref (render-isl-aref id type-map :genid #'(lambda (x) (intern (format nil "c~a" x))))))
		   (if (string= ref "")
		       (format nil "~(~a~)" id)
		       (format nil "~(~a~)[~(~a~)]" id ref)))))
	(dolist (node (graph-nodes graph))
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
	       (line "~(~a~) = ~a;" (render-aref (car (node-reads node))) value)))
	    (:WMMA
	     (multiple-value-bind (c a b) (apply #'values (node-reads node))
	       (line "~(~a~) += ~(~a~) * ~(~a~);" (render-aref c) (render-aref a) (render-aref b))))
	    (:STORE
	     (multiple-value-bind (a b) (apply #'values (node-reads node))
	       (when (not (equal a b))
		 (line "~(~a~) = ~(~a~);" (render-aref a) (render-aref b)))))
	    (:SIN
	     (line "~(~a~) = sin(~(~a~));" (render-aref (car (node-reads node))) (render-aref (car (node-reads node)))))
	    (otherwise
	     (case (node-class node)
	       (:BinaryOps
		(let* ((r (getattr node :reduction))
		       (op (ecase (node-type node)
			     (:ADD (if r "+=" "+")) (:MUL (if r "*=" "=")))))
		  (if r
		      (line "~(~a~) ~a ~(~a~);" (render-aref (car (node-reads node))) op (render-aref (second (node-reads node))))
		      (line "~(~a~) = ~(~a~);" (render-aref (car (node-reads node)))
			    (apply
			     #'concatenate
			     'string
			     (butlast
			      (loop for r in (node-reads node)
				    append (list (render-aref r) op))))))))
	       (otherwise
		(error "Renderer for ~a is not implemented yet." node))))))))))
