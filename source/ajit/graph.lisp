(in-package :caten/ajit)
;; A special graph dedicated to the rendering process
(defun r/for (idx upfrom below by) (make-node :Render :FOR nil nil :idx idx :upfrom upfrom :below below :by by))
(defun r/endfor (idx) (make-node :Render :ENDFOR nil nil :idx idx))
(defun r/funcall (name args)
  ;; :idx = (T12 -> 12)
  (make-node :Render :FUNCALL nil nil :name name :args args :idx (parse-integer (subseq name 1))))
(defun r/if (condition) (make-node :Render :IF nil nil :condition condition))
(defun r/else () (make-node :Render :ELSE nil nil))
(defun r/endif () (make-node :Render :ENDIF nil nil))
(defun create-rendering-graph (polyhedron lisp-ast)
  (declare (type polyhedral polyhedron)
	   (ignore polyhedron))
  (let ((new-graph nil))
    (labels ((lower (object)
	       (when (listp object) (return-from lower (map 'list #'lower object)))
	       (trivia:ematch object
		 ((AstBlock :body body) (map 'list #'lower body))
		 ((AstFor :idx idx :from upfrom :to to :by by :body body :execute-once _)
		  (push (r/for idx upfrom to by) new-graph)
		  (lower body)
		  (push (r/endfor idx) new-graph))
		 ((User :name name :args args)
		  (push (r/funcall name args) new-graph))
		 ((AstIf :condition cond :then-node then :else-node else)
		  (push (r/if cond) new-graph)
		  (lower then)
		  (when else
		    (push (r/else) new-graph)
		    (lower else))
		  (push (r/endif) new-graph))
		 ((Expr :op _ :x _ :y _)
		  (error "create-rendering-graph: Expr should not occur here!")))))
      (lower lisp-ast))
    ;;(apply #'make-graph (append (list (r/funcall "T-1" nil)) (reverse new-graph)))
    (apply #'make-graph (reverse new-graph))))

(defun expr-recursive-replace (expr map)
  (declare (type expr) (type hash-table map))
  (flet ((->new (x) (if (stringp x) (format nil "~a" (or (gethash (intern x) map) x)) x)))
    (when (expr-p (expr-x expr))
      (expr-recursive-replace (expr-x expr) map))
    (when (expr-p (expr-y expr))
      (expr-recursive-replace (expr-y expr) map))
    (when (eql (expr-op expr) :Const)
      (setf (expr-x expr) (->new (expr-x expr))))))

;; ~~ [From aIR -> Expr] ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defparameter *allocated-aref* nil)
(defun expr-aref (id buffer &aux (aref (make-expr :Aref id buffer)))
  (push aref *allocated-aref*)
  aref)
(defun expr-const (object) (make-expr :Const object))
(defun expr-cast (obj dtype) (make-expr :Cast obj dtype))
(declaim (ftype (function (Symbol Graph) Expr) create-expr-from-air))
(defun create-expr-from-air (output graph)
  (declare (type symbol output) (type graph graph))
  (let* ((node (id->value graph output))
	 (parents (loop for arg in (node-reads node)
			for typ in (relay-reads (read-type-relay node))
			if (symbolp arg)
			  collect
			  (let ((val (id->value graph arg)))
			    (if val
				(create-expr-from-air arg graph)
				(expr-aref arg typ)))		    
			else
			  collect (expr-const arg))))
    (assert (<= (length parents) 2) () "~a cannot be grouped to multi expr! (too many arguments)" node)
    (case (node-type node)
      (:Load (expr-const (getattr node :value)))
      (:Cast (expr-cast (second parents) (getattr node :dtype)))
      (otherwise (make-expr (node-type node) (first parents) (second parents))))))

(defun create-multiexpr-node (graph output output-type read-from read-type)
  (declare (type symbol output read-from) (type graph graph)
	   (type Buffer read-type output-type))
  (let* ((*allocated-aref*)
	 (expr (create-expr-from-air output graph)))
    (make-node :EXPR :EXPR (list output) `(,read-from ,@(map 'list #'expr-x *allocated-aref*)) :expr expr
	       :_type_relay (make-inferred-type `(,read-type ,@(map 'list #'expr-y *allocated-aref*)) (list output-type))
	       :buffers *allocated-aref*)))
