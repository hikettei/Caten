(in-package :caten/air)

;; ~~ utils ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun verify-attrs (attrs)
  (declare (type list attrs))
  (assert (= (mod (length attrs) 2) 0)
	  ()
	  "verity-attrs: Key/Value pairs do not match. ~a" attrs)
  (loop for i upfrom 0 below (/ (length attrs) 2) by 2 do
    (assert (keywordp (nth i attrs))
	    ()
	    "verify-attrs: key must be a keyword but got ~a.~%In ~a"
	    (nth i attrs) attrs))
  attrs)

(defun verify-buffers (buffers)
  (declare (type list buffers))
  (assert (every #'(lambda (x) (or (numberp x) (symbolp x))) buffers)
	  ()
	  "verify-buffers: Buffers are number or symbol. ~a" buffers)
  buffers)
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defstruct (Node
	    (:copier %copy-node)
	    (:constructor %make-node (class type writes reads &rest attrs
				      &aux
					(writes (verify-buffers writes))
					(reads  (verify-buffers reads))
					(attr   (apply #'make-attr type (verify-attrs attrs)))
					(id (progn (verify-args attr writes reads) (gensym "NID"))))))
  "y1 y2 y3 ... <- f(x1 ... xn)"
  (class class :type keyword)
  (id id :type symbol)
  (type type :type keyword)
  (writes writes :type list)
  (reads  reads :type list)
  (attr attr :type Attribute))

(defun make-node (class type writes reads &rest attrs)
  (apply #'%make-node class type writes reads attrs))

(define-compiler-macro make-node (class type writes reads &rest attrs)
  (if (keywordp type)
      (let ((instance-key (cdr (gethash type *attribute->instance*))))
	(when (null instance-key)
	  (error "The node :~a is not defined. It needs to be defined by `defnode` before being used in make-node.
The nodes defined at compile time are as follows:
~a" type (debug/render-defined-nodes :ignore (if (find class `(:Testing :TMP)) nil `(:Testing :TMP)))))
	(let ((initargs (%attr-initargs type))
	      (input-keys (loop for i upfrom 0 below (length attrs) by 2
			        if (keywordp (nth i attrs)) collect (nth i attrs))))
	  (dolist (key input-keys)
	    (assert (find key initargs) () "make-node: The attr :~a is not defined in the definition of ~a.
Possible keywords are following: ~a" key type initargs)))
	`(%make-node ,class ,type ,writes ,reads ,@attrs))
      `(%make-node ,class ,type ,writes ,reads ,@attrs)))

(defun copy-node (node)
  (declare (type node node))
  (let ((copied-node (%copy-node node)))
    (setf (node-reads copied-node) (copy-list (node-reads node))
	  (node-writes copied-node) (copy-list (node-writes node))
	  (node-attr copied-node) (apply #'make-attr (node-type node) (dump-into-list (node-attr node))))
    copied-node))

(deftype dumpable-type () `(or symbol number keyword))
(defmethod make-load-form ((node Node) &optional env &aux (debug-dump (= 1 (ctx:getenv :AOT_VERBOSE))))
  (declare (ignore env))
  `(make-node
    ,(node-class node) ,(node-type node) ',(node-writes node) ',(node-reads node)
    ,@(loop for attr in (getattrs node)
	    for val = (getattr node attr)
	    if (or (eql attr :jit-info) (typep val 'dumpable-type) (and (listp val) (every #'(lambda (x) (typep x 'dumpable-type)) val)))
	      append (if (or (listp val) (and (symbolp val) (not (keywordp val))))
			 `(,attr ',val)
			 `(,attr ,val))
	    else
	      do (when debug-dump (warn "The slot ~a=~a is skipped when dumping ~a" attr val node)))))

(defgeneric print-node (node id))
(defmethod print-node ((node Node) id)
  (if (next-method-p)
      (call-next-method)
      nil))
(defmethod print-object ((node Node) stream)
  (let ((ext (print-node node (node-type node))))
    (if ext
	(format stream ext)
	(flet ((render-list (list)
		 (apply #'concatenate 'string
			(butlast (loop for n in list
				       append (list (format nil "~a" n) ", "))))))
	  (format stream "<Node[~a] ~a(~a) : ~a <- (~a)~a>"
		  (node-class node)
		  (node-type node)
		  (node-id node)
		  (render-list (node-writes node))
		  (render-list (node-reads node))
		  (if (and
		       (dump-into-list (node-attr node) :allow-unbound nil)
		       (some #'identity (map 'list #'(lambda (x) (getattr node x)) (getattrs node))))
		      (with-output-to-string (out)
			(format out " where")
			(dolist (k (getattrs node))
			  (when (and k (getattr node k))
			    (format out " :~(~a~)=~a" k (getattr node k)))))
		      ""))))))

(defun getattrs (node)
  (declare (type node node))
  (let ((attrs (dump-into-list (node-attr node) :allow-unbound nil)))
    (loop for attr in attrs if attr collect attr)))

(defun getattr (node id &key (allow-undefined nil))
  (declare (type node node) (type keyword id))
  (if (and allow-undefined (null (find id (getattrs node))))
      nil
      (%getattr (node-attr node) id)))
(defun (setf getattr) (new-value node id &key (allow-undefined nil))
  (if (and allow-undefined (null (find id (getattrs node))))
      nil
      (%setattr (node-attr node) id new-value)))
(defun setattr (node id value)
  (declare (type node node) (type keyword id))
  (%setattr (node-attr node) id value))
(defun remattr (node id &key (allow-undefined))
  (declare (type node node) (type keyword id))
  (setf (getattr node id :allow-undefined allow-undefined) nil)) 
(defun node->id (node) (car (node-writes node)))
