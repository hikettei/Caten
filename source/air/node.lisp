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
	    (:constructor make-node (class type writes reads &rest attrs
				     &aux
				       (writes (verify-buffers writes))
				       (reads  (verify-buffers reads))
				       (attrs (verify-attrs attrs)))))
  "y1 y2 y3 ... <- f(x1 ... xn)"
  (class class :type keyword)
  (id (gensym "NID") :type symbol)
  (type type :type keyword)
  (writes writes :type list)
  (reads  reads :type list)
  (attrs  attrs :type list))
(defmethod print-object ((node Node) stream)
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
	    (if (node-attrs node)	      
		(with-output-to-string (out)
		  (format out " where")
		  (dolist (k (getattrs node))
		    (format out " :~(~a~)=~a" k (getattr node k))))
		""))))
;;(defgeneric lower ())
;;(defgeneric mutate ())
;; NOTE: attrs must be updated via simplifier, not (setf getattr)
(defun getattrs (node)
  (declare (type node node))
  (verify-attrs (node-attrs node))
  (loop for i upfrom 0 to (/ (length (node-attrs node)) 2) by 2
	collect (nth i (node-attrs node))))
(defun getattr (node id)
  (declare (type node node) (type keyword id))
  (ematch (node-attrs node) ((property id value) value)))
(defun node->id (node) (car (node-writes node)))
;; ~~ syntax sugar for make-node ~~~~~~~
;;(declaim (inline <>))
;;(defun <> (type writes reads &rest attrs) (apply #'make-node :node type writes reads attrs))
