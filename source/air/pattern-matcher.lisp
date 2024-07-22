
(in-package :caten/air)

;; ~~ utils ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defpattern <>Node
    (type args attrs)
    `(list*
      (and (type keyword) (place ,type))
      (and (list* _) (place ,args))
      (place ,attrs)))

(defun ->property (attrs)
  (verify-attrs attrs)
  `(and
    ,@(loop for nth upfrom 0 to (/ (length attrs) 2) by 2
	    for x = (nth nth attrs)
	    for y = (nth (1+ nth) attrs)
	    collect `(property ,x ,y))))

(defpattern %Node
    (type args attrs)
    (with-gensyms (id)
      `(and
	(Node
	 :type ,type
	 :reads ,args
	 :attrs ,(->property attrs))
	(Node :id ,id)
	(satisfies
	 (lambda (x)
	   (declare (ignore x))
	   (push ,id *matched-bind*)
	   t)))))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun find/replace-rules (rules graph-bind &optional (recursive nil))
  (flet ((replace-form (r) (find/replace-rules r graph-bind t)))
    (match rules
      ((<>Node type args attrs)
       (if recursive
	   `(access #'(lambda (x) (id->value ,graph-bind x)) (%Node ,type (list ,@(map 'list #'replace-form args)) ,attrs))
	   `(%Node ,type (list ,@(map 'list #'replace-form args)) ,attrs)))
      ((type list) (map 'list #'replace-form rules))
      (_ rules))))

(defun parse-rule (rule bind graph-bind)
  (declare (type list rule))
  (match rule
    ((list from (eql '->) to)
     `((and
	(satisfies (lambda (x) (declare (ignore x)) (setf *matched-bind* nil) t))
	,(find/replace-rules from graph-bind))
       (values
	,@(match to
	    ((<>Node type reads attrs)
	     `((list (make-node (node-class ,bind) ,type (node-writes ,bind) (list ,@reads) ,@attrs))))
	    (_ to))
	*matched-bind*)))
    (_ (error "Follow this notation: (From_Pattern) -> (To_Pattern).~%~a" rule))))

(defparameter *matched-bind* nil "a temporary place to store matched nodes during simplifying")
(defmacro defsimplifier ((name &key (speed 3)) &rest rules)
  "
## [Macro] defsimplifier
Defines graph simplification rule
TODO: Docs"
  (with-gensyms (graph simplifier-bind apply-bind node-top)
    `(defun ,name (,graph)
       (declare (type graph ,graph)
		(optimize (speed ,speed)))
       (verify-graph ,graph)
       (labels ((,simplifier-bind (,node-top &aux (*matched-bind* nil))
		  (declare (type list *matched-bind*))
		  (when (null ,node-top) (return-from ,simplifier-bind))
		  (multiple-value-bind (replace-rule matched)
		      (match ,node-top
			,@(map 'list #'(lambda (x) (parse-rule x node-top graph)) rules)
			(_ nil))
		    (when (and replace-rule matched)
		      (dolist (r matched) (remnode ,graph r))
		      (setf (graph-nodes ,graph)
			    (append replace-rule (graph-nodes ,graph)))
		      t)
		    nil))
		(,apply-bind (graph &aux (changed-p nil))
		  (dotimes (nth (length (graph-nodes graph)))
		    (when (,simplifier-bind (nth nth (graph-nodes graph)))
		      (setf changed-p t)))
		  changed-p))
	 (loop while (,apply-bind ,graph))
	 ,graph))))
