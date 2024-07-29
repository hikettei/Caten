
(in-package :caten/air)

;; ~~ utils ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defpattern symbol-eq (to-what)
    `(and (type symbol) (satisfies (lambda (x) (equalp (symbol-name x) ,to-what)))))
(defpattern <>Node
    (type args attrs)
    `(list*
      (and (type keyword) (place ,type))
      (and (list* _) (place ,args))
      (place ,attrs)))

(defun ->property (attrs)
  (verify-attrs attrs)
  `(and
    ,@(loop for nth upfrom 0 to (1+ (/ (length attrs) 2)) by 2
	    for x = (nth nth attrs)
	    for y = (nth (1+ nth) attrs)
	    if (and x y)
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
       (let ((args (match args
		     ((list (symbol-eq "~") x) `(list* ,x))
		     (_ `(list ,@(map 'list #'replace-form args))))))
	 (if recursive
	     ;; [opt memo] fewer calls of id->value, no worth to consider optimizing it.
	     `(access #'(lambda (x) (id->value ,graph-bind x)) (%Node ,type ,args ,attrs))
	     `(%Node ,type ,args ,attrs))))
      ((type list) (map 'list #'replace-form rules))
      (_ rules))))

(defun parse-rule (rule bind graph-bind)
  (declare (type list rule))
  (match rule
    ((list from (symbol-eq "->") to)
     `((and
	(satisfies (lambda (x) (declare (ignore x)) (setf *matched-bind* nil) t))
	,(find/replace-rules from graph-bind))
       (values
	,@(match to
	    ((<>Node type reads attrs)
	     `((list (make-node (node-class ,bind) ,type (node-writes ,bind) (list ,@reads) ,@attrs))))
	    ((list* (list node graph) body)
	     `((let ((,node ,bind)
		     (,graph ,graph-bind))
		 (declare (ignorable ,node ,graph))
		 ,@body)))
	    (_
	     (error "Replace case must be one of:
    - (:Type (Args) attrs)
    - ((node graph) body)")))
	*matched-bind*)))
    (_ (error "Follow this notation: (From_Pattern) -> (To_Pattern).~%~a" rule))))

(defparameter *matched-bind* nil "a temporary place to store matched nodes during simplifying")
(defmacro defsimplifier ((name &key (speed 3)) &rest rules)
  "
## [Macro] defsimplifier
Defines graph simplification rule
Tips: return nil to skip the simplification process.
Tips: (~ x) to accept n-args.
TODO: Docs"
  (with-gensyms (graph simplifier-bind apply-bind node-top count-bind)
    `(defun ,name (,graph &key (no-verify nil))
       (declare (type graph ,graph)
		(type boolean no-verify)
		(optimize (speed ,speed)))
       (when (null (graph-nodes ,graph)) (return-from ,name))
       (unless no-verify (verify-graph ,graph))
       (labels ((,simplifier-bind (,node-top ,count-bind
				   &aux
				     (*matched-bind* nil)
				     (fixed-writes-to
				      (when ,node-top
					(loop for o in (graph-outputs ,graph)
					      if (find o (node-writes ,node-top) :test #'eql)
						collect o))))
		  (declare (type list *matched-bind*))
		  (when fixed-writes-to (return-from ,simplifier-bind))
		  (when (null ,node-top) (return-from ,simplifier-bind))		  
		  (multiple-value-bind (replace-rule matched)
		      (match ,node-top
			,@(map 'list #'(lambda (x) (parse-rule x node-top graph)) rules)
			(_ nil))
		    (when (and replace-rule matched)
		      (when (node-p replace-rule) (setf replace-rule (list replace-rule)))
		      ;; reject the replace-rule only when:
		      ;; - the original node writes the output to (graph-outputs graph)
		      ;; - the replaced node breaks this rule
		      ;;(when fixed-writes-to
		      ;;  (let ((out (apply #'append (map 'list #'node-writes replace-rule))))
		      ;;    (when (some #'(lambda (x) (null (find x out))) fixed-writes-to)
		      ;;      (return-from ,simplifier-bind))))		      
		      (setf (graph-nodes ,graph)
			    (append
			     (subseq (graph-nodes ,graph) 0 ,count-bind)
			     replace-rule
			     (subseq (graph-nodes ,graph) (1+ ,count-bind))))
		      ;; this may not be required but reduce the number of nodes as many as possible
		      (dolist (r matched)
			;; the top node of matched patten is always replaced.
			(when (not (eql r (node-id ,node-top)))
			  ;; Subsequent nodes are removed if they are not used.
			  (let ((writes (node-writes (id->node ,graph r))))
			    (when (every #'(lambda (w) (= (length (the list (id->users ,graph w))) 0)) writes)
			      (remnode ,graph r)))))
		      t)))
		(,apply-bind (graph &aux (changed-p nil))
		  (dotimes (nth (length (graph-nodes graph)))
		    (when (,simplifier-bind (nth nth (graph-nodes graph)) nth)
		      (setf changed-p t)))
		  changed-p))
	 (loop while (,apply-bind ,graph))
	 (unless no-verify (verify-graph ,graph))
	 ,graph))))
