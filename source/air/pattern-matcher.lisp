
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
       (let ((args (match args
		     ((list (symbol-eq "~") x) `(list* ,x))
		     (_ `(list ,@(map 'list #'replace-form args))))))
	 (if recursive
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
		    (when (node-p replace-rule)
		      (setf replace-rule (list replace-rule)))
		    (when (and replace-rule matched)
		      (dolist (r matched)
			;; keep the top of pattern, writes are not lost.
			(if (eql r (node-id ,node-top))
			    (remnode ,graph r)
			    ;; Purge the matched node if not anymore used.
			    (let ((writes (node-writes (id->node ,graph r))))
			      (when (every #'(lambda (w) (= (length (the list (id->users ,graph w))) 0)) writes)
				(remnode ,graph r)))))
		      ;; [TODO] node[-1] dake swap sinaiyouni suru hituyou ga aru
		      ;; janaito top no graph ga baguru
		      (setf (graph-nodes ,graph)
			    (append replace-rule (graph-nodes ,graph)))
		      t)
		    nil))
		(,apply-bind (graph &aux (changed-p nil))
		  (dotimes (nth (length (graph-nodes graph)))
		    (when (,simplifier-bind (nth nth (graph-nodes graph)))
		      (setf changed-p t)))
		  changed-p))
	 (dotimes (i 2) (loop while (,apply-bind ,graph)))
	 (verify-graph ,graph)
	 ,graph))))
