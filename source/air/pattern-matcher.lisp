(in-package :caten/air)
;; [Reimplement PatternMa] Workload
;; = Add: ./test-suite/test-pattern-matcher.lisp
;; = Support: ~ notation in args
;; ~~~ Reimplementation ~~~~~~~~~~
(defstruct GraphRewrite)
(defstruct (Pattern-Matcher
            (:constructor %make-pattern-matcher (patterns pdict)))
  (patterns patterns :type list)
  (pdict pdict :type hash-table))
(defstruct Pattern
  (match-upat)
  (accept-upat))
;; PatternAST
(defstruct UForm (form))
(defstruct UPat
  (ops nil :type list)
  (module-bind nil :type symbol)
  (op-bind nil :type symbol)
  (src nil :type list) ;; a list of UPat or UForm
  (attrs nil :type list))

(defun make-pattern-matcher (patterns)
  (declare (type list patterns))
  (let ((pdict (make-hash-table)))
    (dolist (p patterns)
      (assert (upat-p p))
      (dolist (op (upat-ops p))
        (setf (gethash op pdict) (append (gethash op pdict) (list p)))))
    (%make-pattern-matcher patterns pdict)))

(defgeneric upat-expand-as-match-form (upat))
(defgeneric upat-expand-as-accept-form (upat))

(defmethod upat-expand-as-match-form ((upat UPat))
  `(Node
    :class ,(upat-module-bind upat)
    :type
    (<> ,(upat-op-bind upat) (or ,@(loop for op in (upat-ops upat) collect `(eql ,op))))
    :reads (list ,@(map 'list #'upat-expand-as-match-form (upat-src upat)))
    :attr nil))

(defmethod upat-expand-as-match-form ((upat UForm)) (uform-form upat))

(defmethod upat-expand-as-accept-form ((upat UPat))
  `(make-node
    :class
    :type
    :writes
    :reads
    :attrs))

(defmethod upat-expand-as-accept-form ((upat UForm)) (uform-form upat))

(defmethod upat-compile ((upat1 UPat) (upat2 UPat))
  (with-gensyms (node-bind)
    `(lambda (,node-bind)
       (declare (type Node ,node-bind))
       (match ,node-bind
         (,(upat-expand upat1) ,(upat-expand upat2))))))

(defun upat-match (upat node)
  (declare (type upat upat) (type node node))
  
  )
;; UPat Creation
(defun recursively-convert-upat (expression &key (module->ops (debug/attrs-by-module)) (allow-not-upat-p nil))
  ;; FindAttr is doable at the moment expanding upat
  (macrolet ((lazy-assert (form is fmt &rest args &aux (id (gensym)))
               (declare (ignore is))
               `(let ((,id ,form))
                  (if allow-not-upat-p
                      (return-from recursively-convert-upat nil)
                      (assert ,id () ,fmt ,@args)))))
    (match expression
      ((list* trigger srcs) ;; MEMO: start w/ upat?
       (multiple-value-bind (ops module-bind op-bind) ;; Parse Trigger
           (match trigger
             ((type keyword)
              (values (list trigger) nil nil))
             ((list* (list module-bind op-bind) _)
              (lazy-assert (every #'keywordp (cdr trigger))
                           ()
                           "The expression ~a should be a list of keywords." trigger)
              (values
               (loop for e in (cdr trigger)
                     for attrs = (gethash e module->ops)
                     do (assert attrs () "The module ~a is not defined." e)
                     append (map 'list #'car attrs))
               module-bind op-bind))
             ((list* (list op-bind) _)
              (lazy-assert (every #'keywordp (cdr trigger)) () "The expression ~a should be a list of keywords." (car expression))
              (values (cdr trigger) nil op-bind))
             (otherwise
              ;; [TODO] Improve this case!
              (lazy-assert nil "The expression ~a should be a list of keywords." (car expression))))
         (make-UPat
          :ops ops
          :module-bind (or module-bind '_)
          :op-bind (or op-bind '_)
          :src (loop for src in (car srcs)
                     for maybe-pat = (recursively-convert-upat src :module->ops module->ops :allow-not-upat-p t)
                     collect (or maybe-pat (make-uform :form src)))
          ;; [TODO] Parse attributes
          :attrs nil)))
      (otherwise
       (lazy-assert nil "Not a valid upat: ~a" expression)))))

(progn
  (let ((upat
          (UPat (((op) :ADD :SUB)
                 ((:ADD (x (:NEG (m)))) b)))))
    (print upat)
    (print (upat-expand upat)))
  nil)

(defun pm-rewrite-graph (pattern-matcher graph)

  )

(defmacro PatternMatcher (&rest forms)
  `(make-pattern-matcher
    (list
     ;; todo: allow doing like t -> ((node graph) )
     (loop for form in forms
           for nth upfrom 0
           collect
           (match form
             ((list match (symbol-eq '->) accept)
              
              )
             (otherwise
              (error "Error accepting in ~ath pattern" nth)))))))

(defmacro define-pattern-matcher ())
;; Rename <UPAT>?
;; If recursive, they should be start w/ <UPAT>?
(defmacro UPat (expression)
  "
```
(UPat Expression)
```

A convenient macro for constructing.

Notation:
```
(<Trigger> (src) :attribute (match) ...)
```

<Trigger> could be one of:
- A opname (e.g.: `:ADD`)`
- A list of opnames `(e.g.: `((<OP_BIND>) :ADD :SUB))`
- A module name (e.g.: `((<MOD_BIND> <OP_BIND>) :Module :GRAPH/MODULE ...)`)

src/dst could be a list or variable.
"
  `(recursively-convert-upat ',expression))
;; ~~ utils ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defpattern symbol-eq (to-what)
  `(and (type symbol) (satisfies (lambda (x) (equalp (symbol-name x) ,to-what)))))

(defpattern <>Node
    (type args attrs)
    `(list*
      (and (type keyword) (place ,type))
      (and (list* _) (place ,args))
      (place ,attrs)))

(defpattern %Node
    (type args attrs)
  (with-gensyms (id)
    `(and
      (Node
       :type ,type
       :reads ,args
       :attr (,(find-attr type) ,@attrs))
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

(defun parse-to-pattern (bind rule)
  (flet ((r (r) (parse-to-pattern bind r)))
    (match rule
      ((<>Node type reads attrs)
       (let ((class (attribute->instance type)))
         (assert (keywordp class))
         `(make-node ,class ,type (list (gensym)) (list ,@(map 'list #'r reads)) ,@attrs)))
      (_ rule))))

(defun parse-rule (rule bind graph-bind)
  (declare (type list rule))
  (flet ((r (r) (parse-to-pattern bind r)))
    (match rule
      ((list from (symbol-eq "->") to)
       `((and
	  (<> *matched-bind* nil)
	  ,(find/replace-rules from graph-bind))
         (values
	  ,@(match to
	      ((<>Node type reads attrs)
               (let ((class (attribute->instance type)))
                 (assert (keywordp class))
	         `((list (make-node ,class ,type (node-writes ,bind) (list ,@(map 'list #'r reads)) ,@attrs)))))
	      ((list* (list node graph) body)
	       `((let ((,node ,bind)
		       (,graph ,graph-bind))
		   (declare (ignorable ,node ,graph))
		   (let* ((result (progn ,@body))
                          (result (if (node-p result) (list result) result)))
                     (when result
                       (assert (listp result))
                       (assert (= (length (the list (node-writes (car (last result))))) (length (the list (node-writes ,bind)))))
                       (setf (node-writes (car (last result))) (node-writes ,bind))
                       result)))))
              ((guard id (typep id 'symbol))
               `((let* ((val (id->value ,graph-bind ,id)))
                   (when (and val (not (eql (node-id val) (node-id ,bind))))
                     (let ((val (copy-node val)))
                       (setf (node-id val) (gensym "NID"))
                       (assert (= 1 (length (the list (node-writes val))) (length (the list (node-writes ,bind)))) () "-> symbol should only support when (length writes) == 1~%~a" ,graph-bind)
                       (setf (node-writes val) (copy-list (node-writes ,bind)))
                       (list val))))))
	      (_ `((let* ((result (progn ,to))
                          (result (if (node-p result) (list result) result)))
                     (when result
                       (assert (listp result))
                       (assert (= (length (the list (node-writes (car (last result))))) (length (the list (node-writes ,bind)))))
                       (setf (node-writes (car (last result))) (node-writes ,bind))
                       result)))))
	  *matched-bind*)))
      (_ (error "Follow this notation: (From_Pattern) -> (To_Pattern).~%~a" rule)))))

(defparameter *matched-bind* nil "a temporary place to store matched nodes during simplifying")
(defvar *node-top* nil)
(defvar *graph-bind* nil)
(defpattern <Rule> (&rest form) (find/replace-rules form '*graph-bind* t))
;; Simplifier Computation Order:
;; - FastGraph: O(n) where n = the depth of rewriting patterns
(defmacro defsimplifier ((name &key (speed 3)) &rest rules)
  "
```
(defsimplifier (name &key (speed 3)) &rest rules)
```
Defines a new simplifier named `name`. The defined function has a following form:
```
(name graph &key (no-verify nil) (return-changed-p nil))
```
The `graph` is a graph to simplify. The `no-verify` is a flag to skip the verification process. The `return-changed-p` is a flag to return the result of the simplification process, or a boolean indicating that the graph was changed during simplification process.. The `speed` is an optimization level. The `rules` are a list of simplification rules. Each rule has a form:

```
(Node_Name (Read_Args) Attrs)
```

(TODO: Documentation)

(See also: `./source/aasm/constant-folding.lisp`)
"
  (with-gensyms (simplifier-bind apply-bind1 apply-bind2 count-bind fast-graph-p seen changed-p counter n-nodes)
    (let ((node-top '*node-top*) (graph '*graph-bind*))
      `(defun ,name (,graph &key (no-verify nil) (return-changed-p nil) (debug-opt nil) &aux (,fast-graph-p (typep ,graph 'FastGraph)) (,seen nil) (,changed-p nil) (,counter 0) (,n-nodes (length (the list (graph-nodes ,graph)))))
         (declare (type graph ,graph)
		  (type boolean no-verify return-changed-p ,fast-graph-p ,changed-p)
		  (type list ,seen)
                  (type fixnum ,counter)
		  (optimize (speed ,speed)))
         (unless ,fast-graph-p (when (null (graph-nodes ,graph)) (return-from ,name)))
         (unless no-verify (verify-graph ,graph))
         (labels ((,simplifier-bind (,node-top ,count-bind
				     &aux
				       (*matched-bind* nil)
				       (fixed-writes-to
				        (when ,node-top
					  (loop for o in (graph-outputs ,graph)
					        if (find (the symbol o) (the list (node-writes ,node-top)) :test #'eql)
						  collect o))))
		    (declare (type list *matched-bind*))
                    ;; (when fixed-writes-to (return-from ,simplifier-bind))
		    (when (null ,node-top) (return-from ,simplifier-bind))
	            (incf ,counter)
		    (multiple-value-bind (replace-rule matched)
		        (match ,node-top
			  ,@(map 'list #'(lambda (x) (parse-rule x node-top graph)) rules)
			  (_ nil))
		      (when (and replace-rule matched)
		        (when (node-p replace-rule) (setf replace-rule (list replace-rule)))
		        ;; reject the replace-rule only when:
		        ;; - the original node writes the output to (graph-outputs graph)
		        ;; - the replaced node breaks this rule
		        (when fixed-writes-to ;; when the node is connected to graph-outputs
                          (dolist (w fixed-writes-to)
                            (when (null (find w replace-rule :key #'node-writes :test #'find))
                              (return-from ,simplifier-bind))))
		        (if ,fast-graph-p
			    (insert-nodes ,graph replace-rule)
			    (setf (graph-nodes ,graph)
				  (nconc
				   (subseq (graph-nodes ,graph) 0 ,count-bind)
				   replace-rule
				   (subseq (graph-nodes ,graph) (1+ ,count-bind)))))
		        (when (not ,fast-graph-p)
			  ;; this may not be required but reduce the number of nodes as many as possible
			  (dolist (r matched)
			    ;; the top node of matched patten is always replaced.
			    (when (and (not (eql r (node-id ,node-top))) (id->node ,graph r))
			      ;; Subsequent nodes are removed if they are not used.
			      (let ((writes (node-writes (id->node ,graph r))))
			        (when (every #'(lambda (w) (= (length (the list (id->users ,graph w))) 0)) writes)
				  (remnode ,graph r))))))
		        t)))
		  (,apply-bind1 (graph &aux (changed-p nil))
		    (dotimes (nth (length (the list (graph-nodes graph))))
		      (when (,simplifier-bind (nth nth (the list (graph-nodes graph))) nth)
		        (setf changed-p t)))
		    changed-p)
		  (,apply-bind2 (id)
		    (let ((node (gethash id (%graph-nodes-table ,graph))))
		      (when node
		        (when (null (find (the symbol id) (the list ,seen) :test #'eq))
			  (push id ,seen)
			  (when (,simplifier-bind node -1)
                            (some #'identity (map 'list #',apply-bind2 (node-reads node)))
                            (return-from ,apply-bind2 t))
                          (some #'identity (map 'list #',apply-bind2 (node-reads node))))))))
	   (if ,fast-graph-p
	       (loop while (and (some #'identity (map 'list #',apply-bind2 (graph-outputs ,graph)))
                                (progn (setf ,seen nil) (setf ,changed-p t))))
	       (loop while (and (,apply-bind1 ,graph) (setf ,changed-p t))))
           (when debug-opt (format t "~a: ~a calls for ~a nodes (~a%)~%" ',name ,counter ,n-nodes (float (* 100.0 (/ ,counter ,n-nodes)))))
	   (unless no-verify (verify-graph ,graph))
	   (when return-changed-p (return-from ,name ,changed-p))
	   ,graph)))))
