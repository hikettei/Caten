(in-package :caten/ajit)
;; A helpers to render the isl object.

(defmacro define-isl-object (print-name docstring ((&rest args) &rest slots) &body body)
  (declare (type string print-name))
  (let* ((name (intern (string-upcase print-name)))
	 (constructor (symb 'make- name)))
    `(progn
       (defstruct (,name
		   (:constructor ,constructor (,@args)))
	 ,docstring
	 ,@slots)
       ;; [TODO] Confirm no memory-leak here...
       (defmethod form ((c ,name)) ,@body)
       (defmethod print-object ((c ,name) stream) (format stream "~a: ~a" ,print-name (form c))))))

(define-isl-object "IConstraint"
    "Equivalent to `upfrom <= var < below`"
    ((var upfrom below)
     (var var :type symbol)
     (upfrom upfrom :type integer-t)
     (below below :type integer-t))
  (format nil "~(~a~) <= ~(~a~) < ~(~a~)"
	  (iconstraint-upfrom c)
	  (iconstraint-var c)
	  (iconstraint-below c)))

(defun iconstraint-scalar-p (c)
  (and (eql 0 (iconstraint-upfrom c)) (eql 1 (iconstraint-below c))))

(define-isl-object "IUnion"
    "Union: [m] where m = alpha * index + beta"
    ((indices &optional (alphas '(1)) (betas '(0)))
     (indices indices :type list)
     (alphas alphas :type list)
     (betas betas :type list))
  (with-output-to-string (out)
    (with-slots ((indices indices) (alphas alphas) (betas betas)) c
      (format out "[ ")
      (loop for index in indices for alpha in alphas for beta in betas do
	(cond
	  ((every #'numberp `(,index ,alpha ,beta))
	   (format out "~a" (+ (* index alpha) beta)))
	  ((and (numberp alpha) (numberp beta) (= alpha 1) (= beta 0))
	   (format out "~(~a~)" index))
	  ((and (numberp alpha) (numberp beta) (= 0 beta alpha))
	   (format out "0"))
	  ((and (numberp beta) (= beta 0))
	   (format out "~(~a~)*~(~a~)" index alpha))
	  ((and (numberp alpha) (= alpha 0))
	   (format out "~(~a~)" beta))
	  (T
	   (format out "(~(~a~)*~(~a~)+~(~a~))" index alpha beta)))
	(format out "+"))
      (format out "0 ]"))))

(define-isl-object "IMap"
    "IMap: { Union -> Union }"
    ((union-read union-write)
     (union-read union-read :type IUnion)
     (union-map union-write :type IUnion))
  (with-slots ((read union-read) (write union-write)) c
    (format nil "{ ~a -> ~a }" (form read) (form write))))

(define-isl-object "CUnion"
    "CUnion (Constrainted Union): { Union : Constraint }"
    ((union constraint)
     (union union :type IUnion)
     (constraint constraint :type IConstraint))
  (with-slots ((u union) (cnst constraint)) c
    (format nil "{ ~a : ~a }" (form u) (form cnst))))

(defun vm-instruction-p (node)
  "Add more classes here if you have a certain node that do not desired to be involved."
  ;; :IR = :FOR :ENDFOR
  (and
   (not (eql (node-class node) :IR))
   (not (eql (node-type node) :Allocate))))

;; ~~ AREF ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun isl-access-expr-no-stride (gid stride upfrom by broadcast-p)
  (declare (ignore stride))
  (assert (numberp by) () "`By` should be a constant otherwise Caten cannot create a quasiaffine constraint.")
  (if broadcast-p
      (make-const 0 nil)
      (simplify-expr
       (make-expr
	:ADD
	(make-expr :MUL (make-const gid nil) (make-const by nil))
	(make-const upfrom nil)))))

(defun isl-access-expr (gid stride upfrom by broadcast-p)
  (if broadcast-p
      (make-const 0 nil)
      (simplify-expr
       (make-expr
	:ADD
	(make-expr
	 :MUL
	 (make-expr :MUL (make-const stride nil) (make-const by nil))
	 (make-const gid nil))
	(make-expr :MUL (make-const upfrom nil) (make-const stride nil))))))

(defun render-isl-aref (buffer &key (genid #'gid) (indexing #'isl-access-expr) (flatten nil) (strides nil) (use-permute nil) (upper nil) (mutate-scalar nil) &aux (c 0))
  "Renders the stride computation for ISL:
```
A[stride1 * view_info1 * index_component_0 + bias1 + stride2 * view_info2 * index_component_1 + bias2 + ...]
```
"
  (declare (type buffer buffer))
  (let ((indices
	  (loop with order = (if (and use-permute (buffer-inferred-permute buffer))
				 (buffer-inferred-permute buffer)
				 (range 0 (buffer-nrank buffer)))
		for nth in order
		for stride-nth in (or strides (buffer-stride buffer))
		for size   = (nth nth (buffer-shape buffer))
		for view   = (nth nth (buffer-views buffer))
		for stride = (reveal-buffer stride-nth)
		for upfrom = (reveal-buffer (or (nth 0 view) 0))
		for by     = (reveal-buffer (or (nth 2 view) 1))
		for broadcast-p = (nth 3 view)
		for gid = (funcall genid nth)
		do (incf c)
		if (and mutate-scalar (eql size 1))
		  collect (make-const 0 nil)
		else
		  collect (funcall indexing gid stride upfrom by broadcast-p))))
    (if flatten
	(apply
	 #'concatenate 'string
	 (butlast
          (loop for idx in (nconc indices (when upper (loop repeat (- upper c) collect (make-expr 0 nil))))
		append (list (render-expr (default-device :clang) idx) ", "))))
	(flet ((add (x y) (make-expr :ADD x y)))
	  (if (null indices)
	      nil
	      (simplify-expr (reduce #'add indices)))))))
;; ~~ DOMAIN ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun render-domain (pipeline target-keys &key (depends-on nil))
  "Render the domain notation from the scheduled subgraphs
```
Domain [depends-on] -> {
  Sched_0_ID(loop_factors_0) : IConstraint_0;
  Sched_1_ID(loop_factors_1) : IConstraint_1;
                 ...
}
```
Pipeline: A hash-table where keys and values are: {T_ID[Fixnum] -> Scheduled_Subgrpah[Graph]}"
  (declare (type list depends-on) (type hash-table pipeline))
  (with-output-to-string (out)
    ;; renders depends-on
    (format out "[~(~a~)] -> {~%" (render-list depends-on))
    (mapc
     #'(lambda (timestamp &aux (subgraph (gethash timestamp pipeline)))
	 (let* ((loop-factors (graph->loop-factors subgraph))
		(constraints
		  (loop for node in (graph-nodes subgraph)
			if (eql (node-type node) :IR/FOR)
			  collect
			  (progn
			    (assert (= 1 (nth 2 (node-reads node))) () "Loop steps should be optimized by the polyhedral compiler. Set=1.")
			    (make-iconstraint (car (node-writes node)) (nth 0 (node-reads node)) (nth 1 (node-reads node)))))))
	   (if loop-factors
	       (progn
		 (format out "  T~a[~(~a~)]" timestamp
			 (render-list
			  (loop for lf in loop-factors
				for c in constraints
				for scal-p = (iconstraint-scalar-p c)
				if scal-p
				  collect (format nil "~a = 0" lf)
				else
				  collect lf)))
		 (format out " : ")
		 (let ((c (apply #'concatenate 'string (butlast (loop for c in constraints unless (iconstraint-scalar-p c) append (list (form c) " and "))))))
		   (format out "~a" (if (string= c "") "true" c)))
		 (format out ";~%"))
	       (format out "  T~a[];~%" timestamp))))
     target-keys)
    (format out "}")))
;; ~~ Access relation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun render-access (alias-f keys mode pipeline &key (depends-on nil) &aux (kernel-rank (pipeline/upper-nrank pipeline)))
  "Render the read/write accessing relation ship in the following notation:
```
[depends-on] -> {
    Sched_0_ID[read_index] -> Tensor_ID_N[strided_access_idx];
        ...
}
```
"
  (declare (type list depends-on)
	   (type (member :read :write) mode)
	   (type hash-table pipeline))
  (with-output-to-string (out)
    (format out "[~(~a~)] -> {~%" (render-list depends-on))
    (mapc
     #'(lambda (timestamp &aux (subgraph (gethash timestamp pipeline)))
	 (let* ((lf (graph->loop-factors subgraph))
		(lf-orig (graph->loop-factors subgraph))
		(occur-from
		  (format nil "T~a[~(~a~)]" ;; = 0
			  timestamp (render-list lf)))
		(scalar (apply #'concatenate 'string (butlast (loop repeat kernel-rank append (list "0" ", "))))))
	   (flet ((pad ()
		    (if (= kernel-rank (length lf-orig))
			""
			(format nil ", ~a"
				(apply #'concatenate 'string
				       (butlast
					(loop repeat (- kernel-rank (length lf-orig)) append (list "0" ", "))))))))
	     (dolist (node (graph-nodes subgraph))
	       (when (not (eql (node-class node) :IR))
		 (loop for r in (map 'list alias-f (funcall (if (eql mode :read) #'node-reads #'node-writes) node))
		       for rt in (funcall (if (eql mode :read) #'relay-reads #'relay-writes) (read-type-relay node)) do
			 ;; When node has a :reduction
			 (when (symbolp r)
			   (if (null lf)
			       (format out "  ~a -> ~(~a~)[~a];~%" occur-from r scalar)
			       (when (vm-instruction-p node)
				 (let ((access (render-isl-aref rt :indexing #'isl-access-expr-no-stride :mutate-scalar t :flatten t :use-permute t)))
				   (if (string= access "")
				       (format out "  ~a -> ~(~a~)[~a];~%" occur-from r scalar)
				       (format out "  ~a -> ~(~a~)[~(~a~)~a];~%" occur-from r access (pad))))))))
		 ;; Symbols for computing the stride
		 (when (and node (eql mode :read))
		   (let* ((symbols
			    (loop for buff in `(,@(relay-reads (read-type-relay node)) ,@(relay-writes (read-type-relay node)))
				  if buff
				    append (append (buffer-shape buff) (buffer-stride buff) (apply #'append (buffer-views buff)))))
			  (symbols
			    (loop for s1 in symbols
				  for s = (reveal-buffer s1)
				  if (and (symbolp s) (not (eql s t)) (not (eql s nil)))
				    collect s)))
		     (dolist (s symbols) (format out "  ~a -> ~(~a~)[~a];~%" occur-from s scalar)))))))))
     keys)
    (format out "}")))

(defmethod render-isl-initial-schedule ((blueprint Graph) (pipeline hash-table) (node->id hash-table) (depends-on list))
  (let ((lex (make-hash-table)) (seen-timestamps) (vars-global) (rank (pipeline/upper-nrank pipeline)))
    (labels ((steps (id)
	       (setf (gethash id lex) (1+ (ensure-gethash id lex 0))))
	     (node->time (node)
	       (gethash (node-id node) node->id))
	     (pad (list node)
	       (padding-list list (+ 1 (* 2 rank)) :with (node->time node)))
	     (->schedule (vars node)
	       (pad
		(loop for v1 in (reverse vars-global)
		      for time1 = (ensure-gethash v1 lex 0)
		      for decl-p = (find v1 vars)
		      for time = (if decl-p time1 (1+ time1))
		      for v = (if decl-p v1 0)
		      append (list time v))
		node)))
      (with-output-to-string (out)
	(format out "[~(~a~)] -> " (render-list depends-on))
	(format out "{~%")
	(loop with vars = nil
	      for node in (graph-nodes blueprint)
	      if (eql (node-type node) :IR/FOR)
		do (steps (car (node-writes node)))
		   (push (car (node-writes node)) vars)
		   (when (null (find (car (node-writes node)) vars-global))
		     (push (car (node-writes node)) vars-global))
	      else if (eql (node-type node) :IR/ENDFOR)
		     do (setf vars (remove (car (node-reads node)) vars))
	      else
		if (null (find (node->time node) seen-timestamps))
		  do (push (node->time node) seen-timestamps)
		     (format out "  T~a[~(~a~)] -> [~(~a~)];~%"
			     (node->time node)
			     (render-list (reverse vars))
			     (render-list (->schedule (reverse vars) node))))
	(format out "}")))))
