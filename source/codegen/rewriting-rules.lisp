(defpackage :caten/codegen/rewriting-rules
  (:use :cl)
  (:import-from
   :caten/avm
   #:AVM
   #:avm-graph
   #:avm-fw-outputs
   #:avm-bw-outputs
   #:avm-id2tensor
   #:avm-variables

   #:Buffer
   #:copy-buffer
   #:buffer-shape
   #:buffer-nrank
   #:buffer-dtype
   #:buffer-stride
   #:buffer-views
   #:buffer-orig-buffer-shape
   #:buffer-inferred-permute)
  (:import-from
   :caten/air
   #:Graph
   #:Node
   #:id->value
   #:getattr
   #:getattrs
   #:node-writes
   #:node-reads
   #:node-type
   #:make-node
   #:graph-nodes
   #:graph-outputs
   #:Attr
   #:defsimplifier
   #:->fast-graph
   #:->graph)
  (:import-from
   :caten/codegen/shape-inference
   #:reveal-buffer
   #:make-inferred-type
   #:read-type-relay
   #:relay-reads
   #:relay-writes)
  (:export
   #:apply-rewriting-rules))

(in-package :caten/codegen/rewriting-rules)

(defun rewrite-views-as-buffer (avm)
  "Rewrite the node :VIEW as an object in the type-relay, so that the code generator can handle view as a buffer."
  (declare (type avm avm))
  (let ((rewrite-map (make-hash-table))
        (id2view (make-hash-table)))
    (loop for node in (graph-nodes (avm-graph avm))
	  if (eql (node-type node) :View) do
	    (setf (gethash (car (node-writes node)) rewrite-map) (car (node-reads node))
                  (gethash (car (node-writes node)) id2view) node))
    (labels ((r (id)
	       (if (and (gethash id rewrite-map) (not (eql (gethash id rewrite-map) id)))
		   (r (gethash id rewrite-map))
		   id))
             (v (id)
               (if (gethash id id2view)
                   (append (list (gethash id id2view)) (v (car (node-reads (gethash id id2view)))))
                   nil)))
      (setf (graph-nodes (avm-graph avm))
	    (loop for n in (graph-nodes (avm-graph avm))
		  unless (eql (node-type n) :View)
		    collect
		    (progn
                      (setf (getattr n :_read_views) (map 'list #'v (node-reads n))
                            (getattr n :_write_views) (map 'list #'v (node-writes n))
                            (node-reads n) (map 'list #'r (node-reads n))
			    (node-writes n) (map 'list #'r (node-writes n)))
		      n)
                  else
                    do (setf (node-reads n) (map 'list #'r (node-reads n))
                             (node-writes n) (map 'list #'r (node-writes n)))))
      (setf (avm-fw-outputs avm) (map 'list #'r (avm-fw-outputs avm))
	    (avm-bw-outputs avm) (map 'list #'r (avm-bw-outputs avm))
            (graph-outputs (avm-graph avm)) (map 'list #'r (graph-outputs (avm-graph avm))))
      (macrolet ((renew (accessor)
		   `(let ((new-table (make-hash-table)))
		      (maphash
		       #'(lambda (k v)
			   (setf (gethash (r k) new-table) v))
		       ,accessor)
		      (setf ,accessor new-table))))
	(renew (avm-id2tensor avm))
	(renew (avm-variables avm))))))

;; [TODO] 多分丸ごといらなくなる
(defun wmma-relay-from (t1 tc nth)
  (make-inferred-type `(,(nth nth (relay-reads tc)) ,@(relay-reads t1)) (relay-writes tc)))

(defun wmma-relay-from-contiguous (t1 t2 t3)
  (let ((a-base (second (relay-reads t3)))
	(a (second (relay-reads t1)))
	(b-base (third (relay-reads t3)))
	(b (second (relay-reads t2)))
	(c (car (relay-reads t3))))
    (flet ((manual-infer (from to)
	     ;; from = broadcasted and contiguous-applied buffer
	     ;; to = before the `from` operation buffer.
	     (let ((from (copy-buffer from)))
	       (unless (= (buffer-nrank from) (buffer-nrank to))
                 (return-from wmma-relay-from-contiguous nil))
	       (assert (eql (buffer-dtype from) (buffer-dtype to)))
	       (setf (buffer-stride from) (buffer-stride to)
		     (buffer-inferred-permute from) (buffer-inferred-permute to)
		     ;; merge views (use broadcasted one)
		     (buffer-views from) (loop for n upfrom 0 below (buffer-nrank from)
					       if (and (nth n (buffer-views from)) (nth n (buffer-views to)))
						 collect (if (fourth (nth n (buffer-views from)))
							     (nth n (buffer-views from))
							     (nth n (buffer-views to)))
					       else
						 collect (or (nth n (buffer-views from))
							     (nth n (buffer-views to)))))
	       from)))
      (make-inferred-type `(,c ,(manual-infer a-base a) ,(manual-infer b-base b)) (relay-writes t3)))))

(defmethod create-scalar-replace-pattern ((graph graph))
  "
Enumerates the following pattern:
```
*val_1 = 1.0;
val_2[0+0+...+0] = *val_1;
out[...] = f(val_2[0+0...+0]);
```
Storing to the dict in a format of:
```
Key:   val_2
Value: 1.0
```
val_2 is broadcasted, but equivalent to using a scalar number, Simplifying the scheduling process.
"
  (flet ((pattern1 (val_2)
	   (declare (type node val_2))
	   (let* ((val_2 (if (eql (node-type val_2) :MOVE) val_2 nil))
		  (val_1 (when val_2 (id->value graph (second (node-reads val_2)))))
		  (val_1 (if (and val_1 (eql (node-type val_1) :LOAD)) val_1 nil)))
	     (when (and val_1 val_2
			;; val_2[0+0+0+...] <- write access pattern is all broadcasted
			(every
			 #'(lambda (x) (eql x 1))
			 (buffer-shape (car (relay-writes (read-type-relay val_2)))))
			(every
			 #'null
			 (buffer-views (car (relay-writes (read-type-relay val_2)))))
			(= 0 (buffer-nrank (car (relay-writes (read-type-relay val_1)))))
			(eql (buffer-dtype (car (relay-writes (read-type-relay val_1))))
			     (buffer-dtype (car (relay-writes (read-type-relay val_2))))))
	       (values (car (node-writes val_2)) val_1))))
	 (pattern2 (val_2)
	   (declare (type node val_2))
	   (let* ((val_2 (if (eql (node-type val_2) :MOVE) val_2 nil))
		  (val_1 (when val_2 (id->value graph (second (node-reads val_2)))))
		  (val_1 (if (and val_1 (not (eql (node-type val_1) :LOAD))) val_1 nil)))
	     (when (and val_1 val_2
			;; val_2[0+0+0+...] <- write access pattern is all broadcasted
			(every
			 #'(lambda (x) (eql x 1))
			 (buffer-shape (car (relay-writes (read-type-relay val_2)))))
			(every
			 #'null
			 (buffer-views (car (relay-writes (read-type-relay val_2)))))
			(= 0 (buffer-nrank (car (relay-writes (read-type-relay val_1)))))
			(eql (buffer-dtype (car (relay-writes (read-type-relay val_1))))
			     (buffer-dtype (car (relay-writes (read-type-relay val_2))))))
	       (values (car (node-writes val_2)) val_1)))))
    (let ((output (make-hash-table)))
      (dolist (node (graph-nodes graph))
	(dolist (pattern (list #'pattern1 #'pattern2))
	  (multiple-value-bind (key value) (funcall pattern node)
	    (when (and key value)
	      (setf (gethash key output) value)))))
      output)))

(defmethod propagate-rebundant-loadp ((graph graph))
  "Rewrites the pattern:
```
*val_1 = 1.0;
val_2[0+0+...+0] = *val_1;
out[...] = f(val_2[0+0...+0]);
```
Into
```
*val_1 = 1.0;
val_2[0+0+...+0] = *val_1;
out[...] = f(*val_1);
```
`"
  (let ((patterns (create-scalar-replace-pattern graph)))
    (setf (graph-nodes graph)
	  (loop for node in (graph-nodes graph)
		for read-new = (map 'list #'(lambda (x) (gethash x patterns)) (node-reads node))
		collect
		(progn
		  (setf (node-reads node)
			(loop for r in (node-reads node)
			      for n in read-new
			      for nth upfrom 0
			      if n
				collect
				(progn
				  (setf (nth nth (relay-reads (read-type-relay node))) (car (relay-writes (read-type-relay n))))
				  (if (and (eql (node-type n) :LOAD)
					   (numberp (getattr n :value)))
				      (getattr n :value)
				      (car (node-writes n))))
			      else
				collect r))
		  node)))))

;; WMMA (c a b) <=> c = c + a * b (:reduction)
(defsimplifier
    (wmma-rewriter :speed 0)
    ((:Add ((:Mul (a b) :_type_relay t1) c) :reduction t :_type_relay t2) -> (:WMMA (c a b) :reduction t :_type_relay (wmma-relay-from t1 t2 1)))
    ((:Add (c (:Mul (a b) :_type_relay t1)) :reduction t :_type_relay t2) -> (:WMMA (c a b) :reduction t :_type_relay (wmma-relay-from t1 t2 0))))

(defsimplifier
    (contiguous-after-wmma :speed 0)
    ((:WMMA (c (:Move (_ a) :_type_relay t1) (:Move (_ b) :_type_relay t2)) :reduction reduction :_type_relay t3)
     ->
     ((node graph)
      (let ((type (wmma-relay-from-contiguous t1 t2 t3)))
        (when type
          (make-node :TernaryOps :WMMA (node-writes node) (list c a b) :reduction reduction :_type_relay type))))))

(defun sync-buffer (buffer f)
  (macrolet ((sync (name)
               `(setf (,name buffer) (map 'list (alexandria:compose f #'reveal-buffer) (,name buffer)))))
    (sync buffer-shape)
    (sync buffer-stride)
    (sync buffer-orig-buffer-shape)
    (flet ((sync-view (v)
             (if (null v) v
                 (list (funcall f (nth 0 v)) (funcall f (nth 1 v)) (funcall f (nth 2 v)) (nth 3 v)))))
      (setf (buffer-views buffer) (map 'list #'sync-view (buffer-views buffer))))))

(defun apply-static-gensym (avm)
  "Rewrites each read/write symbols to a unique and static symbol, improving the readability of the generated code when debugging."
  (declare (type avm avm))
  (let ((alias-table (make-hash-table))
	(val-count 0))
    (dolist (node (graph-nodes (avm-graph avm)))
      (when (eql (node-type node) :Load)
        (when (symbolp (getattr node :value))
          (setf (gethash (getattr node :value) alias-table) (getattr node :value)))))
    (labels ((val-gensym (id)
	       (if (symbolp id)
		   (or
		    (gethash id alias-table)
		    (let ((new-id (intern (format nil "val_~a" val-count))))
		      (setf (gethash id alias-table) new-id)
		      (incf val-count)
                      (setf (gethash new-id alias-table) new-id)
                      new-id))
		   id))
             (start-with-tid-p (sym &aux (str (princ-to-string sym)))
               (and (>= (length str) 3) (or (equalp "TID" (subseq str 0 3)) (equalp "SID" (subseq str 0 3))))))
      (dolist (node (graph-nodes (avm-graph avm)))
        (when (and (eql (node-type node) :Allocate)
                   (not (start-with-tid-p (car (node-writes node)))))
          ;; If :Allocate is labelled with a unique ID by user, keep using it.
          (let ((id (car (node-writes node))))
            (setf (gethash id alias-table) id)))
	(setf (node-writes node) (map 'list #'val-gensym (node-writes node))
	      (node-reads node) (map 'list #'val-gensym (node-reads node)))
        (dolist (r (append (relay-reads (read-type-relay node)) (relay-writes (read-type-relay node))))
          (when r
            (sync-buffer r #'val-gensym))))
      (setf (avm-fw-outputs avm) (map 'list #'val-gensym (avm-fw-outputs avm))
	    (avm-bw-outputs avm) (map 'list #'val-gensym (avm-bw-outputs avm)))
      (let ((new-id2tensor (make-hash-table)))
	(maphash
	 #'(lambda (k v)
	     (setf (gethash (val-gensym k) new-id2tensor) v))
	 (avm-id2tensor avm))
	(setf (avm-id2tensor avm) new-id2tensor))
      (let ((new-variables (make-hash-table)))
	(maphash
	 #'(lambda (k v)
	     (setf (gethash (val-gensym k) new-variables) v))
	 (avm-variables avm))
	(setf (avm-variables avm) new-variables
              (graph-outputs (avm-graph avm))
              (map 'list #'val-gensym (graph-outputs (avm-graph avm))))))))

(defun apply-rewriting-rules (avm)
  (declare (type AVM avm))
  (rewrite-views-as-buffer avm)
  ;;Try optimizing kernels without relying on them...
  ;(wmma-rewriter (avm-graph avm) :no-verify t)
  ;(contiguous-after-wmma (avm-graph avm) :no-verify t)
  (propagate-rebundant-loadp (avm-graph avm))
  (apply-static-gensym avm)
  (setf (avm-graph avm) (->graph (avm-graph avm)))
  avm)
