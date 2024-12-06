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
   :caten/codegen/renderer
   #:make-define-global)
  (:import-from
   :caten/air
   #:Graph
   #:Node
   #:id->value
   #:id->users
   #:getattr
   #:getattrs
   #:make-graph
   #:node-writes
   #:node-reads
   #:node-type
   #:make-node
   #:get-output-to
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
   #:relay-writes
   #:relay-read-iters
   #:relay-write-iters)
  (:import-from
   :caten/codegen/helpers
   #:nodes-depends-on)
  (:export
   #:schedule-item-write-define-global
   #:apply-rewriting-rules
   #:nodes-apply-static-gensym
   #:apply-static-gensym))

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
                            (node-reads n) (map 'list #'r (node-reads n))
			    (node-writes n) (map 'list #'r (node-writes n)))
		      n)))
      ;; Gather views for avm-fw-outputs and avm-bw-outputs, storing them in the :_output_type
      (let ((views (map 'list #'v (append (avm-fw-outputs avm) (avm-bw-outputs avm)))))
        (setf (avm-fw-outputs avm) (map 'list #'r (avm-fw-outputs avm))
	      (avm-bw-outputs avm) (map 'list #'r (avm-bw-outputs avm))
              (graph-outputs (avm-graph avm)) (map 'list #'r (graph-outputs (avm-graph avm))))
        (loop for id in (append (avm-fw-outputs avm) (avm-bw-outputs avm))
              for view in views
              for node = (id->value (avm-graph avm) id) do
                (when (null node) (warn "The output ~a is not found in the graph." id))
                (when (> (length view) 1) (warn "(No simplifier?) Detected multiple views in a single buffer: ~a~%Using the first one ~a~%" views (car view)))
                (when node (setf (getattr node :_output_type) (car view)))))
      (macrolet ((renew (accessor)
		   `(let ((new-table (make-hash-table)))
		      (maphash
		       #'(lambda (k v)
			   (setf (gethash (r k) new-table) v))
		       ,accessor)
		      (setf ,accessor new-table))))
	(renew (avm-id2tensor avm))
	(renew (avm-variables avm)))
      id2view)))

(defun wmma-relay-from (t1 tc nth)
  (make-inferred-type `(,(nth nth (relay-reads tc)) ,@(relay-reads t1)) (relay-writes tc)))
;; WMMA (c a b) <=> c = c + a * b (:reduction)
(defsimplifier
    (wmma-rewriter :speed 0)
    ((:Add ((:Mul (a b) :_type_relay t1) c) :reduction t :_type_relay t2) -> (:WMMA (c a b) :reduction t :_type_relay (wmma-relay-from t1 t2 1)))
    ((:Add (c (:Mul (a b) :_type_relay t1)) :reduction t :_type_relay t2) -> (:WMMA (c a b) :reduction t :_type_relay (wmma-relay-from t1 t2 0))))

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
;; TODO(hikettei): apply-static-gensym == nodes-apply-static-gensym. Remove one of them.
(defun apply-static-gensym (avm &optional (id2view))
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
      (dolist (node (append (graph-nodes (avm-graph avm)) (when id2view (alexandria:hash-table-values id2view))))
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

(defun nodes-apply-static-gensym (nodes)
  (let ((alias-table (make-hash-table))
        (val-count 0))
    (dolist (node nodes)
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
      (dolist (node nodes)
        (when (and (eql (node-type node) :Allocate)
                   (not (start-with-tid-p (car (node-writes node)))))
          ;; If :Allocate is labelled with a unique ID by user, keep using it.
          (let ((id (car (node-writes node))))
            (setf (gethash id alias-table) id)))
	(setf (node-writes node) (map 'list #'val-gensym (node-writes node))
	      (node-reads node) (map 'list #'val-gensym (node-reads node))))))
  nodes)
    
(defun apply-rewriting-rules (avm)
  (declare (type AVM avm))
  (let ((id2view (rewrite-views-as-buffer avm)))
    ;; (wmma-rewriter (avm-graph avm) :no-verify t)
    (apply-static-gensym avm id2view))
  (setf (avm-graph avm) (->graph (avm-graph avm)))
  avm)

(defmethod schedule-item-write-define-global ((schedule-item Node))
  "Inserts DEFINE_GLOBAL to the top of graph"
  (declare (type node schedule-item))
  (assert (eql (node-type schedule-item) :Schedule-Item))
  (assert (= (length (getattr schedule-item :storage-id-src)) (length (getattr schedule-item :read-types))))
  (assert (= (length (getattr schedule-item :storage-id-dst)) (length (getattr schedule-item :write-types))))
  (setf (getattr schedule-item :blueprint)
        (append
         ;; writes
         (loop for write in (getattr schedule-item :storage-id-dst)
               for wt in (getattr schedule-item :write-types)
               for nth upfrom 0
               collect
               (progn
                 (setf (nth nth (getattr schedule-item :write-types)) wt)
                 (make-define-global write (buffer-dtype wt) t :output (buffer-nrank wt))))
         ;; dynamic shapes
         (loop for item in (getattr schedule-item :dynamic-shapes)
               collect
               (make-define-global (car item) (cdr item) nil :shape 0))
         (loop for read in (getattr schedule-item :storage-id-src)
               for rt in (getattr schedule-item :read-types)
               collect
               (make-define-global read (buffer-dtype rt) (> (buffer-nrank rt) 0) :input (buffer-nrank rt)))
         (getattr schedule-item :blueprint))))
