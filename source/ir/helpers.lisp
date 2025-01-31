(in-package :caten/ir)

(defmacro range (from below &optional (by 1))
  `(loop for i from ,from below ,below by ,by collect i))

(defun %column-major-calc-strides (shape)
  (declare (type list shape))
  (flet ((const (n) (if (node-p n) n (%iconst n))))
    (let* ((num-dims (length shape))
           (strides (make-list num-dims :initial-element (%iconst 1))))
      (loop for i from 1 to (- num-dims 1) do
	(setf (nth i strides) (%mul (const (nth (- i 1) strides)) (const (nth (- i 1) shape)))))
      strides)))

(defun %row-major-calc-strides (shape)
  (declare (type list shape))
  (flet ((const (n) (if (node-p n) n (%iconst n))))
    (let* ((num-dims (length shape))
           (strides (make-list num-dims :initial-element (%iconst 1))))
      (loop for i downfrom (- num-dims 2) to 0 do
	(setf (nth i strides) (%mul (const (nth (+ i 1) strides)) (const (nth (+ i 1) shape)))))
      strides)))

(defun render-list (list) (apply #'concatenate 'string (butlast (loop for n in list append (list (format nil "~a" n) ", ")))))

(defun render-attrs (node &key (except-for nil))
  (let ((attrs (dump-into-list (node-attr node) :allow-unbound nil)))
    (if attrs
	(with-output-to-string (out)
	  (format out " where")
	  (dolist (k (getattrs node))
	    (when (and k (null (find k except-for)))
	      (format out " :~(~a~)=~a" k (getattr node k)))))
	"")))
;; ~~ with-context macro utils ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defparameter *ctx* nil)

(defmacro with-context (&rest ssa-forms)
  "
The macro (emit node) is used to emit a node into the graph locally binded to the *ctx* variable by the with-context macro.
e.g.:
```
(with-context
  (X (%add ... ...)))
```
will return a graph."
  `(let ((*ctx* (make-graph)))
     (with-asm ,@ssa-forms)
     (setf (graph-nodes *ctx*) (reverse (graph-nodes *ctx*)))
     *ctx*))

(defmacro with-context-nodes (&rest ssa-forms)
  `(graph-nodes (with-context ,@ssa-forms)))

(defmacro with-asm (&rest ssa-forms)
  "ssa-forms: (bind-to form)"
  `(let* (,@ssa-forms) (declare (ignorable ,@(map 'list #'car ssa-forms))) ,(caar (last ssa-forms))))

(defmacro emit (form)
  `(if *ctx*
       (let ((out ,form))
	 (push out (graph-nodes *ctx*))
	 out)
       ,form))

(defmacro with-context-from-parents ((&rest parents) &rest ssa-forms)
  "Basically the same as with-context, but it is useful when merging another graph into the returned graph."
  `(let ((g (with-context ,@ssa-forms)))
     (setf (graph-nodes g)
           (append 
            (loop for g in (flatten (list ,@parents))
                  if (graph-p g) append (graph-nodes g))
            (graph-nodes g)))
     g))
