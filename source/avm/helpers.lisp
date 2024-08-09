(in-package :caten/avm)

(defun parse-allocate-node (alloc-node args)
  "Return: (values shape stride)"
  (declare (type node alloc-node))
  (assert (eql (node-type alloc-node) :allocate))
  (let ((nrank (getattr alloc-node :nrank)))
    (values (subseq args 0 nrank) (subseq args nrank))))

(defun parse-view-node (view-node args)
  (declare (type node view-node))
  (assert (eql (node-type view-node) :view))
  (flet ((subseq1p (list from to) (subseq list (1+ from) (1+ to))))
    (let ((nrank (getattr view-node :nrank)))
      (values (subseq1p args 0 nrank) ;; shape
	      (subseq1p args nrank (* 2 nrank)) ;;view1
	      (subseq1p args (* 2 nrank) (* 3 nrank)) ;;view2
	      (subseq1p args (* 3 nrank) (* 4 nrank)) ;;view3
	      (subseq1p args (* 4 nrank) (* 5 nrank)) ;; stride
	      (getattr view-node :broadcast))))) ;; broadcast

(defmacro range (from below &optional (by 1))
  `(loop for i from ,from below ,below by ,by collect i))

(defun reveal-buffer (object)
  (if (buffer-p object)
      (buffer-value object)
      object))

