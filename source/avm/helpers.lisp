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
  (let ((nrank (getattr view-node :nrank)))
    (values (subseq args 0 nrank) ;; shape
	    (subseq args nrank (* 2 nrank)) ;;view1
	    (subseq args (* 2 nrank) (* 3 nrank)) ;;view2
	    (subseq args (* 3 nrank) (* 4 nrank)) ;;view3
	    (subseq args (* 4 nrank) (* 5 nrank)) ;; stride
	    (getattr view-node :broadcast)))) ;; broadcast

