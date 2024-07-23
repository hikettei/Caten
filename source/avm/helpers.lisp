(in-package :caten/avm)

(defun parse-allocate-node (alloc-node args)
  "Return: (values shape stride)"
  (declare (type node alloc-node))
  (assert (eql (node-type alloc-node) :allocate))
  (let ((nrank (getattr alloc-node :nrank)))
    (values (subseq args 0 nrank) (subseq args nrank))))

(defun parse-view-node (view-node args)
  (declare (type node view-node))
  (assert (eql (node-type view-node) :allocate))

  )
