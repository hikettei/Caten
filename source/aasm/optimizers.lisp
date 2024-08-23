(in-package :caten/aasm)

(defsimplifier
    (fuse-duplicated-store :speed 0)
    ((:Store ((:Allocate (~ s1) :nrank nrank :dtype dtype1) (:Allocate (~ s2) :dtype dtype2)))
     ->
     ((node graph)
      (when (and (eql dtype1 dtype2) (equal s1 s2))
	(make-node :Buffer :Allocate (node-writes node) s1 :nrank nrank :dtype dtype1)))))

(defun remove-duplicated-load (graph)
  "
Variables are allocated once.
tmp1 <- A
tmp2 <- A
=>
tmp1 <- A
tmp2 <- tmp1"
  (declare (type graph graph))
  (let ((variables (make-hash-table :test #'eql)))
    (flet ((getvar (id) (gethash id variables))
	   (setvar (id val) (setf (gethash id variables) val)))
      (setf (graph-nodes graph)
	    (loop for node in (graph-nodes graph)
		  for type = (node-type node)
		  if (eql type :Load)
		    collect
		    (if (getvar (getattr node :value))
			(progn
			  
			  )
			(progn
			  (setvar (getattr node :value) (car (node-writes node)))
			  node)))))))

(defun optimize-aasm (graph)
  (declare (type graph graph))
  (fold-constant graph)
  (fuse-duplicated-store graph))
