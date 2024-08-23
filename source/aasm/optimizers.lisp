(in-package :caten/aasm)

(defsimplifier
    (fuse-duplicated-move :speed 0)
    ((:MOVE ((:Allocate (~ s1) :nrank nrank :dtype dtype1) (:Allocate (~ s2) :dtype dtype2)))
     ->
     ((node graph)
      (when (and (eql dtype1 dtype2) (equal s1 s2))
	(make-node :Buffer :Allocate (node-writes node) s1 :nrank nrank :dtype dtype1)))))

(defun rewrite-duplicated-load (graph)
  "
Variables are allocated once.
tmp1 <- A
tmp2 <- A
=>
tmp1 <- A
tmp2 <- tmp1"
  (declare (type graph graph))
  (let ((variables (make-hash-table :test #'eql))
	(assigns))
    (flet ((getvar (id) (gethash id variables))
	   (setvar (id val) (setf (gethash id variables) val)))
      (setf (graph-nodes graph)
	    (loop for node in (graph-nodes graph)
		  for type = (node-type node)
		  if (and
		      (eql type :Load)
		      (let ((prev (id->value graph (car (node-reads node)))))
			(eql (node-type prev) :Allocate)
			))
		    collect
		    (if (getvar (getattr node :value))
			(let ((assign (make-node :Buffer :ASSIGN (list (car (node-writes node))) (list (getvar (getattr node :value))))))
			  (push assign assigns)
			  assign)
			(progn
			  (setvar (getattr node :value) (car (node-writes node)))
			  node))
		  else
		    collect node))
      (loop for assign in (nreverse assigns)
	    for key = (car (node-writes assign))
	    for to = (car (node-reads assign))
	    if (null (find to (graph-outputs graph))) do
	      (flet ((new (id) (if (eql id key) to id)))
		(dolist (node (graph-nodes graph))
		  (setf (node-reads node) (map 'list #'new (node-reads node))))
		(remnode graph (node-id assign)))))))

(defun optimize-aasm (graph)
  (declare (type graph graph))
  (fold-constant graph)
  (fuse-duplicated-move graph)
  (rewrite-duplicated-load graph)
  (verify-graph graph))
