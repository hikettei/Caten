(in-package :caten/aasm)
;; View is eliminated when lowering
;; i.e.: view is used to generate aref index lsit.
(defun %view (base from to by broadcast &key (id (gensym "VID")))
  "Creates a view against base. (views are only created against the original buffer)"
  ;; Allocation w/o allocation
  (declare (type node base)
	   (type list from to by broadcast))
  (flet ((->const (x)
	   (if (node-p x)
	       x
	       (%iconst x))))
    (setf from (map 'list #'->const from)
	  to   (map 'list #'->const to)
	  by   (map 'list #'->const by)))
  (assert (and (every #'node-p to) (every #'node-p by) (every #'node-p from))
	  ()
	  "Assertion Failed: from/to/by must be a list of Node.")
  (assert (= (length from) (length to) (length by) (length broadcast))
	  ()
	  "Assertion Failed: the rank must be determined before the compilation.
nrank=~a
shape=~a
stride=~a
broadcast=~a"
	  from to by broadcast)
  (let ((nrank (length from)))
    (emit (make-node :Buffer :View
		     (list id)
		     (append (list (node->id base))
			     (map 'list #'node->id from)
			     (map 'list #'node->id to)
			     (map 'list #'node->id by))
		     :nrank nrank :broadcast broadcast))))

(defun %reshape (x shape &key (id (gensym "RID")))
  "In-placed reshape"
  (declare (type node x)
	   (type list shape))
  (flet ((->const (x) (if (node-p x) x (%iconst x))))
    (setf shape (map 'list #'->const shape)))
  
  (assert (every #'node-p shape)
	  ()
	  "Assertion Failed: shape must be a list of Node.")
  (emit (make-node :Buffer :View (list id)
		   (append
		    (list (node->id x))
		    ;; from
		    (loop for i in shape collect (node->id (%iconst 0)))
		    ;; to
		    (map 'list #'node->id shape)
		    ;; by
		    (loop for i in shape collect (node->id (%iconst 1))))
		   :nrank (length shape)
		   :broadcast (loop for i in shape collect nil))))

(defun %permute ())
(defun %bitcast ())
