(in-package :caten/aasm)
;; View is eliminated when lowering
;; i.e.: view is used to generate aref index list
(defun infer-tensor-info (graph id)
  "Return: [nrank, shape, stride, dtype, (view_from, view_to, view_by, broadcast)]"
  (declare (type graph graph) (type symbol id) (optimize (speed 3)))
  ;; needs fold-constant?
  (let ((nrank) (shape) (dtype) (view-from) (view-to) (view-by) (broadcast) (stride))
    (flet ((complete? ()
	     (or (and nrank (= nrank 0) dtype)
		 (and nrank shape dtype view-from view-to view-by broadcast stride)))
	   (identity1 (x) (and (integerp x) (= x -1)))
	   (base? ()
	     (or (and nrank (= nrank 0) dtype)
		 (and nrank shape dtype stride)))
	   (finish ()
	     (return-from infer-tensor-info (values nrank shape stride dtype (list view-from view-to view-by broadcast)))))
      (macrolet ((update (place value &key (test nil))
		   `(if (null ,place)
			(setf ,place ,value)
			,(when test `(assert (= ,place ,value) () "infer-shape: ~a is inconsistent (~a vs ~a)" ,test ,place ,value))))
		 (subseq1p (list from to) `(subseq ,list (1+ (the fixnum ,from)) (1+ (the fixnum ,to)))))
	(labels ((helper (x &aux (node (id->value graph x)))
		   (when node
		     (case (node-type node)
		       (:Cast
			(let* ((dtype1 (getattr node :dtype)))
			  (update dtype dtype1)))
		       (:View
			(let* ((nrank1     (getattr node :nrank))
			       (broadcast1 (getattr node :broadcast))
			       (shape1     (subseq1p (node-reads node) 0 nrank1))
			       (view-from1 (subseq1p (node-reads node) nrank1 (+ nrank1 nrank1)))
			       (view-to1   (subseq1p (node-reads node) (+ nrank1 nrank1) (* 3 nrank1)))
			       (view-by1   (subseq1p (node-reads node) (* 3 nrank1) (* 4 nrank1)))
			       (stride1    (subseq1p (node-reads node) (* 4 nrank1) (* 5 nrank1))))			       
			  (update nrank nrank1)
			  (update broadcast broadcast1)
			  (update view-from view-from1)
			  (update view-to view-to1)
			  (update view-by view-by1)
			  (when (not (some #'identity1 shape1))
			    (update shape shape1))
			  (when (not (some #'identity1 stride1))
			    (update stride stride1))
			  (when (complete?) (finish))
			  (helper (car (node-reads node)))))
		       (:Allocate
			(let* ((nrank1 (getattr node :nrank))
			       (dtype1 (getattr node :dtype))
			       (shape1 (subseq (node-reads node) 0 nrank1))
			       (stride1 (subseq (node-reads node) nrank1 (+ nrank1 nrank1))))
			  (update nrank nrank1)
			  (update dtype dtype1)
			  (update shape shape1)
			  (update stride stride1)))
		       (:Where
			(when (complete?) (finish))
			(dolist (r (cdr (node-reads node))) (helper r)))
		       (otherwise
			(when (complete?) (finish))
			(dolist (r (node-reads node)) (helper r)))))))
	  (helper id))
	(when (base?) (finish))
	(finish)
;;	(error "infer-shape: Failed to infer the shape of ~a.
;;Results:
;;nrank=~a, shape=~a, dtype=~a, view-from=~a, view-to=~a, view-by=~a, broadcast=~a, stride=~a"
;;	       id nrank shape dtype view-from view-to view-by broadcast stride)
	))))

(defun %view (base shape from to by broadcast stride &key (id (gensym "VID")) (permute nil))
  "Creates a view against base. (views are only created against the original buffer)
Permute is an optional parameter and does nothing, but MUST required to enable Polyhedral Compiler, to inference an index of iteration by type-relay.lisp"
  ;; Allocation w/o allocation
  (declare (type node base)
	   (type list from to by broadcast shape stride permute))
  (flet ((->const (x)
	   (if (node-p x)
	       x
	       (%iconst x))))
    (setf from   (map 'list #'->const from)
	  to     (map 'list #'->const to)
	  by     (map 'list #'->const by)
	  shape  (map 'list #'->const shape)
	  stride (map 'list #'->const stride)))
  (assert (and (every #'node-p to) (every #'node-p by) (every #'node-p from))
	  ()
	  "Assertion Failed: from/to/by must be a list of Node.")
  (assert (= (length from) (length to) (length by) (length broadcast) (length shape) (length stride))
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
			     (map 'list #'node->id shape)
			     (map 'list #'node->id from)
			     (map 'list #'node->id to)
			     (map 'list #'node->id by)
			     (map 'list #'node->id stride))
		     :nrank nrank :broadcast broadcast :permute permute))))
;; Not recommended; use %view instead
(defun %reshape (x shape &key (id (gensym "RID")) (order :row))
  "In-placed reshape"
  (declare (type node x)
	   (type list shape)
	   (type (member :row :column) order))
  (flet ((->const (x) (if (node-p x) x (%iconst x))))
    (setf shape (map 'list #'->const shape)))
  
  (assert (every #'node-p shape)
	  ()
	  "Assertion Failed: shape must be a list of Node.")
  (emit (make-node :Buffer :View (list id)
		   (append
		    (list (node->id x))
		    ;;shape
		    (map 'list #'node->id shape)
		    ;; from
		    (loop for i in shape collect (node->id (%iconst 0)))
		    ;; to
		    (map 'list #'node->id shape)
		    ;; by
		    (loop for i in shape collect (node->id (%iconst 1)))
		    ;; stride
		    (map 'list #'node->id (%stride shape order)))
		   :nrank (length shape)
		   :broadcast (loop for i in shape collect nil))))

(defmethod print-node ((node Node) (id (eql :View)))
  (let ((nrank (getattr node :nrank)))
    (when (and nrank (not (= nrank 0)) (eql (node-class node) :Buffer))
      (flet ((subseq1p (x y z) (subseq x (1+ y) (1+ z))))
	(format nil "<~a : ~a <- (~a, shape=(~a), views=(~a), stride=(~a)~a)>"
		(node-type node)
		(render-list (node-writes node))
		(car (node-reads node))
		(render-list (subseq1p (node-reads node) 0 nrank))
		(let ((upfrom (subseq1p (node-reads node) nrank (* 2 nrank)))
		      (below (subseq1p (node-reads node) (* 2 nrank) (* 3 nrank)))
		      (by (subseq1p (node-reads node) (* 3 nrank) (* 4 nrank)))
		      (bc (getattr node :broadcast)))
		  (render-list
		   (map 'list #'(lambda (x y z l) (format nil "(~a)" (render-list (list x y z l)))) upfrom below by bc)))
		(render-list (subseq1p (node-reads node) (* 4 nrank) (* 5 nrank)))
		(if (getattr node :permute)
		    (format nil ", permute=~a" (getattr node :permute))
		    ""))))))
