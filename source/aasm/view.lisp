(in-package :caten/aasm)
;; View is eliminated when lowering
;; i.e.: view is used to generate aref index list
(defun infer-tensor-info (graph id)
  "Return: [nrank, shape, stride, dtype, (view_from, view_to, view_by, broadcast)]"
  (declare (type graph graph) (type symbol id) (optimize (speed 3)))
  ;; needs fold-constant
  (let ((nrank) (shape) (dtype) (view-from) (view-to) (view-by) (broadcast) (stride))
    (flet ((complete? ()
	     (or (and nrank (= nrank 0) dtype)
		 (and nrank shape dtype view-from view-to view-by broadcast stride)))
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
		       (:View
			(let* ((nrank1     (getattr node :nrank))
			       (broadcast1 (getattr node :broadcast))
			       (restride   (the fixnum (getattr node :restride)))
			       (view-from1 (subseq1p (node-reads node) 0 nrank1))
			       (view-to1   (subseq1p (node-reads node) nrank1 (+ nrank1 nrank1)))
			       (view-by1   (subseq1p (node-reads node) (+ nrank1 nrank1) (* 3 nrank1)))
			       (stride1    (when (= restride 1) (subseq1p (node-reads node) (* 3 nrank1) (* 4 nrank1)))))
			  (update nrank nrank1)
			  (update broadcast broadcast1)
			  (update view-from view-from1)
			  (update view-to view-to1)
			  (update view-by view-by1)
			  (when restride (update stride stride1))
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
		       (otherwise
			(when (complete?) (finish))
			(dolist (r (node-reads node)) (helper r)))))))
	  (helper id))
	(when (base?) (finish))
	(error "infer-shape: Failed to infer the shape of ~a.
Results:
nrank=~a, shape=~a, dtype=~a, view-from=~a, view-to=~a, view-by=~a, broadcast=~a, stride=~a"
	       id nrank shape dtype view-from view-to view-by broadcast stride)))))

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
		     :nrank nrank :broadcast broadcast :restride 0))))

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
		   :broadcast (loop for i in shape collect nil) :restride 0)))

(defun %restride (x stride &key (id (gensym "RID")))
  (declare (type node x) (type list stride))
  (flet ((->const (x) (if (node-p x) x (%iconst x))))
    (setf stride (map 'list #'->const stride)))

  (assert (every #'node-p stride)
	  ()
	  "Assertion Failed: stride must be a list of Node.")
  )

;; infer view infer stride infer shapeを実装+test
;; できたらfrontendでlower_gemm, gemm-ctxを実装する
(defun %permute ())
(defun %bitcast ())
