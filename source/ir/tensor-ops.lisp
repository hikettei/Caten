(in-package :caten/ir)

(defclass TensorGraph (FastGraph) nil)

(defmethod print-object ((graph TensorGraph) stream)
  (print-unreadable-object (graph stream)
    (format stream "TensorGraph{
  tir::function (~(~a~)) {
~a    return ~(~a~); // graph-outputs
  }
}"
	    (render-list (graph-seen graph))
            (let ((offset 0) (nodes (graph-nodes (->graph-with-tpsort graph))))
              (dolist (node nodes)
                (setf offset (max offset (length (render-list (list (car (node-writes node))))))))
              (flet ((indent (node)
                       (with-output-to-string (offsets)
                         (dotimes (i (1+ (max 0 (- offset (length (render-list (list (car (node-writes node)))))))))
                           (princ " " offsets))))
                     (render-type-relay (node)
                       (if (node-type-relay node)
                           (with-output-to-string (typ)
                             (format typ " // ")
                             (loop for wt in (relay-writes (read-type-relay node))
                                   if wt
                                     do (format typ "~(~a~)(~a) " (tensor-relay-dtype wt) (render-list (tensor-relay-shape wt))))
                             (when (every #'identity (relay-reads (read-type-relay node)))
                               (format typ "<- "))
                             (loop for rt in (relay-reads (read-type-relay node))
                                   if rt
                                     do (format typ "~(~a~)(~a) " (tensor-relay-dtype rt) (render-list (tensor-relay-shape rt)))))
                           "")))
	        (with-output-to-string (out)
	          (dolist (node nodes)
                    (case (node-type node)
                      (:ALLOCATE
                       (let ((nrank (getattr node :nrank)))
                         (format out "    ~(~a~)~a= allocate(shape=(~(~a~)), stride=(~(~a~)), dtype=~(~a~));~a~%"
                                 (render-list (node-writes node))
                                 (indent node)
                                 (render-list (subseq (node-reads node) 0 nrank))
                                 (render-list (subseq (node-reads node) nrank (* 2 nrank)))
                                 (getattr node :dtype)
                                 (render-type-relay node))))
                      (:VIEW
                       (let ((nrank (getattr node :nrank)))
                         (format out "    ~(~a~)~a= view(~(~a~), shape=(~(~a~)), β=(~(~a~)), α=(~(~a~)), stride=(~(~a~)));~a~%"
                                 (render-list (node-writes node))
                                 (indent node)
                                 (car (node-reads node))
                                 (render-list (subseq (node-reads node) 1 (1+ nrank)))
                                 (render-list (subseq (node-reads node) (1+ nrank) (1+ (* 2 nrank))))
                                 (render-list (subseq (node-reads node) (1+ (* 2 nrank)) (1+ (* 3 nrank))))
                                 (render-list (subseq (node-reads node) (1+ (* 3 nrank)) (1+ (* 4 nrank))))
                                 (render-type-relay node))))
                      (otherwise
                       (format out "    ~(~a~)~a= ~(~a~)(~(~a~)~a);~a~%"
                               (render-list (node-writes node))
                               (indent node)
                               (node-type node)
                               (render-list (node-reads node))
                               (render-attrs node :except-for `(:from :pool))
                               (render-type-relay node))))))))
            (render-list (graph-outputs graph)))))

(defparameter *default-order* (ctx:getenv :DEFAULT_ORDER))
(defparameter *default-float* (ctx:getenv :DEFAULT_FLOAT))
(defparameter *default-int*   (ctx:getenv :DEFAULT_INT))
(defparameter *default-uint*  (ctx:getenv :DEFAULT_UINT))

(defun size-p (x) (or (integerp x) (node-p x)))
(defun node->id1 (x) (if (or (symbolp x) (integerp x)) x (node->id x)))

(defun %alloc (nrank shape stride &key (dtype *default-float*) (id (lgensym "ALLOC_")) (from nil))
  "Equivalent to `dtype i[shape];`
From can be either of nil or Buffer, if buffer is specified, it loads the content of buffer instead of allocating it."
  (declare (type fixnum nrank)
	   (type list shape stride)
	   (type dtype-t dtype)
	   (type symbol id))
  (assert (every #'size-p shape) () "Assertion Failed: Shapes must be a list of Node or integer.")
  (assert (every #'size-p stride) () "Assertion Failed: Strides must be a list of Node or integer.")
  (assert (= nrank (length shape) (length stride)) () "Assertion Failed: the rank must be determined before the compilation.
nrank=~a
shape=~a
stride=~a" nrank shape stride)
  (multiple-value-bind (shape stride)
      (values
       (map 'list #'node->id1 shape)
       (map 'list #'node->id1 stride))
    (emit (make-node :Buffer :Allocate (list id) (append shape stride) :nrank nrank :dtype dtype :from from))))

(defun %salloc (&key (dtype *default-float*) (id (lgensym "ALLOC_")))
  "Equivalent to: `dtype i;` but nrank=0"
  (declare (type dtype-t dtype)
	   (type symbol id))
  (emit (make-node :Buffer :Allocate (list id) nil :nrank 0 :dtype dtype)))

(defun %load (node value &key (id (lgensym "LOAD_")))
  "Equivalent to: `i = initial_value;` where i is a scalar of tensor.
If i is a tensor, %load fills the visible area of i with value."
  (declare (type Node node))
  (assert (eql (node-class node) :Buffer)   ())
  (assert (eql (node-type  node) :Allocate) ())
  (let* ((value (if (numberp value) (dtype/cast value (getattr node :dtype)) value)))
    (emit (make-node :Buffer :Load (list id) (list (node->id node)) :value value))))

(defmethod print-node ((node Node) (id (eql :Allocate)))
  (let ((nrank (getattr node :nrank)))
    (when (and nrank (not (= nrank 0)))
      (format nil "<~a : ~a <- (shape=(~a), stride=(~a)~a)~a>"
	      (node-type node)
	      (render-list (node-writes node))
	      (render-list (subseq (node-reads node) 0 nrank))
	      (render-list (subseq (node-reads node) nrank))
	      (if (getattr node :from)
		  (let ((buffer (getattr node :from)))
		    (if (symbolp buffer)
			(format nil ", from=<~a, Realized Array>" buffer)
			(format nil ", from=<~a Realized Array>" (uiop:symbol-call :caten/runtime :buffer-shape buffer))))
		  "")
	      (render-attrs node :except-for `(:from :_loop_bound_Nodes :_loop_bound_nodes_type :_no_group_realize_on_vm :pool))))))

(defun %uconst (value &key (dtype *default-uint*))
  "Creates an unsigned integer"
  (declare (type (or number symbol) value))
  (%load (%salloc :dtype dtype) value))
(defun %iconst (value &key (dtype *default-int*))
  "Creates a signed integer"
  (declare (type (or number symbol) value))
  (%load (%salloc :dtype dtype) value))
(defun %fconst (value &key (dtype *default-float*))
  "Creates a float const"
  (declare (type (or number symbol) value))
  (%load (%salloc :dtype dtype) value))

(defun %stride (shape order &key (dtype :int64))
  "Compute the stride based on permute and shape."
  (declare (type list shape)
	   (type (member :row :column) order))
  (if (eql order :row)
      (%row-major-calc-strides shape :dtype dtype)
      (%column-major-calc-strides shape :dtype dtype)))

(defun %shape (shape &key (dtype :int64))
  "Initialize the shape."
  (declare (type list shape) (type dtype-t dtype))
  (flet ((const (n) (if (node-p n) n
                        (progn
                          (assert (or (numberp n) (symbolp n)) () "%shape: ~a is not a size." n)
                          (%load (%salloc :dtype dtype) n)))))
    (map 'list #'const shape)))

(defun %make-tensor (shape &key (dtype-indexing *default-int*) (dtype *default-float*) (order *default-order*) (id (lgensym "ALLOC_")) (from nil))
  (declare (type list shape)
	   (type dtype-t dtype)
	   (type (member :row :column) order))
  (assert (every #'(lambda (x) (or (symbolp x) (integerp x) (node-p x))) shape)
	  ()
	  "%make-tensor: Shape is designed as symbol, integer or node.~%but got ~a" shape)
  (when (= 0 (length shape))
    (assert (null from) () ":from for a scalar creation is ignored. Use %load instead.")
    (return-from %make-tensor (%salloc :dtype dtype :id id)))
  (%alloc (length shape) (%shape shape :dtype dtype-indexing) (%stride shape order :dtype dtype-indexing) :dtype dtype :id id :from from))

(defun %index-components (x shape &key (id (lgensym "IC_")))
  "the equivalent to doing: `for (int i=x.view.from;i<x.view.to;i+=x.view.by) { id[i] = i; }`"
  (declare (type node x) (type list shape))
  (emit (make-node :Indexing :Index-Components (list id) `(,(node->id x) ,@(map 'list #'node->id (%stride shape :row))))))

(macrolet ((def (fname opname)
	     `(defun ,fname (x &key (id (lgensym "ID_")))
		(declare (type (or number symbol node) x))
		(emit (make-node :UnaryOps ,opname (list id) (list (node->id1 x)))))))
  (def %neg   :NEG)
  (def %recip :RECIP)
  (def %sin   :SIN)
  (def %log2  :LOG2)
  (def %exp2  :EXP2)
  (def %sqrt  :SQRT)
  (def %not   :NOT))
;; x <- cast(y)
(defun %cast (x y dtype &key (id (lgensym "ID_")))
  (declare (type node x y) (type dtype-t dtype))
  (emit (make-node :UnaryOps :CAST (list id) (list (node->id x) (node->id y)) :dtype dtype)))
;; BinaryOps := [MOVE, Add, Mul, NEQ, LT, AND, OR, MAX, GCD]
;; reduction = nil -> | c = a + b
;; reduction = t   -> | a += b
(defparameter *wrap-around-mode* nil)
(macrolet ((def (fname opname &optional possibly-overflow)
	     `(defun ,fname (x y &key (id (lgensym "ID_")) (reduction nil) (wrap-around ,(if possibly-overflow '*wrap-around-mode* nil)))
		"If wrap-around=t -> (mod (op x y) (max_value_of (dtype x)))"
		(declare (type (or number symbol node) x y))
		(when (and (null ,possibly-overflow) wrap-around)
		  (error "~a does not support the wrap-around option." ',fname))
		(emit (make-node :BinaryOps ,opname (list id) (list (node->id1 x) (node->id1 y)) :reduction reduction :wrap-around wrap-around)))))
  (def %add :ADD t)
  (def %mul :MUL t)
  (def %idiv :IDIV nil)
  (def %and :AND)
  (def %or :OR)
  (def %xor :XOR)
  (def %move :MOVE)
  (def %max :MAX)
  (def %mod :MOD))

(defun %min (x y &key (id (lgensym "ID_")) (reduction nil)) (%neg (%max (%neg x) (%neg y) :reduction reduction) :id id))
(defun %sub (x y &key (reduction nil) (id (lgensym "ID_"))) (%add x (%neg y) :reduction reduction :id id))
(defun %div (x y &key (reduction nil) (id (lgensym "ID_"))) (%mul x (%recip y) :reduction reduction :id id))

;; CompareOps: map <- [map{bool}, x, y]
(macrolet ((def (fname opname)
	     `(defun ,fname (shape order x y &key (id (lgensym "ID_")) (out nil))
		(declare (type (or number symbol node) x y))
		(let ((out (or
			    out
			    (if shape
				(%make-tensor shape :dtype :bool :order order)
				(%salloc :dtype :bool)))))
		  (emit (make-node :TernaryOps ,opname (list id) (list (node->id1 out) (node->id1 x) (node->id1 y))))))))
  (def %!= :!=)
  (def %< :<))

(defun %= (shape order x y &key out (id (lgensym "ID_")))  (%not (%!= shape order x y :out out) :id id))
(defun %<= (shape order x y &key out (id (lgensym "ID_"))) (%or (%< shape order x y :out out) (%= shape order x y :out out) :id id))
(defun %>  (shape order x y &key out (id (lgensym "ID_"))) (%not (%<= shape order x y :out out) :id id))
(defun %>= (shape order x y &key out (id (lgensym "ID_"))) (%or (%> shape order x y :out out) (%= shape order x y :out out) :id id))

(defun %where (condition x y &key (id (lgensym "ID_")))
  "id = where(condition, x{true-then}, y{false-then})"
  (declare (type node condition x y))
  (emit (make-node :TernaryOps :WHERE (list id) (list (node->id condition) (node->id x) (node->id y)))))

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
			       (view-by1   (subseq1p (node-reads node) (+ nrank1 nrank1) (* 3 nrank1)))
			       (stride1   (subseq1p (node-reads node) (* 3 nrank1) (* 4 nrank1))))
			  (update nrank nrank1)
			  (update broadcast broadcast1)
			  (update view-from view-from1)
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

(defun %view (base shape offsets coeffs stride &key (id (lgensym "ID_")))
  "Creates a view against base. (views are only created against the original buffer)
Each dimension grid_id is mapped by linear function specified by offsets and coeffs.
i.e.: _gid_n = _gid_n'*coeffs[n]+offsets[n]"
  (declare (type node base)
	   (type list offsets coeffs shape stride))
  (flet ((->const (x)
	   (if (node-p x)
	       x
	       (%iconst x))))
    (setf offsets (map 'list #'->const offsets)
	  coeffs  (map 'list #'->const coeffs)
	  shape   (map 'list #'->const shape)
	  stride  (map 'list #'->const stride)))
  (assert (and (every #'size-p coeffs) (every #'size-p offsets))
	  ()
	  "Assertion Failed: offsets/coeffs must be a list of Node or integer.")
  (assert (= (length offsets) (length coeffs) (length shape) (length stride))
	  ()
	  "Assertion Failed: the rank must be determined before the compilation.
nrank=~a
stride=~a"
	  offsets coeffs)
  (let ((nrank (length offsets)))
    (emit (make-node :Buffer :View
		     (list id)
		     (append (list (node->id1 base))
			     (map 'list #'node->id1 shape)
			     (map 'list #'node->id1 offsets)
			     (map 'list #'node->id1 coeffs)
			     (map 'list #'node->id1 stride))
		     :nrank nrank))))

(defun %partial-view (base size stride dilation offset &key (dims 0) (id (lgensym "PV_")))
  "Creates a partial view on base."
  (declare (type (or number symbol node) base size stride dilation offset))
  (emit (make-node :Schedule :PartialView (list id) (map 'list #'node->id1 (list base size stride dilation offset))
                   :dims dims)))

(defmethod print-node ((node Node) (id (eql :View)))
  (let ((nrank (getattr node :nrank)))
    (when (and nrank (not (= nrank 0)) (eql (node-class node) :Buffer))
      (flet ((subseq1p (x y z) (subseq x (1+ y) (1+ z))))
	(format nil "<~a : ~a <- (~a, shape=(~a), views=(~a), stride=(~a))>"
		(node-type node)
		(render-list (node-writes node))
		(car (node-reads node))
		(render-list (subseq1p (node-reads node) 0 nrank))
		(let ((upfrom (subseq1p (node-reads node) nrank (* 2 nrank)))
		      (by (subseq1p (node-reads node) (* 2 nrank) (* 3 nrank))))
		  (render-list
		   (map 'list #'(lambda (x y) (format nil "(~a)" (render-list (list x y)))) upfrom by)))
		(render-list (subseq1p (node-reads node) (* 3 nrank) (* 4 nrank))))))))
