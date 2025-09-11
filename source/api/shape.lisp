(in-package :caten/api)
;; ~~ NumpySemantic Broadcast ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun align-left (shapes)
  (let ((max-dim (reduce #'max (map 'list #'length shapes))))
    (values
     max-dim
     (loop for shape in shapes
           collect (append (make-list (- max-dim (length shape)) :initial-element 1) shape)))))

(defun broadcast-shape (shapes)
  (multiple-value-bind (max-dim shapes) (align-left shapes)
    (loop for dim upfrom 0 below max-dim
          for sdim-sizes = (map 'list #'(lambda (x) (nth dim x)) shapes)
          collect (or (find-if #'(lambda (x) (not (eql x 1))) sdim-sizes) 1))))

(defun broadcast-elwise (a b)
  (declare (type Tensor a b))
  (cond
    ((= 0 (tensor-nrank a) (tensor-nrank b))
     (values a b))
    (T
     (let ((broadcasted (broadcast-shape (list (tensor-shape a) (tensor-shape b)))))
       (values (!expand a broadcasted) (!expand b broadcasted))))))
;; ~~ View Parser ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(deftype axis-t () `(or number symbol Tensor))
(defstruct (ViewRange
	    (:constructor make-vrange (from to by broadcast size subscript)))
  (from from :type Tensor) (to to :type Tensor)
  (by by :type Tensor) (broadcast broadcast :type boolean)
  (size size :type Tensor) (subscript subscript))

(defun vrange-size (vrange)
  (declare (type ViewRange vrange))
  (!idiv (!sub (viewrange-to vrange) (viewrange-from vrange)) (viewrange-by vrange)))

(defun parse-view-subscript (size subscript)
  (declare (type axis-t size))
  (flet ((normalize (x) (if (and (numberp x) (< x 0)) (!add (->size size) (->size x)) (->size x)))
	 (1p (x) (if (tensor-p x) (!add x (->size 1)) (!add (->size x) (->size 1)))))
    (ematch subscript
      ((list :~ n) (make-vrange (->size 0) (normalize n) (->size 1) t (->size size) subscript)) ;; broadcasting (:~ N)
      ((eql t)  (make-vrange (->size 0) (->size size) (->size 1) nil (->size size) subscript)) ;; nothing
      ((guard x (typep x 'axis-t)) (make-vrange (normalize x) (1p (normalize x)) (->size 1) nil (->size size) subscript)) ;; A[i]
      ((list (guard from (typep from 'axis-t)) (guard to (typep to 'axis-t)))
       (make-vrange (normalize from) (normalize to) (->size 1) nil (->size size) subscript)) ;; A[from:to]
      ((list (guard from (typep from 'axis-t)) (guard to (typep to 'axis-t)) (guard by (typep to 'axis-t)))
       (make-vrange (normalize from) (normalize to) (->size by) nil (->size size) subscript))))) ;; A[from:to:by]
;; ~~ Reduction ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun parse-reduce-axes (x lst)
  (declare (type tensor x))
  (ematch lst
    ((eql t)
     (values (loop for i in (tensor-shape x) collect 1) (loop for i in (tensor-shape x) collect `(:~ ,i)) (range 0 (length (tensor-shape x)))))
    ((guard axis (numberp axis))
     (let ((axis (normalize-axis x axis))
	   (shape-after (tensor-shape x))
	   (view-after (loop for i in (tensor-shape x) collect t)))
       (setf (nth axis shape-after) 1
	     (nth axis view-after) `(:~ ,(nth axis (tensor-shape x))))
       (values shape-after view-after (list axis))))
    ((list* axes)
     (let ((axes (map 'list #'(lambda (a) (normalize-axis x a)) axes))
	   (shape-after (tensor-shape x))
	   (view-after  (loop for i in (tensor-shape x) collect t)))
       (dolist (axis axes)
	 (setf (nth axis shape-after) 1
	       (nth axis view-after) `(:~ ,(nth axis (tensor-shape x)))))
       (values shape-after view-after axes)))))
;; ~~ ShapeError Handling ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defparameter *restart-point* nil)
(defclass Restart-Point nil nil)
;; [TODO] Prefer @caten.trace reader macro!
;; [TODO] caten/lang
;; - [ ] Different Symbols will never intersect (i.e: N and M = empty)
;; - [ ] Use UnionMap to identity two symbolics
;; - [ ] TensorGraph.equalities (A, B)
;; - [ ] define-compiler-macro
;; - [ ] (defun x (a) (declare (type (Tensor ~ M N) a)))
(defmacro %internal-caten/defun (name lambda-list &body body)
  `(defun ,name (,@lambda-list)
     ,@body))
;; ~~ Early View Simplifier ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun create-glo-from-relays (&rest relays)
  (declare (type list relays))
  (let* ((strides (remove-duplicates (reduce #'append (map 'list #'tensor-relay-stride relays))))
         (glo (caten/codegen/polyhedral:make-global-lex-order :session-id (gensym "SESSION")))
         (dict (caten/codegen/polyhedral:global-lex-order-dict glo)))
    (loop for nth upfrom 0 for stride in strides do
      (setf (gethash stride dict) nth))
    ;; [TODO] Quasiaffine    
    glo))

(defun schedule-from-umap (umap)
  (declare (type isl::union-map umap))
  (let* ((sched (isl:schedule-get-root (isl:schedule-from-domain (isl:union-map-domain umap))))
         (domain (isl:set-from-union-set (isl:union-map-domain umap)))
         (dims (loop for dim upfrom 0 below (isl:set-dim domain :dim-set)
                     collect (isl:set-get-dim-name domain :dim-set dim))))
    (loop with dom-name = (isl:set-get-tuple-name domain)
          for dim in dims
          do (setf sched
                   (isl:schedule-node-insert-partial-schedule
                    (isl:schedule-node-first-child sched)
                    (isl:multi-union-pw-aff-from-str (format nil "[{~a[~{~(~a~)~^, ~}] -> [(~(~a~))]}]" dom-name dims dim)))))
    (isl:schedule-node-get-schedule sched)))
;; [todo] move to schedule.lisp?
(defun compute-equalities-matrix (writes reads)
  (let* ((deps (isl:union-map-apply-range writes (isl:union-map-reverse reads)))
         (map  (isl:map-compute-divs (isl:map-from-union-map deps)))
         (bmap (isl:map-affine-hull map))
         (E (isl:basic-map-equalities-matrix bmap))
         (I (isl:basic-map-inequalities-matrix bmap))
         (cols
           (append
            (list 1) ;; dim_cst
            (loop for type in `(:dim-param :dim-in :dim-out)
                  append
                  (loop for i upfrom 0 below (isl:basic-map-dim bmap type)
                        for suffix = (case type (:dim-in "_in") (:dim-out "_out") (otherwise ""))
                        collect (intern (format nil "~a~a" (isl:basic-map-get-dim-name bmap type i) suffix)))))))
    ;; cols = [dim_cst | dim_param | dim_in | dim_out | dim_divs]
    (assert (= (isl:mat-cols E) (isl:mat-cols I) (length cols)))
    (assert (= 0 (isl:map-dim map :dim-div)))
    (values E cols)))

(defun %view-from-equalities-matrix (view-base E cols)
  (declare (type node view-base) (type isl::mat E) (type list cols))
  (loop for i upfrom 0 below (isl:mat-rows E)
        for affs = (with-inlined-tir
                       (out)
                       (affs
                        (reduce
                         #'%add
                         (loop for j upfrom 0 below (isl:mat-cols E)
                               for col in cols
                               for coeff = (isl:mat-ref E i j)
                               collect
                               (%mul (%iconst col) (%iconst coeff)))))
                       (out affs))
        do (print (tensor-from-graph affs))
           (print affs)))


(defsimplifier
    (%graph-simplify-views :speed 0)
    ;; Extra !contiguous
    ((:MOVE ((:ALLOCATE (~ _)) (:ALLOCATE (~ _))) :reduction (guard r (null r)))
     ->
     ((node graph)
      ;; actually X == Y is asserted by running graph-infer-type-relay
      (multiple-value-bind (x y) (values (id->value graph (car (node-reads node))) (id->value graph (second (node-reads node))))
        (when (and x y (equal (cdr (node-reads x)) (cdr (node-reads y))) (null (getattr x :from)) (null (getattr y :from)))
          y))))
    ;; A case for view is creating identity view from contiguous.
    ((:VIEW (~ args))
     ->
     ((node graph)
      (let ((alloc (id->value graph (car args))))
        (when (eql (node-type alloc) :ALLOCATE)
          (let ((alloc-rel (car (relay-writes (read-type-relay alloc))))
                (view-rel  (car (relay-writes (read-type-relay node)))))
            (when (tensor-relay-equal alloc-rel view-rel)
              alloc))))))
    ((:VIEW (list* (:VIEW (~ _)) _))
     ->
     ((node graph)
      (let ((val (id->value graph (car (node-reads node)))))
        (assert val)
        (let ((node (copy-node node)))
          (setf (node-id node) (gensym "NID")
                (car (node-reads node)) (car (node-reads val)))
          node))))
    ;; [TODO]
    ;; (tensor-graph (!sin (!reshape (!sin (!sin (make-tensor `(3 3)))) `(3 3))))
    ;; Obvious case for !contiguous is rebundant: MOVE(CONTIGUOUS, CONTIGUOUS)
    ((:VIEW (list* (:MOVE ((:ALLOCATE (~ _)) maybe-contiguous) :reduction (guard r (null r))) _))
     ->
     ((node graph)
      (let ((allocate (id->value graph maybe-contiguous)))
        (when (and allocate (or (eql (node-type allocate) :ALLOCATE) (eql (node-class allocate) :UnaryOps)))
          (loop until (eql (node-type allocate) :ALLOCATE)
                for parent = (id->value graph (car (node-reads allocate))) do
                  (if (eql (node-class parent) :UnaryOps)
                      (setf allocate parent)
                      (return)))
          (when (eql (node-type allocate) :ALLOCATE)
            (print "FOUND")
            (print allocate)
            nil
            )))))
    ;; Remove extra Allocation by !contiguous: VIEW(MOVE(ALLOCATE, _))
    ((:VIEW (list* (:MOVE ((:ALLOCATE (~ _)) y) :reduction (guard r (null r))) _))
     ->
     ((view-x graph)
      ;; Y -> M -> X
      (let ((Y (id->value graph y))
            (move (id->value graph (car (node-reads view-x)))))
        (when (and Y move)
          (let* ((xt (car (relay-writes (read-type-relay view-x))))
                 (yt (car (relay-writes (read-type-relay Y))))
                 (mt (car (relay-reads (read-type-relay move))))
                 (glo (create-glo-from-relays xt yt mt))
                 (Xa (caten/codegen/polyhedral:relay-on-global-lex-order glo xt :domid "DST" :varid "Y"))
                 (Ya (caten/codegen/polyhedral:relay-on-global-lex-order glo yt :domid "SRC" :varid "X"))
                 (Ma (caten/codegen/polyhedral:relay-on-global-lex-order glo mt :domid "SRC" :varid "Y")))
            (when (and Xa Ya Ma) ;; Y -> M -> X
              ;; [src]
              ;; for i in schedule_from_domain(M and YT)
              ;;  M = Transform(YT)
              ;; [dst]
              ;; for i in schedule_from_domain(XT)
              ;;  read(M)
              ;; If it is fusible, Transform(TY) is a new view object because it is simplified so
              (let* ((dom-src (schedule-from-umap Ma))
                     (dom-dst (schedule-from-umap Xa))
                     (theta (isl:schedule-sequence dom-src dom-dst))
                     (deps (caten/codegen/schedule:compute-dependence-relation (isl:union-map-union Xa Ya) Ma theta)))
                (multiple-value-bind (E cols) (compute-equalities-matrix Ma (isl:union-map-union Xa Ya))
                  (print (isl:schedule-get-root theta))
                  (print E)
                  (print cols)
                  
                ;; dom_src_new = apply(dom_src, WaR.coefficient_matrix)
                ;; - dom_src_new and dom_dst deps are corresponding one-by-one
                ;; - and node dependency was broken
                ;; ==> dom_src_new == dom_dst and dom_dst is the only read
                ;; thus view is replaceable with only single dom_src_new
                  (let ((view (%view-from-equalities-matrix view-x E cols)))
                    (print view)))
                nil))))))))

(defun graph-simplify-views (graph)
  (declare (type TensorGraph graph))
  (graph-infer-type-relay graph)
  (%graph-simplify-views graph)
  graph)
