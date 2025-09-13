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

;; Problem1: (9) -> (3, 3) Reshape is not doable.
;; Problem2: Symbolic
;; TODO: Unravel
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
    ;; ALLOC CONTIGUOUS_PATH
    ;;    \   /                 CONTIGUOUS_PATH
    ;;     MOVE          =====>        |
    ;;      |                        VIEW
    ;;     VIEW
    ((:VIEW (list* (:MOVE ((:ALLOCATE (~ _)) y) :reduction (guard r (null r))) _))
     ->
     ((node graph)
      (let* ((top (id->value graph y)) (removable-p t))
        (when (and top (not (eql (node-type top) :VIEW)))
          (loop while top for parent = (id->value graph (get-output-to top)) do
            (setf top parent)
            (when (and top (eql (node-type top) :VIEW))
              (setf removable-p nil)
              (return)))
          (when removable-p
            (let ((node (copy-node node)))
              (setf (node-id node) (gensym "NID")
                    (car (node-reads node)) y)
              node))))))
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
    ;; VIEW(VIEW(x)) is the first VIEW.
    ((:VIEW (list* (:VIEW (~ _)) _))
     ->
     ((node graph)
      (let ((val (id->value graph (car (node-reads node)))))
        (assert val)
        (let ((node (copy-node node)))
          (setf (node-id node) (gensym "NID")
                (car (node-reads node)) (car (node-reads val)))
          node))))
    ;; VIEW(CONTIGUOUS(VIEW(x))) is directly view-able?
    ;; ALLOC VIEW
    ;;    \   /             VIEW
    ;;     MOVE    =====>     |        ==> VIEW
    ;;      |             MERGED_VIEW
    ;;     VIEW
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
              ;; [TODO] decompose strides to lcm
              (print (tensor-relay-nrank xt))
              (print (tensor-relay-nrank yt))
              (print (tensor-relay-nrank mt))
              ;; [src]
              ;; for i in schedule_from_domain(M and YT)
              ;;  M = Transform(YT)
              ;; [dst]
              ;; for i in schedule_from_domain(XT)
              ;;  read(M)
              ;; If it is fusible, Transform(TY) is a new view object because it is simplified so
              ;; Special Notation for Reshape?
              ;; Reshape Semantic Review
              ;; - [ ] Produce Shape Error
              ;; - [ ] Reshape Unravel is doable from given strides (add max/min)
              ;; - [ ] (!reshape (!t (make-tensor `(3 3))) `(3 3))
              ;;  - [ ] Normalize first
              ;; AST Simplify
              (let* ((dom-src (schedule-from-umap Ma))
                     (dom-dst (schedule-from-umap Xa))
                     (theta (isl:schedule-sequence dom-src dom-dst))
                     (deps (caten/codegen/schedule:compute-dependence-relation (isl:union-map-union Xa Ya) Ma theta))
                     (cst (caten/codegen/schedule:compute-schedule-constraints (isl:union-set-union (isl:union-map-domain Ma) (isl:union-map-domain Xa)) deps)))
                (print (isl:union-map-union Xa Ya))
                (print Ma)
                (let ((fused (isl:schedule-constraints-compute-schedule cst)))
                  ;(print (isl:schedule-get-root fused))
                  (print (caten/codegen/ast::ast->str (caten/codegen/ast:compute-ast-from-schedule fused)))
                  )
                ;; dom_src_new = apply(dom_src, WaR.coefficient_matrix)
                ;; - dom_src_new and dom_dst deps are corresponding one-by-one
                ;; - and node dependency was broken
                ;; ==> dom_src_new == dom_dst and dom_dst is the only read
                ;; thus view is replaceable with only single dom_src_new
                nil))))))))

(defun graph-simplify-views (graph)
  (declare (type TensorGraph graph))
  (graph-infer-type-relay graph)
  (%graph-simplify-views graph)
  graph)
