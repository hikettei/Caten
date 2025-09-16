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

(defgeneric search-merged-view (graph parent child))
(defmethod search-merged-view ((graph TensorGraph) (parent Node) (child Node))
  (assert (eql (node-type parent) :VIEW))
  (assert (eql (node-type child) :VIEW))
  (error "wip"))

(defun compute-equalities-matrix-on-relay (X Y &aux (order (list :dim-cst :dim-param :dim-in :dim-out :dim-div)))
  "X = View(Y, ...) align Y domain in respect to X"
  (declare (type TensorRelay X Y))
  (let* ((gid2shape/stride (make-hash-table :test 'equal))
         (grids (create-glo-from-relays X Y))
         ;; [TODO]
         ;; - ensure grids support symbolic by fixing dimensions
         ;; - [ ] mod stride[n]での周期性を保証する
         ;; - [ ] Insert Realize when edges are not tensor-symbolics-p
         (access-umap-X
           (caten/codegen/polyhedral:relay-on-global-lex-order
            grids X :domid "X" :varid "VAR"))
         (access-umap-Y
           (caten/codegen/polyhedral:relay-on-global-lex-order
            grids Y :domid "Y" :varid "VAR")))
    (when (and access-umap-X access-umap-Y)
      (let* ((D (isl:union-map-apply-range access-umap-Y (isl:union-map-reverse access-umap-X)))
             (Equalities)
             (EqualitiesCols))
        (caten/codegen/schedule:%foreach-map
         D
         #'(lambda (map)
             (let* ((bmap (isl:map-affine-hull (isl:map-compute-divs map)))
                    (E (isl:basic-map-equalities-matrix bmap :order order)))
               (assert (and (null Equalities) (null Equalitiescols)) () "Multiple Equalities Detected.")
               (assert (= 0 (isl:basic-map-dim bmap :dim-div)) () "basic_map_dim(bmap, isl_dim_div) != 0 is not expected?...")
               (setf Equalities E
                     EqualitiesCols
                     (loop for type in order
                           append
                           (loop for i upfrom 0 below (isl:basic-map-dim bmap type)
                                 for name = (isl:basic-map-get-dim-name bmap type i)
                                 collect
                                 (case type
                                   (:dim-in
                                    (cons "X" name))
                                   (:dim-out
                                    (setf (gethash name gid2shape/stride)
                                          (list (nth i (tensor-relay-shape X)) (nth i (tensor-relay-stride X)) i))
                                    (cons "Y" name))
                                   (:dim-cst (assert (null name) () ":dim_cst should not introduce a tuple_name") :dim-cst)
                                   (otherwise name)))))
               map)))
        (values Equalities EqualitiesCols D gid2shape/stride)))))
;; Ops.DOMAIN
;; Ops.PartialView ==> Directly Mappable to Affine
;; Ops.FILTER
;; Ops.REALIZE
(defun solve-equalities-on-cols (E cols col)
  (declare (type isl::mat E) (type list cols))
  ;; A = B
  (let* ((col-dim-pos (or (position col cols :test #'equal) (error "solve-equalities-on-cols: the col ~a not found in cols ~a" col cols)))
         (var-space-list
           (loop for i upfrom 0 for c in cols
                 if (and (listp c) (string= "X" (car c)))
                   collect i))
         (col-involving-rows
           (loop for i upfrom 0 below (isl:mat-rows E)
                 for coeff = (isl:mat-ref E i col-dim-pos) do
                   (let ((c (map 'list #'(lambda (pos) (isl:mat-ref E i pos)) var-space-list)))
                     (assert (<= (count-if (alexandria:compose #'not #'zerop) c) 1)))
                 if (not (= 0 coeff))
                   collect (cons i (* -1 coeff)))))
    (assert (= 1 (length col-involving-rows)))
    (multiple-value-bind (tgt-row coeff) (values (car (car col-involving-rows)) (cdr (car col-involving-rows)))
      (or
       (loop for j upfrom 0 below (isl:mat-cols E)
             for col in cols
             for val = (isl:mat-ref E tgt-row j)
             if (and (null (find j var-space-list)) (not (= val 0)) (not (= coeff 0)))
               collect
             `(* ,(/ val coeff) ,(case col (:dim-cst 1) (otherwise (if (listp col) (cdr col) col)))))
       0))))

(defun aff->partial-schedule (dom aff gid2shape/stride)
  (declare (type hash-table gid2shape/stride))
  (match aff
    ((list (list '* (guard a (numberp a)) (guard b (stringp b))))
     (let ((shape/stride/dim (gethash b gid2shape/stride)))
       (multiple-value-bind (shape stride dim) (apply #'values shape/stride/dim)
         (assert (and shape stride dim))
         ;; [TODO] Inherit (Connect?) Parent's partial view.
         ;; - [ ] Identify who and who are equivalent.
         (%partial-schedule 'placeholder shape stride a 0))))
    (0
     (%partial-schedule 'placeholder 1 0 1 0))
    (_
     (error "FAILED DUE TO: ~a" aff) ;; [todo] remove
     :failed)))
;; Domain
;; PartialSchedule
(defsimplifier
    (%graph-force-view :speed 0)
    ;; Force view on toplevel
    ((:ALLOCATE (list* _) :nrank (guard nrank (> nrank 0)))
     ->
     ((node graph)
      (let ((users (id->users graph (car (node-writes node)))))
        (when (not (and (= 1 (length users)) (eql :VIEW (node-type (car users)))))
          (let* ((alc (copy-node node))
                 (dst (car (node-writes node)))
                 (via (caten/utilities/gensym:lgensym "ALC_"))
                 (rel (car (relay-writes (read-type-relay node)))))
            (setf (node-id alc) (gensym "NID") (node-writes alc) (list via))
            (print (tensor-relay-shape rel))
            (list
             alc
             (print (%view via (tensor-relay-shape rel) (map 'list #'car (tensor-relay-views rel))
                    (map 'list #'second (tensor-relay-views rel)) (tensor-relay-stride rel) :id dst)))))))))

(defsimplifier
    ;; top_down?
    (%graph-rewrite-view-as-partial-schedule :speed 0)
    ((:VIEW (list* base _))
     ->
     ((node graph)
      ;; [TODO]
      ;; - [ ] Extract Access Relations in one dimensional polyhedral space
      ;; - [ ] View => Decompose Into PartialView
      ;;   - [ ] If it is difficult, keep view (use it like a realize)
      ;;   - [ ] Solve on equlities matrix. (more pattern, more likely to purge views, it is simple)
      ;; - [ ] Fix TypeInference
      ;; - [ ] Fix infer-tensor-info in simplifiers.lisp (reinitialize-tensor)
      ;;   - [ ] It should use node-type-relay right?
      ;; - [ ] LoopRangeとは違う？
      ;; - [ ] Introducing > 1 constants => ?
      ;; X = View(Y, ...)
      ;; (graph-infer-type-relay node) ;; <= compute only diffs?
      ;; Ops.GRID or Ops.DOMAIN(SIZE)
      ;; PartialView(A, DOMAIN[], dilation, offset)
      ;; - ParentのOps.GRIDを使うようにAccessMapを解析していく。
      ;; - 同一のOps.GRIDを使うグループ=Fusion
      ;; - [ ] BinaryOps/TernaryOps can provide equalities of doms
      (let ((X (car (relay-writes (read-type-relay node))))
            (Y (car (relay-reads (read-type-relay node))))
            (views) (failed nil))
        
        (multiple-value-bind (E cols D gid2shape/stride) (compute-equalities-matrix-on-relay X Y)
          (when E
            (print "PartialView")
            (print node)
            (print (tensor-relay-nrank (car (relay-reads (read-type-relay node)))))
            (print (tensor-relay-nrank (car (relay-writes (read-type-relay node)))))
            (print E)
            (print D)
            (dolist (col cols)
              (when (and (listp col) (string= "X" (car col)))
                (let ((pwv (aff->partial-schedule (cdr col) (solve-equalities-on-cols E cols col) gid2shape/stride)))
                  (when (eql pwv :failed) (setf failed t))
                  (push pwv views))))
            (when (and views (null failed))
              (setf views (nreverse views))
              (let ((top (car (node-reads node))))
                (dolist (view views)
                  (setf (car (node-reads view)) top
                        top (car (node-writes view))))
                (setf (node-writes (car (last views))) (copy-list (node-writes node)))
                (print node)
                (print views)
                ))))))))
;; Loop Fusion is:
;; Only command is required.
;; Only view and view matters
;; Use One dimensional affine expression, symbolic is replaced w/ some prime numbers
;; Problem1: (9) -> (3, 3) Reshape is not doable.
;; Problem2: Symbolic
;; TODO: Unravel
(defun graph-simplify-views (graph)
  (declare (type TensorGraph graph))
  (graph-infer-type-relay graph)
  ;; (%graph-simplify-views graph)
  ;; (%graph-rewrite-view-as-partial-schedule graph)
  graph)

(defun tensor-realize (tensor)
  ;; Likewise RANGIFY, this can be pullback into TensorGraph w/ VIEW
  ;; for fast autodiff
  (%graph-force-view (tensor-graph tensor))
  (tensor-simplify tensor)
  (graph-infer-type-relay (tensor-graph tensor))
  ;; (%graph-rewrite-view-as-partial-schedule (tensor-graph tensor))
  (tensor-graph tensor)
  ;; lower-hlops
  )
