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

(defun compose-view (x y)

  )

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
    ;; is_contiguous detection. Remove away extra !contiguous
    ((:VIEW (list* (:MOVE ((:ALLOCATE (~ _)) root-id) :reduction (guard r (null r))) rest))
     ->
     ((view graph)
      (let ((root (id->value graph root-id)))
        (when root
          (let* ((root-type (car (relay-writes (read-type-relay root))))
                 (child-type (car (relay-writes (read-type-relay view))))
                 (glo (create-glo-from-relays root-type child-type))
                 (child-access (caten/codegen/polyhedral:relay-on-global-lex-order glo root root-type))
                 (view-access (caten/codegen/polyhedral:relay-on-global-lex-order glo view child-type)))
            (when (and child-access view-access)
              (let* ((F (isl:union-map-apply-range view-access (isl:union-map-reverse child-access)))
                     (Mergeable (and
                                 (isl:union-map-is-single-valued F)
                                 (isl:union-set-equalp (isl:union-map-domain F) (isl:union-map-domain view-access)))))
                (when Mergeable
                  (let ((view (copy-node view)))
                    (setf (node-id view) (gensym "NID")
                          (car (node-reads view)) root-id)
                    view)))))))))
    ;; Merge two views into a single one
    ((:VIEW (list* (:MOVE ((:ALLOCATE (~ _)) y) :reduction (guard r (null r))) _))
     ->
     ((view-x graph)
      (let ((view-y (id->value graph y))
            (move (id->value graph (car (node-reads view-x)))))
        (when (and view-y move (eql :VIEW (node-type view-y)))
          (let* ((xt (car (relay-writes (read-type-relay view-x))))
                 (yt (car (relay-writes (read-type-relay view-y))))
                 (mt (car (relay-reads (read-type-relay move))))
                 (glo (create-glo-from-relays xt yt mt))
                 (Xa (caten/codegen/polyhedral:relay-on-global-lex-order glo view-x xt))
                 (Ya (caten/codegen/polyhedral:relay-on-global-lex-order glo view-y yt))
                 (Ma (caten/codegen/polyhedral:relay-on-global-lex-order glo move mt)))
            ;; Domain wo 5zigen ni padding sita houga ii?
            (when (and Xa Ya Ma) ;; Y -> M -> X
              (print "CASE")
              (print (alexandria:hash-table-keys (caten/codegen/polyhedral:global-lex-order-dict glo)))
              (print view-y)
              (print move)
              (print view-x)
              (print (node-id view-y)) (print (node-id move)) (print (node-id view-x))
              (print xa) (print ya) (print ma)
              ;; RaW
              (let* (;(Y->M (isl:union-map-apply-range Ya (isl:union-map-reverse Ma)))
                     ;(M->X (isl:union-map-apply-range Ma (isl:union-map-reverse Xa)))
                     (Y->M (isl:union-map-from-domain-and-range (isl:union-map-range Ya) (isl:union-map-range Ma)))
                     (M->X (isl:union-map-from-domain-and-range (isl:union-map-range Ma) (isl:union-map-range Xa))))
                     
                (print "RESULT")
                (print Y->M)
                (print M->X)
                (print (isl:union-map-range (isl:union-map-apply-range Y->M M->X)))
                )
              nil)))))))

(defun graph-simplify-views (graph)
  (declare (type TensorGraph graph))
  (graph-infer-type-relay graph)
  (%graph-simplify-views graph)
  graph)
