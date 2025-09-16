(in-package :caten/api)

(defparameter *default-indexing-dtype* :int64)
;; NewTensor = apply_tensor_tir(Tensor, TensorProgram)
(defstruct (Tensor
            (:constructor %%make-tensor (graph id)))
  (graph graph :type TensorGraph)
  (id id :type symbol)
  (buffer nil :type (or caten/runtime/buffer:AbstractBuffer null)))
;; ~~ TensorOps ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun tensor-simplify (tensor)
  (declare (type Tensor tensor))
  ;; 99% of computation time consist of optimize-aasm
  ;; we have to optimize it
  (setf (tensor-graph tensor) (optimize-aasm (tensor-graph tensor) :heavy-opt-threshold 0))
  (graph-simplify-views (tensor-graph tensor))
  tensor)

(defun tensor-verify (tensor)
  (declare (type Tensor tensor))
  (graph-infer-type-relay (tensor-graph tensor))
  tensor)

(defun tensor-node (tensor)
  (declare (type Tensor tensor))
  (id->value (tensor-graph tensor) (tensor-id tensor)))

(defun tensor->id (tensor)
  (etypecase tensor
    (number tensor) (symbol tensor)
    (Tensor (tensor-node tensor))))

(defun tensor-type (tensor)
  (declare (type Tensor tensor))
  (car (relay-writes (read-type-relay (tensor-node tensor)))))

(defun node->tensor (node graph)
  (assert (= 1 (length (node-writes node))))
  (assert (id->value graph (car (node-writes node))))
  (%%make-tensor graph (car (node-writes node))))

(defun ensure-node-is-tensor (node graph)
  (if (numberp node)
      node
      (if (or (eql node t) (null node))
          node
          (node->tensor node graph))))

(defun tensor-shape (tensor)
  (declare (type Tensor tensor))
  (map 'list #'(lambda (x)
                 (ensure-node-is-tensor
                  (or (id->value (tensor-graph tensor) x) x)
                  (tensor-graph tensor)))
       (tensor-relay-shape (tensor-type tensor))))

(defun tensor-nrank (tensor)
  (declare (type Tensor tensor))
  (tensor-relay-nrank (tensor-type tensor)))

(defun tensor-stride (tensor)
  (declare (type Tensor tensor))
  (map 'list #'(lambda (x)
                 (ensure-node-is-tensor
                  (or (id->value (tensor-graph tensor) x) x)
                  (tensor-graph tensor)))
       (tensor-relay-stride (tensor-type tensor))))

(defun tensor-dtype (tensor)
  (declare (type Tensor tensor))
  (tensor-relay-dtype (tensor-type tensor)))

(defun tensor-views (tensor)
  (declare (type Tensor tensor))
  (flet ((ensure-node (id) (ensure-node-is-tensor (or (id->value (tensor-graph tensor) id) id) (tensor-graph tensor))))
    (loop for view in (tensor-relay-views (tensor-type tensor))
          collect (map 'list #'ensure-node view))))

(defun tensor-is-symbolic-p (tensor)
  (declare (type Tensor tensor))
  (and (null (tensor-shape tensor)) ;; if it is a scalar graph
       (flet ((is-scalar-p (rel) (or (null rel) (= 0 (tensor-relay-nrank rel)))))
         (every #'(lambda (node)
                    (and (node-type-relay node)
                         (every #'is-scalar-p (relay-reads (read-type-relay node)))
                         (every #'is-scalar-p (relay-writes (read-type-relay node)))))
                (graph-nodes (tensor-graph tensor))))))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defmethod print-object ((tensor Tensor) stream)
  ;; [TODO] PrintObject: Render Scalar Directly
  (print-unreadable-object (tensor stream)
    (format stream "{Tensor~a[~(~a~)] :shape ~a :id ~a
~a~a
  :node ~a
  :requires-grad nil}"
            (if (tensor-buffer tensor)
                (format nil "{~a}" (class-name (class-of (tensor-buffer tensor))))
                "")
	    (tensor-dtype tensor)
            (tensor-shape tensor)
	    (tensor-id tensor)
            (if (tensor-is-symbolic-p tensor)
                (format nil "  <~a>~%"
                        (caten/runtime/renderer:render-node
                         (make-instance 'caten/runtime/byoc:Default-Renderer :graph (tensor-graph tensor))
                         (tensor-id tensor)))
                "")
	    (if (tensor-buffer tensor)
	        (caten/runtime/buffer:pprint-buffer (tensor-buffer tensor) :indent 2)
	        "  :buffer nil")
            (tensor-node tensor))))

(defun %concat-tensor-graph (parents child-graph)
  (declare (type list parents) (type TensorGraph child-graph))
  (let ((g (make-instance 'TensorGraph :output (graph-outputs child-graph) :seen nil :nodes nil)))
    (dolist (graph (append parents (list child-graph)))
      (assert (typep graph 'TensorGraph) () "%concat-tensor-graph: each of parents must be a TensorGraph.")
      (maphash #'(lambda (k v) (setf (gethash k (%graph-nodes-table g)) v)) (%graph-nodes-table graph)))
    g))

(defun tensor-from-graph (tensor-graph) ;; Root
  (declare (type TensorGraph tensor-graph))
  (assert (= 1 (length (graph-outputs tensor-graph))) () "tensor-from-graph: The output tensor id must be identical")
  (tensor-verify (tensor-simplify (%%make-tensor tensor-graph (car (graph-outputs tensor-graph))))))

(defun apply-tensor-graph (variables tensor-graph) ;; Forward
  "MakeTensor(Graph=Concat(Tensor->graph, tensor_graph))"
  (declare (type TensorGraph tensor-graph) (type list variables))
  (let ((variables (loop for tensor in (alexandria:flatten variables) if (tensor-p tensor) collect tensor)))
    (assert (= 1 (length (graph-outputs tensor-graph))) () "apply-tensor-graph: The output tensor id must be identical")
    (tensor-verify (tensor-simplify (%%make-tensor (%concat-tensor-graph (map 'list #'tensor-graph variables) tensor-graph) (car (graph-outputs tensor-graph)))))))

(defmacro with-inlined-tir ((&rest out-binds) &rest forms)
  (alexandria:with-gensyms (outputs)
    `(->fast-graph
      (let* ((,outputs)
             (g (let* ((*ctx* (make-graph))
                       ,@forms)
                  ,@(loop for dst in out-binds
                          collect
                          `(progn
                             (assert (node-p ,dst) () "with-inlined-tir: Each of out-binds must produce a node, getting ~a" ,dst)
                             (assert (= 1 (length (node-writes ,dst))) () "with-inlined-tir: Each of out-binds must produce a single output, getting ~a" ,dst)
                             (push (car (node-writes ,dst)) ,outputs)))
                  *ctx*)))
        (setf (graph-outputs g) ,outputs)
        g)
      :cls 'TensorGraph)))

(defmacro apply-tir ((&rest variables) (&rest out-binds) &rest program)
  `(apply-tensor-graph (list ,@variables) (with-inlined-tir (,@out-binds) ,@program)))
;; ~~ APIS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun make-tensor (shape &key (dtype *default-float*) (requires-grad nil) (initial-element nil) (from nil) (parent nil))
  (declare (type (or number symbol list) shape))
  (typecase shape
    ((or number symbol) (make-scalar shape :dtype dtype))
    (otherwise
     (apply-tir
         (parent shape)
         (out)
         (x (%make-tensor (map 'list #'tensor->id shape) :dtype-indexing *default-indexing-dtype* :dtype dtype :order (ctx:getenv :DEFAULT_ORDER) :from from))
         (out (if initial-element (%load x initial-element) x))))))

(defun make-scalar (value &key (dtype *default-float*))
  (declare (type (or symbol number) value))
  (tensor-from-graph (with-inlined-tir (out) (out (%load (%salloc :dtype dtype) value)))))

(defun ->size (value)
  (if (tensor-p value)
      value
      (make-scalar value :dtype *default-indexing-dtype*)))

(macrolet ((def (lisp-name1 lisp-name2 lisp-name3 ir-name)
             `(progn
                (declaim (ftype (function (Tensor Tensor &key (:reduction boolean)) (values Tensor)) ,lisp-name3))
                ;; Primitive Binary Operation (Private)
                (defun ,lisp-name3 (x y &key (reduction nil))
                  (declare (type Tensor x y) (type boolean reduction))
                  (multiple-value-bind (x y) (if reduction (values x y) (broadcast-elwise x y))
                    (apply-tir (x y) (out) (out (,ir-name (tensor-node x) (tensor-node y) :reduction reduction)))))
                (declaim (ftype (function (t t &key (:reduction boolean)) (values Tensor)) ,lisp-name2))
                (defun ,lisp-name2 (x y &key (reduction nil))
                  ,(format nil "[TODO] Docs here")
                  (,lisp-name3 (change-facet x :tensor) (change-facet y :tensor) :reduction reduction))
                ,(when lisp-name1
                   `(defun ,lisp-name1 (&rest tensors) (reduce #',lisp-name2 tensors))))))
  (def !+    !add primitive/add-binary %add)
  (def !-    !sub primitive/sub-binary %sub)
  (def !*    !mul primitive/mul-binary %mul)
  (def !/    !div primitive/div-binary %div)
  (def nil   !idiv primitive/idiv-binary %idiv)
  (def nil   !move primitive/move-binary %move)
  (def nil   !maximum primitive/maximum-binary %max)
  (def nil   !minimum primitive/minimum-binary %min))

(macrolet ((def (lisp-name ir-name)
             `(progn
                (declaim (ftype (function (T) (values Tensor)) ,lisp-name))
                (defun ,lisp-name (x)
                  ,(format nil "[TODO] Docs here")
                  (let ((x (change-facet x :Tensor)))
                    (apply-tir (x) (out) (out (,ir-name (tensor-node x)))))))))
  (def !sin %sin)

  )

(defun !contiguous (x)
  (declare (type tensor x))
  (let ((dst (make-tensor (tensor-shape x) :dtype (tensor-dtype x) :parent x)))
    (apply-tir (x dst) (out) (out (%move (tensor-node dst) (tensor-node x))))))
;; ~~ MovementOps ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun !reshape (x &rest shape)
  (declare (type Tensor x) (type list shape))
  ;; [TODO] Check total count matches
  (let* ((shapes (the list (alexandria:flatten shape)))
         (shape (map 'list #'tensor->id shapes))
         (x (!contiguous x)))
    (apply-tir (x shapes) (reshaped)
               (reshaped
                (%view (tensor-node x) (%shape shape :dtype *default-indexing-dtype*)
                       (loop for i upfrom 0 below (length shape) collect (%iconst 0 :dtype *default-indexing-dtype*))
                       (loop for i upfrom 0 below (length shape) collect (%iconst 1 :dtype *default-indexing-dtype*))
                       (%stride shape (ctx:getenv :DEFAULT_ORDER) :dtype *default-indexing-dtype*))))))

(defun permute-list (order list) (loop for nth in order collect (nth nth list)))
(defun !permute (x &rest order)
  (let ((order (alexandria:flatten order))
        (x (!contiguous x)))
    (flet ((views (n views default)
             (loop for i upfrom 0 below (length views)
                   collect (or (nth n (nth i views)) default)))
           (ids (lst) (map 'list #'tensor->id lst)))
      (apply-tir (x)
          (permuted)
          (permuted
           (%view (tensor-node x)
                  (%shape (ids (permute-list order (tensor-shape x))) :dtype *default-indexing-dtype*)
                  (%shape (ids (permute-list order (views 0 (tensor-views x) 0))) :dtype *default-indexing-dtype*)
                  (%shape (ids (permute-list order (views 1 (tensor-views x) 1))) :dtype *default-indexing-dtype*)
                  (%shape (ids (permute-list order (tensor-stride x))) :dtype *default-indexing-dtype*)))))))

(defun !t (tensor)
  "
```
(!t tensor)
```

Transposes the last two axes of the tensor
"
  (let ((range (range 0 (tensor-nrank tensor)))
	(n (tensor-nrank tensor)))
    (setf (nth (- n 2) range) (nth (- n 1) range)
	  (nth (- n 1) range) (1- (nth (- n 2) range)))
    (!permute tensor range)))

(defun !transpose (tensor &optional (dim0 1) (dim1 0))
  "
```
(!transpose tensor &optional (dim0 1) (dim1 0))
```

Transposes `dim0` and `dim1`.
"
  (declare (type tensor tensor))
  (let* ((range (range 0 (tensor-nrank tensor)))
	 (tmp (nth1 dim0 range)))
    (setf (nth1 dim0 range) (nth1 dim1 range)
	  (nth1 dim1 range) tmp)
    (!permute tensor range)))

(defun !uprank (x n)
  "
```
(!uprank x n)
```

Returns a tensor with one is inserted at the beginning of the shape of `x` for n times.
"
  (declare (type tensor x) (type (integer 0) n))
  (!reshape x (append (loop for i upfrom 0 below n collect 1) (tensor-shape x))))

(defun !flatten (x &key (axis 1))
  "
```
(!flatten x &key (axis 1))
```

Flattens the input tensor into a 2D matrix. If input tensor has shape (d_0, d_1, ... d_n) then the output will have shape (d_0 X d_1 ... d_(axis-1), d_axis X d_(axis+1) ... X dn).
"
  (declare (type tensor x) (type fixnum axis))
  (let* ((axis (normalize-axis x axis))
         (s1 (apply #'!* (map 'list #'->size (subseq (tensor-shape x) 0 axis))))
         (s2 (apply #'!* (map 'list #'->size (subseq (tensor-shape x) axis)))))
    (!reshape x s1 s2)))

(defun !repeat (x &rest repeats &aux (repeats (alexandria:flatten repeats)))
  "
```
(!repeat x &rest repeats)
```

Returns a tensor with the shape of `x` broadcasted by `repeats`.
"
  (let* ((base-shape (append (loop repeat (- (length repeats) (tensor-nrank x)) collect 1) (tensor-shape x)))
	 (new-shape (loop for s in (tensor-shape x) append (list 1 s)))
	 (expand-shape (loop for r in repeats for b in base-shape append (list `(:~ ,r) t)))
	 (final-shape (loop for s in (tensor-shape x) for r in repeats collect (!mul s r))))
    (apply #'!view (!reshape (apply #'!view (!reshape x new-shape) expand-shape) final-shape) (loop for f in final-shape collect t))))

(defun !expand (x &rest shape &aux (shape (alexandria:flatten shape)))
  "
```
(!expand x &rest shape)
```

Returns a tensor that is expanded to the shape that is specified. Expand can also increase the number of dimensions that a tensor has.
"
  (multiple-value-bind (view-index reshape-to) (apply #'values (pad-left (tensor-shape x) shape))
    (let* ((x (if (= (tensor-nrank x) (length shape)) x (!reshape x reshape-to))))
      (apply #'!view x (map 'list #'(lambda (x y) (if (eql x y) t (if (eql x 1) `(:~ ,y) t))) view-index reshape-to)))))

(defun !squeeze (a &rest axis)
  (declare (type Tensor a))
  (let ((axes (normalize-axes a (alexandria:flatten axis))))
    (prog1
        (!reshape a (loop for dim upfrom 0 for size in (tensor-shape a)
                          if (find dim axes) do
                            (progn
                              (assert (eql size 1) () "!squeeze: Cannot squeeze dim size ~a" size)
                              (setf axes (remove dim axes)))
                          else
                            collect size))
      (assert (null axes) () "!squeeze: Cannot squeeze dims ~a" axes))))

(defun !unsqueeze (a axis)
  (declare (type Tensor a) (type fixnum axis))
  (let ((axis (normalize-axis a axis :extra t)))
    (!reshape a (append (subseq (tensor-shape a) 0 axis) (list 1) (subseq (tensor-shape a) axis)))))

(defun !view (x &rest subscripts)
  (declare (type list subscripts) (type tensor x))
  (let* ((x (!contiguous x))
         (views (map 'list #'parse-view-subscript (tensor-shape x) subscripts))
         (sizes (map 'list #'vrange-size views)))
    (apply-tir
        (x (map 'list #'viewrange-from views) (map 'list #'viewrange-to views) (map 'list #'viewrange-by views) sizes)
        (viewed)
        (viewed
         (%view
          (tensor-node x)
          (map 'list #'tensor->id sizes)
          (map 'list (alexandria:compose #'tensor->id #'viewrange-from) views)
          (map 'list (alexandria:compose #'tensor->id #'viewrange-by) views)
          (loop for bc in (map 'list #'viewrange-broadcast views)
                for st in (map 'list #'tensor->id (tensor-stride x))
                if bc collect (%iconst 0 :dtype *default-indexing-dtype*) else collect st))))))
;; [TODO]
;; - [ ] ShapeError
;; - [ ] View Compose
;; - [ ] Renderer Bring Back, Codegen

;; - [ ] Shape Checkとかをちゃんと作る
;; - [ ] VIEW Compose, How to implement them?
;;   - [ ] Option1: Bring Back Shape Tracker
;;   - [ ] Option2: Polyhedral Fusion
;;     - [ ] Solve ILP
;;     - [ ] maximize proximity(v1, v2) s.t.: deps
;;     - [ ] MOVE(X, VIEW(X:Contiguous, *)) = VIEW(x, alpha)
;;   - [ ] SimplifyViewsは必要
;;   - [ ] Always Singleton Optimization: TensorGraphを*CTX*にする
;; things to handle (when dealing (!add Tensor[3 3] Tensor[3 3]))
;; 1. Shape Error
;; 2. Cannot Change Facet Error
;; 3. Broadcasting Error
;; BinaryOps
;; ShapeTracker?

(defun caten (tensor)
  (let ((schedule-graph
          (caten/codegen/lowerer:make-schedule-graph (tensor-graph tensor))))
    schedule-graph))
;; - [ ] 残っている懸念事項
;; - [x] ShapeInference
;;  - [ ] RuntimeCheck => handler-caseで対応？てかもういらないか
;;    - [ ] Error Messageはちゃんとする
;;  - [x] AOTCheck     => defunが難しい
;;    - [x] AOTSimplify => How to do that? 諦める
;;  - [ ] Autograd
;; - [ ] tensor.lisp clean up
;; - [ ] HLOps
;; - [ ] all primitive ops to tensor.lisp?
;; - [ ] nn feats to here?
;; Tensor V2 Design Concepts:
;; - [ ] 計算グラフの表記すらIRへ統合する
;; - [ ] Support Autograd
;;  - [ ] Introduce (defnode :gradient option)
;;  - [ ] VIEW Autodiffを実装可能かどうか
;; - [ ] SINK
;; - [ ] caten/langで記述したコードをVIEWに書き戻せるかどうかを考える。
;;   - [ ] Reschedule(Separate sccs)
;;   - [ ] Polyhedral Modelの自動微分
;;   - [ ] POLYAREF <--> VIEWは相互変換可能
;;   - [ ] 512(i+j) = 512 512の軸を作る
;;   - [ ] Unfoldとかも上の考えで
;; - [ ] einsum
;; - [ ] Goal in a week:
;;   - [ ] API軽量化，Autograd修正，Polyhedral Model Integration
;;   - [ ] Advanced Loop Fusion.
;;   - [ ] Shape CheckをTypeRelayInferで実施可能？
;;   - [ ] !add !subなどにdefine-compiler-macroを適用，
;;   - [ ] ErrorPointみたいなのを登録してあげる
;;   - [ ] GlobalGraphとコンパイル時Shape/DTypeCheck
;;   - [ ] Simplify and FuseされたTensor or ViewをDBに保存，これ

;; - [ ] compile-timeでグラフの作成
;; - [ ] 
;; 1. make-tensorする
;; 2. %alloc/stride computationをemitする (to where?)
;; 3. seen, reshape
;; POW, SIGMOIDとかはdefnode+rewriting ruleでautodiffできそう。
;; VIEWが無理そうだったら，!reshapeで代用

(defun inf (&key (dtype *default-float*))
  "
```
(inf &key (dtype *default-float*))
```
Returns positive infinity of the dtype for the current Common Lisp implementation.

This feature is supported by [float-features](https://shinmera.github.io/float-features/)
"
  (ecase dtype
    (:float64 float-features:double-float-positive-infinity)
    (:float32 float-features:single-float-positive-infinity)
    (:float16 (error "Not ready (TODO)"))
    (:bfloat16 (error "Not ready (TODO)"))))

(defun -inf (&key (dtype *default-float*))
  "
```
(-inf &key (dtype *default-float*))
```

Returns negative infinity of the dtype for the current Common Lisp implementation.

This feature is supported by [float-features](https://shinmera.github.io/float-features/)
"
  (ecase dtype
    (:float64 float-features:double-float-negative-infinity)
    (:float32 float-features:single-float-negative-infinity)
    (:float16 (error "Not ready (TODO)"))
    (:bfloat16 (error "Not ready (TODO)"))))

(defun nan (&key (dtype *default-float*))
  "
```
(nan &key (dtype *default-float*))
```

Returns NaN of the dtype for the current Common Lisp implementation.

This feature is supported by [float-features](https://shinmera.github.io/float-features/)
"
  (ecase dtype
    (:float64 float-features:double-float-nan)
    (:float32 float-features:single-float-nan)
    (:float16 (error "Not ready (TODO)"))
    (:bfloat16 (error "Not ready (TODO)"))))

(defun float-infinity-p (x)
  (declare (type (or symbol number) x))
  (typecase x
    (float
     (float-features:float-infinity-p x))
    (t
     nil)))

(defun float-nan-p (x)
  (declare (type (or symbol number) x))
  (typecase x
    (float (eql x (nan)))
    (t
     nil)))

(declaim (ftype (function ((or symbol number)) (member :inf :-inf :nan t)) float-type-of))
(defun float-type-of (x)
  "
```
(float-type-of x)
```

Returns `:INF` if the number is negative infinity, `:-INF` if the number is negative infinity, `:nan` if the number is NaN, or T otherwise.
"
  (declare (type (or symbol number) x))
  (cond
    ((float-infinity-p x)
     (if (> x 0) :inf :-inf))
    ((float-nan-p x) :nan)
    (t t)))
