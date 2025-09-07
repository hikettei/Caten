(in-package :caten/api)
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
(defparameter *default-indexing-dtype* :int64)
;; NewTensor = apply_tensor_tir(Tensor, TensorProgram)
(defstruct (Tensor
            (:constructor %%make-tensor (graph id)))
  (graph graph :type TensorGraph)
  (id id :type symbol)
  (buffer nil :type null))

(defun tensor-simplify (tensor)
  (declare (type Tensor tensor))
  (optimize-aasm (tensor-graph tensor))
  tensor)
;; POW, SIGMOIDとかはdefnode+rewriting ruleでautodiffできそうね。
(defun %concat-tensor-graph (parent-graph child-graph)
  (declare (type TensorGraph parent-graph child-graph))
  (assert (null (graph-seen parent-graph)))
  (assert (null (graph-seen child-graph)))
  (let ((g (make-instance 'TensorGraph :output (graph-outputs child-graph) :seen nil :nodes nil)))
    (maphash #'(lambda (k v) (setf (gethash k (%graph-nodes-table g)) v)) (%graph-nodes-table parent-graph))
    (maphash #'(lambda (k v) (setf (gethash k (%graph-nodes-table g)) v)) (%graph-nodes-table child-graph))
    g))

(defun tensor-from-graph (tensor-graph) ;; Root
  (declare (type TensorGraph tensor-graph))
  (assert (= 1 (length (graph-outputs tensor-graph))) () "tensor-from-graph: The output tensor id must be identical")
  (tensor-simplify (%%make-tensor tensor-graph (car (graph-outputs tensor-graph)))))

(defun apply-tensor-graph (tensor tensor-graph) ;; Forward
  "MakeTensor(Graph=Concat(Tensor->graph, tensor_graph))"
  (declare (type Tensor tensor) (type TensorGraph tensor-graph))
  (assert (= 1 (length (graph-outputs tensor-graph))) () "apply-tensor-graph: The output tensor id must be identical")
  (%%make-tensor (%concat-tensor-graph (tensor-graph tensor) tensor-graph) (car (graph-outputs tensor-graph))))

(defmacro with-inlined-tir ((&rest out-binds) &rest forms)
  `(->fast-graph
    (alexandria:with-gensyms (,@out-binds)
      (let ((g (with-context ,@forms)))
        (setf (graph-outputs g) (list ,@out-binds))
        g))
    :cls 'TensorGraph))

(defmacro apply-tir (tensor (out-binds) &rest program)
  `(apply-tensor-graph ,tensor (with-inlined-tir (,@out-binds) ,@program)))
;; コンパイル時は*ctx*を別に作ってSimplifyすることができる
(defun make-tensor (shape &key (dtype *default-float*) (order *default-order*) (requires-grad nil) (from nil))
  (tensor-from-graph
   (with-inlined-tir
       (tensor)
       (_ (%make-tensor shape :dtype-indexing *default-indexing-dtype* :dtype dtype :order order :id tensor :from from)))))

(defun %retrive (tensor)
  ;; If ID exists in DB =>
  ;; Otherwise => Recompile
  )
;; (!add (make-tensor `(3 3)) (make-tensor `(3 3)))
;; (A B) (A B)
;;    \   /
;;      +
;;    Tensor
