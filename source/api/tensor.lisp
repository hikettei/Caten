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

(defun tensor-node (tensor)
  (declare (type Tensor tensor))
  (id->value (tensor-graph tensor) (tensor-id tensor)))
;; POW, SIGMOIDとかはdefnode+rewriting ruleでautodiffできそうね。
;; VIEWが無理そうだったら，!reshapeで代用
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
  (tensor-simplify (%%make-tensor tensor-graph (car (graph-outputs tensor-graph)))))

(defun apply-tensor-graph (variables tensor-graph) ;; = Forward
  "MakeTensor(Graph=Concat(Tensor->graph, tensor_graph))"
  (declare (type TensorGraph tensor-graph) (type list variables))
  (assert (every #'tensor-p variables) () "apply-tensor-graph: Each of variables must be a Tensor.")
  (assert (= 1 (length (graph-outputs tensor-graph))) () "apply-tensor-graph: The output tensor id must be identical")
  (tensor-simplify (%%make-tensor (%concat-tensor-graph (map 'list #'tensor-graph variables) tensor-graph) (car (graph-outputs tensor-graph)))))

(defmacro with-inlined-tir ((&rest out-binds) &rest forms)
  (alexandria:with-gensyms (outputs)
    `(locally (declare (optimize (speed 3)))
       (->fast-graph
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
        :cls 'TensorGraph))))

(defmacro apply-tir ((&rest variables) (&rest out-binds) &rest program)
  `(apply-tensor-graph (list ,@variables) (with-inlined-tir (,@out-binds) ,@program)))
;; コンパイル時は*ctx*を別に作ってSimplifyすることができる
(defun make-tensor (shape &key (dtype *default-float*) (order *default-order*) (requires-grad nil) (from nil))
  (tensor-from-graph
   (with-inlined-tir (out)
       (out (%make-tensor shape :dtype-indexing *default-indexing-dtype* :dtype dtype :order order :from from)))))

(defun !add (x y)
  (apply-tir (x y) (out) (out (%add (tensor-node x) (tensor-node y)))))

(defun %retrive (tensor)
  ;; If ID exists in DB =>
  ;; Otherwise => Recompile
  )

(defun tensor-realize (tensor)
  (tensor-graph tensor))
;; (!add (make-tensor `(3 3)) (make-tensor `(3 3)))
;; (A B) (A B)
;;    \   /
;;      +
;;    Tensor
