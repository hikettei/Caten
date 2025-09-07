(in-package :caten/api)

(defparameter *default-indexing-dtype* :int64)
;; NewTensor = apply_tensor_tir(Tensor, TensorProgram)
(defstruct (Tensor
            (:constructor %%make-tensor (graph id)))
  (graph graph :type TensorGraph)
  (id id :type symbol)
  (buffer nil :type (or caten/runtime/buffer:AbstractBuffer null)))

(defun tensor-simplify (tensor)
  (declare (type Tensor tensor))
  ;; 99% of computation time consist of optimize-aasm
  ;; we have to optimize it
  (setf (tensor-graph tensor) (optimize-aasm (tensor-graph tensor) :heavy-opt-threshold 0))
  tensor)

(defun tensor-verify (tensor)
  (declare (type Tensor tensor))
  (graph-infer-type-relay (tensor-graph tensor))
  tensor)

(defun tensor-node (tensor)
  (declare (type Tensor tensor))
  (id->value (tensor-graph tensor) (tensor-id tensor)))

(defun tensor-type (tensor)
  (declare (type Tensor tensor))
  (car (relay-writes (read-type-relay (tensor-node tensor)))))

(defun tensor-shape (tensor)
  (declare (type Tensor tensor))
  (tensor-relay-shape (tensor-type tensor)))

(defun tensor-stride (tensor)
  (declare (type Tensor tensor))
  (tensor-relay-stride (tensor-type tensor)))

(defun tensor-dtype (tensor)
  (declare (type Tensor tensor))
  (tensor-relay-dtype (tensor-type tensor)))

(defun tensor-views (tensor)
  (declare (type Tensor tensor))
  (tensor-relay-views (tensor-type tensor)))

(defmethod print-object ((tensor Tensor) stream)
  (print-unreadable-object (tensor stream :type t)
    (format stream "{Tensor~a[~(~a~)] :shape ~a :id ~a
~a
  :node ~a
  :requires-grad nil}"
            (if (tensor-buffer tensor)
                (format nil "{~a}" (class-name (class-of (tensor-buffer tensor))))
                "")
	    (tensor-dtype tensor)
            (tensor-shape tensor)
	    (tensor-id tensor)
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
;; ~~ APIS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun make-tensor (shape &key (dtype *default-float*) (requires-grad nil) (from nil))
  (tensor-from-graph
   (with-inlined-tir
       (out)
       (out (%make-tensor shape :dtype-indexing *default-indexing-dtype* :dtype dtype :order (ctx:getenv :DEFAULT_ORDER) :from from)))))

(defun !reshape (x &rest shape)
  (declare (type Tensor x) (type list shape))
  ;; [TODO] Check total count matches
  (let ((shape (the list (alexandria:flatten shape))))
    (apply-tir (x shape) (reshaped)
               (reshaped
                (%view (tensor-node x) (%shape shape :dtype *default-indexing-dtype*)
                       (loop for i upfrom 0 below (length shape) collect (%iconst 0 :dtype *default-indexing-dtype*))
                       (loop for i in shape collect (%iconst 0 :dtype *default-indexing-dtype*))
                       (loop for i upfrom 0 below (length shape) collect (%iconst 1 :dtype *default-indexing-dtype*))
                       (loop for i upfrom 0 below (length shape) collect nil)
                       (%stride shape (ctx:getenv :DEFAULT_ORDER) :dtype *default-indexing-dtype*))))))
(defun !add (x y)
  (apply-tir (x y) (out) (out (%add (tensor-node x) (tensor-node y)))))

(defun !mul (x y)
  (apply-tir (x y) (out) (out (%mul (tensor-node x) (tensor-node y)))))

(defun !sin (x)
  (apply-tir (x) (out) (out (%sin (tensor-node x)))))

(defun tensor-realize (tensor)
  (tensor-graph tensor)
  ;; lower-hlops
  )
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
