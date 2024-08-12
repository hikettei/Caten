(in-package :cl-user)

(defpackage :caten/ajit.test
  (:use :cl :rove :caten :caten/nn :caten/air :caten/avm :trivia :caten/ajit))

(in-package :caten/ajit.test)

(defun equal-to (a) #'(lambda (x) (= x a)))
(defun pproceed (params tensor)
  (let ((mdl (caten tensor)))
    (apply #'forward mdl params)))
(defun elements (tensor) (buffer-value (tensor-buffer tensor)))
(defun get-jit-info (avm)
  (declare (type avm avm))
  (let ((jit-info (car (last (graph-nodes (avm-graph avm))))))
    (assert (eql (node-type jit-info) :JIT_KERNEL) () "the kernel didn't returned a jit_kernel")
    jit-info))
(defun n-kernels (avm)
  (declare (type avm avm))
  (jit-info-n-kernels (getattr (get-jit-info avm) :jit-info)))
(defun check-kernels (n avm)
  (if (= 1 (ctx:getenv :JIT))
      (ok (= n (n-kernels avm)))
      (skip "Needs JIT")))
;; [TODO] MULTIEXPRを実装
;; TODO: Pooling2D, Conv2D, Gemm, Composed Gemm (count the number of kernels)
;; TODO: Tensor-Shaped-Tesnor Operation
;; TODO: Tensor-Shaped-Tensor Iteration Rendering (The scalar result should be passed via arguments)
;; TODO: Mixed use of dynamic shape and scalar values (e.g.: Adding Float[n, 10] += n)
;; (jit (caten (!add (make-tensor `(a 10)) (!cast (fconst 'a) :float32))) :debug 4)

;; TestCase1. (caten (!mean (make-tensor `(a b c)) :axis t))
;; TestCase2. (caten (!tan (make-tensor `(10 10))))
;; TestCase3. (let ((*external-simplifiers* nil)) (let ((a (pproceed `((a . 2)) (make-tensor `(a 10) :initial-element 'a :dtype :uint32)))) (ok (and (every (equal-to 2) (elements a)) (= (length (elements a)) 20)))))
;; TestCase4. (caten (!add (!view (make-tensor `(n)) `(froma toa bya)) (!view (make-tensor `(n)) `(fromb tob byb))))

(deftest check-kernel-counts
  (with-no-grad
    (check-kernels 1 (caten (!matmul (make-tensor `(10 20)) (make-tensor `(20 10)))))
    (check-kernels 1 (caten (!softmax (ax+b `(10 10) 1 1))))
    (check-kernels 1 (caten (!cos (!sin (!padding (make-tensor `(10 10) :initial-element 2.0) `((2 2) (2 2)) :value 0.0)))))
    (check-kernels 1 (caten (!sin (!matmul (make-tensor `(10 20)) (make-tensor `(20 30))))))
    (check-kernels 2 (caten (!matmul (make-tensor `(128 32)) (!matmul (make-tensor `(32 64)) (make-tensor `(64 128))))))
    (check-kernels 1 (caten (!add (!view (make-tensor `(n)) `(froma toa bya)) (!view (make-tensor `(n)) `(fromb tob byb)))))
    (check-kernels 1 (caten (!tan (make-tensor `(10 10)))))
    (check-kernels 2 (caten (forward (ConvND 3 6 `(5 5)) (make-tensor `(10 3 25 25)))))))
    
(deftest tensor-shaped-tensor-test
  (let* ((size1 (!add (iconst 'a) (iconst 'a)))
	 (size2 (!add (iconst 'b) (iconst 'b)))
	 (tensor (caten (make-tensor `(,size1 ,size2) :initial-element 'a)))
	 (out (elements (forward tensor `(a . 4) `(b . 8)))))
    (ok (= (length out) 128))
    (ok (every (equal-to 4) out)))
  (let* ((size1 (!add (iconst 'a) (iconst 'a)))
	 (size2 (!add (iconst 'b) (iconst 'b)))
	 (tensor (caten (!sin (make-tensor `(,size1 ,size2) :initial-element 'a))))
	 (out (elements (forward tensor `(a . 4) `(b . 8)))))
    (ok (= (length out) 128))
    (ok (every (equal-to (sin 4)) out))))

;; TODO LIST:
;;  TODO: making isl objects gc-reachable (-> ctxに紐付けておく)
;;  TODO: Symbolic Graph Compilation
;;      - Step1 Numberと同じようにCompileできるようにする (if not appeared in strides)
;;              - Testing: Stride計算がTensorでもOK?
;;      - Step2 (Strideの計算は適当な整数値(素数)で置き換える)
;;  FIX:  Bugs (more symbolic deps needed)
;;  ADD: METAL/OMP, parallelize dependencies analysis
;;  ADD: If/For Node in the early stage!!!!
;; 正しいコンパイル:
;;   Step1, Dynamic Shapeと入力変数のみを受け入れる
;;   Step2, tmpvarの振る舞い...
;;   Step3, JIT-CompiledのArgsのテスト (axpy, symbolic meanで検証)
;;   Aref ga buffer no toki overwrite???
;; TODO: View計算もExprに含めたい (OK)
;; 今やってないこと:
;; apply-multiexpr-grouping無しでも動作するべき (:MULTIEXPR=1, CI Testに含める)
;; Backward?
;; RendererをRefactorする。aRI GraphのRenderingを廃止する？
;; MULTIEXPR=0でテストを通すべきだと思う
;; TODO: Ternary Ops %where
;; 一時領域の判定ができると思う = (Allocationに宣言されてないUndefined Variable)
;; Pipelineを跨いでWriteに依存はない？
;; Esp: when creating backwards
;; Write-toのUpdateがおかしい
;; やること
;; 1. Tanを動かす (ok ) -> Undefined-Varの処理を追加 (ok)
;; 2. In-place-mutationをapply-memory-plannerにする (ok)
;; 3. MULTIEXPR=1 or 0をCIに含める (no)
;; 4. JIT-Compilation Backwardを実装
;; 5. ^ 途中でMoveが含まれる時，うまく分割する
;; Backward実装したら，int xxx = x[...];を実装
;; JIT=0 JIT=1 でBackwardが同じかTestする
;; (!tan (!matmul ...))のScheduler修正
;; Step1 ~ 4をテストに含める
;; multiexprを実装
;; Softmax ... reductionの依存関係の記述の問題 or Bufferの一次領域の問題
;; TODO: Compilerのレベルで作業したくない， CopyNodeとIn-Place Mutationを実装する
;; Symbolic動かすには？？？
;; DIVが一つのKernelにFuseされないと困るのでは・・・
;; Softmax: needs to be fused into a single kernel.
;; Softmax/Upfromが動かない理由は同じ(In-placeの区別がない)
;; In-place-test

;; Here's TODO List
;; - 1. 最初のSchedulingアルゴリズムを見直す: (recip(x)はmulと同じようにScheduleされるべき)
;; - 2. ノードを跨いで依存がある時は"Compilerが"一次領域を作成する
;; - 3. SaveForbackward/Copyを実装する

#+(or)(progn
(caten (!sin (!matmul (make-tensor `(10 20)) (make-tensor `(20 30)))))
(caten (!matmul (make-tensor `(10 20)) (make-tensor `(20 30))))
(caten (!softmax (make-tensor `(10 10) :initial-element 1.0)))
(caten (!softmax (ax+b `(10 10) 1 1)))
(caten (!cos (!sin (!padding (make-tensor `(10 10) :initial-element 2.0) `((2 2) (2 2)) :value 0.0))))
(caten (!matmul (make-tensor `(128 32)) (!matmul (make-tensor `(32 64)) (make-tensor `(64 128)))))
(caten (!add (!view (make-tensor `(n)) `(froma toa bya)) (!view (make-tensor `(n)) `(fromb tob byb))))
(caten (!mean (make-tensor `(a b c))))
(caten (!tan (make-tensor `(10 10))))
;;(with-no-grad (caten (forward (ConvND 3 6 `(3 3)) (make-tensor `(10 3 25 25)))))
(forward (caten (!sin (!sin (!sin (ax+b `(10 10) 1 0))))))
(let ((*external-simplifiers* nil)) (let ((a (pproceed `((a . 2)) (make-tensor `(a 10) :initial-element 'a :dtype :uint32)))) (ok (and (every (equal-to 2) (elements a)) (= (length (elements a)) 20))))))

;; (let ((a (make-tensor `(3))))
;;	 (caten (!div a a)))
(deftest in-place-test
  (let* ((a (make-tensor `(10 10) :initial-element 1.0))
	 (b (!exp a))
	 (c (!div a b)))
    ;; 同一のKernelにScheduleされる Or Copyを作成する必要がある
    (caten c))

  ;; todo: softmax in a single kernel
  
  )
;; tensor-viewded-tensor-testはどうしようか
(deftest tensor-viewed-tensor-test
  (testing "Upfrom"
    (let* ((v1 (!add (iconst 'a) (iconst 'a)))
	   (v2 (!add (iconst 'b) (iconst 'b)))
	   (c (make-tensor `(10 10) :initial-element 1.0))
	   (out (!contiguous (!view c `(,v1 10) `(,v2 10))))
	   (model (caten out))
	   (result (forward model `(a . 1) `(b . 2))))
      (ok (equal `(8 6) (buffer-shape (tensor-buffer result))))
      (ok (= 48 (length (elements result))))
      (ok (every (equal-to 1.0) (elements result)))))
  (testing "Below"

    )
  (testing "By"

    )
  (testing "Broadcast"

    ))
