;;;; Note: This file is not intended to be loaded directly from Lisp,
;;;; but to be executed expression by expression.
;;;; If you are using Emacs/Lem, you can simply press `C-C C-c` while your cursor is hovering over an expression
(ql:quickload :caten)

(defpackage :getting-started
  (:use :cl :caten/air :caten/aasm :caten/apis :caten/avm))

(in-package :getting-started)
;; [NOTE] since my first language is Japanese; it is efficient to write a draft in Japanese, and translate them into English.

;; A function that displays the present tensor.
(defun present (&rest tensors)
  ;; Variables = (X X X)
  (format t "~{~& =>~% ~A~}" (multiple-value-list (apply #'proceed tensors))))

;; CatenはCommon Lisp製のTensor Libraryです。Catenの仕組みを理解するためには，この資料を用いて以下の3つの主要なシステムの理解をしてください。
;; - 1. caten/apis    (High Level Graph Interface)
;; - 2. caten/air     (Low Level Graph Interface)
;; - 3. caten/codegen (Low Level Graph Interface => Kernel Generator)


;; ~~~[1. High Level Intercace (caten/apis)]~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Caten is a tensor library. Just like Petalisp or Tinygrad, it creates a computation node lazily, and then executes it.

;; 例えば，`make-tensor`を呼び出しても何も現れません。
(print (make-tensor `(3 3) :initial-element 1.0))

;; しかし，答えが欲しいTensorを`(Proceed)`でかこうことによって，Catenは計算グラフを即時にコンパイルし，実行します。
(print (proceed (make-tensor `(3 3) :initial-element 1.0)))

(print (caten (!matmul (make-tensor `(3 3)) (make-tensor `(3 3)))))

;; caten/apisはCatenのUIであり，Numpy-LikeなAPIや自動微分，ShapeTrackerなどの機能を提供します。
;; このシステムを提供するゴールは，Low Level Interfaceをより高レベルなAPIでWrapすることによって，バグを防いだりユーザーがCatenの使用を
;; 簡単にすることです。

;; ~~~[Low Level Interface (caten/air)]~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; 一般に，深層学習コンパイラはDAGに対するコンパイルを実装します。
;; Catenは`caten/air`という汎用的なグラフ処理ライブラリを持っています。
;; Catenは，この`caten/air`を用いた計算グラフを`caten/avm`が実行する機構を保有しています
;; %で始まる関数は，caten/airの計算を作成します。with-contextで%の計算を囲むことで，Catenは自動で実行可能グラフに整形してくれます。
;; caten/avm:%realize関数を用いることで，Graphを実行することができます。
;; ->dotで，全てのAIR GraphをBrowserで可視化することができます。

;; 全てのAIR NodeはWrites/Readsのリストと，Attrsを持っています。
;; 全てのAIRはDumpableであるべきです。(つまり，コンパイルしたグラフをFaslに直接書き込めるべきです。)
(print (make-node :BinaryOps :ADD (list 'a) (list 'b 'c) :reduction t))

(print (make-graph (make-node :BinaryOps :ADD (list 'a) (list 'b 'c))))

;; これらのAPIを綺麗にしたのがaasmです。
(let ((graph
        (with-context
          (x (%fconst 1.0))
          (y (%fconst 2.0))
          (out (%add x y)))))
  (print graph)
  (print (%realize graph))
  ;; (->dot graph)
  )
;; caten/aasmは，AIRを用いた命令セットの定義を提供します。

;; ~~~[A Bridge between AIR and APIs]~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; High Level InterfaceとLow Level Interfaceの架け橋
;; Catenはどうやってcaten/apisのHigh Level InterfaceをLow Level Interfaceに変換するのでしょうか？
;; CatenのHigh Level APIは，aasmをwrapすることで実装できます。試しに新しい計算グラフを実装してみましょう。
(defclass SinCos (Func) nil
  (:documentation "The func SinCos computes sin(cos(x))"))

(defmethod forward ((op SinCos) &rest tensors) (st "A[~] -> A[~]" (tensors)))
;; Optinonal
(defmethod backward ((op SinCos) &optional prev-grad) (declare (ignore prev-grad)) nil)

(defmethod lower ((op SinCos) &rest inputs)
  (let ((x (car inputs)))
    (with-context
      (a (%sin (%add x (%fconst (/ pi 2)))))
      (b (%sin a)))))

(defun !sincos (tensor)
  (forward (make-instance 'SinCos) tensor))

(print (!sincos (make-tensor `(3 3))))

(present
 (!sincos (make-tensor `(3 3) :initial-element 1.0)))

(print (sin (cos 1.0)))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Let's move to another package, 
(defpackage :codegen-example
  (:use :cl :caten/air :caten/aasm :caten/apis :caten/avm :caten/codegen/expr-cache)
  ;; Import some low-level apis
  (:import-from
   :caten/codegen/scheduler
   #:graph-schedule)
  (:import-from
   :caten/codegen/rewriting-rules
   #:apply-rewriting-rules)
  (:import-from
   :caten/codegen/shape-inference
   #:run-type-infer)
  (:import-from
   :caten/codegen/blueprint
   #:lower-schedule-item
   #:print-blueprint))

(in-package :codegen-example)

(defun run-shape-inference (avm)
  (run-type-infer avm)
  (apply-rewriting-rules avm))

;; Two number addition example
(let* ((graph
         ;; Creates a graph: Z(3 3) = X(3 3) + Y(3 3)
         (with-context
           (a (%make-tensor `(3 3) :dtype :float32 :from 'x))
           (b (%make-tensor `(3 3) :dtype :float32 :from 'y))
           (c (%add a b :id 'z))))
       ;; Simplifying the graph
       (_ (optimize-aasm graph))
       ;; Graph: Input=NIL, Output=Z, NAME=axpy-demo graph=graph
       (vm (make-avm graph :axpy-demo nil (list 'z) nil)))
  (declare (ignore _))
  (print vm)
  ;; Running a shape inference
  (run-shape-inference vm)
  ;; Ready for running the scheduler
  (let ((schedule-graph (graph-schedule (avm-graph vm)))
        (*expr-cache* (make-expr-cache)))
    ;; This is your schedule graph
    (print schedule-graph)
    ;; Optimization/Lowering is applied to each schedule-item
    (dolist (item (graph-nodes schedule-graph))
      ;; If schedule-item was labelled as jitable, you can lower this
      (when (getattr item :jitable)
        (lower-schedule-item item (avm-graph vm) schedule-graph)
        ))

    ))

;; ~~~ Forget ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Matmul Example
;; avm-graphを用いてコンパイルされたMatmulのデータ構造を取り出してみましょう。
(defparameter *matmul-graph* (caten (!matmul (make-tensor `(10 20)) (make-tensor `(20 30)))))
(print *matmul-graph*)

;; そのためには，グラフの全てのShapeやOffsetの情報を推論する必要があります。

;; グラフの前処理が必要です。
(run-shape-inference *matmul-graph*)
(print *matmul-graph*)

;; これで準備が整いました。

;; GemmのAASM実装があったはず
(defun schedule-item-from-avm (avm)
  (let ((group (make-group :items (graph-nodes (avm-graph avm)))))
    (group->schedule group (avm-graph avm))))

(defparameter *schedule-item* (schedule-item-from-avm *matmul-graph*))

(print *schedule-item*)

;; [TODO] README.MDを更新する(How To InstallとGetting-Started.lispへ誘導)
