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
   #:apply-rewriting-rules
   #:schedule-item-write-define-global)
  (:import-from
   :caten/codegen/shape-inference
   #:run-type-infer)
  (:import-from
   :caten/codegen/blueprint
   #:lower-schedule-item
   #:print-blueprint)
  (:import-from
   :caten/codegen/renderer
   #:CStyle-Renderer
   #:%render-kernel
   #:%compile-kernel)
  (:import-from
   :caten/codegen/jit
   #:schedule-graph->avm-graph))

(in-package :codegen-example)

(defun run-shape-inference (avm)
  (run-type-infer avm)
  (apply-rewriting-rules avm))

(defparameter *width* 120)
(defun saying (number title object)
  (dotimes (i *width*) (princ "="))
  (fresh-line)
  (format t "~a. ~a~%~a~%" number title object)
  object)

;; Utils
(defun try-codegen! (graph outputs)
  (optimize-aasm graph)
  (let ((vm (make-avm graph :main nil outputs nil)))
    (fresh-line)
    (saying 1 "Compiling the following initial computation graph:" graph)
    (saying 2 "Created AVM (Abstract VM) with the computation graph:" vm)
    (run-shape-inference vm)
    (let ((schedule-graph (graph-schedule (avm-graph vm)))
          (*expr-cache* (make-expr-cache))
          (renderer (make-instance 'CStyle-Renderer)))
      (saying 3 "Generated schedule-graph with the computation graph" schedule-graph)
      (dolist (item (graph-nodes schedule-graph))
        ;; If schedule-item was labelled as jitable, you can lower this
        (when (getattr item :jitable)
          (lower-schedule-item item (avm-graph vm) schedule-graph)
          (saying 4 "Lowered schedule item to a blueprint suitable for code generation:" (print-blueprint (getattr item :blueprint) nil))
          (schedule-item-write-define-global item)
          (let ((c-kernel (%render-kernel renderer item)))
            (saying 5 "Generated C code from the blueprint:" c-kernel)
            (setf (getattr item :rendered-object) c-kernel))))
      ;; Invoking gcc ...
      (%compile-kernel renderer (graph-nodes schedule-graph) nil)
      ;; Overwrite the base graph with compiled graph
      (setf (avm-graph vm) (schedule-graph->avm-graph (avm-graph vm) schedule-graph))
      (avm-reset vm))))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; 1. Two matrices addition example:
(let* ((graph
         ;; This is a graph to compile: Z(3 3) = X(3 3) + Y(3 3)
         (with-context
           (x (%make-tensor `(3 3) :dtype :float32 :from 'x))
           (y (%make-tensor `(3 3) :dtype :float32 :from 'y))
           (c (%add x y :id 'z))))
       ;; Simplifying the input graph
       (_ (optimize-aasm graph))
       ;; Wrap the graph as an instance of AVM to manage allocations
       (vm (make-avm graph :axpy-demo nil (list 'z) nil)))
  (declare (ignore _))
  (fresh-line)
  (saying 1 "Compiling the following initial computation graph:" graph)
  (saying 2 "Created AVM (Abstract VM) with the computation graph:" vm)
  ;; caten/codegen requires the shape of all computation nodes to be known!
  (run-shape-inference vm)
  ;; Ready for running the scheduler. `graph-schedule` to partition the input graph.
  (let ((schedule-graph (graph-schedule (avm-graph vm)))
        (*expr-cache* (make-expr-cache))
        (renderer (make-instance 'CStyle-Renderer)))
    (saying 3 "Generated schedule-graph with the computation graph" schedule-graph)
    (dolist (item (graph-nodes schedule-graph))
      ;; If schedule-item was labelled as jitable, you can lower this
      (when (getattr item :jitable)
        (lower-schedule-item item (avm-graph vm) schedule-graph)
        (saying 4 "Lowered schedule item to a blueprint suitable for code generation:" (print-blueprint (getattr item :blueprint) nil))
        (schedule-item-write-define-global item)
        (let ((c-kernel (%render-kernel renderer item)))
          (saying 5 "Generated C code from the blueprint:" c-kernel)
          (setf (getattr item :rendered-object) c-kernel))))
    ;; Invoking gcc ...
    (%compile-kernel renderer (graph-nodes schedule-graph) nil)
    ;; Overwrite the base graph with compiled graph
    (setf (avm-graph vm) (schedule-graph->avm-graph (avm-graph vm) schedule-graph))
    (avm-reset vm)
    ;; Try axpy!
    (saying
     6 "Running the computation X(3x3) + Y(3x3), the result is:"
     (%run vm (cons 'x (linspace `(3 3) 1 0)) (cons 'y (linspace `(3 3) 1 0))))))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun %make-squared-gemm (N out)
  "a @ b.T"
  (optimize-aasm
   (with-context
     (a  (%make-tensor `(,n ,n)))
     (b  (%make-tensor `(,n ,n)))
     (a1 (%view a `(,n ,n ,n) `(0 0 0) `(,n ,n ,n) `(1 1 1) `(nil t nil) (%stride `(,n 1 ,n) :row)))
     (b1 (%view b `(,n ,n ,n) `(0 0 0) `(,n ,n ,n) `(1 1 1) `(t nil nil) (%stride `(1 ,n ,n) :row)))
     (o  (%mul a1 b1))
     (c  (%load (%make-tensor `(,n ,n 1)) 0.0))
     (c  (%view c `(,n ,n 1) `(0 0 0) `(,n ,n ,n) `(1 1 1) `(nil nil t) (%stride `(,n ,n 1) :row)))
     (c  (%add c o :reduction t))
     (c  (%reshape c `(,n ,n) :id out)))))
;; Try matmul
(try-codegen!
 (%make-squared-gemm 128 'out)
 (list 'out))

#+(or nil)
(try-codegen!
 (make-graph
  ;; your code follows ...
  )
 (list ... ))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; 前のセクションでExampleとして提供されたコードは，JIT_DEBUG >= 3を与えることでより美しくデバッグできます。
(defun present-beautiful (tensor &key (auto-scheduler 0))
  (ctx:with-contextvar (:JIT 1 :JIT_DEBUG 3 :AUTO_SCHEDULER auto-scheduler)
    (with-no-grad (caten tensor))))

;; auto-scheduler is WIP as of this writing!
(present-beautiful
 (!matmul (make-tensor `(128 512)) (make-tensor `(512 1024)))
 :auto-scheduler 1)

(present-beautiful
 (!matmul (make-tensor `(a b)) (make-tensor `(b c))))

(present-beautiful
 (forward (caten/nn:Embedding 10 20) (make-tensor `(b 20))))

;; [TODO] README.MDを更新する(How To InstallとGetting-Started.lispへ誘導)
