(in-package :caten/workflow)

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Goal: Export the entire code (including tokenizer, data loader, matrix computation kernel) for any language
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; caten/workflow
;; - Includes a wrapper for `caten`.
;; - Includes a class for Tokenizer.
;; - Each workflow can be compiled into C, or any language, using caten/ajit renderer (comptible with them).

;; Workflow and Concepts
;; - Implement Export2C Mode
;; - Implement Control, High-Level Interface, Including Function defining, IF/FOR
;;   - DSL

;; [TODO]
;; - ここでこのモデル全体をCommon Lispで読み込むことによって，モデルのコンパイル結果をCache可能にする
;; - 若しくは，C言語/Python/Smaller Common LispへCompileして実行を可能に

;; Backend has two facets:
;; - Kernel Renderer   [ Low-Level  ]
;; - Workflow Renderer [ High-Level ]

;; TODO: Compile the entire graph. Including token sampling
;;      Input
;;        |
;; [Action: Tokenizer]
;;        |
;;  |-[Action:Run]-|
;;  | Transformer  | <-------------------------------|
;;  |--------------|                                 |
;;        |                                          | x N
;;        |----[Action: Logits Argmax Concatenate] --|
;;        |    
;;    [Output]
;;        |

;; Action:
;; - Is a CLOS class
;; - Only initialized in defworkflow macro
;; - Workflow is a aIR graph

(defclass Action ()
  nil
  )

;; Each action can be compiled into Render-Graph first, and then each language

(defmacro defaction (name (&rest args) &body body)
  ""

  )

(defaction Tokenizer (tokens)
  ;; 各ActionはConfigを受け取って各自Classを初期化できる
  ;; 各ActionはDSLを使って動作を定義できる
  )

;; TODO
;; - RendererのRefactorが必要
;; - Polyhedralを使わない

;; =, Length are action
;; TODO: workflow configを一緒に提供する

;; (defaction switch (condition action1 action2)
;;
;;
;;
;;
;;

