(in-package :caten/workflow)

(defclass WorkflowConfig ()
  ;; 各Actionを初期化するのに使用する予定
  nil)

(defmacro defworkflow (name (&rest args) &rest actions)
  `(progn
     ))

;; Syntax ->: 次の入力のcarがoutputになる

;; (defmethod export-to ())
;; (export )
;; Here, = and Length are the action
(defworkflow LLMInference (tokens transformer)
  (SentencePiece Encode tokens)
  ->
  (LoopUntil
   (= (Length tokens) max-tokens)
   (Run Transformer))
  ->
  (GetLogits)
  ->
  (SentencePiece Decode tokens))

;; w/ kv-cache?
(defworkflow LLMInference (tokens transformer max-tokens)
  (ResetKVCache transformer)
  (ResetKVCache transformer)
  (Tokenize tokens)
  ->
  (LoopUntil
   (= (Length) max-tokens)
   (Run Transformer))
  ->
  (GetLogits))

(defworkflow ImageClassifier (model image)
  (Preprocess) -> (Run model) -> (ArgmaxLabel))

             
             
