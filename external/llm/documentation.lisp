(defpackage :caten/llm.documentation
  (:use :cl :caten/common.documentation))

(in-package :caten/llm.documentation)

(define-page ("caten/llm" "packages/caten.external.llm.md")
  (title "Overview")
  (body "TODO: Docs")
  (subtitle "Tokenizers")
  (body "TODO: Docs")
  (doc/class "Tokenizer" 'caten/llm:Tokenizer)
  (doc/class "BPETokenizer" 'caten/llm:BPETokenizer)
  (doc/generic "encode" 'caten/llm:encode)
  (doc/generic "decode" 'caten/llm:decode)
  (subtitle "Models")
  (body "
(TODO: Complete docs)

Definitions are here: https://github.com/hikettei/Caten/blob/main/external/llm/layers.lisp

Currently, the following models are implemented and tested:

- Attention

- Scaled-Dot-Product-Attention

- FeedForward

- TransformerBlock

- Transformer
"))
