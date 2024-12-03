(defpackage :caten/gguf.docs
  (:use :cl :caten/common.documentation))
(in-package :caten/gguf.docs)

(define-page ("caten/gguf" "packages/caten.external.gguf.md")
  (title "GGUF")
  (body "Looking to work with quantized models? The `caten/gguf` package enables you to read quantized model weights directly from GGUF files, construct a StateDict, and create tokenizers for `caten/llm`. It's a convenient tool for integrating quantized models into your projects.

Please note that the quantization functionality currently supports only a limited number of bit depths. We appreciate your understanding and are continuously working to expand this feature.

(As of this writing, fp32/fp16 only, Int8 Quant will be added soon!)
")
  (doc/class "GGUF" 'caten/gguf:GGUF)
  (doc/function "make-gguf" #'caten/gguf:make-gguf)
  (doc/function "load-gguf" #'caten/gguf:load-gguf)
  (doc/function "load-gguf-url" #'caten/gguf:load-gguf-url)
  (doc/function "gguf->state-dict" #'caten/gguf:gguf->state-dict)
  (doc/function "gguf->bpe-tokenizer" #'caten/gguf:gguf->bpe-tokenizer)
  (doc/struct "Metadata" 'caten/gguf:Metadata)
  (doc/struct "Tensor-Info" 'caten/gguf:Tensor-Info))
