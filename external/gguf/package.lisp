(defpackage :caten/gguf
  (:documentation "[GGUF](https://github.com/ggerganov/ggml/blob/master/docs/gguf.md) to Caten Translator.")
  (:use :cl :fast-io)
  (:export
   #:load-gguf
   #:load-gguf-url
   #:make-gguf
   #:GGUF
   #:gguf-version
   #:gguf-tensor-count
   #:gguf-metadata-kv-count
   #:gguf->state-dict
   #:gguf->bpe-tokenizer
   #:metadata
   #:metadata-key
   #:metadata-value-type
   #:metadata-value
   #:tensor-info
   #:tensor-info-name
   #:tensor-info-n-dimension
   #:tensor-info-dimensions
   #:tensor-info-ggml-type
   #:tensor-info-relative-offset
   #:tensor-info-absolute-offset
   #:tensor-info-buffer
   #:tensor-info->tensor))

(in-package :caten/gguf)
