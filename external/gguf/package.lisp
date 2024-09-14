(defpackage :caten/gguf
  (:documentation "[GGUF](https://github.com/ggerganov/ggml/blob/master/docs/gguf.md) to Caten Translator.")
  (:use :cl :fast-io)
  (:export
   #:load-gguf
   #:make-gguf))

(in-package :caten/gguf)
