(asdf:defsystem "caten"
  :description "Programmable Deep Learning Framework"
  :author      "hikettei <ichndm@gmail.com>"
  :version     "0.0"
  :licence     "MIT"
  :depends-on
  ("caten.apis" "caten.nn")
  :serial t
  :components ((:file "source/caten-user") (:file "source/documentation"))
  :in-order-to
  ((test-op (test-op "caten/test"))))

(asdf:defsystem "caten/test"
  :description "Test suites for Caten"
  :depends-on ("caten" "caten/llm" "caten.test-suite")
  :in-order-to
  ((test-op
    (asdf:test-op "caten.apis")
    (asdf:test-op "caten.nn")
    (asdf:test-op "caten.test-suite"))))
;; External system for Caten.
;; Systems including non-portable dependencies (e.g.: CUDA, Metal) or systems cannot be guaranteed to be maintained, are separated from caten.
(asdf:defsystem "caten/metal"
  :description "Metal extension for Caten"
  :author      "hikettei <ichndm@gmail.com>"
  :licence     "MIT"
  :depends-on ("cl-metal" "caten.apis")
  :serial t
  :components
  ((:file "external/backends/metal")))

(asdf:defsystem "caten/gguf"
  :description "[gguf](https://github.com/ggerganov/ggml/blob/master/docs/gguf.md) format translator."
  :author "hikettei <ichndm@gmail.com>"
  :depends-on ("caten.apis" "babel" "fast-io")
  :serial t
  :pathname "external/gguf"
  :components
  ((:file "package")
   (:file "helpers")
   (:file "metadata")
   (:file "tensor-info")
   (:file "gguf-file")
   (:file "dequantize")))

(asdf:defsystem "caten/llm"
  :description "Various Generative Language Model Implementation in Caten"
  :author "hikettei <ichndm@gmail.com>"
  :depends-on ("caten")
  :pathname "external/llm"
  :components
  ((:file "package")
   (:file "bpe")
   (:file "tokenizer")
   (:file "generate")
   (:file "layers")))
