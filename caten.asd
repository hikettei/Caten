(asdf:defsystem "caten"
  :description "Programmable Deep Learning Framework"
  :author      "hikettei <ichndm@gmail.com>"
  :licence     "MIT"
  :depends-on
  ("caten.apis" "caten.nn" "caten.test-suite")
  :serial t
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
  :depends-on ("caten.apis")
  :serial t
  :components
  ((:file "external/gguf/package")))
