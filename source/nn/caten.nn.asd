(asdf:defsystem "caten.nn"
  :description "Caten Neural Network Library"
  :author      "hikettei <ichndm@gmail.com>"
  :licence     "MIT"
  :depends-on ("rove" "caten.apis")
  :serial t
  :components
  ((:file "package")
   (:file "activations")
   (:file "conv")
   (:file "padding")
   (:file "pool")
   (:file "normalization")
   (:file "embedding")
   (:file "linear")
   (:file "positional-encoding"))
  :perform (test-op (o s) (uiop:symbol-call (find-package :rove) :run s :style :dot)))
