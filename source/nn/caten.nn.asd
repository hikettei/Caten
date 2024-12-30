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
   (:file "criterion")
   (:file "padding")
   (:file "pool")
   (:file "normalization")
   (:file "embedding")
   (:file "linear")
   (:file "positional-encoding")
   (:file "optimizers")
   (:file "documentation")))
