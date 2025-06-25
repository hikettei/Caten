(asdf:defsystem "caten.ir"
  :description "ir = Internal Representation"
  :author      "hikettei <ichndm@gmail.com>"
  :licence     "MIT"
  :depends-on ("alexandria" "caten.common" "caten.air")
  :serial t
  :components
  ((:file "package")
   (:file "helpers")
   (:file "attrs")
   (:file "ctx")
   (:file "tensor-ir")
   (:file "ops")
   (:file "constant-folding")
   (:file "optimizers")
   (:file "view")
   (:file "logical")
   (:file "documentation")))
