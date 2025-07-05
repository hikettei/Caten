(asdf:defsystem "caten.aasm"
  :description "aasm = Abstract + (internal) assembly"
  :author      "hikettei <ichndm@gmail.com>"
  :licence     "MIT"
  :depends-on ("alexandria" "caten.common" "caten.air")
  :serial t
  :components
  ((:file "package")
   (:file "helpers")
   (:file "specs/tensor-ops")
   (:file "specs/render-ops")
   (:file "specs/runtime-ops")
   (:file "ctx")
   (:file "tensor-ir")
   (:file "ops")
   (:file "constant-folding")
   (:file "optimizers")
   (:file "view")
   (:file "logical")
   (:file "documentation")))
