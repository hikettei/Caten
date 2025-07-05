(asdf:defsystem "caten.aasm"
  :description "aasm = Abstract + ASseMbly."
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
   (:file "tensor-ops")
   (:file "simplifiers")
   (:file "documentation")))
