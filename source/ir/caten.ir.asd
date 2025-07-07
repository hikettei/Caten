(asdf:defsystem "caten.ir"
  :description "Caten IR (Implements caten.aIR)"
  :author      "hikettei <ichndm@gmail.com>"
  :licence     "MIT"
  :depends-on ("alexandria" "caten.common" "caten.air")
  :serial t
  :components
  ((:file "package")
   (:file "helpers")
   (:file "ops")
   (:file "simplifiers")
   (:file "tensor-ops")
   (:file "expr")
   (:file "render-ops")
   (:file "documentation")))
