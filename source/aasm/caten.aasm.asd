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
   (:file "specs/schedule-ops")
   (:file "tensor-ops")
   (:file "runtime-ops")
   (:file "schedule-ops")
   (:file "simplifiers")
   (:file "expr")
   (:file "render-ops")
   (:file "documentation")))
