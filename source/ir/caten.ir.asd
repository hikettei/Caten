(asdf:defsystem "caten.ir"
  :description "IR"
  :author      "hikettei <ichndm@gmail.com>"
  :licence     "MIT"
  :depends-on ("alexandria" "caten.utilities" "caten.graph")
  :serial t
  :components
  ((:file "package")
   (:file "helpers")
   (:file "specs/tensor-ops")
   (:file "specs/render-ops")
   (:file "specs/schedule-ops")
   (:file "tensor-ops")
   (:file "runtime-ops")
   (:file "schedule-ops")
   (:file "simplifiers")
   (:file "render-ops")))
