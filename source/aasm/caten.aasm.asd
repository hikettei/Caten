(asdf:defsystem "caten.aasm"
  :description "aasm = Abstract + (internal) assembly"
  :author      "hikettei <ichndm@gmail.com>"
  :licence     "MIT"
  :depends-on ("alexandria" "caten.common" "caten.air")
  :serial t
  :components
  ((:file "package")
   (:file "helpers")
   (:file "ops")
   (:file "tensor-ops")
   (:file "render-ops")   
   (:file "simplifiers")
   (:file "documentation")))
