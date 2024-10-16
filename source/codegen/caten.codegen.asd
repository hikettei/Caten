(asdf:defsystem "caten.codegen"
  :description "Caten Code Generator Engine"
  :author "hikettei <ichndm@gmail.com>"
  :licence "MIT"
  :depends-on ("caten.avm" "caten.aasm" "caten.air")
  :serial t
  :components
  ((:file "helpers")
   (:file "shape-inference")
   (:file "rewriting-rules")
   (:file "scheduler")
   (:file "jit")
   (:file "package")))
