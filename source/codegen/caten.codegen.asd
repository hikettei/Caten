(asdf:defsystem "caten.codegen"
  :description "Caten Code Generator Engine"
  :author "hikettei <ichndm@gmail.com>"
  :licence "MIT"
  :depends-on ("caten.avm" "caten.aasm" "caten.air")
  :serial t
  :components
  ((:file "helpers")
   (:file "expr")
   (:file "renderer")
   (:file "shape-inference")
   (:file "rewriting-rules")
   (:file "scheduler")
   (:file "blueprint")
   (:file "scop")
   (:file "jit")
   (:file "package")))
