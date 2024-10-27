(asdf:defsystem "caten.codegen"
  :description "Caten Code Generator Engine"
  :author "hikettei <ichndm@gmail.com>"
  :licence "MIT"
  :depends-on ("caten.avm" "caten.aasm" "caten.air" "caten.polyhedral" "cl-ppcre" "alexandria")
  :serial t
  :components
  ((:file "helpers")
   (:file "expr")
   (:file "shape-inference")
   (:file "renderer")
   (:file "expr-cache")
   (:file "rewriting-rules")
   (:file "scheduler")
   (:file "exprify")
   (:file "blueprint")
   (:file "polyhedral-ast")
   (:file "scop")
   (:file "memory-planner")
   (:file "jit")
   (:file "backends/clang")
   (:file "package")))
