(asdf:defsystem "caten.codegen"
  :description "Caten Code Generator Engine"
  :author "hikettei <ichndm@gmail.com>"
  :licence "MIT"
  :depends-on ("caten.avm" "caten.aasm" "caten.air" "caten.polyhedral" "cl-ppcre" "alexandria" "lparallel")
  :serial t
  :components
  ((:file "helpers")
   (:file "expr")
   (:file "shape-inference")
   (:file "renderer")
   (:file "expr-cache")
   (:file "rewriting-rules")
   (:file "memory-planner")
   (:file "scheduler")
   (:file "exprify")
   (:file "blueprint")
   (:file "auto-scheduler/polyhedral")
   (:file "auto-scheduler/polyhedral-ast")
   (:file "auto-scheduler/tiling")
   (:file "auto-scheduler/coincidence")
   (:file "auto-scheduler/vectorize")
   (:file "auto-scheduler/unroll")
   (:file "auto-scheduler/directive")
   (:file "auto-scheduler/ast-parser")
   (:file "auto-scheduler/scop")
   (:file "auto-scheduler/config")
   (:file "auto-scheduler/auto-scheduler")
   (:file "jit")
   (:file "runner")
   (:file "backends/clang")
   (:file "package")
   (:file "documentation")))
