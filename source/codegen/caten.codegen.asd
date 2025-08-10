(asdf:defsystem "caten.codegen"
  :description "Caten Code Generator Engine"
  :author "hikettei <ichndm@gmail.com>"
  :licence "MIT"
  :depends-on ("caten.runtime" "caten.aasm" "caten.air" "caten.isl" "cl-ppcre" "alexandria" "lparallel" "cl-yaml" "cffi")
  :serial t
  :components
  ((:file "helpers")
   (:file "byoc")
   (:file "iteration")
   (:file "renderer")
   (:file "rewriting-rules")
   (:file "scheduler")
   (:file "realize")
   (:file "blueprint")
   (:file "memory-planner")
   (:file "pprinter")
   ;; AutoSchedulers
   (:file "search/directive")
   (:file "search/schedule")
   (:file "search/polyhedral")
   (:file "search/ast")
   (:file "search/evaluator")
   (:file "search/optimization-rule")
   
   (:file "jit")
   (:file "package")
   ;(:file "documentation")
   ))
