(asdf:defsystem "caten.codegen"
  :description "Caten Code Generator Engine"
  :author "hikettei <ichndm@gmail.com>"
  :licence "MIT"
  :depends-on ("caten.runtime" "caten.ir" "caten.graph" "caten.isl" "cl-ppcre" "alexandria" "lparallel" "cffi" "ironclad" "mito")
  :serial t
  :components
  ((:file "directive")
   (:file "schedule")
   (:file "polyhedral")
   (:file "diskcache")
   (:file "optimization-rule")
   (:file "ast")
   (:file "search")
   (:file "memory-planner")
   (:file "lowerer")))
