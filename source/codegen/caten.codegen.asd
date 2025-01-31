(asdf:defsystem "caten.codegen"
  :description "Caten JIT Engine"
  :author "hikettei <ichndm@gmail.com>"
  :licence "MIT"
  :depends-on ("caten.runtime" "caten.ir" "caten.air" "cl-ppcre" "alexandria" "lparallel")
  :serial t
  :components
  ((:file "helpers")
   (:file "backend")
   (:file "type-relay")
   (:file "scheduler")
   (:file "blueprint")
   (:file "jit")
   (:file "package")))
