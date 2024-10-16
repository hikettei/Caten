(asdf:defsystem "caten.codegen"
  :description "Caten Code Generator Engine"
  :author "hikettei <ichndm@gmail.com>"
  :licence "MIT"
  :depends-on ()
  :serial t
  :components
  ((:file "package")
   (:file "helpers")
   (:file "shape-inference")
   (:file "scheduler")
   (:file "jit")))
