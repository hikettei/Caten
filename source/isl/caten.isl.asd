(asdf:defsystem "caten.isl"
  :description "ISL Binding and gc-extension for Codegen"
  :author "hikettei <ichndm@gmail.com>"
  :licence "MIT"
  :defsystem-depends-on ("cffi-grovel")
  :depends-on ("cffi" "trivial-garbage" "alexandria")
  :serial t
  :components
  ((:file "package")
   (:file "helpers")
   (:cffi-grovel-file "grovel")
   (:file "isl-binding")
   (:file "isl-object")
   (:file "isl-function")
   (:file "object-specs")
   (:file "function-specs")))
(asdf:defsystem "caten.codegen.is
