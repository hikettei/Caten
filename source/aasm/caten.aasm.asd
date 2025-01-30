(asdf:defsystem "caten.aasm"
  :description "aasm = Abstract + (internal) assembly"
  :author      "hikettei <ichndm@gmail.com>"
  :licence     "MIT"
  :depends-on ("alexandria" "caten.common" "caten.air")
  :serial t
  :components
  ((:file "package")
   (:file "helpers")
   (:file "attrs") ;; todo: rename -> ops.lisp

   (:file "tensor-ops")
   (:file "render-ops")
   
   (:file "tensor-ir")
   (:file "ops")
   
   (:file "constant-folding")
   (:file "optimizers")
   (:file "view")
   (:file "logical")
   (:file "documentation")))
