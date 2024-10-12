(asdf:defsystem "caten.lang"
  :description "From Common Lisp to RenderGraph Compiler"
  :author      "hikettei <ichndm@gmail.com>"
  :licence     "MIT"
  :depends-on ("trivia")
  :components
  ((:file "package")
   (:file "conditions")
   (:file "macros")
   (:file "builtin")
   (:file "documentation")))
