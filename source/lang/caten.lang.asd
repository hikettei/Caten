(asdf:defsystem "caten.lang"
  :description "caten.lang provides a beautiful integration w/ Common Lisp Frontend enabled by @caten reader macro."
  :author      "hikettei <ichndm@gmail.com>"
  :depends-on
  ("with-c-syntax" "named-readtables" "cl-ppcre" "trivia" "alexandria")
  :serial t
  :components ((:file "package")
               (:file "feature")
               (:file "reader")
               (:file "features/jit")))
