(asdf:defsystem "caten.lang"
  :description "Extra Frontend for Caten. It translates CStyle Code into blueprint"
  :author      "hikettei <ichndm@gmail.com>"
  :depends-on
  ("with-c-syntax" "named-readtables" "cl-ppcre" "trivia" "alexandria")
  :serial t
  :components ((:file "package")
               (:file "feature")
               (:file "reader")))
