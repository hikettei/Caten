(asdf:defsystem "caten.lang"
  :description "Extra Frontend for Caten. It translates CStyle Code into blueprint"
  :author      "hikettei <ichndm@gmail.com>"
  :depends-on
  ("caten.api" "with-c-syntax")
  :serial t
  :components ((:file "package")))
