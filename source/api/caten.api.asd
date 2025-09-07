(asdf:defsystem "caten.api"
  :description "High-Level Interface for Caten Compiler, Tensor Library, and VM etc..."
  :author      "hikettei <ichndm@gmail.com>"
  :depends-on
  ("trivia" "cl-ppcre" "float-features")
  :components ((:file "package")
               (:file "helpers")
               (:file "shape")
               (:file "tensor")
               (:file "facets")
               (:file "hlops")))
