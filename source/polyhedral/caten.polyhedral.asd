(asdf:defsystem "caten.polyhedral"
  :description "Polyhedral Structure Analyzer"
  :author "hikettei <ichndm@gmail.com>"
  :licence "MIT"
  :depends-on ("cffi" "caten.isl" "caten.common" "cl-yaml")
  :serial t
  :components
  ((:file "ir")
   (:file "config")
   (:file "tiling")
   (:file "transforms")
   (:file "auto-scheduler")
   (:file "package")
   (:file "documentation")))
