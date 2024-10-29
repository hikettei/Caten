(asdf:defsystem "caten.polyhedral"
  :description "Polyhedral Structure Analyzer"
  :author "hikettei <ichndm@gmail.com>"
  :licence "MIT"
  :depends-on ("cffi" "caten.isl" "caten.common")
  :serial t
  :components
  ((:file "ir")
   (:file "config")
   (:file "auto-scheduler")
   (:file "package")
   (:file "documentation")))
