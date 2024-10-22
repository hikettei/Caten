(asdf:defsystem "caten.polyhedral"
  :description "Polyhedral Structure Analyzer"
  :author "hikettei <ichndm@gmail.com>"
  :licence "MIT"
  :depends-on ("cffi" "caten.isl")
  :serial t
  :components
  ((:file "package")
   (:file "ir")
   (:file "auto-scheduler")))
