(asdf:defsystem "caten.polyhedral"
  :description "Polyhedral Structure Analyzer"
  :author "hikettei <ichndm@gmail.com>"
  :licence "MIT"
  :depends-on ("caten.isl")
  :serial t
  :components
  ((:file "package")
   (:file "polyhedral")))
