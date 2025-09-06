(asdf:defsystem "caten2"
  :description "Programmable Deep Learning Framework"
  :author "hikettei <ichndm@gmail.com>"
  :version "0.2"
  :licence "MIT"
  :depends-on ("caten.graph")
  :serial t
  :in-order-to
  ((test-op (test-op "caten2/test"))))

(asdf:defsystem "caten2/test"
  :depends-on ("caten2" "caten2.test-suite")
  :in-order-to
  ((test-op (asdf:test-op "caten2.test-suite"))))
  
