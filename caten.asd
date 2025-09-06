(asdf:defsystem "caten"
  :description "Programmable Deep Learning Framework"
  :author "hikettei <ichndm@gmail.com>"
  :version "0.2"
  :licence "MIT"
  :depends-on ("caten.graph")
  :pathname "source"
  :serial t
  :in-order-to
  ((test-op (test-op "caten/test"))))

(asdf:defsystem "caten/test"
  :depends-on ("caten" "caten.test-suite")
  :in-order-to
  ((test-op (asdf:test-op "caten.test-suite"))))
