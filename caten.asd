(asdf:defsystem "caten"
  :description "Programmable Deep Learning Framework"
  :author      "hikettei <ichndm@gmail.com>"
  :licence     "MIT"
  :depends-on
  ("caten.apis" "caten.nn" "caten.test-suite")
  :serial t
  :in-order-to
  ((test-op
    (asdf:test-op "caten.apis")
    (asdf:test-op "caten.nn")
    (asdf:test-op "caten.test-suite")))) 
