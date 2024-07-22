(asdf:defsystem "caten"
  :description "Programmable Deep Learning Framework"
  :author      "hikettei <ichndm@gmail.com>"
  :licence     "MIT"
  :depends-on
  ("rove" "caten.air" "caten.api")
  :in-order-to
  ((test-op
    (asdf:test-op "caten.air")
    (asdf:test-op "caten.api")
    ;; as well as other systems
    )))
