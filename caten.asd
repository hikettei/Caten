(asdf:defsystem "caten"
  :description "Programmable Deep Learning Framework"
  :author      "hikettei <ichndm@gmail.com>"
  :licence     "MIT"
  :depends-on
  ("rove" "caten.air")
  :in-order-to
  ((test-op
    (asdf:test-op "caten.air")
    ;; as well as other systems
    )))
