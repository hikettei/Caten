(asdf:defsystem "caten"
  :description "Programmable Deep Learning Framework"
  :author      "hikettei <ichndm@gmail.com>"
  :licence     "MIT"
  :depends-on
  ("rove" "caten.air" "caten.aasm")
  :in-order-to
  ((test-op
    (asdf:test-op "caten.air")
    (asdf:test-op "caten.aasm")
    ;; as well as other systems
    )))
