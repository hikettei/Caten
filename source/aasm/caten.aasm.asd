(asdf:defsystem "caten.aasm"
  :description "aasm = Abstract + (internal) assembly"
  :author      "hikettei <ichndm@gmail.com>"
  :licence     "MIT"
  :depends-on ("alexandria")
  :serial t
  :components
  ((:file "package")
   (:file "helpers")
   (:file "ctx")
   (:file "tensor-ir")
   (:file "ops")
   (:file "constant-folding"))
  :in-order-to ((test-op (asdf:test-op "caten.aasm/test"))))

(asdf:defsystem "caten.aasm/test"
  :depends-on
  ("rove" "caten.aasm")
  :components
  ((:file "test-suites"))
  :perform
  (test-op (o s) (uiop:symbol-call (find-package :rove) :run-suite :caten/aasm.test)))
