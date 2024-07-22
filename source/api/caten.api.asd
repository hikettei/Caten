(asdf:defsystem "caten.api"
  :description ""
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
  :in-order-to ((test-op (asdf:test-op "caten.api/test"))))

(asdf:defsystem "caten.api/test"
  :depends-on
  ("rove" "caten.api")
  :components
  ((:file "test-suites"))
  :perform
  (test-op (o s) (uiop:symbol-call (find-package :rove) :run-suite :caten/api.test)))
