(asdf:defsystem "caten.aasm"
  :description "aasm = Abstract + (internal) assembly"
  :author      "hikettei <ichndm@gmail.com>"
  :licence     "MIT"
  :depends-on ("alexandria" "caten.common" "caten.air")
  :serial t
  :components
  ((:file "package")
   (:file "helpers")
   (:file "attrs")
   (:file "ctx")
   (:file "tensor-ir")
   (:file "ops")
   (:file "constant-folding")
   (:file "optimizers")
   (:file "view")
   (:file "logical"))
  :in-order-to ((test-op (asdf:test-op "caten.aasm/test"))))

(asdf:defsystem "caten.aasm/test"
  :depends-on
  ("rove" "caten.aasm")
  :components
  ((:file "test-suites"))
  :perform
  (test-op (o s)
	   (let ((result (uiop:symbol-call (find-package :rove) :run s :style :dot)))
	     (assert (or (null (uiop:getenv "CI")) result)))))

