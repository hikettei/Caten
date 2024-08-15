(asdf:defsystem "caten.apis"
  :description "High-Level Interface for Caten Compiler, Tensor Library, and VM etc..."
  :author      "hikettei <ichndm@gmail.com>"
  :depends-on
  ("rove" "trivia" "cl-ppcre"
   "caten.common" "caten.air" "caten.aasm" "caten.avm" "caten.ajit")
  :serial t
  :components ((:file "package")
	       (:file "conditions")
	       (:file "tensor")
	       (:file "iseq")
	       (:file "shape-tracker")
	       (:file "helpers")
	       (:file "merge-views")
	       (:file "function")
	       (:file "module")
	       (:file "model")
	       (:file "ahead-of-time")
	       (:file "initializers")
	       (:file "syntax-sugar"))
  :in-order-to
  ((test-op
    (asdf:test-op "caten.air")
    (asdf:test-op "caten.aasm")
    (asdf:test-op "caten.avm")
    (asdf:test-op "caten.apis/test")
    (asdf:test-op "caten.ajit/test"))))

(asdf:defsystem "caten.apis/test"
  :depends-on
  ("rove")
  :components ((:file "test-suites"))
  :perform
  (test-op (o s) (uiop:symbol-call (find-package :rove) :run s :style :dot)))
