(asdf:defsystem "caten.apis"
  :description "High-Level Interface for Caten Compiler, Tensor Library, and VM etc..."
  :author      "hikettei <ichndm@gmail.com>"
  :depends-on
  ("rove" "trivia" "cl-ppcre" "float-features"
   "caten.common" "caten.air" "caten.aasm" "caten.avm" "caten.codegen")
  :serial t
  :components ((:file "package")
	       (:file "attrs")
	       (:file "conditions")
	       (:file "tensor")
	       (:file "iseq")
	       (:file "shape-tracker")
	       (:file "helpers")
	       (:file "merge-views")
	       (:file "function")
	       (:file "module")
	       (:file "model")
               (:file "high-level-ops")
	       (:file "ahead-of-time")
	       (:file "initializers")
               (:file "documentation"))
  :in-order-to
  ((test-op
    (asdf:test-op "caten.air")
    (asdf:test-op "caten.aasm")
    (asdf:test-op "caten.avm")
    (asdf:test-op "caten.apis/test"))))

(asdf:defsystem "caten.apis/test"
  :depends-on
  ("rove")
  :components ((:file "test-suites"))
  :perform
  (asdf:test-op
   (o s)
   (let ((result (uiop:symbol-call (find-package :rove) :run s :style (if (uiop:getenv "DOT") :dot :spec))))
     (assert (or (null (uiop:getenv "CI")) result)))))
