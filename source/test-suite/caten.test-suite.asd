(asdf:defsystem "caten.test-suite"
  :description "This is where unittest occur for Caten.
Tests that are not related to the core functionality of Caten or are time-consuming are tested here."
  :author      "hikettei <ichndm@gmail.com>"
  :depends-on
  ("rove" "trivia" "cl-ppcre")
  :serial t
  :components ((:file "package")
	       (:file "helpers")
	       (:file "test-randomness")
	       (:file "test-iseq-lowerer"))
  :perform
  (asdf:test-op
   (o s)
   (let ((result (uiop:symbol-call (find-package :rove) :run s :style :dot)))
     (assert (or (null (uiop:getenv "CI")) result)))))
