(asdf:defsystem "caten.workflow"
  :description "A Comprehensive abstraction for Tokenizer, Model Compilation, and export to foreign language mode"
  :author      "hikettei <ichndm@gmail.com>"
  :licence     "MIT"
  :depends-on ("trivia" "caten.lang")
  :components
  ((:file "package")
   (:file "helpers")
   (:file "action"))
  :in-order-to ((test-op (asdf:test-op "caten.workflow/test"))))

(asdf:defsystem "caten.workflow/test"
  :depends-on
  ("rove" "caten.workflow")
  :components
  ((:file "test-suites"))
  :perform
  (asdf:test-op (o s)
           (let ((result (uiop:symbol-call (find-package :rove) :run s :style :dot)))
	     (assert (or (null (uiop:getenv "CI")) result)))))
