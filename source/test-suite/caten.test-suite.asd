(asdf:defsystem "caten.test-suite"
  :description "This is where unittest occur for Caten"
  :author      "hikettei <ichndm@gmail.com>"
  :depends-on
  ("rove" "trivia" "cl-ppcre")
  :serial t
  :components ((:file "package")
	       (:file "helpers"))
  :perform
  (asdf:test-op
   (o s)
   (let ((result (uiop:symbol-call (find-package :rove) :run s :style :dot)))
     (assert (or (null (uiop:getenv "CI")) result)))))
