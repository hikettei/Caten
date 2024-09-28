(asdf:defsystem "caten.air"
  :description "aIR = Abstract + IR, General purpose IR system including pattern matcher."
  :author      "hikettei <ichndm@gmail.com>"
  :licence     "MIT"
  :depends-on ("trivia" "alexandria" "closer-mop")
  :serial t
  :components
  ((:file "package")
   (:file "attr")
   (:file "node")
   (:file "graph")
   (:file "pattern-matcher")
   (:file "viz"))
  :in-order-to ((test-op (asdf:test-op "caten.air/test"))))

(asdf:defsystem "caten.air/test"
  :depends-on
  ("rove" "caten.air")
  :components
  ((:file "test-suites"))
  :perform
  (test-op (o s) (let ((result (uiop:symbol-call (find-package :rove) :run s :style :dot)))
		   (assert (or (null (uiop:getenv "CI")) result)))))
