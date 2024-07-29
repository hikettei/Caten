(asdf:defsystem "caten"
  :description "Programmable Deep Learning Framework"
  :author      "hikettei <ichndm@gmail.com>"
  :licence     "MIT"
  :depends-on
  ("rove" "trivia" "cl-ppcre" "caten.air" "caten.aasm" "caten.avm")
  :pathname "source"
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
	       (:file "initializers")
	       (:file "syntax-sugar"))
  :in-order-to
  ((test-op
    (asdf:test-op "caten.air")
    (asdf:test-op "caten.aasm")
    (asdf:test-op "caten.avm")
    ;; as well as other systems
    (asdf:test-op "caten/test")    
    )))

(asdf:defsystem "caten/test"
  :depends-on
  ("rove")
  :pathname "source"
  :components ((:file "test-suites"))
  :perform
  (test-op (o s) (uiop:symbol-call (find-package :rove) :run s :style :dot)))
