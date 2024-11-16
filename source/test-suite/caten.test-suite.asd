(asdf:defsystem "caten.test-suite"
  :description "This is where unittest occur for Caten.
Tests that are not related to the core functionality of Caten or are time-consuming are tested here."
  :author      "hikettei <ichndm@gmail.com>"
  :depends-on
  ("rove" "trivia" "cl-ppcre" "py4cl")
  :serial t
  :components ((:file "package")
	       (:file "helpers")
               (:file "test-ops")
               (:file "test-view")
               (:file "test-regression-test")
               (:file "test-normalization")
	       (:file "test-randomness")
	       (:file "test-iseq-lowerer")
	       (:file "test-llm")
	       (:file "test-gemm")
	       (:file "test-conv")
               (:file "test-pool")
               (:file "test-shape-tracker")
               (:file "test-schedule")
               (:file "test-scheduler")
               (:file "test-dynamic-shape")
               (:file "test-memory-planner")
               (:file "test-schedule-cache"))
  :perform
  (asdf:test-op
   (o s)
   (let ((result (uiop:symbol-call (find-package :rove) :run s :style :spec)))
     (assert (or (null (uiop:getenv "CI")) result)))))
