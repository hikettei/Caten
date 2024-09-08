(asdf:defsystem "caten.ajit"
  :description "Abstract JIT System"
  :author      "hikettei <ichndm@gmail.com>"
  :licence     "MIT"
  :depends-on ("caten.isl" "trivia" "alexandria" "cffi" "trivial-garbage" "cl-yaml" "linear-programming")
  :serial t
  :components
  ((:file "package")
   (:file "helpers")
   (:file "simplifiers")
   (:file "type-relay")
   (:file "polyhedral")
   (:file "scheduler")
   (:file "isl-objects")
   (:file "isl-ast-helpers")
   (:file "transform")
   (:file "graph")   
   (:file "kernel-info")
   (:file "multiexpr")
   (:file "memory-planner")
   (:file "device")
   (:file "renderer")
   (:file "backends/clang"))
  :in-order-to ((test-op (asdf:test-op "caten.ajit/test"))))

(asdf:defsystem "caten.ajit/test"
  :depends-on
  ("rove" "caten.ajit")
  :components
  ((:file "test-suites"))
  :perform
  (test-op (o s)
	   (let ((result (uiop:symbol-call (find-package :rove) :run s :style (if (uiop:getenv "DOT") :dot :spec))))
	     (assert (or (null (uiop:getenv "CI")) result)))))
