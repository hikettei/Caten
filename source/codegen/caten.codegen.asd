(asdf:defsystem "caten.codegen"
  :description "Caten Code Generator Engine"
  :author "hikettei <ichndm@gmail.com>"
  :licence "MIT"
  :depends-on ("caten.avm" "caten.aasm" "caten.air" "caten.polyhedral" "cl-ppcre" "alexandria")
  :serial t
  :components
  ((:file "helpers")
   (:file "expr")
   (:file "shape-inference")
   (:file "renderer")
   (:file "expr-cache")
   (:file "rewriting-rules")
   (:file "memory-planner")
   (:file "scheduler")
   (:file "exprify")
   (:file "blueprint")
   (:file "polyhedral-ast")
   (:file "scop")
   (:file "jit")
   (:file "backends/clang")
   (:file "package"))
  :in-order-to ((test-op (asdf:test-op "caten.codegen/test"))))

(asdf:defsystem "caten.codegen/test"
  :depends-on
  ("rove" "caten.codegen")
  :components
  ((:file "test-suites"))
  :perform
  (asdf:test-op (o s)
	   (let ((result (uiop:symbol-call (find-package :rove) :run s :style (if (uiop:getenv "DOT") :dot :spec))))
	     (assert (or (null (uiop:getenv "CI")) result)))))
