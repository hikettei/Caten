(asdf:defsystem "caten.ajit"
  :description "Abstract JIT System"
  :author      "hikettei <ichndm@gmail.com>"
  :licence     "MIT"
  :depends-on ("trivia" "alexandria" "cffi" "trivial-garbage")
  :serial t
  :components
  ((:file "package")
   (:file "helpers")
   (:file "simplifiers")
   (:file "type-relay")
   (:file "scheduler")
   (:file "isl-wrappers")
   (:file "isl-objects")
   (:file "isl-ast-helpers")
   (:file "graph")
   (:file "kernel-info")
   (:file "multiexpr")
   (:file "renderer")
   (:file "polyhedral")
   (:file "backends/clang"))
  :in-order-to ((test-op (asdf:test-op "caten.ajit/test"))))

(asdf:defsystem "caten.ajit/test"
  :depends-on
  ("rove" "caten.ajit")
  :components
  ((:file "test-suites"))
  :perform
  (test-op (o s) (uiop:symbol-call (find-package :rove) :run s :style :dot)))
