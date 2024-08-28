(asdf:defsystem "caten.isl"
  :description "ISL Wrapper for Caten, with a little tweaking. The original code is derivered from https://github.com/marcoheisig/cl-isl. Please see `./LICENCE`."
  :author      "hikettei <ichndm@gmail.com>"
  :licence     "MIT"
  :defsystem-depends-on ("cffi-grovel")
  :depends-on ("alexandria" "cffi" "trivial-garbage")
  :serial t
  :components
  ((:file "package")
   (:file "helpers")
   (:cffi-grovel-file "grovel")
   (:file "isl-binding")
   (:file "isl-function")
   (:file "isl-object"))
  :in-order-to ((test-op (asdf:test-op "caten.isl/test"))))

(asdf:defsystem "caten.isl/test"
  :depends-on
  ("rove" "caten.isl")
  :components
  ((:file "test-suites"))
  :perform
  (test-op (o s) (uiop:symbol-call (find-package :rove) :run s :style (if (uiop:getenv "DOT") :dot :spec))))
