(asdf:defsystem "caten.isl"
  :description "ISL Wrapper for Caten, with a little tweaking. The original code is derivered from https://github.com/marcoheisig/cl-isl. Please see `./LICENCE`."
  :author      "hikettei <ichndm@gmail.com>"
  :licence     "MIT"
  :defsystem-depends-on ("cffi-grovel")
  :depends-on ("alexandria" "cffi" "trivial-garbage")
  :components
  ((:file "package")
   (:file "helpers")
   (:cffi-grovel-file "grovel")
   (:cffi-grovel-file "codegen")
   (:file "isl-binding")
   (:file "isl-object")
   (:file "isl-function")
   (:file "types")
   (:file "isl-ctx")
   (:file "id")
   (:file "value")
   (:file "space")
   (:file "local-space")
   (:file "constraint")
   (:file "affine")
   (:file "basic-set")
   (:file "set")
   (:file "union-set")
   (:file "basic-map")
   (:file "map")
   (:file "union-map")
   (:file "schedule-constraints")
   (:file "schedule")
   (:file "union-access")
   (:file "multi-union-pw-aff")
   (:file "ast-expr")
   (:file "ast-node")
   (:file "ast-build")
   (:file "ast")
   (:file "printer")
   (:file "schedule-node")))
