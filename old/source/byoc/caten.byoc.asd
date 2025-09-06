(asdf:defsystem "caten.byoc"
  :description "BYOC (Bring Your Own Codegen) implements an extension of caten/codegen targeting multiple devices."
  :author "hikettei <ichndm@gmail.com>"
  :license "MIT"
  :defsystem-depends-on ("cffi-grovel")
  :depends-on ("caten.codegen" "caten.runtime" "cffi" "flexi-streams" "float-features" "babel" "cl-pack" "cl-cpus" "lparallel")
  :components ((:file "lisp")
               (:file "native")
               (:file "clang")
               (:cffi-wrapper-file "helpers/callback" :soname "callback_helper")
               (:file "metal")
               (:file "cuda")
               (:file "llvm")
               (:file "webgpu")))
