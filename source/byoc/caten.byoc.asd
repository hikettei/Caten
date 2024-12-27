(asdf:defsystem "caten.byoc"
  :description "BYOC (Bring Your Own Codegen) implements an extension of caten/codegen targeting multiple devices."
  :author "hikettei <ichndm@gmail.com>"
  :license "MIT"
  :depends-on ("caten.codegen" "caten.runtime" "cffi" "flexi-streams" "float-features")
  :components ((:file "lisp")
               (:file "clang")
               (:file "metal")))
