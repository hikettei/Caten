(asdf:defsystem "caten.runtime"
  :description ""
  :author "hikettei <ichndm@gmail.com>"
  :license "MIT"
  :depends-on ("caten.graph" "caten.ir" "closer-mop")
  :serial t
  :components ((:file "buffer")
               (:file "runtime")
               (:file "kernel")
               (:file "renderer")
               (:file "bring-your-own-backend")))
