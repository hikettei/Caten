(asdf:defsystem "caten.runtime"
  :description ""
  :author "hikettei <ichndm@gmail.com>"
  :license "MIT"
  :depends-on ("caten.graph" "caten.ir" "closer-mop")
  :serial t
  :components ((:file "buffer")
               (:file "runtime")
               (:file "renderer")
               (:file "kernel")
               (:file "bring-your-own-backend")))
