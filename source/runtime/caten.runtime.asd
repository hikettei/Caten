(asdf:defsystem "caten.runtime"
  :description "A compiled air graph runner for multiple hardwares"
  :author "hikettei <ichndm@gmail.com>"
  :license "MIT"
  :depends-on ("caten.air" "caten.ir" "closer-mop")
  :serial t
  :components ((:file "buffer")
               (:file "profile")
               (:file "runtime")
               (:file "package")))
