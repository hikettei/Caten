(asdf:defsystem "caten.runtime"
  :description "A compiled air graph runner for multiple hardwares"
  :author "hikettei <ichndm@gmail.com>"
  :license "MIT"
  :depends-on ("caten.air" "caten.aasm")
  :serial t
  :components ((:file "package")))
  