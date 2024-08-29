(asdf:defsystem "caten/external.backends.metal"
  :description "Metal Backend for Caten"
  :author      "hikettei <ichndm@gmail.com>"
  :licence     "MIT"
  :depends-on ("cl-metal" "caten.apis")
  :serial t
  :components
  ((:file "metal")))
