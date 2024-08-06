(asdf:defsystem "caten.common"
  :description "Helpers for Caten"
  :author      "hikettei <ichndm@gmail.com>"
  :licence     "MIT"
  :depends-on ("uiop" "ieee-floats")
  :serial t
  :components
  ((:file "contextvar")
   (:file "tqdm")
   (:file "dtype")))
