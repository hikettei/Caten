(asdf:defsystem "caten.common"
  :description "Helpers for Caten"
  :author      "hikettei <ichndm@gmail.com>"
  :licence     "MIT"
  :depends-on ("uiop" "ieee-floats" "cl-ppcre" "cl-ansi-text")
  :serial t
  :components
  ((:file "contextvar")
   (:file "tqdm")
   (:file "dtype")
   (:file "logger")))
