(asdf:defsystem "caten.common"
  :description "Helpers for Caten"
  :author      "hikettei <ichndm@gmail.com>"
  :licence     "MIT"
  :depends-on ("uiop" "ieee-floats" "cl-ppcre" "cl-ansi-text")
  :components
  ((:file "logger")
   (:file "documentation")
   (:file "contextvar")
   (:file "tqdm")
   (:file "dtype")))
