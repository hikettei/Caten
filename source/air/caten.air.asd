(asdf:defsystem "caten.air"
  :description "aIR = Abstract + IR, General purpose IR system including pattern matcher."
  :author      "hikettei <ichndm@gmail.com>"
  :licence     "MIT"
  :depends-on ("trivia" "alexandria")
  :serial t
  :components
  ((:file "package")
   (:file "node")
   (:file "graph")
   (:file "pattern-matcher")))
