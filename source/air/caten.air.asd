(asdf:defsystem "caten.air"
  :description "aIR = Abstract + IR, General purpose IR system including pattern matcher."
  :author      "hikettei <ichndm@gmail.com>"
  :licence     "MIT"
  :depends-on ("trivia" "alexandria" "closer-mop")
  :serial t
  :components
  ((:file "package")
   (:file "attr")
   (:file "node")
   (:file "graph")
   (:file "pattern-matcher")
   (:file "viz")
   (:file "documentation")))
