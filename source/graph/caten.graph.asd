(asdf:defsystem "caten.graph"
  :description "Graph is a foundation data structure for IRs"
  :author      "hikettei <ichndm@gmail.com>"
  :licence     "MIT"
  :depends-on ("trivia" "alexandria" "closer-mop" "cl-ppcre")
  :serial t
  :components
  ((:file "package")
   (:file "attr")
   (:file "node")
   (:file "graph")
   (:file "pattern-matcher")
   (:file "type-relay")
   (:file "viz")))
