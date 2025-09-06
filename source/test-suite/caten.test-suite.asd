(asdf:defsystem "caten.test-suite"
  :description "This is where unittest occur for Caten."
  :author      "hikettei <ichndm@gmail.com>"
  :depends-on
  ("rove" "trivia" "cl-ppcre" "py4cl")
  :serial t
  :components ((:file "graph/test-rewrite"))
  :perform
  (asdf:test-op
   (o s)
   (let ((result (uiop:symbol-call (find-package :rove) :run* "caten/test-suite/" :style :spec)))
     (print result))))
