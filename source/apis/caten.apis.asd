(asdf:defsystem "caten.apis"
  :description "High-Level Interface for Caten Compiler, Tensor Library, and VM etc..."
  :author      "hikettei <ichndm@gmail.com>"
  :depends-on
  ("rove" "trivia" "cl-ppcre" "float-features"
   "caten.common" "caten.air" "caten.aasm" "caten.runtime" "caten.codegen" "caten.byoc")
  :serial t
  :components ((:file "package")
	       (:file "attrs")
	       (:file "conditions")
	       (:file "tensor")
	       (:file "iseq")
	       (:file "shape-tracker")
	       (:file "helpers")
	       (:file "merge-views")
	       (:file "function")
	       (:file "module")
	       (:file "model")
               (:file "high-level-ops")
	       (:file "ahead-of-time")
	       (:file "initializers")
               (:file "facets")
               (:file "documentation")))
