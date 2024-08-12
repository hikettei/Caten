(asdf:defsystem "caten/demos.llama"
  :description "LLaMA Inference Demo"
  :author "hikettei <ichndm@gmail.com>"
  :licence "MIT"
  :depends-on ("caten.apis" "caten.nn")
  :serial t
  :components ((:file "model")))
