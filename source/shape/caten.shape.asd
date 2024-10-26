(asdf:defsystem "caten.shape"
  :description "Shape Tracker"
  :author "hikettei <ichndm@gmail.com>"
  :licence "MIT"
  :depends-on ("caten.air" "caten.aasm")
  :serial t
  :components
  ((:file "shape-tracker")
   (:file "package")))
