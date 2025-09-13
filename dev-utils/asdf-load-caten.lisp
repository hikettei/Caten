(in-package :cl-user)

(asdf:initialize-source-registry
 `(:source-registry (:tree ,(directory-namestring
			     (or *load-pathname* *compile-file-pathname*)))
   :inherit-configuration))

(ql:quickload :caten)
