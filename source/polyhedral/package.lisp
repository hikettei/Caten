(in-package :cl-user)

(defpackage :caten/polyhedral
  (:use :cl :caten/isl)
  (:shadow #:set #:space)
  (:shadowing-import-from :cl :map))
