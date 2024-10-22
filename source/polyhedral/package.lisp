(in-package :cl-user)

(defpackage :caten/polyhedral
  (:use :cl)
  (:import-from
   :caten/polyhedral/ir
   #:Polyhedral-IR)
  (:import-from
   :caten/polyhedral/auto-scheduler
   #:auto-schedule)
  (:export
   #:Polyhedral-IR
   #:auto-schedule))
  
