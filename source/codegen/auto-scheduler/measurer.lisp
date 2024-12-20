(defpackage :caten/codegen/auto-scheduler/measurer
  (:documentation "Profiles the performance of the generated code.")
  (:use :cl :caten/avm :caten/air :caten/codegen/renderer)
  (:export
   #:schedule-item->runner))

(in-package :caten/codegen/auto-scheduler/measurer)

(defmethod schedule-item->runner ((node Node) renderer)
  (assert (eql (node-type node) :Schedule-Item))
  (setf (getattr node :rendered-object) (print (%render-kernel renderer node)))
  (%compile-kernel renderer (list node) nil)
  )

;; TODO: GNUPLOT
