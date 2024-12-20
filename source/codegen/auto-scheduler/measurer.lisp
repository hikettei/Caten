(defpackage :caten/codegen/auto-scheduler/measurer
  (:documentation "Profiles the performance of the generated code.")
  (:use :cl :caten/avm :caten/air :caten/codegen/renderer :caten/aasm)
  (:export
   #:schedule-item->runner))

(in-package :caten/codegen/auto-scheduler/measurer)

;; (defstruct Profiler )

(defmethod schedule-item->runner ((node Node) renderer)
  (assert (eql (node-type node) :Schedule-Item))
  (setf (getattr node :rendered-object) (%render-kernel renderer node))
  (%compile-kernel renderer (list node) nil) ;; 最後にReadyにする when it has multiple candidates
  ;; [TODO] Break jit.lisp into multiple files and remove symbol-call
  (let ((kernel (uiop:symbol-call :caten/codegen/jit :make-compiled-kernel-node node)))
    (print kernel)))
;; TODO: GNUPLOT
