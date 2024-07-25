(in-package :caten)

(defstruct (IntermidateTape))
;; A List of Func/Module
(defclass Func () nil)
(defun %make-function ())
(defgeneric mutate->bw ())
(defgeneric mutate->asm ())
(defun tpsort ())

;; ~~ implementations ~~~~~~~~~~~~~~~~~~~~~~~~
