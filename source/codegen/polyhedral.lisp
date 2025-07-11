(defpackage :caten/codegen/polyhedral
  (:use :cl :caten/air :caten/aasm)
  (:export
   #:make-polyhedral-from-blueprint
   #:get-blueprint-from-polyhedral))

(in-package :caten/codegen/polyhedral)

(defun get-blueprint-from-polyhedral (polyhedral)

  )

(defun make-polyhedral-from-blueprint (blueprint)
  "Constructs Polyhedral IR from blueprint which is a static graph."
  (declare (type Graph blueprint))
  ;; あ ~ Indexingをどうするかの解釈...
  ;; -> 普通に1Dのままで良さそうに見える？
  ;; Reductionのaccess repをどう解釈するか，という話もある
  )
