(defpackage :caten/codegen/polyhedral
  (:shadow #:set #:space)
  (:shadowing-import-from :cl :map)
  (:use :cl :caten/air :caten/aasm :caten/isl)
  (:export
   #:make-polyhedral-from-blueprint
   #:get-blueprint-from-polyhedral))

(in-package :caten/codegen/polyhedral)

(defun get-blueprint-from-polyhedral (polyhedral)

  )
(defun render-list (list) (apply #'concatenate 'string (butlast (loop for n in list append (list (format nil "~a" n) ", ")))))

;; No Polyhedral IR!
;; :noopt :reduce :coincident is all you need
(defun make-polyhedral-from-blueprint (blueprint)
  "Constructs Polyhedral IR from blueprint which is a static graph."
  (declare (type Graph blueprint))
  ;; あ ~ Indexingをどうするかの解釈...
  ;; -> 普通に1Dのままで良さそうに見える？
  ;; Reductionのaccess repをどう解釈するか，という話もある
  (print blueprint)
  ;; OptCandidates,
  )
