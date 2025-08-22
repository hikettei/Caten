(defpackage :caten/codegen/schedule-graph
  (:use :cl :caten/air)
  (:export
   #:tensor-graph->schedule-graph))

(in-package :caten/codegen/schedule-graph)

(defun tensor-graph->schedule-graph (graph)
  (declare (type FastGraph graph))
  
  )
