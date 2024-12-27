(defpackage :caten/runtime/runtime
  (:documentation "
This package provides GraphRuntime, which is a class to run an air graph.
")
  (:use :cl :caten/air :caten/aasm)
  (:export

   ))

(in-package :caten/runtime/runtime)

(defclass GraphRuntime ()
  ((graph :accessor runtime-graph :type Graph)
   )
  (:documentation ""))

(defmethod free-runtime ((runtime GraphRuntime))
  "Frees all allocations in the runtime"
  
  )
