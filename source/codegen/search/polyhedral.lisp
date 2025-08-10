(defpackage :caten/codegen/search/polyhedral
  (:use :cl)
  (:export
   #:Polyhedral-Schedule-Item
   #:theta #:psi-theta
   #:dependency-graph #:psi-dependency-graph
   #:opt-history #:psi-opt-history
   ))
(in-package :caten/codegen/search/polyhedral)

(defclass Polyhedral-Schedule-Item ()
  ((theta :accessor psi-theta :initarg :initial-theta)
   (dependency-graph :accessor psi-dependency-graph :initarg :dependency-graph)
   (opt-history :accessor psi-opt-history :initform nil)
   ;; ctx?
   ;; during transformation blueprint should not be used
   )
   
  (:documentation "
Class `Polyhedral-Schedule-Item` is a wrapper around a Blueprint.

It stores the memory-dependence graph—analyzed from the original computation graph—together with its scheduling information.

Conceptually:
```
DependencyGraph, θ_0 = CreatePolyhedral(Blueprint)
θ_n+1 = ApplyTransformation(θ_n, OptimizationRule_n)
```

After applying a series of loop transformations to θ₀ an optimized Blueprint can be produced:

```
Blueprint_optimized = MakeBlueprintFromPolyhedral(Blueprint, θ_0) s.t.: ScheduleIsValid(DependencyGraph)
```

In other words, this class encapsulates both the analysis results and the scheduling state, enabling generation of a valid, optimized Blueprint from the polyhedral representation."))

(defun make-polyhedral-schedule-item (blueprint)

  )

