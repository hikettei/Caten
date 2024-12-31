(defpackage :caten/codegen/engine
  (:documentation "
Provides an abstraction for the auto scheduler:
- Opt (a sequence of optimization)
- Search (A optimizing strategy)
- CostModel
")
  (:use :cl))

(in-package :caten/codegen/engine)
