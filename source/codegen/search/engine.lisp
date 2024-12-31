(defpackage :caten/codegen/engine
  (:documentation "
Provides an abstraction for the auto scheduler:
- Opt (a sequence of optimization)
- Search (A optimizing strategy)
- CostModel
")
  (:use :cl))

(in-package :caten/codegen/engine)

;; ~~ Opt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass Opt () nil)
(defgeneric apply-opt (opt node schedule-item))

(defclass SearchMethod () nil)

(defclass CostModel () nil)
;; [TODO] Try with various cost models:
;; - Random Forest
;; - LightGBM (Gradient Boosting Decision Tree)
;; - LSTM
;; - Actual execution time

;; Requirements on our auto scheduler
;; - 1. Optimization degree should be adjustable.
;; - 2. Support for the dynamic shape
;; - 3. Tiling Parameter Optimization
;; - 4. Our optimization is limited to directive. (Not optimizing kernelfusion)
