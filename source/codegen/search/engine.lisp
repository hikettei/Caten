(defpackage :caten/codegen/engine
  (:documentation "
Provides an abstraction for the auto scheduler:
- Opt (a sequence of optimization)
- Search (A optimizing strategy)
- CostModel
")
  (:use :cl)
  (:export
   #:Opt #:opt-id #:opt-degree
   #:apply-opt #:opt-applicable-p))

(in-package :caten/codegen/engine)
;; ~~ Opt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass Opt () ((id :accessor opt-id) (degree :accessor opt-degree))
  (:documentation "
- ID[symbol] is an id of the target :FOR loop
- degree[integer] represents the optimization degree."))

(defgeneric apply-opt (opt node schedule-item) (:documentation "Returns a new schedule-item with the current opt is applied."))
(defgeneric opt-applicable-p (opt node schedule-item) (:documentation "Returns T if the current opt is applicable to the schedule-item."))
(defmethod print-object ((opt Opt) stream)
  (print-unreadable-object (opt stream :type t :identity nil)
    (format stream "ID=~a, DEGREE=~a" (opt-id opt) (opt-degree opt))))
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
