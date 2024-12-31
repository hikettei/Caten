(defpackage :caten/codegen/auto-scheduler
  (:use :cl :caten/air :caten/codegen/shape-inference :caten/codegen/expr :caten/codegen/config)
  (:import-from
   :caten/codegen/engine
   #:Opt #:apply-opt #:opt-applicable-p)
  (:export #:auto-schedule))

(in-package :caten/codegen/auto-scheduler)
;; ~~~ Opts ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass OptTile (Opt) nil)
(defmethod apply-opt ((opt OptTile) node si)
  
  )
(defmethod opt-applicable-p ((opt OptTile) node si)
  ;; HasDataReuse?
  ;; Coincident?
  ;; HasReduction?
  )

(defclass OptParallel (Opt) nil)

(defclass OptUnroll (Opt) nil) ;; vertical unroll (used to remove away small :FOR)
(defclass OptPacking (Opt) nil) ;; horizontal unroll (used to float4 grouping, simd transformation, reduce, etc)
;; (defclass OptGroup (Opt) nil) Implement decent GPU Support First.
;; (defclass OptCollapse (Opt) nil) Effective for Tile+Paralle Fusion, but is it beyond the compiler?
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Purpose: get a list of optimal scheduling commands
;; [TODO] JIT_DEBUG >= 2 to see optimized schedule sequence by BEAM (TODO: Searching method like tiramisu)
;; ~~~ Schedule Templates ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun auto-schedule (auto-scheduler node)
  (assert (getattr node :polyhedral))
  (symbol-macrolet ((OPTIMIZE (the (integer 0 2) (ctx:getenv :OPTIMIZE))))
    ;; generating sketch
    (when (>= OPTIMIZE 1)

      )
    (when (>= OPTIMIZE 2)
      
      )
    ;; Load blueprint from optimized polyhedral IR
    (setf (getattr node :blueprint) (caten/codegen/ast-parser:lower-into-bp-from-polyhedral (caten/codegen/polyhedral:->ast (getattr node :polyhedral) (getattr node :rank)) node))))
