(defpackage :caten/codegen/auto-scheduler
  (:use :cl :caten/air :caten/codegen/shape-inference :caten/codegen/expr :caten/codegen/config)
  (:import-from
   :caten/codegen/engine
   #:Opt #:apply-opt #:opt-applicable-p)
  (:export #:auto-schedule))

(in-package :caten/codegen/auto-scheduler)
;; ~~~ Possible Optiimzations  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass OptTile (Opt) nil)
(defmethod apply-opt ((opt OptTile) node-id si)
  ;; [TODO] AutoTune the tiling size
  (caten/codegen/tiling:apply-tile node-id (getattr si :polyhedral) 32))
(defmethod opt-applicable-p ((opt OptTile) node-id si)
  (caten/codegen/tiling:get-tileable-band (getattr si :polyhedral) node-id))

(defclass OptUnroll (Opt) nil) ;; vertical unroll (used to remove away small :FOR)
(defmethod apply-opt ((opt OptUnroll) node-id si)

  )
(defmethod opt-applicable-p ((opt OptUnroll) node-id si)
  
  )

(defclass OptPacking (Opt) nil) ;; horizontal unroll (used to float4 grouping, simd transformation, reduce, etc)
;; (defclass OptInterchange (Opt) nil)
;; (defclass OptGroup (Opt) nil) Implement decent GPU Support First.
;; (defclass OptCollapse (Opt) nil) Effective for Tile+Paralle Fusion, but is it beyond the compiler?
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Purpose: get a list of optimal scheduling commands
;; [TODO] JIT_DEBUG >= 2 to see optimized schedule sequence by BEAM (TODO: Searching method like tiramisu)
;; ~~~ Schedule Templates ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun gensketch (schedule-item &key (actions '(OptTile OptUnroll)) &aux (collected-sketchgen))
  (loop for bp in (getattr schedule-item :blueprint)
        if (eql (node-type bp) :EXPR) do
          (loop for act in actions
                for opt = (make-instance act :id (node-id bp))
                when (opt-applicable-p opt (princ-to-string (node-id bp)) schedule-item)
                  do (push opt collected-sketchgen)))
  collected-sketchgen)

(defun auto-schedule (auto-scheduler node)
  (assert (getattr node :polyhedral))
  (symbol-macrolet ((OPTIMIZE (the (integer 0 2) (ctx:getenv :OPTIMIZE))))
    (when (= 0 OPTIMIZE) (return-from auto-schedule)) ;; No optimization
    (let ((sketch (gensketch node)))
      (print "OPTIMIZATION CANDIDATES")
      (print sketch)
      ;; generating sketch
      (when (>= OPTIMIZE 2)
        
        )
      
      (when (>= OPTIMIZE 1)
        ;; Finally applies the outermost n loop parallelization for the interchanged loops.
        (caten/codegen/coincidence:apply-parallel
         (getattr node :polyhedral)
         (auto-scheduler-n-global-loops auto-scheduler)))
      ;; [TODO] Final BEAM Search for local/global size, or tiling size.
      ;; Load blueprint from optimized polyhedral IR
      (setf (getattr node :blueprint) (caten/codegen/ast-parser:lower-into-bp-from-polyhedral (caten/codegen/polyhedral:->ast (getattr node :polyhedral) (getattr node :rank)) node)))))
