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
  ;; TODO:
  ;; - [ ] Remove away small loops by adjusting the unroll factor
  (caten/codegen/unroll:apply-unroll si 4 node-id))
(defmethod opt-applicable-p ((opt OptUnroll) node-id si)
  (caten/codegen/unroll:get-packable-band node-id (getattr si :polyhedral) #'caten/codegen/unroll:mark-unroll-p))

(defclass OptPacking (Opt) nil) ;; horizontal unroll (used to float4 grouping, simd transformation, reduce, etc)
(defclass OptInterchange (Opt) nil)
(defclass OptGrouptop (Opt) nil) ;; Implement decent GPU Support First.
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Purpose: get a list of optimal scheduling commands
;; [TODO] JIT_DEBUG >= 2 to see optimized schedule sequence by BEAM (TODO: Searching method like tiramisu)
;; ~~~ Schedule Templates ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [TODO] Parallelize == apply-tile 100あるループを4分割とかする
;; CPU=local_size(1, 1, 1)
;; GPU=local_size(32, 1, 1) global_size=(32, ...) -> gidx gidy gidz lidx lidy lid3
;; ^ i.e.: parallelize = Tiling with size
;; Implementation:
;; - [ ] ISL Schedule Nodeに対してBEAM Searchを実施する
;; - [ ] OPTIMIZE=2 will cache the optimized sequence of Opt
;; - [ ] GPU: gid.x * local_size + lid.xこれをFirst Class Supportにする
;; - [ ] (caten x :variables (list (variable 'b 0 10 2))) -> 全部に対してSampleする
;; - [ ] Unroll/Tiling/Parallelizeを全部Tileで実装する
;; - [ ] Loop Tilingした後にInterchangeができるようにScheduleする
;; TODO: Cache the result from OPTIMIZE=2
;; BEAM Search: ISL Schedule Treeで実施する
(defun beam (schedule-node)
  
  )

(defun auto-schedule (auto-scheduler node)
  (assert (getattr node :polyhedral))
  (symbol-macrolet ((OPTIMIZE (the (integer 0 2) (ctx:getenv :OPTIMIZE))))
    (when (= 0 OPTIMIZE) (return-from auto-schedule)) ;; No optimization
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
    (setf (getattr node :blueprint) (caten/codegen/ast-parser:lower-into-bp-from-polyhedral (caten/codegen/polyhedral:->ast (getattr node :polyhedral) (getattr node :rank)) node))))
