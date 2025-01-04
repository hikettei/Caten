(defpackage :caten/codegen/auto-scheduler
  (:use :cl :caten/air :caten/codegen/shape-inference :caten/codegen/expr :caten/codegen/config
        :caten/codegen/polyhedral)
  (:export #:auto-schedule))

(in-package :caten/codegen/auto-scheduler)
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
;; remove tiling, unroll


;; ~~~ Optimizations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass Opt () ((id :initarg :id :accessor opt-id) (amount :initarg :amount :accessor opt-amount)))
(defgeneric apply-opt (opt schedule-node config) (:documentation "Returns a new isl:schedule-node with current optimization was applied."))
(defgeneric opt-applicable-p (opt schedule-node config) (:documentation "Returns T if the current optimization is applicable to the given schedule-node"))

(defclass Parallel (Opt) nil (:documentation "Tiles the current schedule-band by amount"))
(defmethod apply-opt ((opt Parallel) schedule-node config)

  )
(defmethod opt-applicable-p ((opt Parallel) schedule-node config)
  
  )
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun beam (schedule-node)
  (declare (type isl:schedule-node-domain schedule-node)
           (optimize (speed 3)))
  (let ((best-schedule schedule-node))
    
    (isl:schedule-node-get-schedule best-schedule)))

(defun auto-schedule (auto-scheduler node)
  (assert (getattr node :polyhedral))
  (symbol-macrolet ((OPTIMIZE (the (integer 0 2) (ctx:getenv :OPTIMIZE))))
    (when (= 0 OPTIMIZE) (return-from auto-schedule)) ;; No optimization
    ;; generating sketch
    (when (>= OPTIMIZE 2)
      (beam (isl:schedule-get-root (poly-schedule (getattr node :polyhedral)))))
    ;; [TODO] Final BEAM Search for local/global size, or tiling size.
    ;; Load blueprint from optimized polyhedral IR
    (setf (getattr node :blueprint) (caten/codegen/ast-parser:lower-into-bp-from-polyhedral (caten/codegen/polyhedral:->ast (getattr node :polyhedral) (getattr node :rank)) node))))
