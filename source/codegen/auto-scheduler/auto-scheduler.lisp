(defpackage :caten/codegen/auto-scheduler
  (:use :cl :caten/air)
  (:export #:auto-schedule))

(in-package :caten/codegen/auto-scheduler)
;; Scheduling commands
;; - [ ] apply-tile
;; - [ ] apply-parallel
;; - [ ] apply-collapse
;; - [ ] apply-fuse
;; - [ ] apply-interchange
;; - [ ] apply-vectorize
;; Purpose: get a list of optimal scheduling commands
;; Note: 積極的にISL ASTとBlueprintを変換しながら変形を施していく
;; [TODO] JIT_DEBUG >= 2 to see optimized schedule sequence by BEAM (todo: search tiramisu)
;; BEAM Search
(defun auto-schedule (auto-scheduler node)
  (assert (getattr node :polyhedral))
  
  ;; Load blueprint from optimized polyhedral IR
  (setf (getattr node :blueprint)
        (caten/codegen/polyhedral-ast:lower-into-bp-from-polyhedral
         (caten/codegen/polyhedral:->ast (getattr node :polyhedral) (getattr node :rank))
         node)))
