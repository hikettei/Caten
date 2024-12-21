(defpackage :caten/codegen/auto-scheduler
  (:use :cl))

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
(defun auto-schedule (item)

  )
