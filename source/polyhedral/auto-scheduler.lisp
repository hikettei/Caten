(defpackage :caten/polyhedral/auto-scheduler
  (:use :cl :caten/polyhedral/ir :cffi :caten/polyhedral/config)
  (:export :auto-schedule #:->ast))

(in-package :caten/polyhedral/auto-scheduler)

(defun gid (n) (intern (format nil "_gid~a" n)))

(defmethod ->ast ((poly Polyhedral-IR) rank)
  (macrolet ((set-option (name level)
	       `(foreign-funcall ,(format nil "isl_options_set_~(~a~)" name)
				 :pointer (isl::context-handle isl::*context*)
				 :int ,level
				 :void)))
    (set-option "ast_build_exploit_nested_bounds" 1)
    (set-option "ast_build_detect_min_max" 1)
    (set-option "ast_build_scale_strides" 1)
    (set-option "ast_build_allow_else" 0)
    (set-option "ast_build_allow_or" 0))
  (let* ((schedule (isl:schedule-set-options (isl:copy (poly-schedule poly)) :separate))
	 (ast-build (isl:ast-build-from-context (isl:set-from-str "{:}")))
         (rank (* 2 rank)) ;; rank * tile_bands * vectorizing
         (ast-build (isl:ast-build-set-iterators ast-build (apply #'isl:make-id-list (loop for i upfrom 0 below rank collect (gid i)))))
         (ast-build (isl:ast-build-set-options ast-build (isl:union-map-from-str "{}")))
	 (ast-build-node (isl:ast-build-node-from-schedule ast-build schedule)))
    ast-build-node))
;; [Note] Deprecated?
(defmethod schedule ((config Auto-Scheduler-Config) (pg Polyhedral-IR))
  (apply-schedule-options-global (auto-scheduler-schedule-options config))
  (let ((schedule-constraints (isl:schedule-constraints-on-domain (poly-domain pg))))
    (dolist (option (auto-scheduler-cost-functions config))
      (ecase option
        (:coincidence
         (setf schedule-constraints (isl:schedule-constraints-set-coincidence schedule-constraints (poly-dependencies pg))))
        (:proximity
         (setf schedule-constraints (isl:schedule-constraints-set-proximity schedule-constraints (poly-dependencies pg))))
        (:validity
         (setf schedule-constraints (isl:schedule-constraints-set-validity schedule-constraints (poly-dependencies pg))))))
    (isl:schedule-constraints-compute-schedule schedule-constraints)))

(defun auto-schedule (scheduler poly)
  "
```
(auto-schedule scheduler poly)
```
An entrypoint for auto-scheduling.

https://github.com/Tiramisu-Compiler/tiramisu/blob/master/src/auto_scheduler/tiramisu_schedules_generator.cpp#L465
"
  (declare (type Auto-Scheduler-Config scheduler) (type Polyhedral-IR poly))
  ;; [TODO] Creating smth like tiramisu
  (caten/polyhedral/transforms::polyir-set-parallel poly 0)
  ;; 1. start by implementing simple case
  ;; (setf (poly-schedule poly) (schedule scheduler poly)) ;; Getting an intial schedule, extracting parallelism
  ;; Tiling
  ;; (caten/polyhedral/tiling:tile-bands scheduler poly)
  ;;  Mark unroll/vectorize/parallel
  ;; (caten/polyhedral/packing:packing scheduler poly)
  poly)
