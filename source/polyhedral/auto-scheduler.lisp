(defpackage :caten/polyhedral/auto-scheduler
  (:use :cl :caten/polyhedral/ir :cffi)
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

(defmethod schedule ((pg Polyhedral-IR))
  (let ((serialize-sccs 0)
        (outer-coincidence 0)
        (maximize-coincidence 1)
        (treat-coalescing 1)
        (maximize-band-depth 1)
        ;; Only schedule the scc. (not to change the structure of kernel)
        (schedule-whole-component 0))
    (macrolet ((set-option (name level)
	         `(progn
		    (foreign-funcall ,(format nil "isl_options_set_~(~a~)" name)
				     :pointer (isl::context-handle isl::*context*)
				     :int ,level
				     :void))))
      (flet ((configure ()
               (set-option "schedule_serialize_sccs" serialize-sccs)
               (set-option "schedule_outer_coincidence" outer-coincidence)
               (set-option "schedule_maximize_coincidence" maximize-coincidence)
               (set-option "schedule_treat_coalescing" treat-coalescing)
               (set-option "schedule_maximize_band_depth" maximize-band-depth)
               (set-option "schedule_whole_component" schedule-whole-component)))
        (configure))))
  (isl:schedule-constraints-compute-schedule
   (isl:schedule-constraints-set-coincidence
    (isl:schedule-constraints-set-proximity
     (isl:schedule-constraints-set-validity
      (isl:schedule-constraints-on-domain (poly-domain pg))
      (poly-dependencies pg))
     (poly-dependencies pg))
    (poly-dependencies pg))))

(defmethod auto-schedule ((poly Polyhedral-IR))
  "An entrypoint for auto-scheduling"
  ;; Getting the initial schedule (TODO: Make configuration changeable)
  (setf (poly-schedule poly) (schedule poly))
  ;; Tiling
  ;; (caten/polyhedral/tiling:tile-bands poly)
  ;; Unrolling/Vectorizing
  ;; (caten/polyhedral/packing:solve poly)
  ;; Parallelize
  
  poly)
