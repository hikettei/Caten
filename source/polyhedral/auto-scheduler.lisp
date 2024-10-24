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

  (let* ((schedule (isl:schedule-set-options (isl:copy (poly-schedule poly)) :atomic))
	 (ast-build (isl:ast-build-from-context (isl:set-from-str "{:}")))
         (rank (* 2 rank)) ;; rank * tile_bands * vectorizing
         (ast-build (isl:ast-build-set-iterators ast-build (apply #'isl:make-id-list (loop for i upfrom 0 below rank collect (gid i)))))
	 (ast-build-node (isl:ast-build-node-from-schedule ast-build schedule)))
    ast-build-node))

(defmethod auto-schedule ((poly Polyhedral-IR))
  
  poly)
