(defpackage :caten/codegen/search/ast
  (:use :cl)
  (:export
   ))
(in-package :caten/codegen/search/ast)

;; [todo]
(defun ->ast (schedule &key (rank 0))
  (macrolet ((set-option (name level)
	       `(cffi:foreign-funcall ,(format nil "isl_options_set_~(~a~)" name) :pointer (isl::context-handle isl::*context*) :int ,level  :void)))
    ;; メモ: このオプションは固定, caten/codegen/search/astの過程を含む
    (set-option "ast_build_atomic_upper_bound" 1) ;; ループサイズは常にgid <= SIZEのatomic
    (set-option "ast_build_detect_min_max" 1)
    (set-option "ast_build_separation_bounds" 0)
    (set-option "ast_build_exploit_nested_bounds" 1)
    (set-option "ast_build_prefer_pdiv" 0)
    (set-option "ast_build_scale_strides" 1)
    (set-option "ast_build_allow_else" 0) ;; caten does not support else
    (set-option "ast_build_allow_or" 0))
  (let* ((schedule (isl:copy schedule))
	 (ast-build (isl:ast-build-from-context (isl:set-from-str "{:}")))
         (ast-build
           (isl:ast-build-set-iterators
            ast-build
            (apply #'isl:make-id-list (loop for i upfrom 0 below rank collect (gid i)))))
         ;; Added to transform partial tile
         ;; (ast-build (isl::%make-ast-build (isl::%isl-ast-build-set-after-each-for (isl::ast-build-handle ast-build) (cffi:callback isl-on-ast-build) (cffi:null-pointer))))
         ;; Added to set annotation
         ;; (ast-build (isl::%make-ast-build (isl::%isl-ast-build-set-before-each-for (isl::ast-build-handle ast-build) (cffi:callback some) (cffi:null-pointer))))
         
         (ast-build-node (isl:ast-build-node-from-schedule ast-build schedule)))
    ast-build-node))
