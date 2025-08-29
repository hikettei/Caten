(defpackage :caten/codegen/dataflow
  (:shadow #:set #:space)
  (:shadowing-import-from :cl :map)
  (:use :cl :caten/isl :caten/air :caten/codegen/search/schedule)
  (:export))
(in-package :caten/codegen/dataflow)


;; ScheduleGraph => AbstractHardware
;;; dataflow-ops.lisp
;;; Domain
;;; Band
;;; Filter
;;; 
(defun make-dataflow-graph (schedule read write)
  (declare (type isl::schedule schedule) (type isl::union-map read write))
  (let ((ast-build (create-ast-build)))
    (labels ((explore (node)
               (case (schedule-node-get-type node)
                 (:schedule-node-domain
                  (let ((domain-set (schedule-node-domain-get-domain node)))
                    ;; isl_ast_build_expr_from_pw_aff
                    (%foreach-set
                     domain-set
                     #'(lambda (set)
                         (let ((ndim (set-dim set :dim-set)))
                           (loop for n upfrom 0 below ndim
                                 for id    = (ast-expr-from-id (set-get-dim-id set :dim-set n))
                                 for lower = (ast-build-expr-from-pw-aff ast-build (set-dim-min set n))
                                 for upper = (ast-build-expr-from-pw-aff ast-build (set-dim-max set n)) do
                                   ;; [TODO] DomainNode
                                   (print id)
                                   (print lower)
                                   (print upper)
                                 ))))
                    ))
                 (:schedule-node-band

                  ))))
      (explore (schedule-get-root schedule)))))
