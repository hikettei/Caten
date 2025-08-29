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
(defstruct Domain
  (name)
  (idx)
  (lower)
  (upper))

(defstruct Scope
  (domain))

(defun make-dataflow-graph (schedule read write)
  (declare (type isl::schedule schedule) (type isl::union-map read write))
  (let ((ast-build (create-ast-build)))
    (labels ((explore (node scope)
               (case (schedule-node-get-type node)
                 (:schedule-node-domain
                  (assert (null (scope-domain scope)) () "Domain should be root")
                  (let ((domain-set (schedule-node-domain-get-domain node))
                        (new-scope))
                    ;; isl_ast_build_expr_from_pw_aff
                    (%foreach-set
                     domain-set
                     #'(lambda (set)
                         (loop with ndim = (set-dim set :dim-set)
                               for n upfrom 0 below ndim
                               for id    = (ast-expr-from-id (set-get-dim-id set :dim-set n))
                               for lower = (ast-build-expr-from-pw-aff ast-build (set-dim-min set n))
                               for upper = (ast-build-expr-from-pw-aff ast-build (set-dim-max set n)) do
                                 ;; [TODO] DomainNode
                                 (print id)
                                 (print lower)
                                 (print upper)
                               )))
                    (explore (schedule-node-first-child node) scope)))
                 ((:schedule-node-sequence :schedule-node-set)
                  )
                 ((:schedule-node-leaf)
                  ;; A list of domain is inserted
                  )
                 (:schedule-node-filter
                  
                  )
                 (:schedule-node-band
                  (let* ((mupa (schedule-node-band-get-partial-schedule node))
                         (size (multi-union-pw-aff-size mupa)))
                    (loop for i upfrom 0 below size
                          for upa = (multi-union-pw-aff-get-union-pw-aff mupa i)
                          for pw-aff-list = (union-pw-aff-get-pw-aff-list upa) do
                            (loop for n upfrom 0 below (pw-aff-list-size pw-aff-list)
                                  for pw-aff = (pw-aff-list-elt pw-aff-list n) do
                                    (print pw-aff)
                                    ;(PRINT "BAND")
                                    ;(print node)
                                   ; (print (ast-build-expr-from-pw-aff ast-build pw-aff))
                                    (print pw-aff)))
                    
                    nil)))))
      (explore (print (schedule-get-root schedule)) (make-scope)))))
