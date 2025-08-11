(defpackage :caten/codegen/search/perf
  (:use :cl :caten/air :caten/aasm :caten/codegen/byoc)
  (:import-from :caten/codegen/renderer #:render-node #:Default-Renderer)
  ;; GFlops Mesaurer
  (:export
   #:GFlops-Measurer
   #:GFlops-Measurer-ops
   #:GFlops-Measurer-succeed-p
   #:compute-gflops
   #:schedule-item-gflops))

(in-package :caten/codegen/search/perf)
;;; ~~~~ GFlops Measurements ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defstruct GFlops-Measurer
  "A helper object to compute GFlops"
  (ops (error "flops must occur") :type (or null caten/aasm/expr:Expr))
  (succeed-p t :type boolean))
(defun cannot-compute-flop () (make-gflops-measurer :ops nil :succeed-p nil))
(defmethod compute-gflops ((gfm GFlops-Measurer) elapsed params)
  (when (null (gflops-measurer-succeed-p gfm)) (return-from compute-gflops nil))
  (assert (gflops-measurer-ops gfm))
  (when (zerop elapsed) (return-from compute-gflops nil)) ;; Elapsed Time = 0.0
  (let* ((ops (apply #'caten/aasm/expr:expr-realize (gflops-measurer-ops gfm) params))
         (_ (assert (numberp (caten/runtime:buffer-value ops)) () "measure-gflpos: the result is not a number."))
         (gflops (/ (caten/runtime:buffer-value ops) (* elapsed 1e9))))
    (declare (ignore _))
    gflops))
(defmethod schedule-item-gflops (blueprint &aux (total-flops))
  (let ((ctx (make-scop-ctx-from-blueprint blueprint)))
    (loop for expr in (ctx-exprs ctx)
          for expr-graph = (caten/aasm::ast-expr-graph blueprint expr) do
            (let ((flop (caten/aasm/expr:expr-const (caten/aasm/expr::nodes-flops (graph-nodes expr-graph)) :int64))
                  (volume
                    (reduce
                     #'caten/aasm/expr:expr-mul
                     (loop for loop-info in (gethash (node-id expr) (ctx-node-to-loops ctx))
                           for loop = (getf loop-info :for-node)
                           for range = (id->value blueprint (car (node-reads loop)))
                           for size = (car (node-reads range))
                           for step = (second (node-reads range))
                           for size-expr = (id->value blueprint size)
                           for step-expr = (id->value blueprint step)
                           for size-graph = (if (numberp size) (caten/aasm/expr:expr-const size :int64) (caten/aasm/expr:expr-from-graph (car (node-reads size-expr)) blueprint))
                           for step-graph = (if (numberp step) (caten/aasm/expr:expr-const step :int64) (caten/aasm/expr:expr-from-graph (car (node-reads step-expr)) blueprint))
                           collect
                           (caten/aasm/expr:expr-div size-graph step-graph)))))
              (push (caten/aasm/expr:expr-mul flop volume) total-flops)))
    (let ((ops (reduce #'caten/aasm/expr:expr-add total-flops)))
      (make-gflops-measurer :ops ops :succeed-p t))))
