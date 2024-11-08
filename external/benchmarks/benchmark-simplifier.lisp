(defpackage :caten/external/benchmarks/simplifier
  (:use :cl :caten/llm :caten/apis :clgplot)
  (:export
   ))

(in-package :caten/external/benchmarks/simplifier)

(defun initialize-transformer (n-layers)
  (ctx:with-contextvar (:JIT 1) (Transformer 128 8 n-layers 1e-5 32)))

(defun measure-simplify-time (model jit)
  (ctx:with-contextvar (:JIT jit)
    (let ((started (get-internal-real-time)))
      (caten (forward model (make-tensor `(b 32)) (iconst 'n)))
      (let ((finished (get-internal-real-time)))
        (float (/ (- finished started) internal-time-units-per-second))))))

;; brew install gnuplot for prepreq
(defun run (&key (n 12) (jit 0))
  (assert (find jit `(0 1)) () "JIT=0 or 1")
  (fresh-line)
  (let ((times
          (loop for n-layer upfrom 0 below n
                do (caten/common.logger:print-info "Running with ~a layer transformers..." n-layer)
                collect
                (let ((val (measure-simplify-time (initialize-transformer n-layer) jit)))
                  (caten/common.logger:print-info "Completed in ~a secs" val)
                  val))))
    (caten/common.logger:print-info "Result")
    (dotimes (n-layer n)
      (format t "N=~a | ~a sec~%" n-layer (nth n-layer times)))
    (clgp:plot
     times
     :x-seq (loop for n-layer upfrom 0 below n collect n-layer)
     :title  (format nil "JIT=~a Transformer Compilation Time" jit)
     :x-label "N_Layers"
     :y-label "Compilation Time (s)")))
