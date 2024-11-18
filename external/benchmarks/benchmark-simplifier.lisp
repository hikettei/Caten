(defpackage :caten/external/benchmarks/simplifier
  (:use :cl :caten/llm :caten/apis :clgplot)
  (:export #:run))

(in-package :caten/external/benchmarks/simplifier)

(defun initialize-transformer (n-layers)
  (ctx:with-contextvar (:JIT 1) (Transformer 128 8 n-layers 1e-5 32)))

(defun measure-simplify-time (model jit)
  (ctx:with-contextvar (:JIT jit)
    (let ((started (get-internal-real-time)))
      (caten (forward model (make-tensor `(1 s)) (iconst 'n)))
      (let ((finished (get-internal-real-time)))
        (float (/ (- finished started) internal-time-units-per-second))))))

;; brew install gnuplot for prepreq
(defun run (&key (n 12) (jit 0) (path nil))
  (assert (find jit `(0 1)) () "JIT=0 or 1")
  (fresh-line)
  (caten/common.logger:print-info "Configuration: N=~a JIT=~a PATH=~a" n jit path)
  (let ((times
          (loop for n-layer upfrom 0 below n
                do (caten/common.logger:print-info "Running with ~a layer transformers..." n-layer)
                collect
                (let ((val (measure-simplify-time (initialize-transformer n-layer) jit)))
                  (caten/common.logger:print-info "Completed in ~a secs" val)
                  val))))
    (format t "[Result]~%")
    (dotimes (n-layer n)
      (format t "N=~a | ~a sec~%" n-layer (nth n-layer times)))
    (clgp:plot
     times
     :x-seq (loop for n-layer upfrom 0 below n collect n-layer)
     :title (format nil "JIT=~a Transformer Compilation Time" jit)
     :output path
     :x-label "Layers"
     :y-label "Compilation Time (s)")
    (when path
      (caten/common.logger:print-info "Plot saved to ~a" path))))
#+(or nil)(run :n 12)
