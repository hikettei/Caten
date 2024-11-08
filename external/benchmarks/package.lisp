(defpackage :caten/benchmarks
  (:use :cl :clingon)
  (:export
   #:run-benchmarks))

(in-package :caten/benchmarks)

(defparameter *benchmarks* (make-hash-table :test #'equal))

(defmacro defbench (name usage handler)
  `(setf (gethash ,name *benchmarks*) (cons ,usage ,handler)))

(defun show-usage ()
  (format t "
Usage:
```
caten benchmark [benchmark_name] options
```
benchmarks:
~a"
          (with-output-to-string (s)
            (maphash
             (lambda (k v)
               (format s "  ~a: ~a~%" k (first v)))
             *benchmarks*))))

(defbench "transformer_compile_time"
  "Measures the time it takes to simplify a transformer.
    options: caten benchmark transformer_simplify_time N[integer] JIT[0 or 1] PATH[string: xxx.png]"
  #'(lambda (&rest options)
      (caten/external/benchmarks/simplifier:run :n (parse-integer (car options)) :JIT (parse-integer (second options)) :path (third options))))

(defun run-benchmarks (cmd)
  (let* ((options (command-arguments cmd))
         (result (gethash (first options) *benchmarks*)))
    ;; options = ("transformer_compile_time" "12" "1" "plot.png") for example
    (if result
        (progn
          (caten/common.logger:print-info "Running the benchmark ~a" (first options))
          (apply (cdr result) (cdr options)))
        (show-usage))))
