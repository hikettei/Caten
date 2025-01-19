;;;; Note: This file is not intended to be loaded directly from Lisp,
;;;; but to be executed expression by expression.
;;;; If you are using Emacs/Lem, you can simply press `C-C C-c` while your cursor is hovering over an expression

;;;; Matmul Benchmark (vs OpenBLAS)

(unless (find-package :caten)
  (ql:quickload :caten))

(defpackage :matmul-cpu
  (:use :cl :cffi :caten))

(in-package :matmul-cpu)

(setf
 (ctx:getenv :BACKEND) "CLANG"
 (ctx:getenv :JIT_DEBUG) 4
 (ctx:getenv :OPTIMIZE) 2
 (ctx:getenv :PROFILE) 1) ;; This will display GFLOPS on your repl.

;; You may also want to modify these parameters to maximize the performance.
(setf
 (ctx:getenv :CC) "gcc-14" ;; Adjust the compiler to your environment!
 (ctx:getenv :OMP) 1)      ;; Set OMP=1 to use OpenMP

;; (TODO: env:print-vectorize-info)

;; Note(hikettei), +darwin is set up for ease of use in my environment. You may need to adjust the compiler flags for your environment.
(caten/byoc/clang:load-foreign-function
 "
#include <cblas.h>
void call_sgemm(int M, int K, int N, float *A, float *B, float *C) {
  // Note: replace it for your own!
  cblas_sgemm(CblasRowMajor, CblasNoTrans, CblasNoTrans, M, K, N, 1.0f, A, N, B, K, 1.0f, C, K);
}"
 :compiler
 #+darwin"gcc-14"
 #-darwin"gcc"
 :compiler-flags
 #+darwin'("-I/opt/homebrew/opt/openblas/include" "-L/opt/homebrew/opt/openblas/lib" "-lopenblas")
 #-darwin'("-lopenblas"))

(defcfun ("call_sgemm" sgemm) :void
  (m :int) (k :int) (n :int)
  (a :pointer) (b :pointer) (c :pointer))

(defmethod change-facet (obj (direction (eql :ext/int-sap)))
  (with-pointer-to-vector-data (a* (change-facet obj :simple-array)) a*))

(defun matmul-openblas (a b n-sample)
  (st "A[~ m n] B[~ n k] -> A[~ m k]" (a b))
  (let* ((m (nth 0 (shape a)))
         (k (nth 1 (shape a)))
         (n (nth 1 (shape b)))
         (out (linspace `(,m ,n) 0.0 0.0)))
    (let ((t1 (get-internal-real-time)))
      (dotimes (i n-sample)
        (sgemm m k n (change-facet a :ext/int-sap) (change-facet b :ext/int-sap) (change-facet out :ext/int-sap)))
      (let ((t2 (get-internal-real-time)))
        (values out (float (/ (- t2 t1) internal-time-units-per-second)))))))

(defun matmul-caten (a b n-sample)
  (with-no-grad
    (let* ((matmul (caten (!matmul a b)))
           (t1 (get-internal-real-time))
           (out)
           (_ (dotimes (i n-sample) (setf out (forward matmul))))
           (t2 (get-internal-real-time)))
      (declare (ignore _))
      (values out (float (/ (- t2 t1) internal-time-units-per-second))))))

(defun compare-speed (M N K &key (n-sample 1))
  (let ((a (rand `(,M ,N)))
        (b (rand `(,N ,K)))
        (flops (* M N K 2 n-sample)))
    (multiple-value-bind (openblas-out openblas-time) (matmul-openblas a b n-sample)
      (multiple-value-bind (caten-out caten-time) (matmul-caten a b n-sample)
        (format t "[M=~a N=~a K=~a]~%" M N K)
        (format t "OpenBLAS: ~a GFLOPS (~a sec)~%" (float (/ (/ flops openblas-time) 1e9)) openblas-time)
        (format t "Caten: ~a GFLOPS (~a sec)~%" (float (/ (/ flops caten-time) 1e9)) caten-time)
        (when (= n-sample 1) ;; OpenBLAS won't fill C with zeros.
          (format t "max_error = ~a" (reduce #'max (map 'list #'abs (map 'list #'- (change-facet openblas-out :simple-array) (change-facet caten-out :simple-array))))))
        ;; (values openblas_gflops caten_gflops)
        (values (float (/ (/ flops openblas-time) 1e9)) (float (/ (/ flops caten-time) 1e9)))))))

;; Accuracy
(compare-speed 1024 1024 1024 :n-sample 1)

;; GFLOPS
(compare-speed 1024 1024 1024 :n-sample 10)

;; (TODO: Plot the graph using cl-gnuplots)
