;;;; Note: This file is not intended to be loaded directly from Lisp,
;;;; but to be executed expression by expression.
;;;; If you are using Emacs/Lem, you can simply press `C-C C-c` while your cursor is hovering over an expression

;; An Gemm Compilation Example
(unless (find-package :caten)
  (ql:quickload :caten))

(unless (find-package :cffi)
  (ql:quickload :cffi))

(unless (find-package :clgplot)
  (ql:quickload :clgplot))

(defpackage :caten-matmul
  (:use :cl :caten/api :caten/lang :caten/aasm))
(in-package :caten-matmul)

(in-caten-toplevel)

(defstruct Config (N 512) (X) (Y))
(defparameter *config* (make-config))

(defmethod make-inputs-from-config ((config Config))
  (with-slots ((N N) (X X) (Y Y)) config
    (ctx:with-contextvar (:BEAM 0)
      (values
       (setf X (or X (proceed (!rand `(,N ,N)))))
       (setf Y (or Y (proceed (!rand `(,N ,N)))))))))
;; ~~ Settings ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass Experiment ()
  ((name :initarg :name :type string :accessor experiment-name)
   (N :initarg :name :type int :accessor experiment-n)))

(defgeneric compute (experiment))
(defgeneric get-result-array (experiment))

(defclass NaiveLispMatmul (Experiment)
  ((caller :accessor naive-lisp-matmul-caller)))

(defun make-naive-lisp-matmul (config)
  (with-slots ((N N)) config
    (multiple-value-bind (X Y) (make-inputs-from-config config)
      (let ((C (make-array (* N N)
                           :element-type 'single-float
                           :initial-element 0.0f0)))
        (lambda ()
          (declare (optimize (speed 3)))
          (let ((x (change-facet X :simple-array))
                (y (change-facet Y :simple-array)))
            (declare (type (simple-array single-float (*)) X Y) (type fixnum N))
            (dotimes (i N)
              (dotimes (j N)
                (let ((sum 0.0f0))
                  (declare (type single-float sum))
                  (dotimes (k N)
                    (incf sum (* (aref x (+ (the fixnum (* i N)) k))
                                 (aref y (+ (the fixnum (* k N)) j)))))
                  (setf (aref C (+ (the fixnum (* i N)) j)) sum))))
            C))))))

(defmethod initialize-instance ((experiment NaiveLispMatmul) &key &allow-other-keys)
  (setf (experiment-name experiment) "NaiveLispMatmul"
        (naive-lisp-matmul-caller experiment)
        (make-naive-lisp-matmul *config*)))

(defmethod compute ((experiment NaiveLispMatmul)) (funcall (naive-lisp-matmul-caller experiment)))
(defmethod get-result-array ((experiment NaiveLispMatmul)) (funcall (naive-lisp-matmul-caller experiment)))

(defclass OpenBLAS (Experiment)
  ((caller :accessor openblas-caller)))

(defconstant +cblas-row-major+ 101)
(defconstant +cblas-col-major+ 102)
(defconstant +cblas-no-trans+ 111)
(defconstant +cblas-trans+    112)

(cffi:defcfun ("cblas_sgemm" sgemm) :void
  (order :char)
  (transa :char) (transb :char)
  (m :int) (n :int) (k :int)
  (alpha :float)
  (a :pointer)
  (lda    :int)
  (b :pointer)
  (ldb :int)
  (beta :float)
  (c :pointer)
  (ldc :int))

(defun make-openblas-matmul (config)
  (with-slots ((N N)) config
    (multiple-value-bind (X Y) (make-inputs-from-config config)
      (let ((C (make-array (* N N) :element-type 'single-float :initial-element 0.0)))
        (cffi:with-pointer-to-vector-data (x* (change-facet X :simple-array))
          (cffi:with-pointer-to-vector-data (y* (change-facet Y :simple-array))
            (cffi:with-pointer-to-vector-data (c* c)
              #'(lambda ()
                  (sgemm +cblas-row-major+ +cblas-no-trans+ +cblas-no-trans+
                         N N N
                         1.0
                         X* N
                         Y* N
                         0.0
                         C* N)
                  c))))))))

(defmethod initialize-instance ((experiment OpenBLAS) &key &allow-other-keys)
  (setf (experiment-name experiment) "OpenBLAS"
        (openblas-caller experiment) (make-openblas-matmul *config*)))

(defmethod compute ((experiment OpenBLAS)) (funcall (openblas-caller experiment)))
(defmethod get-result-array ((experiment OpenBLAS)) (funcall (openblas-caller experiment)))

(defclass CatenMatmul (Experiment)
  ((caller :accessor caten-caller)))

(defun make-caten-matmul (config)
  (multiple-value-bind (x y) (make-inputs-from-config config)
    (caten (!matmul x y))))

(defmethod initialize-instance ((experiment CatenMatmul) &key &allow-other-keys)
  (setf (experiment-name experiment) (format nil "Caten(BEAM=~a)" (ctx:getenv :BEAM))
        (caten-caller experiment) (make-caten-matmul *config*)))

(defmethod compute ((e CatenMatmul)) (forward (caten-caller e)))
(defmethod get-result-array ((e CatenMatmul)) (change-facet (forward (caten-caller e)) :simple-array))
;; ~~ OMP (Static) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defparameter *best-tile-size* 4)

(defclass ClangOMP (Experiment)
  ((caller :accessor clang-caller) (x :accessor clang-x) (y :accessor clang-y) (z :accessor clang-z)
   (tile :accessor clang-tile :initarg :tile :initform *best-tile-size*)))

(defmethod initialize-instance ((experiment ClangOMP) &rest initargs &key &allow-other-keys)
  (let* ((id (format nil "kernel_~(~a~)" (gensym)))
         (kernel (make-instance 'caten/byoc/clang::ClangKernel
                                :name id
                                :args (list (%global 'left :float32 t) (%global 'right :float32 t) (%global 'out :float32 t))))
         (N (config-n *config*))
         (tile-size (or (getf initargs :tile) *best-tile-size*)))
    (format t "tile-size=~a name=~a~%" tile-size id)
    (setf (experiment-name experiment) "OMP(STATIC)"
          (caten/byoc/clang::clang-program kernel)
          (print (format nil
                         "
#include <math.h>
#include <stdint.h>
#include <omp.h>
#define total ~a
#define rows ~a
#define columns ~a
#define inners ~a
#define tileSize ~a
#include <stddef.h>
#define min(a, b) ((a) < (b) ? (a) : (b))
#define max(a, b) ((a) > (b) ? (a) : (b))
void ~a(const float * restrict left, const float * restrict right, float * restrict result);
void ~a(const float * restrict left, const float * restrict right, float * restrict result) {
#pragma omp parallel for
for (int i=0; i<total; i++) result[i] = 0.0f;
#pragma omp parallel for shared(result, left, right) default(none) collapse(2)
  for (int rowTile = 0; rowTile < rows; rowTile += 256) {
    for (int columnTile = 0; columnTile < columns; columnTile += 256) {
      for (int innerTile = 0; innerTile < inners; innerTile += tileSize) {
        for (int row = rowTile; row < rowTile + 256; row++) {
          int innerTileEnd = min(inners, innerTile + tileSize);
          for (int inner = innerTile; inner < innerTileEnd; inner++) {
            for (int col = columnTile; col < columnTile + 256; col++) {
              result[row * columns + col] +=
                  left[row * inners + inner] * right[inner * columns + col];
}}}}}}}
"
                  (* N N) N N N tile-size id id))
          (caten/byoc/clang::clang-caller kernel)
          (compile
           nil
           (caten/byoc/clang::make-foreign-function-caller
            (caten/codegen/byoc:kernel-name kernel)
            (caten/codegen/byoc:kernel-args kernel)))
          (clang-caller experiment) kernel)
    (caten/byoc/clang::load-foreign-function
     (caten/byoc/clang::clang-program kernel)
     :use-omp t :compiler (ctx:getenv :CC) :lang "c" :compiler-flags '("-O3"))
    (multiple-value-bind (x y) (make-inputs-from-config *config*)
      (setf (clang-x experiment) x
            (clang-y experiment) y
            (clang-z experiment) (change-facet (make-array (* N N) :element-type 'single-float :initial-element 0.0) :tensor)))))

(defmethod compute ((e ClangOMP))
  (funcall (caten/byoc/clang::clang-caller (clang-caller e)) (tensor-buffer (clang-x e)) (tensor-buffer (clang-y e)) (tensor-buffer (clang-z e))))

(defmethod get-result-array ((e ClangOMP)) (change-facet (clang-z e) :simple-array))

(defun omp-autotune-best-tile-size (&key (candidates `(1 2 4 8 16 32 64)))
  (let ((results))
    (format t "Autotuning tilesize for omp matmul. N=~A~%" (config-n *config*))
    (loop for c in candidates
          for mm = (make-instance 'ClangOMP :tile c) for sum = 0.0 do
            (dotimes (i 100) (incf sum (compute mm)))
            (push (cons c sum) results))
    (print "best tile size is:")
    (print results)
    (setf *best-tile-size* (car (print (car (sort results #'< :key #'cdr)))))))
;; ~~ OpenBLAS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defparameter *openblas-available-p* nil)
(when (eql :CLANG (ctx:getenv :BACKEND))
  (handler-case
      (progn
        (cffi:load-foreign-library (or (uiop:getenv "OPENBLAS") "libblas.dylib"))
        (setf *openblas-available-p* t))
    (error (c)
      (warn "
Could not locate the OpenBLAS library.
It looks like OpenBLAS is not present in the Experiment environment, or the path is incorrect.
Please set the OPENBLAS environment variable to the full path of your OpenBLAS library and try again.
(e.g.: OPENBLAS=\"libopenblas.dylib\" qlot exec ...)

Error details: ~a" c))))
;; ~~ Reporter ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun compute-diffs (A B)
  (let ((max-abs 0.0f0) (max-rel 0.0f0))
    (dotimes (i (length A))
      (let* ((a (aref A i)) (b (aref B i))
             (absd (abs (- a b)))
             (reld (if (/= a 0.0f0) (/ absd (abs a)) 0.0f0)))
        (when (> absd max-abs) (setf max-abs absd))
        (when (> reld  max-rel) (setf max-rel reld))))
    (values max-abs max-rel)))

(defun get-naive-array-for-size (size results)
  (some (lambda (entry)
          (when (and (typep (first entry) 'OpenBLAS)
                     (= size (isqrt (length (second entry)))))
            (second entry)))
        results))

(defun make-report (results)
  (let ((csv-lines (list '("Experiment" "GFLops" "Time" "atol" "rtol" "Size"))))
    (dolist (res (reverse results))
      (destructuring-bind (exp arr time) res
        (let* ((name  (experiment-name exp))
               (size  (isqrt (length arr)))
               (gflops (/ (* 2 (expt size 3)) time 1e9))
               (naive (get-naive-array-for-size size results))
               (atol 0.0f0) (rtol 0.0f0))
          (multiple-value-setq (atol rtol) (compute-diffs naive arr))
          (push (list name
                      (format nil "~a" gflops)
                      (format nil "~a" time)
                      (format nil "~a" atol)
                      (format nil "~a" rtol)
                      (princ-to-string size))
                csv-lines))))
    (with-open-file (out "./experiment.csv"
                         :direction :output
                         :if-exists  :supersede)
      (dolist (line (nreverse csv-lines))
        ;; join the list of strings with commas via FORMAT
        (write-line (format nil "~{~a~^,~}" line) out)))
    (let ((sizes (mapcar (lambda (res)
                           (isqrt (length (second res))))
                         results))
          (times (mapcar (lambda (res)
                           (third res))
                         results)))
      (clgplot:plot
       times                    
       :x-seq        sizes      
       :x-label     "Matrix Size"
       :y-label     "Execution Time (s)"
       :main        "Execution Time vs Matrix Size (Log Scale)"
       :y-logscale  t
       :output      "./experiment-time-log.png"
       :output-format :png))))

(defun benchmark (&key
                  (impls (list 'ClangOMP 'CatenMatmul))
                  (n-profile 100)
                  &aux (results))
  (when *openblas-available-p* (push 'OpenBLAS impls))
  (loop for N in `(256 512 1024 2048 4096)
        for *config* = (make-config :N N)
        for *best-tile-size* = (omp-autotune-best-tile-size)
        for settings = (map 'list #'make-instance impls) do
          (loop for setting in settings do
            (format t "N=~a, setting=~a~%" n setting)
            (compute setting) ;; pre allocation
            (push
             (list setting
                   (get-result-array setting)
                   (caten/runtime/profile:with-real-time
                     (dotimes (i n-profile) (compute setting))))
             results)))
  (make-report results))

(print (benchmark))
