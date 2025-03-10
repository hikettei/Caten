(defpackage :caten/codegen/byoc/simd
  (:documentation "A set of helpers to generate vectorized clang code")
  (:use :cl :caten/codegen/helpers)
  (:export
   
   ))

(in-package :caten/codegen/byoc/simd)
;; ~~~ SIMD Codegen ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(deftype packed-storage-t ()
  `(member :__m256))

(defun avx2/dtype->storage (dtype)
  (ecase dtype
    (:float32 :__m256)))

(defun simd-pack (id storage)
  (ecase storage
    (:__m256 (format nil "__m256 ~(~a~) = _mm256_setzero_ps();~%" id))))

(defun simd-unpack ())
;; ~~~ Gemm Microkernels ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; TODO: Support Symbolic
;; val_2_0 = (val_2_0+(val_4[((512*_gid0)+_gid4)]*val_6[(_gid2+(512*_gid4))])); // :reduction=t
;; A: [M K] -> [s1 s2]
;; B: [K N] -> [s3 s4]
;; C: [M N] -> [s5 s6]
(defun make-gemmAxB-naive (dtype a b s1 s2 s3 s4 s5 s6)
  "Generates gemm microkernel (A, B). M N K is a stride."
  (declare (type keyword dtype) (type fixnum a b) (type (or symbol fixnum) s1 s2 s3 s4 s5 s6))
  (let ((dtype (string-downcase (princ-to-string (->cdtype dtype)))))
    (with-output-to-string (out)
      (macrolet ((c (designator &rest args) `(format out ,designator ,@args))
                 (maybe-mul (a b) `(if (zerop ,a) 0 (if (numberp ,b) (* ,a ,b) (format nil "~(~a~)*~(~a~)" ,a ,b)))))
        ;; Header declaration: static inline void _gemm_...(float *a, float *b, float *c0 ... *cn)
        (c "static inline void _gemm_~(~a~)_~ax~a_tc(" dtype a b)
        (c "~a *a, ~a *b, " dtype dtype)
        ;; unroll by I
        (dotimes (i a) (c "float *c~a, " i))
        (c ") {~%")
        (progn
          ;; TODO Symbolic Stride
          (dotimes (kth b)
            (c "  ~a* _a~a = *(a+~a);" dtype kth (maybe-mul kth s2))
            (c " ~a* _b~a = *(b+~a);" dtype kth (maybe-mul kth s3))
            (c "~%  ")
            (dotimes (i a)
              (c "*c~a += (*(_a~a))*(*(_b~a+~a)); " i kth kth (maybe-mul i s4)))
            (c "~%")))
        (c "}")))))

(defun gemm8x8-avx2 (dtype)
  (%gemm8x8 :AVX2 dtype (avx2/dtype->storage dtype))
  )

(defun gemm8x8-avx512 (dtype)

  )

(defun gemm8x8-arm-neon (dtype)
  (%gemm8x8 dtype))
;; ~~~ Vectorized SLEEF Codegen Extensions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
