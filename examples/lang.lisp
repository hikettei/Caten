;;;; Note: This file is not intended to be loaded directly from Lisp,
;;;; but to be executed expression by expression.
;;;; If you are using Emacs/Lem, you can simply press `C-C C-c` while your cursor is hovering over an expression

;; Caten/Lang Example
(unless (find-package :caten)
  (ql:quickload :caten))

(defpackage :caten-lang-example
  (:use :cl :caten/api :caten/lang))

(in-package :caten-lang-example)

(setf (ctx:getenv :BACKEND) "NATIVE") ;; Caten/Lang requires JIT enabled backend

;; [Introduction]
;; Caten/Lang is an experimental, second frontend contrasting with Caten/API.
;; - Caten/API automatically generates optimal kernels from NumPy-style computation graphs.
;; - Caten/Lang allows you to describe memory accesses with C-style explicit loops.
;; [What we want to build]
;; Caten/Lang ultimately aims to construct a workflow like the one shown below.
;; ==[caten/lang workflow]================================================================================
;; | [Common Lisp Code]  -- [ preprocessor(:LISP)   ] -*                                                 |
;; |                                                   |                                                 |
;; |  [C Code (TODO)]    -- [ preprocessor(:C)      ] -* => <Lisp Tiny IR> => [Translator] => <Blueprint>|
;; |                                                   |                                                 |
;; |[Python Code (TODO)] -- [ preprocessor(:PYTHON) ] -*                                                 |
;; |(*) preprocessor = caten/lang:caten-jit-style-handler method                                         |
;; =====================================================================================================
;; [What is missing?]
;; *WARNING* To bring Caten/Lang to production-level quality, the following features are missing (welcome to open a PR!):
;; - [ ] Language features
;;   - [ ] Define language specification and write documentation
;;   - [ ] Error detection (line numbers, a beautiful error display like Coalton, type checking)
;;   - [ ] Automatic Aref/Stride calculation (keep NDArray directly)
;;   - [ ] Automatic loop-bound checking (using the polyhedral model)
;;   - [ ] Shape validation
;; - [ ] Language frontends from C and Python
;;   - [ ] In particular, aim to support running all PolyBench benchmarks from C code
;; - [ ] Quality
;;   - [ ] Comprehensive feature tests
;; [Why Caten/Lang was needed?]
;; - To express complex linear algebra operations that are difficult in NumPy-style.
;; - Our low-level IR adopts the polyhedral model.
;;   - 99% of array processing is affine.
;;   - Our low-level IR can automatically parallelize access patterns like [i] -> [i-1] within mathematically valid bounds.
;;   - We want to leverage this capability to the fullest.

;; Caten/Lang can be directly embedded into Common Lisp code.
;; TopLevel is implemented as a reader macro.
;; (in-caten-toplevel) adds the readtable ’caten/lang:caten to the current readtables.
(in-caten-toplevel)

;; This will introduce a new syntax:
;; ```
;; @caten.<feature_name> (args) { CODE }
;; ```
;; - JIT is implemented as <feature_name> = jit.
;; - You can pass style=<language> to specify how jit parses the code

@caten.jit (:style :lisp) {
(defun example ((Pointer X Type (A)))
  (for idx = (Range A 1) do
       (setf (aref A idx) (sin (aref A idx)))))
}

;; My recommended style is to wrap @caten.jit with (progn ...)
;; This will explict code blocks to your editor and you can do C-c C-c
(progn
  @caten.jit (:style :lisp) {
  (defun sumreduce ((Pointer X Type (A B)))
    (with-locals ((acc 0.0))
      (for idx = (Range (* A B) 1) do
           (setf acc (+= acc (aref X idx))))
      (setf (aref X 0) acc)))})

(print (caten (SumReduce (make-tensor `(10 10) :initial-element 1.0))))

(progn
  @caten.jit () {
  (defun Gemm ((Pointer Z Type (M K)) (Pointer X Type (M N)) (Pointer Y Type (N K)))
    ;; [Note] Why nothing is scheduled?
    (for i = (Range 0 M) do
         (for j = (Range 0 K) do
              (with-locals ((acc 0.0))
                (for k = (Range 0 N) do
                     (setf acc (+ acc (* (aref X (+ (* N i) k)) (aref Y (+ (* K k) j))))))
                (setf (aref Z (+ (* K i) j)) acc)))))})

(defun !matmul-jit (a b)
  (let ((out (st "A[i j] B[j k] -> A[i k]" (a b))))
    (Gemm out a b)))

;; fix: !rand
;; todo: add this to readme
;; merge this to main (see develop...)
;; todo: add tensor-schedule
;; todo: scheduler should consider the tpsort and $sync
