(defpackage :caten/lang.jit.clang-helper
  (:use :cl))
(in-package :caten/lang.jit.clang-helper)

(named-readtables:in-readtable with-c-syntax:with-c-syntax-readtable)
(defun string->common-lisp (code &key (n 1))
  (declare (type string code))
  (let ((code (format nil "~a ~%}#" code)))
    (with-input-from-string (stream code)
      (let* ((level (or n with-c-syntax.core::*with-c-syntax-reader-level*))
             (readtable-case (or with-c-syntax.core::*with-c-syntax-reader-case*
                                 (with-c-syntax.core::readtable-case *readtable*)))
             (readtable (with-c-syntax.core::find-c-readtable level readtable-case))
             (with-c-syntax.core::*previous-readtable* *readtable*)
             (tokens (with-c-syntax.core::tokenize-source stream t readtable)))
        (with-c-syntax.core::expand-c-syntax
         (with-c-syntax.core::preprocessor tokens level readtable-case nil) t)))))

(in-package :caten/lang)
;; @caten.jit creates blueprint graph from ANSI Common Lisp Program
;; Also by translating C code into Common Lisp (using with-c-syntax) ultimately it allows to embody C code in Common Lisp
;; and get optimized kernel for any language

;;; ~~ Any language ==> Common Lisp Translator ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defparameter *supported-styles* `(:lisp))

(defgeneric caten-jit-style-handler (style-id code))

(defmethod caten-jit-style-handler (style-id code)
  (if (next-method-p)
      (call-next-method)
      (error "Failed to expand the macro @caten.jit(style=~a).~%  The style ~a is not supported.~%Currently available: ~a" style-id style-id *supported-styles*)))

(defmethod caten-jit-style-handler ((style-id (eql :lisp)) code)
  (handler-case (read-from-string code)
    (error (c) (error "@caten.jit(:lisp,...): Failed to parse the code due to:~%  ~a" c))))

(defmethod caten-jit-style-handler ((style-id (eql :c)) code)
  (error "NOT READY")
  (caten/lang.jit.clang-helper::string->common-lisp code))

(define-caten-feature (jit :docstring "
```
@caten.jit(:style style :n-profile) {

}
```")
    ((ctx style)
      (let* ((code (directive-context-code ctx))
             (pos-first-char (position-if #'(lambda (x) (and (not (char= x #.(aref " " 0))) (not (char= x #\newline)))) code))
             (pos-last-char (position-if #'(lambda (x) (and (not (char= x #.(aref " " 0))) (not (char= x #\newline)))) (reverse code)))
             (code (subseq code pos-first-char (- (length code) pos-last-char)))
             (code (if (and (char= (aref code 0) #\") (char= (aref code (1- (length code))) #\"))
                       (subseq code 1 (1- (length code)))
                       code))
             (form (caten-jit-style-handler style code)))
        (print form)
        nil)))

(in-caten-toplevel)

(progn
  @caten.jit (:lisp) {
  (defun flash-attention (Q<T>[Batch Head N D] K<T>[Batch Head N D] V<T>[Batch Head N D]
                          O<Float>[BATCH Head N D] L<Float>[Batch Head N] M<Float>[Batch Head N])
    (let ((scale (/ 1.0 (sqrt (coerce D 'single-float))))
          (outer (* batch n head)))
      (for idx = (Range 0 outer) do
           (let* ((tmp idx)
                  (i (mod tmp N))
                  (tmp (/ tmp N))
                  (h (mod tmp HEAD))
                  (tmp (/ tmp HEAD))
                  (b tmp)
                  (q-base-idx (* D (+ i (* n (+ (* b head) h)))))
                  (k-base-idx (* D (* n (+ (* b head) h))))
                  (v-base-idx (* D (* n (+ (* b head) h))))
                  (o-base-idx (* D (+ i (* n (+ (* b head) h)))))
                  (row-m (aref M (+ i (* n (+ (* b head) h)))))
                  (row-l (aref L (+ i (* n (+ (* b head) h))))))
             (for j = (Range 0 N 1) do
                  (let ((dot 0.0))
                    (for dth = (Range 0 D 1) do
                         (setf dot (+ dot (* (aref Q (+ q-base-idx d)) (aref K (+ k-base-idx d))))))
                    (let* ((S (* dot scale))
                           (new-max (max row-m S))
                           (exp-prev (exp (- row-m new-max)))
                           (exp-cur (exp (- S new-max)))
                           (l-new (+ (* exp-prev row-l) exp-cur)))
                      (for dth = (Range 0 D 1) do
                           (setf (aref O (+ o-base-idx d))
                                 (/ (+ (* exp-cur (aref V (+ v-base-idx d))) (* exp-prev row-l (aref O (+ o-base-idx d)))) l-new)))
                      (setf row-m new-max
                            row-l l-new))))
             (setf
              (aref M (+ i (* n (+ (* b head) h)))) row-m
              (aref L (+ i (* n (+ (* b head) h)))) row-l)))))
  })
