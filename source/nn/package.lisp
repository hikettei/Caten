(cl:in-package :cl-user)
(defpackage :caten/nn
  (:documentation "Caten Neural Network Frontends
Policy:
  - We only provide the official implementation to established and well used nn modules.
  - You have to ensure that each files works standalone. (Use 1 file, 1 package coding rule at caten/nn)
  - Each module should be tested well (we expected to have a ton of regression tests here); 1 file 1 test-suite.
  - TODO: Add test-helpers.lisp")
  (:use :cl :caten :caten/air :alexandria)
  ;; from activations.lisp
  (:export
   #:!relu
   )
  ;; from conv.lisp
  (:export
   #:ConvND
   #:convnd-weight
   #:convnd-bias)

  ;; from padding.lisp
  (:export
   #:!padding
   #:!padding2d
   )
  ;; from pool.lisp
  (:export
   )
  )

(in-package :caten/nn)

(defmacro slice (list upfrom &optional (below (length list)) (by 1))
  (with-gensyms (upfrom1 below1)
    `(let* ((redirect (signum ,by))
	    (,upfrom1 (if (>= ,upfrom 0) ,upfrom (+ (length ,list) ,upfrom)))
	    (,below1  (when ,below (if (>= ,below 0) ,below (+ (length ,list) ,below))))
	    (out
	      (loop for i upfrom ,upfrom1 below (or ,below1 (length ,list)) by (abs ,by) collect (nth i ,list))))
       (if (= 1 redirect)
	   out
	   (reverse out)))))

(defmacro range (from below &optional (by 1))
  `(loop for i from ,from below ,below by ,by collect i))

(defpackage :caten/nn.test
  (:use :cl :caten :caten/nn :caten/avm :rove))

(in-package :caten/nn.test)
;; Common Utils for caten/nn.test
(defun elements (tensor) (buffer-value (tensor-buffer tensor)))
