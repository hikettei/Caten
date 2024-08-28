(in-package :caten/isl)

(define-isl-object context :free identity)
(defun make-context ()
  (let ((handle (%isl-ctx-alloc)))
    (%isl-options-set-on-error handle +isl-on-error-continue+)
    (%make-context handle)))
(defvar *context* (make-context))
(defun isl-error ()
  (error "isl yields an error: ~a"
         (cffi:foreign-string-to-lisp
          (%isl-ctx-last-error-msg (isl-object-handle *context*))
          :encoding :ascii)))
(defun lispify-isl-bool (isl-bool)
  (ecase isl-bool
    (:bool-true t)
    (:bool-false nil)
    (:bool-error
     (isl-error))))
(defun lispify-isl-size (isl-size)
  (if (= isl-size +isl-size-error+)
      (isl-error)
      (the size isl-size)))
