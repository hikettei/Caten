(in-package :caten/isl)

(define-isl-object isl-printer :free %isl-printer-free)

(define-isl-function isl-printer-to-str %isl-printer-to-str
  (:give isl-printer)
  (:parm context *context*))
