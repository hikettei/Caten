(in-package :caten)

(define-condition caten-forward-error ()
  ((c :initarg :c) (inputs :initarg :inputs) (op :initarg :op))
  (:report
   (lambda (c s)
     (format s "Failed to forward the function ~a.
Inputs:
~a

Conditions:

~a"
	     (slot-value c 'op)
	     (slot-value c 'inputs)
	     (slot-value c 'c)))))

(define-condition caten-backward-error ()
  ((c :initarg :c) (inputs :initarg :inputs) (op :initarg :op))
  (:report
   (lambda (c s)
     (format s "Failed to backward the function ~a.
Inputs:
~a

Conditions:

~a"
	     (slot-value c 'op)
	     (slot-value c 'inputs)
	     (slot-value c 'c)))))

