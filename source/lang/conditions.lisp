(in-package :caten/lang)

(defun pprint-form (form position)
  (let ((out 0) (count 0))
    (values
     (with-output-to-string (s)
       (if (listp form)
           (progn
             (incf count) (format s "(")
             (loop for f in form
                   for nth upfrom 0
                   if (= nth position) do (setf out count)
                     do (if (or (not (listp f)) (every #'(lambda (x) (not (listp x))) f)) ;; not nested -> render
                            (progn
                              (format s "~a" f)
                              (incf count (length (format nil "~a" f))))
                            (progn
                              ;; Nested -> Omit
                              (format s "...")
                              (incf count 5)))
                   if (not (= nth (1- (length form)))) do (format s " ") (incf count))
             (format s ")"))
           (format s "~a" form)))
     out)))

(define-condition Action-Syntax-Error ()
  ((message :initarg :message :reader action-syntax-error-message)
   (form    :initarg :form :reader action-syntax-error-form)
   (position :initarg :position :reader action-syntax-error-position))
  (:report
   (lambda (c s)
     (multiple-value-bind (code count) (pprint-form (action-syntax-error-form c) (action-syntax-error-position c))
       (format s "Action-Syntax-Error:
```
~a
~a^~a
```
"
               code
               (make-string count :initial-element #\space)
               (action-syntax-error-message c))))))
