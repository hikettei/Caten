(cl:in-package :cl-user)
(defpackage :caten/common.logger
  (:documentation "")
  (:nicknames #:log)
  (:use :cl :cl-ansi-text)
  (:export
   #:*default-stream*
   #:print-info
   #:with-progress
   #:print-progress))

(in-package :caten/common.logger)

(defparameter *default-stream* t "A default stream to put out logs")
(macrolet ((defcolor (name color &key (effect :unset) (style :foreground))
	     `(defun ,name (txt)
		(with-output-to-string (out)
		  (with-color (,color :stream out :effect ,effect :style ,style)
		    (write-string txt out))))))
  (defcolor gray 90)
  (defcolor white-bright :white :effect :bright))

(defmacro maybe-ansi (op &rest args)
  `(if (= 1 (ctx:getenv :COLOR))
       (,op ,@args)
       ,@args))

(defun timestamp ()
  (multiple-value-bind
        (second minute hour day month year day-of-week dst-p tz)
      (get-decoded-time)
    (declare (ignore day-of-week dst-p))
    (maybe-ansi
     blue
     (format nil "[~2,'0d:~2,'0d:~2,'0d, ~d/~2,'0d/~d (GMT~@d)]"
	     hour
	     minute
	     second
	     month
	     day
	     year
	     (- tz)))))

(defun print-info (content &rest args)
  (when (>= (ctx:getenv :DEBUG) 0)
    (format *default-stream* "~a : ~a~%" (timestamp) (apply #'format nil content args))))

;; (1/n) ... (xxx ms)
;; (2/n) ... (yyy ms)
(defparameter *progress* nil)
(defstruct (Progress
	    (:constructor make-progress (total-count &key (debug 0))))
  (debug debug) (n 0)
  (total total-count)
  (last-time (get-internal-run-time)))

(defmacro with-progress ((total &key (title nil) (debug 0)) &body body)
  `(let ((*progress* (make-progress ,total :debug ,debug)))
     (when ,title
       (format *default-stream* (maybe-ansi white-bright (format nil "~a (n/~a)~%" ,title ,total))))
     ,@body))

(defun print-progress (content &rest args)
  (when (>= (ctx:getenv :DEBUG) 0)
    (assert *progress* () "print-progress: *progress* was not initialized.")
    (let* ((last-time (progress-last-time *progress*))
	   (now (get-internal-run-time))
	   (ms (- now last-time)))
      (setf (progress-last-time *progress*) now)
      (incf (progress-n *progress*))
      (format *default-stream* "* ~a ~a ~a~%"
	      (maybe-ansi blue (format nil "(~a/~a)" (progress-n *progress*) (progress-total *progress*)))
	      (maybe-ansi white-bright (apply #'format nil content args))
	      (if (or (= 0 ms) (>= ms 1000))
		  (maybe-ansi gray (format nil "(~a sec)" (/ ms 1000)))
		  (maybe-ansi gray (format nil "(~a ms)" ms)))))))
