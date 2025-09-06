(cl:in-package :cl-user)
(defpackage :caten/common.tqdm
  (:documentation "Customized version of cl-tqdm.
Codes are from https://github.com/hikettei/cl-tqdm/blob/main/cl-tqdm.lisp
Usage:
```
(tqdm:with (x 10 :description ...)
  (tqdm:update x))
```")
  (:nicknames #:tqdm)
  (:use :cl)
  (:export
   #:with
   #:update
   #:*space-string*
   #:*bar-string*
   #:*indent*))
(in-package :caten/common.tqdm)
;; TODO: Fix ETA
(declaim (inline time-since))
(defun time-since (now then) (/ (- now then) internal-time-units-per-second))
(defstruct (EMA)
  "EMA (Exponential Moving Average) Structure."
  (alpha 0.1 :type single-float)
  (value 1.0 :type single-float))
(defun add-ema (ema value)
  (declare (type EMA ema))
  (setf (ema-value ema)
        (+ (* (ema-alpha ema) value)
           (* (- 1.0 (ema-alpha ema)) (ema-value ema)))))
(defun get-ema (ema)
  (declare (type EMA ema))
  (ema-value ema))

(defstruct (TqdmBar
            (:conc-name tqdm-)
            (:print-function
             (lambda (tqdm stream depth)
               (declare (ignore depth))
               (update tqdm :incf 0 :stream stream)
               nil))
            (:constructor
                tqdm (total-count
                      &optional
                        (description "")
                      &aux (creation-time (get-universal-time)))))
  (animate (= 1 (ctx:getenv :ANIMATE)) :type boolean)
  (identifier :no-name :type symbol)
  (total-count 0 :type fixnum)
  (ema (make-ema :alpha 0.1))
  (count-idx 0 :type fixnum)
  (creation-time 0 :type (integer 0 4611686018427387903))
  (last-update-time 0 :type (integer 0 4611686018427387903))
  (description "" :type string))

(defparameter *space-string* " ")
(defparameter *bar-string* "█")
(defparameter *indent* 0)

(defmacro with ((bind total-size &key (description "")) &body body)
  `(let ((,bind (tqdm ,total-size ,description)))
     (fresh-line)
     ,@body))

(defun update (tqdm &key (incf 1) (description (tqdm-description tqdm)) total-count (stream t))
  (declare (type fixnum incf) (type tqdmbar tqdm))
  (when total-count (setf (tqdm-total-count tqdm) total-count))
  (let ((now (get-internal-real-time)))
    (incf (tqdm-count-idx tqdm) incf)
    (setf (tqdm-description tqdm) description)
    (when (>= incf 1)
      (add-ema (tqdm-ema tqdm) (/ (time-since now (tqdm-last-update-time tqdm)) incf)))
    (setf (tqdm-last-update-time tqdm) now)
    (render-progress-bar stream tqdm :animate t))
  incf)

(defun progress-percent (status) (fround (* 100 (/ (tqdm-count-idx status) (tqdm-total-count status)))))

(declaim (ftype (function (tqdmbar) string) render))
(defun render (status)
  "Rendering given status (which is the structure of tqdmbar), render returns the output string."
  (declare (type tqdmbar status))
  (with-output-to-string (bar)
    (let ((spl (- *indent* (length (tqdm-description status)) -1)))
      (write-string (tqdm-description status) bar)
      (dotimes (_ spl) (write-string " " bar))
      (unless (equal (tqdm-description status) "")
        (write-string ":" bar)
        (write-string " " bar)))
    (let* ((n (the fixnum (round (the single-float (progress-percent status)))))
           (r (round (if (>= (/ n 10) 10.0) 10 (/ n 10)))))
      (if (< n 100)
          (write-string " " bar))
      (write-string (write-to-string n) bar)
      (write-string "% |" bar)
      (dotimes (_ r) (write-string *bar-string* bar))
      (dotimes (_ (- 10 r)) (write-string *space-string* bar)))
    (write-string "| " bar)
    (write-string (write-to-string (tqdm-count-idx status)) bar)
    (write-string "/" bar)
    (write-string (write-to-string (tqdm-total-count status)) bar)
    (write-string " [" bar)
    (let* ((now-time (get-universal-time))
           (dif (- now-time (tqdm-creation-time status))))
      (write-string (write-to-string (coerce dif 'single-float)) bar)
      (write-string "s<" bar))
    (let* ((average-sec (get-ema (tqdm-ema status)))
           (total (tqdm-total-count status))
           (remaining (- total (tqdm-count-idx status))))
      (write-string (write-to-string (* remaining average-sec)) bar)
      (write-string "s, " bar)
      (write-string (write-to-string average-sec) bar)
      (write-string "s/it]" bar))))

(defun backward-lines ()
  (write-char #\Return)
  (write-char #\Rubout))

(defun render-progress-bar (stream tqdm &key (animate nil))
  (declare (type TqdmBar tqdm))
  (when (>= (ctx:getenv :DEBUG) 0)
    (if (and animate (tqdm-animate tqdm))
	(backward-lines)
	(fresh-line))
    (format stream (render tqdm))
    nil))
