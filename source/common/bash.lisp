(defpackage :caten/common/bash
  (:use :cl)
  (:export
    #:async-run-with-input))

(in-package :caten/common/bash)
;; Ref: https://solb.io/blog/asynchronize-your-life%3A-shell-commands-10x-faster
;; [TODO] make it run in paralle for beam
(defparameter *service* (uiop:launch-program "bash" :input :stream :output :stream))

(defun %async-launch (command)
  ;; read all streams from the previous command
  (let ((stream (uiop:process-info-output *service*)))
    (loop while (listen stream) do (read-line stream)))
  (write-line command (uiop:process-info-input *service*))
  (force-output (uiop:process-info-input *service*))
  (let* ((string (read-line (uiop:process-info-output *service*)))
         (stream (uiop:process-info-output *service*)))
    (loop while (listen stream) do (setf string (concatenate 'string string '(#\Newline) (read-line stream))))
    string))

(defun async-run-with-input (command input)
  "Return: (values return-code outputs)"
  (declare (type string command))
  (let* ((outputs (%async-launch (format nil "echo ~S | ( ~A ) 2>&1; echo \"RETURNCODE=$?\"" input command)))
         (outputs (cl-ppcre:split #\newline outputs))
         (return-code-p (cl-ppcre:scan "RETURNCODE=" (car (last outputs)) :end (min (length (car (last outputs))) (length "RETURNCODE="))))
         (return-code (when return-code-p (parse-integer (subseq (the string (car (last outputs))) (length "RETURNCODE="))))))    
    (values
     (or return-code 1)
     (with-output-to-string (out)
       (dolist (o (butlast outputs))
         (princ o out) (princ #\newline out))))))
                
      
