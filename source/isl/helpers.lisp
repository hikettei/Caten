(in-package :caten/isl)

(defun make-isl-sym (&rest things) (intern (apply #'concatenate 'string (mapcar #'string things)) (find-package "CATEN/ISL")))
(defun string-from-symbol (symbol)
  (let ((*package* (find-package "KEYWORD"))
        (*print-readably*)
        (*print-case* :upcase))
    (with-output-to-string (stream)
      (format stream "~S" symbol))))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun swig-lispify (c-name kind)
    (labels ((hyphenate (string)
               (substitute #\- #\_ string))
             (peel-initial-underscores (string)
               (substitute #\% #\_ string :end (position #\_ string :test-not #'char=)))
             (lispify (string)
               (string-upcase
		(hyphenate
		 (peel-initial-underscores string)))))
      (intern
       (ecase kind
	 (constant
          (concatenate 'string "+" (lispify c-name) "+"))
	 (variable
          (concatenate 'string "*" (lispify c-name) "*"))
	 (function
          (concatenate 'string "%" (lispify c-name)))
	 (classname
          (concatenate 'string (lispify c-name) "-CSTRUCT"))
	 (slotname
          (concatenate 'string (lispify c-name) "-SLOT"))
	 (enumname
          (lispify c-name)))
       (find-package "CATEN/ISL"))))
  (defun pkg-config-isl ()
    (let ((output-stream (make-string-output-stream))
          (program+args (list "pkg-config" "isl" "--libs" "--cflags")))
      (format *debug-io* "~&;~{ ~a~}~%" program+args)
      (handler-case
          (progn
            (uiop:run-program program+args
			 :output (make-broadcast-stream output-stream *debug-io*)
			 :error-output output-stream)
	    (return-from pkg-config-isl (cffi-grovel::parse-command-flags (get-output-stream-string output-stream))))
	(error (e)
          (let ((message (format nil "~a~&~%~a~&"
				 e (get-output-stream-string output-stream))))
	    (format *debug-io* "~&; ERROR: ~a" message)
            (format *debug-io* "~&~%; Attempting to continue anyway.~%")
	    nil)))))
  ;; [FixME] doing really weird thing and hacking cffi-grovel
  ;; When I try to link isl on macOS (especially, apple silicon with rosetta2 installed), I have to rm -rf /usr/local/include.
  ;; This is due to arm64/x64 environments are mixed in a single computer.
  ;; This can be avoided by using pkg-config and giving the appropriate LDCONFIG when linking, but cffi-grovel does not allow this.
  (defun cffi-grovel::process-grovel-file (input-file &optional (output-defaults input-file))
    (with-standard-io-syntax
      (let* ((c-file (cffi-grovel::generate-c-file input-file output-defaults))
             (o-file (cffi-grovel::make-o-file-name c-file))
             (exe-file (cffi-grovel::make-exe-file-name c-file))
             (lisp-file (cffi-grovel::tmp-lisp-file-name c-file))
             (inputs (list (cffi-grovel::cc-include-grovel-argument) c-file)))
	(handler-case
            (progn
              ;; at least MKCL wants to separate compile and link
              (cffi-grovel::cc-compile o-file inputs)
              (cffi-grovel::link-executable exe-file `(,o-file ,@(pkg-config-isl))))
          (error (e)
            (cffi-grovel::grovel-error "~a" e)))
	(cffi-grovel::invoke exe-file lisp-file)
	lisp-file))))
