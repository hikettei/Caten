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
  (defun cffi-grovel::process-wrapper-file (input-file
                               &key
				 (output-defaults (make-pathname :defaults input-file :type "processed"))
				 lib-soname)
    (with-standard-io-syntax
      (multiple-value-bind (c-file lisp-forms)
          (cffi-grovel::generate-c-lib-file input-file output-defaults)
	(let ((lib-file (cffi-grovel::make-so-file-name (cffi-grovel::make-soname lib-soname output-defaults)))
              (o-file (cffi-grovel::make-o-file-name output-defaults "__wrapper")))
          (cffi-grovel::cc-compile o-file (list (cffi-grovel::cc-include-grovel-argument) c-file))
          (cffi-grovel::link-shared-library lib-file `(,o-file ,@(pkg-config-isl)))
          ;; FIXME: hardcoded library path.
          (values (cffi-grovel::generate-bindings-file lib-file lib-soname lisp-forms output-defaults) lib-file))))))
