(in-package :cl-user)
(defpackage :caten/ajit
  (:use :cl :alexandria :caten/aasm :caten/air :caten/avm :cffi)
  ;; from integer-linear-programming.lisp
  (:export
   #:form
   
   ))
(in-package :caten/ajit)

;; Loading ISL
(defvar *src-dir* (asdf:component-pathname (asdf:find-system "caten")))
(labels ((load-helper ()
	   (restart-case
	       (handler-case
		   (cffi:load-foreign-library
		    '(:default "libisl")
		    :search-path (merge-pathnames
				  #P"usr/"
				  (user-homedir-pathname)))
		 (cffi:load-foreign-library-error (c)
		   (warn "Caten could not find libisl the shared library.~% (Recommended) Ensure that the ISL was installed and CFFI is able to find out libisl.dylib:~% - sudo apt install libisl-dev~% - brew install libisl")
		   (error c)))
	     (retry-load-foreign-library ()
	       :report "Try doing cffi:load-foreign-library again."
	       (load-helper))
	     (build-from-source-and-try ()
	       :report "Building ISL from the source, after that, Caten will try to load the shared library again."
	       (let* ((cmd
			(format
			 nil
			 "~a ~a ~a ~a ~a"
			 "cd"
			 (namestring
			  (merge-pathnames
			   #P"isl/"
			   *src-dir*))
			 " && sh ./autogen.sh"
			 " && CFLAGS=\"$(pkg-config --libs --cflags gmp)\" ./configure --prefix $HOME/usr"
			 " && make -j 4 && make install"))
		      (info
			(progn
			  (warn "Caten: Building ISL from the source with:~%    ~a~%Ensure that all dependencies for ISL are installed." cmd)
			  (uiop:launch-program
			   cmd
			   :error-output :stream)))
		      (error-output
			(uiop:process-info-error-output info)))
		 (unless (zerop (uiop:wait-process info))
		   (error
		    "Caten: Building from source was failed due to:~%~a~%Ensure that all dependencies for ISL are installed on your device.~% Or consider installing libisl manually."
		    (alexandria:read-stream-content-into-string error-output)))
		 (load-helper))))))
  (load-helper))
