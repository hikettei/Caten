(cl:in-package :cl-user)
(defpackage :caten/isl
  (:use :cl :cffi)
  (:export

   ))

(in-package :caten/isl)

(labels ((load-helper ()
	   (restart-case
	       (handler-case
		   (cffi:load-foreign-library
		    '(:default "libisl")
		    :search-path (merge-pathnames
				  #P"usr/"
				  (user-homedir-pathname)))
		 (cffi:load-foreign-library-error (c)
		   (warn "Caten/ISL depends on ISL but could not find the shared library.~% (Recommended) Ensure that the ISL was installed and CFFI is able to find out libisl.dylib:~% - sudo apt install libisl-dev~% - brew install libisl")
		   (error c)))
	     (retry-load-foreign-library ()
	       :report "Try doing cffi:load-foreign-library again."
	       (load-helper)))))
  (load-helper))

