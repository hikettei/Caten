(in-package :cl-user)
(defpackage :caten/ajit
  (:use :cl :alexandria :caten/aasm :caten/air :caten/avm :cffi)
  ;; from isl-objects.lisp
  (:export
   #:form
   )
  ;; from scheduler.lisp
  (:export
   #:create-polyhedral-model
   #:auto-schedule)
  ;; from type-relay.lisp
  (:export
   #:run-type-infer)
  )

(in-package :caten/ajit)

(labels ((load-helper ()
	   (restart-case
	       (handler-case
		   (cffi:load-foreign-library
		    '(:default "libisl")
		    :search-path (merge-pathnames
				  #P"usr/"
				  (user-homedir-pathname)))
		 (cffi:load-foreign-library-error (c)
		   (warn "Caten/ajit depends on ISL but could not find the shared library.~% (Recommended) Ensure that the ISL was installed and CFFI is able to find out libisl.dylib:~% - sudo apt install libisl-dev~% - brew install libisl")
		   (error c)))
	     (retry-load-foreign-library ()
	       :report "Try doing cffi:load-foreign-library again."
	       (load-helper)))))
  (load-helper))

