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
       (find-package "CATEN/ISL")))))
