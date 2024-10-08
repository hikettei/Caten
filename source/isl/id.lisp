(in-package :caten/isl)

(define-isl-object identifier
  :free %isl-id-free
  :copy %isl-id-copy
  :list-type identifier-list)

(defmethod print-object ((identifier identifier) stream)
  (print-unreadable-object (identifier stream :type t)
    (write-string (%isl-id-to-str (identifier-handle identifier)) stream)))

(defun make-id-from-str (name)
  (declare (type string name))
  (%make-identifier
   (cffi:with-foreign-string (char* name)
     (%isl-id-alloc (context-handle *context*) char* (cffi:null-pointer)))))
;; If we call this function twice with the same arguments it will create the same result
(defun make-identifier (name &key (no-intern nil))
  (declare (symbol name))
  (%make-identifier
   (cffi:with-foreign-string (char* (if no-intern (format nil "~(~a~)" name) (string-from-symbol name)))
     (%isl-id-alloc (context-handle *context*) char* (cffi:null-pointer)))))

;; Not the same result if we call this function twice
(defun make-gensym-identifier (name)
  (declare (symbol name))
  (%make-identifier
   (cffi:with-foreign-string (char* (string-from-symbol
                                     (gensym
                                      (string-from-symbol name))))
     (%isl-id-alloc (context-handle *context*) char* (cffi:null-pointer)))))

(defun identifier-name (identifier)
  (declare (identifier identifier))
  (let* ((handle (identifier-handle identifier))
         (char* (%isl-id-get-name handle)))
    (values
     (read-from-string
      (cffi:foreign-string-to-lisp char*)))))

(define-isl-function identifier-context %isl-id-get-ctx
    (:give context)
    (:keep identifier))

(defun make-id-list (&rest id-list)
  (declare (type list id-list))
  (let* ((n (length id-list))
	 (id-list (cl:map 'list #'(lambda (x) (make-identifier x :no-intern t)) id-list))
	 (ls (%make-identifier-list (%isl-id-list-alloc (context-handle *context*) n))))
    (loop for id in id-list
	  for nth upfrom 0
	  do (%isl-id-list-add (identifier-list-handle ls) (identifier-handle id)))
    ls))
