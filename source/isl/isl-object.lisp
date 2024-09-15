(in-package :caten/isl)
;; Codes are inspired from https://github.com/marcoheisig/cl-isl/blob/main/code/isl-object.lisp
(defgeneric copy (isl-object))
(defstruct (isl-object (:copier nil))
  (handle (alexandria:required-argument :handle)
   :type cffi:foreign-pointer))

(defmethod print-object ((isl-object isl-object) stream)
  (print-unreadable-object (isl-object stream :type t)
    (format stream "{~X}" (cffi:pointer-address (isl-object-handle isl-object)))))

(defmacro isl-object-%copy (object-name)
  "Returns the name of the function for creating a fresh copy of the
handle of the ISL object denoted by OBJECT-NAME."
  `(getf (symbol-plist ,object-name) '%copy))

(defmacro isl-object-%make (object-name)
  "Returns the name of the function for turning a suitable handle into an
ISL object of type OBJECT-NAME."
  `(getf (symbol-plist ,object-name) '%make))

(defmacro isl-object-%free (object-name)
  "Returns the name of the function for freeing a handle of an ISL object
of type OBJECT-NAME."
  `(getf (symbol-plist ,object-name) '%free))

(defun isl-object-name-p (x)
  (and (symbolp x)
       (not (null (isl-object-%make x)))))

;;; This hash table is used in each ISL object constructor to ensure that
;;; each handle has exactly one corresponding wrapper object.
(defvar *isl-object-table* (trivial-garbage:make-weak-hash-table :weakness :value))
(defmacro with-isl-context (&body body)
  "Assumes under the body is a thread-safe"
  `(let* ((*isl-object-table* (trivial-garbage:make-weak-hash-table :weakness :value))
	  (*context* (make-context)))
     ,@body))

(defmacro define-isl-object
    (name &key (abstract nil)
            (list-type nil)
            (superclass 'isl-object)
            (from-str nil)
            ((:free %free) (isl-object-%free superclass))
            ((:copy %copy) (isl-object-%copy superclass)))
  (let ((predicate (make-isl-sym name (if (find #\- (string name)) "-P" "P")))
        (%make (make-isl-sym "%MAKE-" name))
        (%%make (make-isl-sym "%%MAKE-" name))
        (%from-str-us (make-isl-sym name "-FROM-STR"))
        (%from-str-library (make-isl-sym "%ISL-" name "-READ-FROM-STR")))
    (setf (isl-object-%copy name) %copy)
    (setf (isl-object-%make name) %make)
    (setf (isl-object-%free name) %free)
    `(progn
       (defstruct (,name (:include ,superclass)
                         (:predicate ,predicate)
                         (:copier nil)
                         (:constructor ,%%make (handle))))
       (declaim (ftype (function (cffi:foreign-pointer) (values ,name &optional)) ,%make))
       ,@(when list-type
           `((define-isl-object-list ,list-type ,name)))
       ,@(when from-str
           `((define-isl-function ,%from-str-us ,%from-str-library
               (:give ,name)
               (:parm context *context*)
               (:keep string))))
       ,@(unless abstract
           `((defun ,%make (handle)
               (when (cffi:null-pointer-p handle) (isl-error))
               (values
                (alexandria:ensure-gethash
                 (cffi:pointer-address handle)
		 *isl-object-table*
                 (trivial-garbage:finalize (,%%make handle)
                                           (lambda ()
					     (remhash (cffi:pointer-address handle) *isl-object-table*)
					     (,%free handle))))))))
       ,@(when %copy
           `((defmethod copy ((,name ,name))
               (,%make (,%copy (isl-object-handle ,name)))))))))

(defmacro define-isl-object-list (name element-type)
  (let* ((%name
           (case name
             (identifier-list "ID-LIST")
             (value-list "VAL-LIST")
             (affine-list "AFF-LIST")
             (otherwise name)))
         (%element-type
           (case element-type
             (identifier "ID")
             (value "VAL")
             (affine "AFF")
             (otherwise element-type)))
         (%free (make-isl-sym "%ISL-" %name "-FREE"))
         (%copy (make-isl-sym "%ISL-" %name "-COPY"))
         (%size (make-isl-sym "%ISL-" %name "-SIZE"))
         (%get-elt (make-isl-sym "%ISL-" %name "-GET-" %element-type))
         (%set-elt (make-isl-sym "%ISL-" %name "-SET-" %element-type))
         (%to-str (make-isl-sym "%ISL-" %name "-TO-STR"))
         (size (make-isl-sym name "-SIZE"))
         (get-elt (make-isl-sym name "-ELT"))
         (set-elt (make-isl-sym "SET-" name "-ELT"))
         (elements (make-isl-sym name "-ELEMENTS")))
    `(progn
       (define-isl-object ,name
         :free ,%free
         :copy ,%copy
         :from-str nil)
       (defmethod print-object ((,name ,name) stream)
         (print-unreadable-object (,name stream :type t)
           (write-string (,%to-str (isl-object-handle ,name)) stream)))
       (define-isl-function ,size ,%size
         (:give size)
         (:keep ,name))
       (define-isl-function ,get-elt ,%get-elt
         (:give ,element-type)
         (:keep ,name)
         (:keep unsigned-byte))
       (define-isl-function ,set-elt ,%set-elt
         (:give ,name)
         (:take ,name)
         (:keep unsigned-byte)
         (:take ,element-type))
       (defun ,elements (,name)
         (declare (type ,name ,name))
         (loop for index below (,size ,name)
               collect
               (,get-elt ,name index))))))
