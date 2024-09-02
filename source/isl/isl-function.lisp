(in-package :caten/isl)
;; Codes are inspired from https://github.com/marcoheisig/cl-isl/blob/main/code/isl-function.lisp
;; but added a little modification:
;;  - Removed swig dependencies
(defgeneric isl-fn-name (isl-fn))
(defgeneric isl-fn-primitive (isl-fn))
(defgeneric isl-fn-result (isl-fn))
(defgeneric isl-fn-args (isl-fn))
(defgeneric isl-name (isl-result-or-arg))
(defgeneric isl-type (isl-result-or-arg))

(defclass isl-result-or-arg ()
  ((%name
    :initarg :name
    :initform (alexandria:required-argument :name)
    :reader isl-name)
   (%type
    :initarg :type
    :initform (alexandria:required-argument :type)
    :reader isl-type)))
;;; Implicit arguments appear as arguments to the primitive C function, but
;;; not in the lambda list of the Lisp function we generate.
(defclass isl-implicit-arg (isl-result-or-arg) ())
(defun isl-implicit-arg-p (x) (typep x 'isl-implicit-arg))
;;; A function result.  The first result is the return value of the
;;; primitive.  All further results are returned by the primitive via
;;; pointers to handles.
(defclass isl-give (isl-implicit-arg) ())
(defun isl-give-p (x) (typep x 'isl-give))
;;; The primary result of primitives that return nothing.
(defclass isl-null (isl-implicit-arg) ())
(defun isl-null-p (x) (typep x 'isl-null))
;;; A reference to a Lisp special variable (or parameter).  The name 'parm'
;;; was chosen so that all qualifiers have four letters, which makes the
;;; source code align nicely.
(defclass isl-parm (isl-implicit-arg) ())
(defun isl-parm-p (x) (typep x 'isl-parm))
;;; An regular argument.
(defclass isl-keep (isl-result-or-arg) ())
(defun isl-keep-p (x) (typep x 'isl-keep))
;;; An argument that is automatically free'd by the primitive.
(defclass isl-take (isl-result-or-arg) ())

(defun isl-take-p (x) (typep x 'isl-take))
(defmethod print-object ((isl-result-or-arg isl-result-or-arg) stream)
  (print-unreadable-object (isl-result-or-arg stream :type t)
    (format stream "~@<~@{~S ~S~^ ~_~}~:>"
            :name (isl-name isl-result-or-arg)
            :type (isl-type isl-result-or-arg))))

(defvar *generate-arg-name*)
(defun generate-arg-name (type) (funcall *generate-arg-name* type))

(defun make-arg-name-generator ()
  (let ((table (make-hash-table :test #'equal)))
    (lambda (type)
      (let* ((prefix
               (etypecase type
                 ;; Symbols
                 (symbol (symbol-name type))
                 ;; Types like (unsigned-byte 32).
                 ((cons symbol (cons integer null))
                  (format nil "~A-~D"
                          (symbol-name (first type))
                          (second type)))
                 (t "ARG")))
             (count (incf (gethash prefix table -1)))
             (suffix (if (zerop count) "" (format nil "~D" count))))
        (make-isl-sym prefix suffix)))))

(defun parse-isl-results-and-args (arg-specs)
  (let ((*generate-arg-name* (make-arg-name-generator)))
    (mapcar #'parse-isl-result-or-arg arg-specs)))

(defun parse-isl-result-or-arg (spec)
  (destructuring-bind (qualifier type &optional (name (generate-arg-name type))) spec
    (make-instance
        (ecase qualifier
          (:null 'isl-null)
          (:give 'isl-give)
          (:keep 'isl-keep)
          (:take 'isl-take)
          (:parm 'isl-parm))
      :type type
      :name name)))

(defclass isl-fn ()
  (;; The name of this function.
   (%name
    :initarg :name
    :initform (alexandria:required-argument :name)
    :type symbol
    :reader isl-fn-name)
   ;; The name of the CFFI function that underlies this function.
   (%primitive
    :initarg :primitive
    :initform (alexandria:required-argument :primitive)
    :type symbol
    :reader isl-fn-primitive)
   (%result
    :initarg :result
    :initform (alexandria:required-argument :result)
    :type isl-give
    :reader isl-fn-result)
   (%args
    :initarg :args
    :type list
    :reader isl-fn-args)))

(defmethod print-object ((isl-fn isl-fn) stream)
  (print-unreadable-object (isl-fn stream :type t)
    (format stream "~@<~@{~S ~S~^ ~_~}~:>"
            :name (isl-fn-name isl-fn)
            :primitive (isl-fn-primitive isl-fn)
            :result (isl-fn-result isl-fn)
            :args (isl-fn-args isl-fn))))

(defvar *isl-fns* (make-hash-table :test #'eq))
(defmacro isl-fn (name)
  `(values (gethash ,name *isl-fns*)))

(defmethod initialize-instance :after ((isl-fn isl-fn) &key &allow-other-keys)
  (setf (isl-fn (isl-fn-name isl-fn))
        isl-fn))

(defun infer-result-wrapper (result-type)
  (cond ((isl-object-name-p result-type)
         (isl-object-%make result-type))
        ((eql result-type 'boolean)
         'lispify-isl-bool)
        ((eql result-type 'size)
         'lispify-isl-size)
        (t 'identity)))

(defmacro define-isl-function (name primitive &rest results-and-args)
  (check-type name symbol)
  (check-type primitive symbol)
  (with-accessors ((name isl-fn-name)
                   (primitive isl-fn-primitive)
                   (result isl-fn-result)
                   (args isl-fn-args))
      (destructuring-bind (result &rest args)
          (parse-isl-results-and-args results-and-args)
        (make-instance 'isl-fn
          :name name
          :primitive primitive
          :result result
          :args args))
    (let* ((explicit-args (remove-if #'isl-implicit-arg-p args))
           (extra-results (remove-if-not #'isl-give-p args))
           (ftype `(function ,(mapcar #'isl-type explicit-args)
                             (values ,@(mapcar #'isl-type (list* result extra-results)) &optional))))
      `(progn
         (declaim (ftype ,ftype ,name))
         (defun ,name ,(mapcar #'isl-name explicit-args)
           ,@(loop for arg in explicit-args
                   collect `(declare (type ,(isl-type arg) ,(isl-name arg))))
           (cffi:with-foreign-objects
               ,(loop for extra-result in extra-results
                      collect `(,(isl-name extra-result) :pointer))
             (values
              (,(infer-result-wrapper (isl-type result))
               (,primitive
                ,@(loop for arg in args
                        for name = (isl-name arg)
                        for type = (isl-type arg)
                        collect
                        (typecase arg
                          (isl-keep
                           (if (isl-object-name-p type)
                               `(isl-object-handle ,name)
                               name))
                          (isl-parm
                           (if (isl-object-name-p type)
                               `(isl-object-handle (the ,type ,name))
                               name))
                          (isl-take
                           (assert (isl-object-name-p type))
                           `(,(isl-object-%copy type) (isl-object-handle ,name)))
                          (isl-null
                           (error "Arguments with :null qualifier are not allowed."))
                          (otherwise name)))))
              ,@(loop for extra-result in extra-results
                      collect
                      `(,(infer-result-wrapper (isl-type extra-result))
                        (cffi:mem-ref ,(isl-name extra-result) :pointer))))))
         ;;(define-compiler-macro ,name (&whole whole ,@(mapcar #'isl-name explicit-args))
         ;;  (declare (ignore ,@(mapcar #'isl-name explicit-args)))
         ;;  (optimize-isl-function-call whole))
	 ))))

(defun constructor-form-p (form isl-object-name)
  (and (consp form)
       (consp (cdr form))
       (null (cddr form))
       (eql (first form)
            (isl-object-%make isl-object-name))))

(defun creation-form-p (form isl-object-name)
  (and (consp form)
       (let ((fn (isl-fn (first form))))
         (and fn
              (= (length (rest form))
                 (length (remove-if #'isl-implicit-arg-p (isl-fn-args fn))))
              (eq (isl-type (isl-fn-result fn)) isl-object-name)))))

(defun optimize-isl-function-call (whole &key (recursive nil))
  (destructuring-bind (name &rest forms) whole
    (with-accessors ((name isl-fn-name)
                     (primitive isl-fn-primitive)
                     (result isl-fn-result)
                     (args isl-fn-args))
        (isl-fn name)
      (let* ((worth-expanding recursive)
             (bindings '())
             (foreign-objects '())
             (cleanup '())
             (expanded-arguments
               (loop for arg in args
                     for name = (isl-name arg)
                     for type = (isl-type arg)
                     collect
                     (etypecase arg
                       (isl-parm `(isl-object-handle (the ,type ,name)))
                       (isl-give
                        (let ((g (gensym)))
                          (push `(,g :pointer) foreign-objects)
                          (push `(,(isl-object-%free type) (cffi:mem-ref ,g :pointer))
                                cleanup)
                          g))
                       (isl-take
                        (let ((form (pop forms)))
                          (cond
                            ((constructor-form-p form type)
                             (setf worth-expanding t)
                             (second form))
                            ((creation-form-p form type)
                             (setf worth-expanding t)
                             (optimize-isl-function-call form :recursive t))
                            (t
                             `(,(isl-object-%copy type)
                               (isl-object-handle (the ,type ,form)))))))
                       (isl-keep
                        (let ((form (pop forms)))
                          (cond
                            ((constructor-form-p form type)
                             (setf worth-expanding t)
                             (let ((handle (gensym)))
                               (push `(,handle ,(second form)) bindings)
                               (push `(,(isl-object-%free type) ,handle) cleanup)
                               handle))
                            ((creation-form-p form type)
                             (setf worth-expanding t)
                             (let ((handle (gensym)))
                               (push `(,handle ,(optimize-isl-function-call form :recursive t)) bindings)
                               (push `(,(isl-object-%free type) ,handle) cleanup)
                               handle))
                            ((isl-object-name-p type)
                             `(isl-object-handle (the ,type ,form)))
                            (t
                             `(the ,type ,form))))))))
             (expansion `(,primitive ,@expanded-arguments)))
        (when cleanup
          (setf expansion `(unwind-protect ,expansion ,@(reverse cleanup))))
        (when foreign-objects
          (setf expansion `(cffi:with-foreign-objects ,(reverse foreign-objects) ,expansion)))
        (when bindings
          (setf expansion `(let ,(reverse bindings) ,expansion)))
        (unless recursive
          (let ((result-wrapper (infer-result-wrapper (isl-type result))))
            (unless (eql result-wrapper 'identity)
              (setf expansion `(,result-wrapper ,expansion)))))
        (if (not worth-expanding) whole expansion)))))
