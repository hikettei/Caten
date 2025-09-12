(in-package :caten/isl)

(deftype size () '(and unsigned-byte fixnum))
(deftype ast-expr-type () `(member ,@(remove :ast-expr-error (cffi:foreign-enum-keyword-list 'isl-ast-expr-type))))
(deftype dim-type () `(member ,@(cffi:foreign-enum-keyword-list 'isl-dim-type)))
(deftype value-designator () '(or rational value))
;;;; Context
(define-isl-object context :free identity)
(defun make-context ()
  (let ((handle (%isl-ctx-alloc)))
    (%isl-options-set-on-error handle +isl-on-error-continue+)
    (%make-context handle)))

(defparameter *context* (make-context))

(defun isl-error ()
  (error "isl yields an error: ~a"
         (cffi:foreign-string-to-lisp
          (%isl-ctx-last-error-msg (isl-object-handle *context*))
          :encoding :ascii)))

(defun lispify-isl-bool (isl-bool)
  (ecase isl-bool
    (:bool-true t)
    (:bool-false nil)
    (:bool-error (isl-error))))

(defun lispify-isl-size (isl-size)
  (if (= isl-size +isl-size-error+)
      (isl-error)
      isl-size))
;;;; Identifier
(define-isl-object identifier
  :free %isl-id-free
  :copy %isl-id-copy
  :list-type identifier-list)

(defmethod print-object ((identifier identifier) stream)
  (print-unreadable-object (identifier stream :type t)
    (write-string (%isl-id-to-str (identifier-handle identifier)) stream)))
;;;; Value
(define-isl-object value
  :free %isl-val-free
  :copy %isl-val-copy
  :list-type value-list)

(defmethod print-object ((value value) stream)
  (print-unreadable-object (value stream :type t)
    (write-string (%isl-val-to-str (value-handle value)) stream)))
;;;; Space
(define-isl-object space
  :free %isl-space-free
  :copy %isl-space-copy)

(defmethod print-object ((value space) stream)
  (print-unreadable-object (value stream :type t)
    (write-string (%isl-space-to-str (space-handle value)) stream)))
;;;; MultiAff
(define-isl-object multi-aff
  :free %isl-multi-aff-free
  :copy %isl-multi-aff-copy
  :from-str t)
;;;; LocalSpace
(define-isl-object local-space
  :free %isl-local-space-free
  :copy %isl-local-space-copy)

(defmethod print-object ((local-space local-space) stream)
  (print-unreadable-object (local-space stream :type t)
    (write-string
     (%isl-space-to-str
      (space-handle (local-space-space local-space))) stream)))
;;;; Constraint
(define-isl-object constraint
  :abstract t
  :free %isl-constraint-free
  :copy %isl-constraint-copy
  :list-type constraint-list)

(export 'constraint-list-get)
(defun constraint-list-get (constraint-list n)
  (let ((constraint (%isl-constraint-list-get-at (constraint-list-handle constraint-list) n)))
    (if (eql :bool-true (%isl-constraint-is-equality constraint))
        (%make-equality-constraint constraint)
        (%make-inequality-constraint constraint))))

(defmethod print-object ((constraint constraint) stream)
  (print-unreadable-object (constraint stream :type t)
    (let ((aff (%isl-constraint-get-aff (constraint-handle constraint))))
      (unless (cffi:null-pointer-p aff)
        (unwind-protect (write-string (%isl-aff-to-str aff) stream)
          (%isl-aff-free aff))))))

(define-isl-object equality-constraint
  :superclass constraint)

(define-isl-object inequality-constraint
  :superclass constraint)
;;;; basic-set
(define-isl-object basic-set
  :free %isl-basic-set-free
  :copy %isl-basic-set-copy
  :list-type basic-set-list
  :from-str t)

(defmethod print-object ((value basic-set) stream)
  (print-unreadable-object (value stream :type t)
    (write-string (%isl-basic-set-to-str (basic-set-handle value)) stream)))
;;;; set
(define-isl-object set
  :free %isl-set-free
  :copy %isl-set-copy
  :list-type set-list
  :from-str t)

(defmethod print-object ((value set) stream)
  (print-unreadable-object (value stream :type t)
    (write-string (%isl-set-to-str (set-handle value)) stream)))
;;;; union-set
(define-isl-object union-set
  :free %isl-union-set-free
  :copy %isl-union-set-copy
  :list-type union-set-list
  :from-str t)

(defmethod print-object ((value union-set) stream)
  (print-unreadable-object (value stream :type t)
    (write-string (%isl-union-set-to-str (union-set-handle value)) stream)))
;;;; basic-map 
(define-isl-object basic-map
  :free %isl-basic-map-free
  :copy %isl-basic-map-copy
  :list-type basic-map-list
  :from-str t)

(defmethod print-object ((value basic-map) stream)
  (print-unreadable-object (value stream :type t)
    (write-string (%isl-basic-map-to-str (basic-map-handle value)) stream)))
;;;; map
(define-isl-object map
  :free %isl-map-free
  :copy %isl-map-copy
  :list-type map-list
  :from-str t)

(defmethod print-object ((value map) stream)
  (print-unreadable-object (value stream :type t)
    (write-string (%isl-map-to-str (map-handle value)) stream)))
;;;; union-map
(define-isl-object union-map
  :free %isl-union-map-free
  :copy %isl-union-map-copy
  :list-type union-map-list
  :from-str t)

(defmethod print-object ((value union-map) stream)
  (print-unreadable-object (value stream :type t)
    (write-string (%isl-union-map-to-str (union-map-handle value)) stream)))
;;;; schedule-constraints
(define-isl-object schedule-constraints
  :free %isl-schedule-constraints-free
  :copy %isl-schedule-constraints-copy
  :from-str t)

(defmethod print-object ((value schedule-constraints) stream)
  (print-unreadable-object (value stream :type t)
    (write-string (%isl-schedule-constraints-to-str (schedule-constraints-handle value)) stream)))
;;;; schedule
(define-isl-object schedule
  :free %isl-schedule-free
  :copy %isl-schedule-copy)

(defmethod print-object ((value schedule) stream)
  (print-unreadable-object (value stream :type t)
    (write-string (%isl-schedule-to-str (schedule-handle value)) stream)))
;;;; union-access

(define-isl-object union-access-info
  :free %isl-union-access-info-free
  :copy %isl-union-access-info-copy)

(define-isl-object union-flow
  :free %isl-union-flow-free
  :copy %isl-union-flow-copy)
;;;; mupa
(define-isl-object multi-union-pw-aff
  :free %isl-multi-union-pw-aff-free
  :copy %isl-multi-union-pw-aff-copy
  :from-str t)

(define-isl-object union-pw-aff
  :free %isl-union-pw-aff-free
  :copy %isl-union-pw-aff-copy
  :list-type union-pw-aff-list)

(define-isl-object union-pw-multi-aff
  :free %isl-union-pw-multi-aff-free
  :copy %isl-union-pw-multi-aff-copy
  :list-type union-pw-multi-aff-list
  :from-str t)

(define-isl-object multi-val
  :free %isl-multi-val-free
  :copy %isl-multi-val-copy)

(define-isl-object pw-aff
  :free %isl-pw-aff-free
  :copy %isl-pw-aff-copy
  :list-type pw-aff-list)

(define-isl-object aff
  :free %isl-aff-free
  :copy %isl-aff-copy
  :list-type affine-list
  :from-str t)

(define-isl-object pw-multi-aff
  :free %isl-pw-multi-aff-free
  :copy %isl-pw-multi-aff-copy
  :from-str t)

(defmethod print-object ((value multi-union-pw-aff) stream)
  (print-unreadable-object (value stream :type t)
    (write-string (%isl-multi-union-pw-aff-to-str (multi-union-pw-aff-handle value)) stream)))

(defmethod print-object ((value union-pw-aff) stream)
  (print-unreadable-object (value stream :type t)
    (write-string (%isl-union-pw-aff-to-str (union-pw-aff-handle value)) stream)))

(defmethod print-object ((value multi-val) stream)
  (print-unreadable-object (value stream :type t)
    (write-string (%isl-multi-val-to-str (multi-val-handle value)) stream)))

(defmethod print-object ((value pw-aff) stream)
  (print-unreadable-object (value stream :type t)
    (write-string (%isl-pw-aff-to-str (pw-aff-handle value)) stream)))

(defmethod print-object ((value aff) stream)
  (print-unreadable-object (value stream :type t)
    (write-string (%isl-aff-to-str (aff-handle value)) stream)))

(defmethod print-object ((value union-pw-multi-aff) stream)
  (print-unreadable-object (value stream :type t)
    (write-string (%isl-union-pw-multi-aff-to-str (union-pw-multi-aff-handle value)) stream)))

(defmethod print-object ((value pw-multi-aff) stream)
  (print-unreadable-object (value stream :type t)
    (write-string (%isl-pw-multi-aff-to-str (pw-multi-aff-handle value)) stream)))
;;;; astexpr
(define-isl-object ast-expr
  :free %isl-ast-expr-free
  :copy %isl-ast-expr-copy
  :list-type ast-expr-list
  :abstract t)

(defmethod print-object ((ast-expr ast-expr) stream)
  (print-unreadable-object (ast-expr stream :type t)
    (write-string (%isl-ast-expr-to-str (ast-expr-handle ast-expr)) stream)))

(define-isl-object op-expr
  :superclass ast-expr
  :abstract t)

(define-isl-object id-expr
  :superclass ast-expr)

(define-isl-object int-expr
  :superclass ast-expr)

(define-isl-object op-and
  :superclass op-expr)

(define-isl-object op-and-then
  :superclass op-expr)

(define-isl-object op-or
  :superclass op-expr)

(define-isl-object op-or-else
  :superclass op-expr)

(define-isl-object op-max
  :superclass op-expr)

(define-isl-object op-min
  :superclass op-expr)

(define-isl-object op-minus
  :superclass op-expr)

(define-isl-object op-add
  :superclass op-expr)

(define-isl-object op-sub
  :superclass op-expr)

(define-isl-object op-mul
  :superclass op-expr)

(define-isl-object op-div
  :superclass op-expr)

(define-isl-object op-fdiv-q
  :superclass op-expr)

(define-isl-object op-pdiv-q
  :superclass op-expr)

(define-isl-object op-pdiv-r
  :superclass op-expr)

(define-isl-object op-zdiv-r
  :superclass op-expr)

(define-isl-object op-cond
  :superclass op-expr)

(define-isl-object op-select
  :superclass op-expr)

(define-isl-object op-eq
  :superclass op-expr)

(define-isl-object op-le
  :superclass op-expr)

(define-isl-object op-lt
  :superclass op-expr)

(define-isl-object op-ge
  :superclass op-expr)

(define-isl-object op-gt
  :superclass op-expr)

(define-isl-object op-call
  :superclass op-expr)

(define-isl-object op-access
  :superclass op-expr)

(define-isl-object op-member
  :superclass op-expr)

(define-isl-object op-address-of
  :superclass op-expr)
;;;; ast-node
(define-isl-object ast-node
  :abstract t
  :free %isl-ast-node-free
  :copy %isl-ast-node-copy
  :list-type ast-node-list)

(defmethod print-object ((ast ast-node) stream)
  (print-unreadable-object (ast stream :type t)
    (write-string (%isl-ast-node-to-str (ast-node-handle ast)) stream)))

(define-isl-object for-node
  :superclass ast-node)

(define-isl-object if-node
  :superclass ast-node)

(define-isl-object block-node
  :superclass ast-node)

(define-isl-object mark-node
  :superclass ast-node)

(define-isl-object user-node
  :superclass ast-node)
;;;; ast-build
(define-isl-object ast-build
  :free %isl-ast-build-free
  :copy %isl-ast-build-copy)
;;;; schedule-node
(define-isl-object schedule-node
  :abstract t
  :free %isl-schedule-node-free
  :copy %isl-schedule-node-copy)

(macrolet ((def (name)
	     `(progn
		(export ',name)
		(define-isl-object ,name :superclass schedule-node)
		(defmethod print-object ((value ,name) stream)
		  (print-unreadable-object (value stream :type t)
		    (write-string (%isl-schedule-node-to-str (isl-object-handle value)) stream))))))
  (def schedule-node-leaf)
  (def schedule-node-filter)
  (def schedule-node-sequence)
  (def schedule-node-band)
  (def schedule-node-domain)
  (def schedule-node-expansion)
  (def schedule-node-extension)
  (def schedule-node-mark)
  (def schedule-node-set)
  (def schedule-node-context)
  (def schedule-node-guard)
  (def schedule-node-error))
;;;; printer
(define-isl-object isl-printer :free %isl-printer-free)
;;;; matrix
(define-isl-object mat
  :free %isl-mat-free
  :copy %isl-mat-copy)

(defmethod print-object ((mat mat) stream)
  (print-unreadable-object (mat stream :type t)
    (format stream ":rows ~a :cols ~a~%~a" (%isl-mat-rows (mat-handle mat)) (%isl-mat-cols (mat-handle mat))
            (with-output-to-string (out)
              (dotimes (i (%isl-mat-rows (mat-handle mat)))
                (format out "    ")
                (dotimes (j (%isl-mat-cols (mat-handle mat)))
                  (format out "~a " (mat-ref mat i j)))
                (format out "~%"))))))
