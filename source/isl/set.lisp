(in-package :caten/isl)

(define-isl-object set
  :free %isl-set-free
  :copy %isl-set-copy
  :list-type set-list
  :from-str t)

(defmethod print-object ((value set) stream)
  (print-unreadable-object (value stream :type t)
    (write-string (%isl-set-to-str (set-handle value)) stream)))

;; Creation
(define-isl-function set-empty %isl-set-empty
  (:give set)
  (:take space))

(define-isl-function set-universe %isl-set-universe
  (:give set)
  (:take space))

;; Conversion
(define-isl-function basic-set-set %isl-set-from-basic-set
  (:give set)
  (:take basic-set))

(define-isl-function set-get-space %isl-set-get-space
  (:give space)
  (:take set))

(define-isl-function set-from-multi-aff %isl-set-from-multi-aff
  (:give set)
  (:take multi-aff))

(defun set-dim-max (set dim)
  (%make-set (%isl-set-dim-max (set-handle (copy set)) dim)))

(defun set-dim (set type)
  (%isl-set-dim (set-handle set) type))

(defun set-project-out (set type first n)
  (%make-set (%isl-set-project-out (set-handle (copy set)) type first n)))

(define-isl-function set-subtract %isl-set-subtract
  (:give set)
  (:take set)
  (:take set))

(define-isl-function set-add-constraint %isl-set-add-constraint
  (:give set)
  (:take set)
  (:take constraint))

(defun set-drop-constraints-involving-dims (set type first n)
  (%make-set
   (%isl-set-drop-constraints-involving-dims (set-handle (copy set)) type first n)))

(define-isl-function set-set-tuple-id %isl-set-set-tuple-id
  (:give set)
  (:take set)
  (:take identifier))

(define-isl-function set-get-basic-set-list %isl-set-get-basic-set-list
  (:give basic-set-list)
  (:keep set))

(defun set-list-n-set (lst)
  (%isl-set-list-n-set (set-list-handle lst)))

(defun set-list-get-at (lst n)
  (%make-set (%isl-set-list-get-at (set-list-handle lst) n)))

(defun basic-set-list-get-at (lst n)
  (%make-basic-set (%isl-basic-set-list-get-at (basic-set-list-handle lst) n)))

(defun basic-set-dim (bset dim)
  (%isl-basic-set-dim (basic-set-handle (copy bset)) dim))

(defun set-get-dim-id (set type pos)
  (%make-identifier (%isl-set-get-dim-id (set-handle set) type pos)))
  
