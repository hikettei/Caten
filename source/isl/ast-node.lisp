(in-package :caten/isl)

(define-isl-object ast-node
  :abstract t
  :free %isl-ast-node-free
  :copy %isl-ast-node-copy
  :list-type ast-node-list)

(defmethod print-object ((ast ast-node) stream)
  (print-unreadable-object (ast stream :type t)
    (write-string (%isl-ast-node-to-str (ast-node-handle ast)) stream)))

;; Print the node in the form of C code. Much prettier than "default" print
(defun pretty-print-node (node)
  (print (%isl-ast-node-to-C-str (isl-object-handle node))))

(define-isl-function node-get-type %isl-ast-node-get-type
  (:give ast-expr-type)
  (:keep ast-node))

;; FOR NODE

(define-isl-object for-node
  :superclass ast-node)

(macrolet ((def (name impl)
             `(define-isl-function ,name ,impl
                (:give ast-expr)
                (:keep for-node))))
  (def for-node-get-iterator %isl-ast-node-for-get-iterator)
  (def for-node-get-init %isl-ast-node-for-get-init)
  (def for-node-get-cond %isl-ast-node-for-get-cond)
  (def for-node-get-inc %isl-ast-node-for-get-inc))

(define-isl-function for-node-get-body %isl-ast-node-for-get-body
  (:give ast-node)
  (:keep for-node))

;; IF NODE

(define-isl-object if-node
  :superclass ast-node)

(macrolet ((def (name impl)
             `(define-isl-function ,name ,impl
                (:give ast-expr)
                (:keep if-node))))
  (def if-node-get-cond %isl-ast-node-if-get-cond)
  (def if-node-get-then %isl-ast-node-if-get-then)
  (def if-node-get-else %isl-ast-node-if-get-else))

(define-isl-function if-node-has-else %isl-ast-node-if-has-else
  (:give boolean)
  (:keep ast-node))

;; BLOCK NODE - a sequence of instruction

(define-isl-object block-node
  :superclass ast-node)

(define-isl-function block-node-getlist %isl-ast-node-block-get-children
  (:give ast-node-list)
  (:keep block-node))

;; MARK NODE

(define-isl-object mark-node
  :superclass ast-node)

;; USER NODE

(define-isl-object user-node
  :superclass ast-node)

(define-isl-function user-node-get-expr %isl-ast-node-user-get-expr
  (:give ast-expr)
  (:keep user-node))

(define-isl-function user-get-expr %isl-ast-node-user-get-expr
  (:give ast-expr)
  (:keep ast-node))

;; Creation of which node it is based on type

(defun %make-ast-node (handle)
  (ecase (%isl-ast-node-get-type handle)
    (:ast-expr-error (isl-error))
    (:ast-node-for (%make-for-node handle))
    (:ast-node-if (%make-if-node handle))
    (:ast-node-block (%make-block-node handle))
    (:ast-node-mark (%make-mark-node handle))
    (:ast-node-user (%make-user-node handle))))
