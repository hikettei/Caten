(cl:in-package :cl-user)
(defpackage :caten/isl
  (:use :cl :cffi)
  (:nicknames #:isl)
  (:shadow #:set #:map #:space)
  (:export
   #:with-isl-context
   ;; building ast
   #:ast-build-from-context
   #:ast-build-node-from-schedule
   
   #:copy
   ;; Context
   #:context
   #:contextp
   #:*context*
   #:make-context
   ;; Identifier
   #:identifier
   #:identifierp
   #:make-identifier
   #:make-gensym-identifier
   #:identifier-name
   #:identifier-context
   #:make-id-list
   ;; Value
   #:make-value-list
   #:value
   #:valuep
   #:value-context
   #:value-zero
   #:value-one
   #:value-minus-one
   #:value-nan
   #:value-positive-infinity
   #:value-negative-infinity
   #:value-from-integer
   #:integer-from-value
   #:value-sign
   #:value-zerop
   #:value-onep
   #:value-minus-one-p
   #:value-not-minusp
   #:value-not-plusp
   #:value-plusp
   #:value-minusp
   #:value-integerp
   #:value-rationalp
   #:value-nan-p
   #:value-positive-infinity-p
   #:value-negative-infinity-p
   #:value-<=
   #:value-<
   #:value->=
   #:value->
   #:value=
   #:value/=
   #:value-abs
   #:value-neg
   #:value-floor
   #:value-ceiling
   #:value-truncate
   #:value-inverse
   #:value-expt2
   #:value-min
   #:value-max
   #:value+
   #:value-
   #:value-mul
   #:value-div
   #:value-mod
   #:value-gcd
   #:value-gcdext
   #:value-object
   ;; Space
   #:space
   #:spacep
   #:space-add-param-id
   #:create-space-params
   #:create-space-set
   #:create-space-map
   #:space-dim
   #:multi-aff-zero
   #:set-from-multi-aff
   ;; Local Space
   #:local-space
   #:local-space-p
   #:local-space-from-space
   #:local-space-space
   ;; Constraint
   #:equality-constraint
   #:equality-constraint-p
   #:make-equality-constraint
   #:equality-constraint-set-constant
   #:equality-constraint-set-coefficient
   #:inequality-constraint
   #:inequality-constraint-p
   #:make-inequality-constraint
   #:inequality-constraint-set-constant
   #:inequality-constraint-set-coefficient
   ;; Affine expression
   #:affine-expression
   #:affine-expression-p
   #:affine-expression-from-str
   #:create-empty-affine
   #:create-val-affine
   #:create-var-affine
   #:affine-add
   #:affine-sub
   #:affine-mul
   #:affine-div
   ;; Union Access Info
   #:union-access-info
   #:union-access-info-from-sink
   #:union-access-info-set-must-source
   #:union-access-info-set-may-source
   
   #:union-access-info-set-schedule
   #:union-access-info-compute-flow
   #:union-flow-get-must-dependence
   #:union-flow-get-may-dependence
   #:union-access-info-set-schedule-map
   ;; Set
   #:set
   #:set-from-union-set
   #:set-get-space
   ;;#:setp
   #:set-from-str
   #:set-empty
   #:set-universe
   ;; Basic set
   #:basic-set
   #:basic-set-p
   #:basic-set-from-str
   #:basic-set-empty
   #:basic-set-universe
   #:basic-set-intersect
   #:basic-set-set
   #:basic-set-add-constraint
   #:basic-set-apply
   ;; MultiUnionPwAff
   #:multi-union-pw-aff
   #:multi-union-pw-aff-from-str
   #:multi-union-pw-aff-intersect-domain
   #:multi-union-pw-aff-size
   #:multi-union-pw-aff-min-multi-val
   #:multi-val-get-val
   #:multi-val-set-val
   #:multi-union-pw-aff-multi-val-on-domain
   #:multi-union-pw-aff-neg
   #:multi-union-pw-aff-add
   #:mupa-from-union-map
   #:multi-union-pw-aff-get-union-pw-aff
   #:multi-union-pw-aff-scale-down-val
   #:union-pw-aff-scale-down-val
   #:multi-union-pw-aff-floor
   #:union-pw-aff-floor
   #:union-pw-aff-scale-val
   #:multi-union-pw-aff-set-union-pw-aff
   #:multi-val-from-val-list
   ;; Union set
   #:union-set
   #:union-set-p
   #:union-set-is-empty
   #:union-set-from-str
   #:union-set-empty
   #:union-set-universe
   #:basic-set-union-set
   #:union-set-from-set
   #:set-union-set
   #:union-set-intersect
   #:union-set-union
   #:union-set-subtract
   #:union-set-product
   #:union-set-lex-lt-union-set
   #:union-set-lex-le-union-set
   #:union-set-lex-gt-union-set
   #:union-set-lex-ge-union-set
   #:union-set-equalp
   #:union-set-subsetp
   #:union-set-strict-subset-p
   #:union-set-intersect-params
   ;; Map
   #:map
   #:mapp
   #:map-from-str
   #:map-empty
   #:map-universe
   ;; Basic map
   #:basic-map
   #:basic-map-p
   #:basic-map-from-str
   #:basic-map-empty
   #:basic-map-universe
   #:basic-map-from-affine
   #:basic-map-map
   #:basic-map-intersect
   #:basic-map-add-constraint
   #:basic-map-insert-dimension
   ;; Union map
   #:union-map
   #:union-map-p
   #:union-map-from-str
   #:union-map-empty
   #:union-map-universe
   #:basic-map-union-map
   #:map-union-map
   #:union-map-reverse
   #:union-map-is-empty
   #:union-map-deltas
   #:union-map-intersect
   #:union-map-union
   #:union-map-subtract
   #:union-map-product
   #:union-map-lex-lt-union-map
   #:union-map-lex-le-union-map
   #:union-map-lex-gt-union-map
   #:union-map-lex-ge-union-map
   #:union-map-equalp
   #:union-map-subsetp
   #:union-map-strict-subset-p
   #:union-map-domain
   #:union-map-range
   #:union-map-from-domain-and-range
   #:union-set-identity
   #:union-map-intersect-params
   #:union-map-intersect-domain
   #:union-map-intersect-range
   #:union-map-subtract-domain
   #:union-map-subtract-range
   #:union-set-apply
   #:union-map-apply-range
   #:union-map-apply-domain
   ;; Ast expr
   #:ast-expr
   #:ast-expr-p
   #:ast-expr-equal-p
   #:create-ast-expr-from-val
   #:create-ast-expr-from-add
   ;; -- Op expr
   #:op-expr
   #:op-expr-p
   #:op-and
   #:op-and-then
   #:op-or
   #:op-or-else
   #:op-max
   #:op-min
   #:op-minus
   #:op-add
   #:op-sub
   #:op-mul
   #:op-div
   #:op-fdiv-q
   #:op-pdiv-q
   #:op-pdiv-r
   #:op-zdiv-r
   #:op-cond
   #:op-select
   #:op-eq
   #:op-le
   #:op-lt
   #:op-ge
   #:op-gt
   #:op-call
   #:op-access
   #:op-member
   #:op-address-of
   #:op-and-p
   #:op-and-then-p
   #:op-or-p
   #:op-or-else-p
   #:op-max-p
   #:op-min-p
   #:op-minus-p
   #:op-add-p
   #:op-sub-p
   #:op-mul-p
   #:op-div-p
   #:op-fdiv-q-p
   #:op-pdiv-q-p
   #:op-pdiv-r-p
   #:op-zdiv-r-p
   #:op-cond-p
   #:op-select-p
   #:op-eq-p
   #:op-le-p
   #:op-lt-p
   #:op-ge-p
   #:op-gt-p
   #:op-call-p
   #:op-access-p
   #:op-member-p
   #:op-address-of-p
   #:op-expr-get-n-arg
   #:op-expr-get-op-arg
   #:op-expr-get-list-args
   #:op-expr-get-operator
   ;; -- Id expr
   #:id-expr
   #:id-expr-p
   #:id-expr-get-id
   ;; -- Int expr
   #:int-expr
   #:int-expr-p
   #:int-expr-get-value
   ;; Ast node
   #:ast-node
   #:ast-node-p
   #:pretty-print-node
   #:ast-node-list-elements ; todo export every list object
   ;; -- For node
   #:for-node
   #:for-node-p
   #:for-node-get-iterator
   #:for-node-get-init
   #:for-node-get-cond
   #:for-node-get-inc
   #:for-node-get-body
   ;; -- If node
   #:if-node
   #:if-node-p
   #:if-node-get-cond
   #:if-node-get-then
   #:if-node-get-else
   #:if-node-has-else
   ;; -- Block node
   #:block-node
   #:block-node-p
   #:block-node-getlist
   ;; -- Mark node
   #:mark-node
   #:mark-node-p
   ;; -- User node
   #:user-node
   #:user-node-p
   #:user-node-get-expr
   #:user-get-expr
   ;; Ast build
   #:ast-build
   #:ast-build-p
   #:ast-build-set-options
   #:create-ast-build
   #:generate-optimized-as
   #:ast-build-set-iterators
   ;; Schedule
   #:schedule
   #:schedule-get-map
   #:schedulep
   #:schedule-constraints-compute-schedule

   #:schedule-from-domain
   #:schedule-sequence
   #:schedule-set
   #:schedule-to-str
   ;; Schedule constraints
   #:schedule-constraints
   #:schedule-constraints-p
   #:schedule-constraints-on-domain
   #:schedule-constraints-set-context
   #:schedule-constraints-set-validity
   #:schedule-constraints-set-coincidence
   #:schedule-constraints-set-proximity
   #:schedule-constraints-set-conditional-validity
   ;; printer.lisp
   #:isl-printer-to-str
   ;; from schedule-node.lisp
   #:schedule-get-root
   #:schedule-node-get-schedule-depth
   #:schedule-node-get-ancestor
   #:schedule-node-band-set-ast-build-options
   #:schedule-node-delete
   #:schedule-node-mark-get-id
   #:schedule-node-band-get-ast-isolate-option
   #:schedule-node-first-child
   #:schedule-node-band-tile
   #:schedule-node-insert-mark
   #:schedule-node-graft-after
   #:schedule-node-graft-before
   #:schedule-insert-partial-schedule
   #:schedule-node-from-domain
   #:schedule-node-get-schedule
   #:schedule-node-get-children
   #:schedule-node-get-type
   #:schedule-node-insert-partial-schedule
   #:schedule-node-band-get-partial-schedule-union-map
   #:schedule-node-get-schedule-depth
   #:schedule-node-get-child
   #:schedule-insert-sequence
   #:schedule-node-get-domain
   #:schedule-node-band-get-partial-schedule
   #:schedule-node-band-get-space
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

