(in-package :caten/isl)

(deftype size () '(and unsigned-byte fixnum))
(deftype ast-expr-type () `(member ,@(remove :ast-expr-error (cffi:foreign-enum-keyword-list 'isl-ast-expr-type))))
(deftype dim-type () `(member ,@(cffi:foreign-enum-keyword-list 'isl-dim-type)))
(deftype value-designator () '(or rational value))
