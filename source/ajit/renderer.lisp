(in-package :caten/ajit)

;; ~~ Abstraction ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defgeneric %render-subroutine (lang kernel-lang jit-graph polyhedral indent)
  (:documentation
   "IRs used in the jit-graph:
(TODO: Docs)
- FOR
- ENDFOR
- FUNCALL
- IF
- ELSE
- ENDIF
"))

(defgeneric %render-expr (lang op lhs rhs)
  (:documentation "
TODO: Deftype
OP :=
:CONST(value, nil)
:AND
:OR
:MAX
:MIN
:+ :- :* :/
:NEG(value, nil)
:% (mod)
:equal
:<=
:>=
:<
:>
"))

(defgeneric %render-nodes (lang graph args indent) (:documentation "Corresponds w/ caten/aasm/ops.lisp"))

(defun render-expr (lang expr)
  "Render-expr"
  (declare (type keyword lang))
  (if (expr-p expr)
      (%render-expr lang (expr-op expr) (expr-x expr) (expr-y expr))
      (%render-expr lang :Const expr nil)))

