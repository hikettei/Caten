(in-package :caten/ajit)

;; ~~ Abstraction ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defgeneric %render-compile (lang avm allocs function)
  (:documentation "Compiles the function"))

(defgeneric %render-function-caller (lang avm allocs)
  (:documentation "Return a lambda function which calles the jit-compiled function."))

(defgeneric %render-program-toplevel (lang body) (:documentation "Renders headers, pragma, etc..."))

(defgeneric %render-function (lang avm allocs body)
  (:documentation "Renders
```
function void (args) { body };
```"))

(defgeneric %render-body (lang kernel-lang jit-graph polyhedral indent allocs)
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

(defgeneric %render-expr (lang op lhs rhs z)
  (:documentation "
TODO: Deftype
OP :=
:WHERE
:LT
:EQ
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

(defgeneric %render-nodes (lang graph args indent)
  (:documentation "
Render the ops in ./source/aasm/ops.lisp
"))

(defun render-expr (lang expr)
  "Recursively render the expr"
  (declare (type keyword lang))
  (if (expr-p expr)
      (%render-expr lang (expr-op expr) (expr-x expr) (expr-y expr) (expr-z expr))
      (%render-expr lang :Const expr nil nil)))

