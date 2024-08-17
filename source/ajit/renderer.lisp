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

(deftype op/body ()
  "A list of ops used for rendering the body."
  `(member :FOR :ENDFOR :FUNCALL :IF :ELSE :ENDIF))

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

(deftype op/expr ()
  "A list of ops used for rendering the computation"
  `(member
    :WHERE ;; x = %where(condition, x, y)
    ;; Comparisons
    :< :<= :> :>= :==
    ;; Arithmetic
    :+ :- :* :/ :% ;; (mod)
    ;; Constant
    :Const ;; Const (Value Nil)
    :Aref
    
    :ADD :MUL
    :AND :OR
    :MAX :MIN
    ;; Unary
    :NEG
    :SIN :LOG2 :EXP2
    :RECIP :SQRT :NOT
    :INDEX-COMPONENTS
    
    :LOAD))

(defgeneric %render-expr (lang op lhs rhs z)
  (:documentation "
op/expr
"))

(deftype op/node ()
  "A list of nodes used for rendering the code"
  `(member :ALLOC :WMMA :EXPR))

(defgeneric %render-nodes (lang graph args indent)
  (:documentation "
Render the ops in ./source/aasm/ops.lisp.
:ALLOC
:EXPR
"))
;; (defstruct metadata)
;; TODO: verify-node
(defun render-expr (lang expr)
  "Recursively render the expr"
  (declare (type keyword lang))
  (if (expr-p expr)
      (%render-expr lang (expr-op expr) (expr-x expr) (expr-y expr) (expr-z expr))
      (%render-expr lang :Const expr nil nil)))
