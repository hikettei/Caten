(in-package :caten/ajit)

(defstruct (Argument)
  (pointer-p nil :type boolean)
  (dtype (error "dtype must occur") :type dtype-t)
  (type :input :type (and keyword (member :shape :input :tmp)))
  (metadata (error "metadata must occur") :type node))
;; (defstruct Metadata
;;  *accessing* ...
;; ~~ Abstraction ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defgeneric %render-compile (lang avm allocs function)
  (:documentation "Compiles the function"))

(defmethod %render-compile :around (lang avm allocs function)
  (restart-case (if (= 1 (ctx:getenv :CALL_ZENITY)) (error "Triggered by CALL_ZENITY=1~%~%~a" function) (call-next-method))
    (zenity/modify-code ()
      :report "Calling a GUI Editor, update the code manually. (SHOULD ONLY BE USED FOR DEBUGGING)"
      (%render-compile lang avm allocs (zenity/prompt-new-value function)))
    (zenity/proceed ()
      :report "Proceed w/ current code."
      (when (= 1 (ctx:getenv :CALL_ZENITY)) (call-next-method)))))

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
  "A list of ops used for rendering the computation.
When creating a MultiExpr, it is only fused if the node-type is op/expr!"
  `(member
    :WHERE ;; x = %where(condition, x, y)
    ;; Comparisons
    :< :<= :> :>= :== :!=
    ;; Arithmetic
    :+ :- :* :/ :% ;; (mod)
    ;; Constant
    :Const ;; Const (Value Nil)
    :Aref
    :CAST
    
    :ADD :MUL
    :AND :OR :XOR
    :MAX :MIN
    ;; Unary
    :NEG
    :SIN :LOG2 :EXP2
    :RECIP :SQRT :NOT
    :INDEX-COMPONENTS
    
    :LOAD :MOVE :STORE))

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

;; TODO: verify-node
(defun render-expr (lang expr)
  "Recursively render the expr"
  (declare (type keyword lang))
  (if (expr-p expr)
      (%render-expr lang (expr-op expr) (expr-x expr) (expr-y expr) (expr-z expr))
      (%render-expr lang :Const expr nil nil)))
