(in-package :caten/ajit)

(defstruct (Argument)
  (name nil :type symbol)
  (pointer-p nil :type boolean)
  (dtype (error "dtype must occur") :type dtype-t)
  (type :input :type (and keyword (member :shape :tmp :user)))
  (io :input :type (and keyword (member :const :input :output :io)))
  (metadata (error "metadata must occur") :type Buffer))
;; (defstruct Metadata
;;  *accessing* ...
;; ~~ Abstraction ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defgeneric %render-compile (lang avm function dir)
  (:documentation "Compiles the function"))

(defmethod %render-compile :around (lang avm function dir)
  (restart-case (if (= 1 (ctx:getenv :CALL_ZENITY)) (error "Triggered by CALL_ZENITY=1~%~%~a" function) (call-next-method))
    (zenity/modify-code ()
      :report "Calling a GUI Editor, update the code manually. (SHOULD ONLY BE USED FOR DEBUGGING)"
      (%render-compile lang avm (zenity/prompt-new-value function) dir))
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
    
    :ADD :MUL :IDIV
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
  (:documentation ""))

(defun render-expr (lang expr)
  "Recursively render the expr"
  (declare (type device lang))
  (if (expr-p expr)
      (%render-expr lang (expr-op expr) (expr-x expr) (expr-y expr) (expr-z expr))
      (%render-expr lang :Const expr nil nil)))

(defun render-aref (lang buffer &key (genid #'gid) (strides))
  (render-expr lang (simplify-expr (render-isl-aref buffer :genid genid :flatten nil :strides strides))))

(defun %render-aref (buffer &key (genid #'gid) (strides))
  (simplify-expr (render-isl-aref buffer :genid genid :flatten nil :strides strides)))

(defun render-index-components (lang lhs rhs access)
  "Renders the index-component.
Lang = Device, access = a list of _gid (the :args of parent :FUNCALL)"
  (let ((strides (map 'list #'(lambda (x) (render-expr lang x)) rhs))
        (buffer (copy-buffer (expr-y lhs))))
    ;; When INDEX-COMPONENT is mutated to scalar? -> Supply the nrank.
    (when (= (buffer-nrank buffer) 0)
      (setf (buffer-nrank buffer) (length rhs)))
    (render-expr
     lang
     (simplify-expr
      (render-isl-aref buffer :genid #'(lambda (x) (or (intern (nth x access)) (error "render-index-components: the argument ~a is too small!" access))) :strides strides)))))
