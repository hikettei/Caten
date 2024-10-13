(in-package :caten/workflow)

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Goal: Export the entire code (including tokenizer, data loader, matrix computation kernel) for any language
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; caten/workflow
;; - Includes a wrapper for `caten`.
;; - Includes a class for Tokenizer.
;; - Each workflow can be compiled into C, or any language, using caten/ajit renderer (comptible with them).

;; Workflow and Concepts
;; - Implement Export2C Mode
;; - Implement Control, High-Level Interface, Including Function defining, IF/FOR
;;   - DSL

;; [TODO]
;; - ここでこのモデル全体をCommon Lispで読み込むことによって，モデルのコンパイル結果をCache可能にする
;; - 若しくは，C言語/Python/Smaller Common LispへCompileして実行を可能に

;; Backend has two facets:
;; - Kernel Renderer   [ Low-Level  ]
;; - Workflow Renderer [ High-Level ]

;; TODO: Compile the entire graph. Including token sampling
;;      Input
;;        |
;; [Action: Tokenizer]
;;        |
;;  |-[Action:Run]-|
;;  | Transformer  | <-------------------------------|
;;  |--------------|                                 |
;;        |                                          | x N
;;        |----[Action: Logits Argmax Concatenate] --|
;;        |    
;;    [Output]
;;        |

;; Action:
;; - Is a CLOS class
;; - Only initialized in defworkflow macro
;; - Workflow is a aIR graph

(eval-when (:compile-toplevel :load-toplevel :execute)

(defclass Action ()
  ((device :type caten/ajit:device :initarg :device :reader action-device)
   (ctx :type Context :accessor action-ctx)))

(defun run-action (device action-name &rest args)
  (declare (type caten/ajit:device device)
           (type symbol action-name))
  (let ((action (make-instance action-name :device device)))
    (ctx-compile (action-ctx action) device)
    (apply #'values (apply #'ctx-run (action-ctx action) args))))

(defun parse-type-designator (type-form other-variables types)
  "Return (values pointer-p dtype stride shape additional-computation-form)"
  (match type-form
    ((guard x (keywordp x))
     (values nil x))
    ((list (guard x (eql x :pointer)) y)
     (values t y))
    ((list (eql :array) order (list* size) element-dtype)
     (dolist (s size)
       (let ((form (find s types :key #'cddr :test #'find)))
         (assert
          (or
           (integerp s)
           (and
            form
            (find s other-variables)
            (caten/common.dtype:dtype/integerp (second form))))
          ()
          "
defaction: `(:array ORDER (~a) :DTYPE)`
                            ^ the variable ~a should be defined in the args as an integer." s s)))
     (assert (member order '(:row :column)) () "defaction: ORDER should be either :row or :column")
     (multiple-value-bind (strides stride-compute-forms) (calc-strides-with-form order size)
       (values t element-dtype strides size stride-compute-forms)))
    (_
     (error "defaction: Invalid type designator: ~A" type-form))))

;; Each action can be compiled into Render-Graph first, and then each language
(defun action-parse-lambda-list-and-body (args body &aux (additional-forms))
  (multiple-value-bind (remaining-form declare docstring) (alexandria:parse-body body :documentation t)
    (multiple-value-bind (params optional rest kw allow-other-keys-p aux key-p)
        (alexandria:parse-ordinary-lambda-list args :normalize nil)
      (declare (ignore key-p))
      (assert (null allow-other-keys-p) () "defaction: &allow-other-keys is not supported")
      (let ((variables
              (nconc
               params
               (map 'list #'car optional)
               (when rest (list rest))
               (map 'list #'car kw)
               (map 'list #'car aux)))
            (type-decl
              (loop for decl in declare
                    append
                    (loop for form in decl
                          when (and (listp form) (equalp (symbol-name (car form)) "TYPE"))
                            collect form))))
        (values
         (loop for var in variables
               for type-form = (find var type-decl :key #'cddr :test #'find)
               do (assert type-form () "defaction: Cannot infer the type of ~A.~%Provide (declare (type type_name variable ...)) form to declare the type." var)
               collect
               (multiple-value-bind (is-pointer dtype strides size additional-compute-form)
                   (parse-type-designator (second type-form) variables type-decl)
                 (assert (= (length strides) (length size)))
                 (when additional-compute-form
                   (dolist (f additional-compute-form)
                     (push f additional-forms)))
                 (let ((dtype (caten/common.dtype:dtype-alias dtype)))
                   (caten/ajit:make-argument
                    :name var
                    :pointer-p is-pointer
                    :dtype dtype
                    :type :user :io :input
                    :metadata (caten/avm:make-buffer (length size) size strides dtype nil)))))
         `(,@(reverse additional-forms) ,@remaining-form)
         docstring
         (let ((optimize-form (find "OPTIMIZE" (cdr (car declare)) :key (alexandria:compose #'symbol-name #'car) :test #'equalp)))
           (second (find "DEBUG" (cdr optimize-form) :key (alexandria:compose #'symbol-name #'car) :test #'equalp))))))))

(defun cname (symbol)
  (intern (cl-ppcre:regex-replace-all "-" (symbol-name symbol) "_")))

(defmacro defaction (name lambda-list &body body)
  "
```
(defaction name lambda-list &body body)
```

Define an action.

- name[symbol]: The name of the action.
- lambda-list[list]: lambda-list
- body[list]: The body of the action. (Lisp-Like DSL, see caten/lang)

Dtype decl:
```
- (:pointer :dtype)
- :dtype
- (:array ORDER (SIZE) :dtype) (e.g.: `(:array :row (M N) :int)), note that M and N should be provided as a variable first!
```

```
(optimize (debug 3))
              ^ debug here corresponds to JIT_DEBUG. JIT_DEBUG=1 to see the compiled code.
```
"
  ;; Args: (Name, Type)
  (multiple-value-bind (args1 body1 docstring1 debug) (action-parse-lambda-list-and-body lambda-list body)
    ;; C-c C-c and the error check
    (let ((ctx (make-context-from-list (cname name) args1 body1))
          (dev (caten/ajit:default-device :clang)) ;; [FIX] Is the device always clang
          (debug (or debug 0)))
      (when (>= debug 1)
        (ctx:with-contextvar (:jit_debug debug)
          (ctx-compile ctx dev))))
    `(progn
       (defclass ,name (Action)
         nil
         (:documentation ,(or docstring1 "")))
       (defmethod initialize-instance :after ((self ,name) &rest initargs)
         (declare (ignore initargs))
         (multiple-value-bind (args body) (action-parse-lambda-list-and-body ',lambda-list ',body)
           (setf (action-ctx self) (make-context-from-list ',(cname name) args body)))))))

) ;; eval-when


;(defaction Test (x i k)
;  (declare (type (:array :row (i k) :float) x)
;           (type :int32 i k))
;  (setf (aref x 2 2) 2.0)
;  (let ((a x))
;    (setf (aref a 3 2) 1.0)
;    a))

;; Segv しそ~...

;; What value do we want to return?
;; A single scalar value
;; 
;; fix the default int! :int and use *default-int*
#|
(defaction TestFunc (n)
  (declare (type :int64 n))
  (let ((m (* 10 n)))
    (if (> m 1)
        (let ((s (* m 10)))
          s)
        n)))

(defaction Test (x i k)
  (declare (type (:array :row (i k) :float) x)
           (type :int32 i k))
  (setf (aref x 2 2) 2.0)
  (let ((a x))
    (setf (aref a 3 2) 1.0)
    a))

(defaction Matmul (x y z m n k)
  (declare (type (:array :row (m n) :float) x)
           (type (:array :row (n k) :float) y)
           (type (:array :row (m k) :float) z)
           (type :int32 m n k))
  (dotimes (mm m)
    (dotimes (nn n)
      (dotimes (kk k)
        (setf (aref z mm kk) (+ (aref z mm kk) (* (aref x mm nn) (aref y nn kk))))))))

;; Defstructできるようにしたくなった。
;; SentencePieceはCatenで実装できそう
(defaction SentencePiece (text vocabulary scores m)
  "Implements SentencePipece
- Paper:
- Reference: https://github.com/karpathy/llama2.c/blob/master/run.c#L452"
  (declare (type (:pointer :char) text vocabulary)
           (type (:pointer :float32) scores)
           (type :int64 m))
  (let ((best-score -1e10)
        (best-id -1)
        (best-index -1)
        (str-buffer (_%allocate-sized-array :char (* m 2)))
        (str-len 0))
    nil
    ))
|#

;; - [ ] Allow to use under_score
;; - [x] does it works for higher rank array? (serf aref) (need tests)
;; - [x] Compile+Runできるようにして
;; - [ ] Test-Suiteできるようにする
;; - [x] funcallする
;; = [x] Return Value
;; - [ ] Allow make-tensor inside the action.
;; - [ ] Implement, Break, Continue, Return w/o new ops
;; - [x] Let
;; - [x] Pointer, Array
;;  - [x] Sized Array
;;  - [x] Aref
;;  - [x] (setf aref)
;; - [x] String(an array of int4)
;; - [x] String Syntax (automatically converted into a list of int8)
;;   - [x] Array Creation in the code.
;;   - [x] Allow to return a pointer
;;   - [x] Allow to initialize a pointer. (float* x;) (due to _%setf)
;; - [x] dotimes
;; - [ ] Free pointer
;;   - [x] (NO) ~~ Or, having objects as a Common Lisp Object. Dont allocate pointer inside the compiled code.~~
;; - [ ] Autoinfer the bound of tensor (M N K is rebundant!)
;; - [ ] Return a value
;; - [ ] with-scop (Auto Scheduler is available!)
;; - [ ] return, return values;
;; - [ ] Implement MoE (That is, Module and Action interop)
;; - [ ] Compile into foreign language
;; - [ ] Provide a test
;; - [ ] Provide a full documentation!
;; - [ ] (declare (optimize (debug 3)))でコードを表示する
;; - [ ] May need new ops: :FREE :FREAD :FWRITE

;; =, Length are action
;; TODO: defaction: workflow configを一緒に提供する (dokode?)
;; - defactionで想定しているもの:
;;  - [ ] MoEのGating (なので，ModuleとのInteropを簡単にする)
;;  - [ ] Model Weight読み込みの実装 (ConfigからPathなどを読み込める必要がある)
;;  - [ ] Tokenizerの実装
;;  - [ ] Tensor Allocation, Dynamic Shapeの計算
;;   - [ ] make-tensor from action
;;  - [ ] Compiled AVMを呼び出す(例えばArgmax単体とか，このコンパイルの判定を自動でやりたい)
;; (defaction switch (condition action1 action2)
;;
;; Add Test:
;; - [ ] Sized :Allocate Test
;; - [ ] :TAKE Test ...
;; - 粒度の細かいテストが欲しい・・・
