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

(defclass Action ()
  ((device :type caten/ajit:device :initarg :device :reader action-device)
   (ctx :type Context :accessor action-ctx)))

(defun run-action (device action-name &rest args)
  (declare (type caten/ajit:device device)
           (type symbol action-name))
  (let ((action (make-instance action-name :device device)))
    ;; [TODO] Each action can be compiled stimultaneously
    action))

;; Each action can be compiled into Render-Graph first, and then each language
(defun action-parse-lambda-list-and-body (args body)
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
                          when (and (listp form) (equalp (symbol-name (car form)) "ATYPE"))
                            collect form))))
        (values
         (loop for var in variables
               for type-form = (find var type-decl :key #'cdr :test #'find)
               do (assert type-form () "defaction: Cannot infer the type of ~A.~%Provide (declare (atype type_name variable ...)) form to declare the type." var)
               collect
               (caten/ajit:make-argument
                :name var
                :pointer-p nil
                :dtype (second type-form)
                :type :user :io :input
                :metadata (caten/avm:make-buffer 0 nil nil (second type-form) nil)))
         remaining-form
         docstring)))))

(defmacro defaction (name (&rest args) &body body)
  "Defines an action"
  ;; Args: (Name, Type)
  (multiple-value-bind (args1 body1 docstring1) (action-parse-lambda-list-and-body args body)
    ;; C-c C-c and the error check
    (print (ctx-render-function (make-context-from-list name args1 body1) (caten/ajit:default-device :clang)))
    `(prog1
         (defclass ,name (Action)
           nil
           (:documentation ,(or docstring1 "")))
       (defmethod initialize-instance :after ((self ,name) &rest initargs)
         (declare (ignore initargs))
         (multiple-value-bind (args body) (action-parse-lambda-list-and-body ',args ',body)
           (setf (action-ctx self) (make-context-from-list ',name args body))

           )))))

(defaction TestFunc (n)
  (declare (atype :int64 n))
  (let ((m (* 10 n)))
    (if (> m 1)
        (let ((s (* m 10)))
          s)
        n)))
    

;; - [x] Let
;; - [ ] Pointer, Array
;; - [ ] String(an array of int4)
;; - [ ] For, dotimes, dolist
;; - [ ] with-scop
;; - [ ] Implement MoE (That is, Module and Action interop)

;; =, Length are action
;; TODO: workflow configを一緒に提供する
;; (defaction switch (condition action1 action2)
;;
;;
;;
;;
;;

