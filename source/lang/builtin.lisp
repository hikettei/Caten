(in-package :caten/lang)

;; [TODO] Update the scope!
(a/defun _%progn (ctx &rest forms &aux (evaluated-forms))
         "Creates a body from multiple forms."
  (let ((new-nodes
          (loop for form in forms
                if (parsed-form-nodes form)
                  do (push (parsed-form-expr form) evaluated-forms) and
                append (parsed-form-nodes form)
                else
                  collect
                  (let ((placeholder (gensym "_TMP")))
                    (push placeholder evaluated-forms)
                    (ctx-define-and-make-funcall-from-expr
                     ctx
                     (parsed-form-expr form)
                     placeholder
                     (parsed-form-type form)
                     (list t))))))
    (make-parsed-form
     new-nodes
     (if (typep (car evaluated-forms) 'caten/ajit:Expr)
         (car evaluated-forms)
         (caten/ajit:make-expr
          :Const (car evaluated-forms)
          (caten/avm:buffer-dtype (parsed-form-type (car (last forms))))))
     (parsed-form-type (car (last forms))))))

(a/defun _%if (ctx condition then-form &optional else-form)
         "Runs the then-form if the condition is true, otherwise runs the else-form."
  (when else-form
    (assert (eql (caten/avm:buffer-dtype (parsed-form-type then-form))
                 (caten/avm:buffer-dtype (parsed-form-type else-form)))
            ()
            "then-form and else-form must have the same type. Inferred as ~a and ~a"
            (caten/avm:buffer-dtype (parsed-form-type then-form))
            (caten/avm:buffer-dtype (parsed-form-type else-form))))
  (when (not (eql :bool (caten/avm:buffer-dtype (parsed-form-type condition))))
    (error "The condition of an IF statement should be boolean. Inferred as ~a" (caten/avm:buffer-dtype (parsed-form-type condition))))
  (let ((output-bind (gensym "_IF_OUT")))
    (multiple-value-bind (condition-nodes condition-expr) (stash-forms ctx condition (gensym "_C"))
      (make-parsed-form
       (append
        condition-nodes
        ;; int _output_tmp;
        (list (ctx-declare-local-var ctx output-bind (caten/avm:buffer-dtype (parsed-form-type then-form))))
        (list (caten/ajit:r/if condition-expr))
        (multiple-value-bind (then-nodes) (stash-forms ctx then-form output-bind)
          (when (null then-nodes) (warn "_%if: then looks empty, is the form created from (_%if condition (PROGN ...)?"))
          then-nodes)
        (when else-form
          (append
           (list (caten/ajit:r/else))
           (multiple-value-bind (else-nodes) (stash-forms ctx else-form output-bind)
             (when (null else-nodes) (warn "_%if: else looks empty, is the form created from (_%if condition then (PROGN ...)?"))
             else-nodes)))
        (list (caten/ajit:r/endif)))
       (caten/ajit:make-expr
        :Const output-bind
        (parsed-form-type then-form))
       (parsed-form-type then-form)))))

(a/defun _%while (ctx condition body)
         "Runs the body while the condition is true."
  (multiple-value-bind (condition-nodes condition-expr) (stash-forms ctx condition nil)
    (make-parsed-form
     (append
      condition-nodes
      (list (caten/ajit:r/while condition-expr))
      (multiple-value-bind (body-nodes) (stash-forms ctx body nil)
        (when (null body-nodes) (warn "_%while: the body is empty. Is the form created from (_%while condition (PROGN ...)?"))
        body-nodes)
      (list (caten/ajit:r/endwhile)))
     (caten/ajit:make-expr :const nil)
     (make-const-buffer :bool))))

(a/defun _%setf (ctx place val)
         "Declare a variable and assign a value to it."
  (assert (symbolp place))
  (make-parsed-form
   (append
    (list (ctx-declare-local-var ctx place (caten/avm:buffer-dtype (parsed-form-type val))))
    (multiple-value-bind (forms) (stash-forms ctx val place)
      (if forms
          forms
          (list (ctx-define-and-make-funcall-from-expr ctx (parsed-form-expr val) place (parsed-form-type val) (list nil))))))
   (caten/ajit:make-expr :const place (parsed-form-type val))
   (parsed-form-type val)))
