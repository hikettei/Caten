(in-package :caten/lang)

;; [TODO] Implement the update of scopes
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
    (multiple-value-bind (condition-nodes condition-expr) (stash-forms ctx condition (gensym "_C") t)
      (make-parsed-form
       (append
        condition-nodes
        ;; int _output_tmp;
        ;l (list (ctx-declare-local-var ctx output-bind (caten/avm:buffer-dtype (parsed-form-type then-form))))
        (list (ctx-define-and-make-funcall-from-expr
               ctx
               (caten/ajit:make-expr :Const (dtype/cast nil (caten/avm:buffer-dtype (parsed-form-type then-form))))
               output-bind (parsed-form-type then-form) (list t)))
        (list (caten/ajit:r/if condition-expr))
        (multiple-value-bind (then-nodes) (stash-forms ctx then-form output-bind nil)
          (when (null then-nodes) (warn "_%if: then looks empty, is the form created from (_%if condition (PROGN ...)?"))
          then-nodes)
        (when else-form
          (append
           (list (caten/ajit:r/else))
           (multiple-value-bind (else-nodes) (stash-forms ctx else-form output-bind nil)
             (when (null else-nodes) (warn "_%if: else looks empty, is the form created from (_%if condition then (PROGN ...)?"))
             else-nodes)))
        (list (caten/ajit:r/endif)))
       (caten/ajit:make-expr
        :Const output-bind
        (parsed-form-type then-form))
       (parsed-form-type then-form)))))

(a/defun _%while (ctx condition body)
         "Runs the body while the condition is true."
  (multiple-value-bind (condition-nodes condition-expr) (stash-forms ctx condition nil nil)
    (make-parsed-form
     (append
      condition-nodes
      (list (caten/ajit:r/while condition-expr))
      (multiple-value-bind (body-nodes) (stash-forms ctx body nil nil)
        (when (null body-nodes) (warn "_%while: the body is empty. Is the form created from (_%while condition (PROGN ...)?"))
        body-nodes)
      (list (caten/ajit:r/endwhile)))
     (caten/ajit:make-expr :const nil)
     (make-const-buffer :bool))))

(a/defun _%setf (ctx decl place val)
         "Assign `val` to `place`. if :decl is set to :T, then declare a new variable."
  (assert (symbolp place))
  (assert place)
  (assert (keywordp decl))
  (assert (member decl `(:t :nil)))
  (make-parsed-form
   (multiple-value-bind (forms) (stash-forms ctx val place (eql decl :t))
     (if forms
         forms
         (list (ctx-define-and-make-funcall-from-expr ctx (parsed-form-expr val) place (parsed-form-type val) (list (eql decl :t))))))
   (caten/ajit:make-expr :const place (parsed-form-type val))
   (parsed-form-type val)))

(a/defun _%setf_aref (ctx place-id place val)
         "Assign place[subscripts] = val."
  (assert (symbolp place-id))
  (assert (eql :TAKE (caten/ajit:expr-op (parsed-form-expr place))))
  (assert (null (parsed-form-nodes place)) () "place should be an expr.")
  (multiple-value-bind (val-form val-expr) (stash-forms ctx val (gensym "_SETFAREF") t)
    (let ((place-type (caten/avm:copy-buffer (parsed-form-type place))))
      (setf (caten/avm:buffer-nrank place-type) 1
            (caten/avm:buffer-shape place-type) `(1)
            (caten/avm:buffer-stride place-type) `(1))
      (make-parsed-form
       (append
        val-form
        (list
         (ctx-define-and-make-funcall-from-expr-and-args
          ctx val-expr place-id place-type (list nil)
          (caten/ajit:simplify-expr (caten/ajit:expr-y (parsed-form-expr place))))))
       val-expr
       (parsed-form-type val)))))

(a/defun take (ctx array position)
         "Access an element of an array."
  (multiple-value-bind (aref-forms aref-expr) (stash-forms ctx array (gensym "_ARF") t)
    (multiple-value-bind (pos-forms pos-expr) (stash-forms ctx position (gensym "_POS") t)
      (make-parsed-form
       (append aref-forms pos-forms)
       (caten/ajit:make-expr :Take aref-expr  pos-expr)
       (let ((type (caten/avm:copy-buffer (parsed-form-type array))))
         (setf (caten/avm:buffer-nrank type) 0
               (caten/avm:buffer-shape type) nil
               (caten/avm:buffer-stride type) nil)
         type)))))

(a/defun aref (ctx array &rest subscripts)
         "Access an element of an array."
  (multiple-value-bind (aref-forms aref-expr) (stash-forms ctx array (gensym "_ARF") t)
    (let ((forms (loop for s in subscripts
                       collect
                       (multiple-value-list (stash-forms ctx s (gensym "_POS") t)))))
      (assert (= (length subscripts) (caten/avm:buffer-nrank (parsed-form-type array)))
              ()
              "The number of subscripts should match the rank of the array. Inferred ~a and ~a"
              (parsed-form-type array)
              (map 'list #'parsed-form-expr subscripts))
      (make-parsed-form
       (append aref-forms (apply #'append (map 'list #'car forms)))
       (caten/ajit:make-expr
        :TAKE aref-expr
        (caten/ajit:simplify-expr
         (flet ((add (x y) (caten/ajit:make-expr :ADD x y)))
           (reduce #'add (loop for stride in (caten/avm:buffer-stride (parsed-form-type array))
                               for s = (caten/ajit:make-expr :const stride)
                               for i in (map 'list #'second forms)
                               collect (caten/ajit:make-expr :MUL i s))))))
       (let ((type (caten/avm:copy-buffer (parsed-form-type array))))
         (setf (caten/avm:buffer-nrank type) 0
               (caten/avm:buffer-shape type) nil
               (caten/avm:buffer-stride type) nil)
         type)))))

(a/defun _%allocate-sized-array (ctx dtype size &aux (place (gensym "_ARRAY")) (size-place (gensym "_SZ")))
         "Creates an array with a given size"
  (multiple-value-bind (size-nodes size-expr) (stash-forms ctx size (gensym "_SIZETMP") t)
    (assert (keywordp dtype) () "dtype is a keyword.")
    (let* ((dtype (caten/common.dtype:dtype-alias dtype))
           (type (caten/avm:make-buffer 1 (list size-place) (list 1) dtype nil)))
      (make-parsed-form
       (append
        size-nodes
        ;; SIZE = SIZE;
        (list (ctx-define-and-make-funcall-from-expr ctx size-expr size-place (parsed-form-type size) (list t)))
        (list (ctx-declare-sized-local-var ctx place size-place dtype)))
       (caten/ajit:make-expr :const place type)
       type))))
