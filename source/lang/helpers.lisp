(in-package :caten/lang)

(defun make-const-buffer (dtype)
  (caten/avm:make-buffer 0 nil nil dtype nil))

(defmethod write-output-to ((ctx Context) (form Parsed-Form) place declare-p)
  "Returns a :FUNCALL that writes the result of form into the place."
  (ctx-define-and-make-funcall-from-expr
   ctx
   (parsed-form-expr form)
   place
   (parsed-form-type form)
   (list declare-p)))

(defmethod stash-forms ((ctx Context) (form Parsed-Form) tmpvar declare-p)
  "Return: (values stashed render-graph, output expr)"
  (if (parsed-form-nodes form)
      (values
       (append
        ;; if (a > b) {
        ;;   return 1 + 1;
        ;; }
        ;; =>
        ;; int _tmp;
        ;; if (a > b) {
        ;;   _tmp = 1
        ;; }
        (parsed-form-nodes form)
        (when tmpvar
          (list (write-output-to ctx form tmpvar declare-p))))
       (caten/ajit:make-expr :Const tmpvar (make-const-buffer (caten/avm:buffer-dtype (parsed-form-type form)))))
      (values nil (parsed-form-expr form))))
