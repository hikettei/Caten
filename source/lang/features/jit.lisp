(in-package :caten/lang)
;; @caten.jit creates blueprint graph from ANSI Common Lisp Program
;; Also by translating C code into Common Lisp (using with-c-syntax) ultimately it allows to embody C code in Common Lisp
;; and get optimized kernel for any language

;;; ~~ Any language ==> Common Lisp Translator ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defparameter *supported-styles* `(:lisp))

(defgeneric caten-jit-style-handler (style-id code))

(defmethod caten-jit-style-handler (style-id code)
  (if (next-method-p)
      (call-next-method)
      (error "Failed to expand the macro @caten.jit(style=~a).~%  The style ~a is not supported.~%Currently available: ~a" style-id style-id *supported-styles*)))

(defmethod caten-jit-style-handler ((style-id (eql :lisp)) code)
  (handler-case (read-from-string code)
    (error (c) (error "@caten.jit(:lisp,...): Failed to parse the code due to:~%  ~a" c))))

(defmethod caten-jit-style-handler ((style-id (eql :c)) code)
  (error "NOT READY"))

(trivia:defpattern Sym (to-what) ;; Compares the symbol-name
  `(and (type symbol) (satisfies (lambda (x) (equalp (symbol-name x) ,(symbol-name to-what))))))

(defun jit-rewrite (form)
  ;; [TODO] Make it readable, hackable, 
  (trivia:match form
    ((list* (Sym for) idx (Sym =) (list (Sym Range) size step) (Sym do) rest)
     (let* ((range-id (gensym (symbol-name idx))))
       (when (eql size 0) (warn "Detected an empty range: ~a.~%Range is defined as (Range SIZE STEP)" size))
       `(caten/aasm::%range
         ',idx ,(jit-rewrite size)
         (let ((,idx ',range-id))
           (caten/aasm:%progn ,@(map 'list #'jit-rewrite rest)))
         :step ,(if (numberp step) step (jit-rewrite step))
         :rid ',range-id)))
    ((list* (Sym let) (list* forms) body)
     `(let* (,@(loop for form in forms collect (list (car form) (jit-rewrite (second form)))))
        (caten/aasm:%progn ,@(map 'list #'jit-rewrite body))))
    ((list* (Sym with-locals) (list* forms) body)
     `(let* (,@(loop for form in forms collect (list (car form) `(caten/aasm:%expr (caten/aasm::node->id1 ,(jit-rewrite (second form))) :out ',(car form)))))
        (caten/aasm:%progn
         ,@(map 'list #'car forms)
         ,@(map 'list #'jit-rewrite body))))
    ((list* (Sym setf) rest)
     (assert (= 0 (mod (length rest) 2)))
     `(caten/aasm:%progn
       ,@(loop while rest
               for bind = (jit-rewrite (pop rest)) for value = (pop rest) for tmp = (gensym)
               collect
               `(let ((,tmp (caten/aasm:%expr (caten/aasm::node->id1 (caten/aasm:%setf ,bind ,(jit-rewrite value))))))
                  ,(when (symbolp bind)
                     `(setf
                       ,bind
                       (caten/aasm::node->id1 (caten/aasm:emit (caten/air:make-node :JIT :BIND (list (gensym)) (list (caten/air:node->id ,tmp)) :value ',bind)))))
                  ,tmp))))
    ((list (Sym aref) name idx) `(caten/aasm:%aref ,name ,(jit-rewrite idx)))
    ;; Operator rewriting
    ((list* (Sym +) rest) `(reduce #'caten/aasm:%add (list ,@(map 'list #'jit-rewrite rest))))
    ((list (Sym +=) a b)  `(caten/aasm:%add ,(jit-rewrite a) ,(jit-rewrite b) :reduction t))
    ((list* (Sym -) rest) `(reduce #'caten/aasm:%sub (list ,@(map 'list #'jit-rewrite rest))))
    ((list* (Sym *) rest) `(reduce #'caten/aasm:%mul (list ,@(map 'list #'jit-rewrite rest))))
    ((list* (Sym /) rest) `(reduce #'caten/aasm:%div (list ,@(map 'list #'jit-rewrite rest))))
    ((list* (Sym max) rest) `(reduce #'caten/aasm:%max (list ,@(map 'list #'jit-rewrite rest))))
    ((list* (Sym idiv) rest) `(reduce #'caten/aasm:%idiv (list ,@(map 'list #'jit-rewrite rest))))
    ((list* (Sym mod) rest) `(reduce #'caten/aasm:%mod (list ,@(map 'list #'jit-rewrite rest))))
    ((list (Sym sqrt) x) `(caten/aasm:%sqrt ,(jit-rewrite x)))
    ((list (Sym exp) x) `(caten/aasm:%exp2 (caten/aasm:%mul ,(jit-rewrite x) ,(jit-rewrite (/ (log 2))))))
    ((list (Sym scast) val type-to) `(caten/aasm:%cast (caten/aasm:%load (caten/aasm:%salloc :dtype ,type-to) 0.0) ,(jit-rewrite val) ,type-to))
    ((number x)
     (if (integerp form)
         `(caten/aasm:%load (caten/aasm:%salloc :dtype :int64) ,form)
         `(caten/aasm:%load (caten/aasm:%salloc :dtype :float32) ,form)))
    (_
     (if (listp form)
         `(,(car form) ,@(map 'list #'jit-rewrite (cdr form)))
         form))))

(defun expand-args (table-place rest-args body)
  (if rest-args
      (let ((args (car rest-args)))
        (trivia:match args
          ((list (Sym Pointer) bind dtype (list* shape))
           (let ((placeholder (gensym)))
             `(let ((,placeholder (gensym ,(format nil "special_~a_" bind))))
                (multiple-value-bind (,bind ,dtype ,@shape)
                    (values
                     (caten/aasm:%global ,placeholder (caten/api:tensor-dtype ,bind) t)
                     (caten/api:tensor-dtype ,bind)
                     ,@(loop for s in shape for nth upfrom 0
                             collect `(caten/aasm:%load (caten/aasm:%salloc :dtype :int64) (nth ,nth (caten/api:tensor-shape ,bind)))))
                  (setf (gethash ',bind ,table-place) ,placeholder)
                  ,(expand-args table-place (cdr rest-args) body)))))
          (_
           (error "Not a valid argument form: ~a" args))))
      body))

(define-caten-feature (jit :docstring "
```
@caten.jit(:style style :n-profile) {

}
```")
    ((ctx &key (style :lisp))
      (let* ((code (directive-context-code ctx))
             (pos-first-char (position-if #'(lambda (x) (and (not (char= x #.(aref " " 0))) (not (char= x #\newline)))) code))
             (pos-last-char (position-if #'(lambda (x) (and (not (char= x #.(aref " " 0))) (not (char= x #\newline)))) (reverse code)))
             (code (subseq code pos-first-char (- (length code) pos-last-char)))
             (code (if (and (char= (aref code 0) #\") (char= (aref code (1- (length code))) #\"))
                       (subseq code 1 (1- (length code)))
                       code))
             (form (caten-jit-style-handler style code)))
        (trivia:match form
          ((list* 'defun kernel-name (list* args) body)
           (let ((table-tmp (gensym)))
             `(defun ,kernel-name (,@(map 'list #'second args) &aux (,table-tmp (make-hash-table)))
                (caten/api::%forward-with-captured-graph
                 ',kernel-name
                 (caten/aasm:with-blueprint ()
                   ,(expand-args
                     table-tmp
                     args
                     `(caten/aasm:%progn ,@(map 'list #'jit-rewrite body))))
                 ,table-tmp
                 ',(map 'list #'second args)
                 ,@(map 'list #'second args)))))
          (_
           (error "@caten.jit: nothing to capture? The code should start w/ defun."))))))
