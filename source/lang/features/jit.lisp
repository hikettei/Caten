(defpackage :caten/lang.jit.clang-helper
  (:use :cl))
(in-package :caten/lang.jit.clang-helper)

(named-readtables:in-readtable with-c-syntax:with-c-syntax-readtable)
(defun string->common-lisp (code &key (n 1))
  (declare (type string code))
  (let ((code (format nil "~a ~%}#" code)))
    (with-input-from-string (stream code)
      (let* ((level (or n with-c-syntax.core::*with-c-syntax-reader-level*))
             (readtable-case (or with-c-syntax.core::*with-c-syntax-reader-case*
                                 (with-c-syntax.core::readtable-case *readtable*)))
             (readtable (with-c-syntax.core::find-c-readtable level readtable-case))
             (with-c-syntax.core::*previous-readtable* *readtable*)
             (tokens (with-c-syntax.core::tokenize-source stream t readtable)))
        (with-c-syntax.core::expand-c-syntax
         (with-c-syntax.core::preprocessor tokens level readtable-case nil) t)))))

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
  (error "NOT READY")
  (caten/lang.jit.clang-helper::string->common-lisp code))

(defun jit-rewrite (form)
  ;; [TODO] Make it readable, hackable, 
  (trivia:match form
    ((list* 'for idx '= (list 'Range size step) 'do rest)
     (let* ((range-id (gensym (symbol-name idx))))
       `(caten/aasm::%range
         ',idx ,(jit-rewrite size)
         (let ((,idx ',range-id))
           (caten/aasm:%progn ,@(map 'list #'jit-rewrite rest)))
         :step ,(if (numberp step) step (jit-rewrite step))
         :rid ',range-id)))
    ((list* 'let (list* forms) body)
     `(let* (,@(loop for form in forms collect (list (car form) (jit-rewrite (second form)))))
        (caten/aasm:%progn ,@(map 'list #'jit-rewrite body))))
    ((list* 'with-locals (list* forms) body)
     `(let* (,@(loop for form in forms collect (list (car form) `(caten/aasm:%expr (caten/aasm::node->id1 ,(jit-rewrite (second form))) :out ',(car form)))))
        (caten/aasm:%progn
         ,@(map 'list #'car forms)
         ,@(map 'list #'jit-rewrite body))))
    ((list* 'setf rest)
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
    ((list 'aref name idx) `(caten/aasm:%aref ,name ,(jit-rewrite idx)))
    ;; Operator rewriting
    ((list* '+ rest) `(reduce #'caten/aasm:%add (list ,@(map 'list #'jit-rewrite rest))))
    ((list '+= a b)  `(caten/aasm:%add ,(jit-rewrite a) ,(jit-rewrite b) :reduction t))
    ((list* '- rest) `(reduce #'caten/aasm:%sub (list ,@(map 'list #'jit-rewrite rest))))
    ((list* '* rest) `(reduce #'caten/aasm:%mul (list ,@(map 'list #'jit-rewrite rest))))
    ((list* '/ rest) `(reduce #'caten/aasm:%div (list ,@(map 'list #'jit-rewrite rest))))
    ((list* 'max rest) `(reduce #'caten/aasm:%max (list ,@(map 'list #'jit-rewrite rest))))
    ((list* 'idiv rest) `(reduce #'caten/aasm:%idiv (list ,@(map 'list #'jit-rewrite rest))))
    ((list* 'mod rest) `(reduce #'caten/aasm:%mod (list ,@(map 'list #'jit-rewrite rest))))
    ((list 'sqrt x) `(caten/aasm:%sqrt ,(jit-rewrite x)))
    ((list 'exp x) `(caten/aasm:%exp2 (caten/aasm:%mul ,(jit-rewrite x) ,(jit-rewrite (/ (log 2))))))
    
    ((list 'scast val type-to) `(caten/aasm:%cast (caten/aasm:%load (caten/aasm:%salloc :dtype ,type-to) 0.0) ,(jit-rewrite val) ,type-to))
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
          ((list 'Pointer bind dtype (list* shape))
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
;; tests
(in-caten-toplevel)

(progn
  @caten.jit () {
  (defun flash-attention ((Pointer Q Type (Batch Head N D)) (Pointer K Type (Batch Head N D)) (Pointer V Type (Batch Head N D))
                          (Pointer O Type (Batch Head N D))
                          (Pointer L Type (Batch Head N)) (Pointer M Type (Batch Head N)))
    (let ((scale (/ 1.0 (sqrt (scast D :float32))))
          (outer (* batch n head)))
      (for b = (Range BATCH 1) do
           (for h = (Range Head 1) do
                (for i = (Range N 1) do
                     (let ((q-base-idx (* D (+ i (* N (+ (* b head) h)))))
                           (k-base-idx (* D (* N (+ (* b head) h))))
                           (v-base-idx (* D (* N (+ (* b head) h))))
                           (o-base-idx (* D (+ i (* N (+ (* b head) h)))))
                           (row-m (aref M (+ i (* N (+ (* b head) h)))))
                           (row-l (aref L (+ i (* N (+ (* b head) h))))))
                       (for j = (Range N 1) do
                            (with-locals ((dot 0.0))
                              (for dth = (Range D 1) do
                                   (setf dot (+= dot (* (aref Q (+ q-base-idx dth)) (aref K (+ k-base-idx dth))))))
                              (let ((S (* dot scale))
                                    (new-max (max row-m S))
                                    (exp-prev (exp (- row-m new-max)))
                                    (exp-cur (exp (- S new-max)))
                                    (l-new (+ (* exp-prev row-l) exp-cur)))
                                (for dth1 = (Range D 1) do
                                     (setf (aref O (+ o-base-idx dth1))
                                           (/ (+ (* exp-cur (aref V (+ v-base-idx dth1))) (* exp-prev row-l (aref O (+ o-base-idx dth1)))) l-new)))
                                (setf row-m new-max
                                      row-l l-new))))
                       (setf
                        (aref M (+ i (* n (+ (* b head) h)))) row-m
                        (aref L (+ i (* n (+ (* b head) h)))) row-l)))))))})

;; TODO: Construct Graph w/ Forward
;; TODO: Make it forwardable
(defun test-flash-attention (&key (batch 10) (head 8) (n 128) (d 512))
  (let ((ast (sumreduce (caten/api:make-tensor (list 10 10)))))
    (print ast)))
;;    (caten/aasm:simplify-ast ast)
;;    (caten/air:pprint-graph ast)
;;    (caten/codegen/blueprint:print-blueprint ast t))
  (let ((ast (Gemm (caten/api:make-tensor (list 128 128)) (caten/api:make-tensor (list 128 128)) (caten/api:make-tensor (list 128 128)))))
    (print ast)
    (caten/aasm:simplify-ast ast)
    (caten/codegen/blueprint:print-blueprint ast t))
  (let ((ast
          (flash-attention
           (caten/api:make-tensor (list batch head n d))
           (caten/api:make-tensor (list batch head n d))
           (caten/api:make-tensor (list batch head n d))
           (caten/api:make-tensor (list batch head n d))
           (caten/api:make-tensor (list batch head n))
           (caten/api:make-tensor (list batch head n)))))
    (caten/aasm:simplify-ast ast)
    (caten/aasm:simplify-ast ast)
    (caten/codegen/blueprint:print-blueprint ast t)))

(progn
  @caten.jit () {
  (defun Gemm ((Pointer X Type (M N)) (Pointer Y Type (N K)) (Pointer Z Type (M K)))
    (for i = (Range 0 M) do
         (for j = (Range 0 K) do
              (with-locals ((acc 0.0))
                (for k = (Range 0 N) do
                     (setf acc (+ acc (* (aref X (+ (* N i) k)) (aref Y (+ (* K k) j))))))
                (setf (aref Z (+ (* K i) j)) acc)))))})

(progn
  @caten.jit () {
  (defun sumreduce ((Pointer X Type (A B)))
    (with-locals ((acc 0.0))
      (for idx = (Range (* A B) 1) do
           (setf acc (+= acc (aref X idx))))
      (setf (aref X 0) acc)))})
