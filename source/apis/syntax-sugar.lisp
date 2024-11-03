(in-package :caten/apis)
;;(defpackage :caten/ops)
;; TODO: Doing an operator overloading at `c::+` and `c::-` (is it a good idea?)
;; Note that doing this in caten/apis package will decrease the performance even the generid methods are inlined.

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Transform: Experimental Notation for manipulating the symbolic shape.
  ;; (WIP)
  (defun %parse-tf (tf)
    "Tf: Either of (~ A B C) or (~ A B C -> A B C)"
    (declare (type list tf))
    (let* ((pos (position "->" tf :test #'string= :key #'(lambda (x) (format nil "~a" x))))
	   (cnt (count "->" tf :test #'string= :key #'(lambda (x) (format nil "~a" x)))))
      (assert (<= cnt 1) () "Failed to compile the shape transformer:
Too many arrows (->).
Follow the either of:
  - (~ A B C)
  - (~ A B C -> A B C)")
      (values (subseq tf 0 pos) (when pos (subseq tf (1+ pos))))))

  (defun %->transform (before after)
    (with-gensyms (thing)
      `(lambda (,thing)
	 (declare (type list ,thing))
	 (match ,thing
	   ((list ,@before) ,@after)
	   (_ (error "Transform"))))))
  
  (defun %->shape (before)
    (with-gensyms (thing count)
      `(lambda (,thing)
	 (declare (type list ,thing))
	 (loop for ,count upfrom 0 below (length ,thing)
	       collect (or (nth ,count ',before) (nth ,count ,thing)))))))

(defstruct Transform (before nil :type list) (after nil :type list) (caller (error "caller should occur")))
(defmethod apply-transform ((op transform) list) (funcall (transform-caller op) list))
;; The code below is out-of-date and not supported. but i think the idea is really good as proven in cl-waffe2 ...
(defmacro ~ (&rest transformation)
  "TODO: Docs
(!reshape x (~ A B C -> (!* A B C)))
(!view x (~ 0))"
  (warn "The api ~~ is deprecated")
  (multiple-value-bind (before after) (%parse-tf transformation)
    `(make-transform :before ',before :after ',after :caller ,(if after (%->transform before after) (%->shape before)))))

;; ~~ Einsum ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defparameter +ascii-letter+ "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defun s-eq (a b)
  (declare (type symbol a b))
  (string= (symbol-name a) (symbol-name b)))
(defun split-list (list key)
  (let ((pos (position key list :test #'s-eq)))
    (assert pos () "split-list: The key ~a is not found in ~a" key list)
    (values (subseq list 0 pos) (subseq list (1+ pos)))))
(defun list-has (val list) (find val list :test #'s-eq))
(defun alphabet-p (char) (and (standard-char-p char) (alpha-char-p char)))
(defun parse-formula (formula &rest operands)
  "Uses ~ instead of ..."
  (declare (type list formula operands))
  (if (list-has '-> formula)
      (multiple-value-bind (bf aft) (split-list formula '->)
        (values (map 'list #'princ-to-string bf) (map 'list #'princ-to-string aft)))
      (values
       (map 'list #'princ-to-string formula)
       (list
        (with-output-to-string (out)
          (loop for char across (sort (princ-to-string formula) #'char-lessp)
                if (alphabet-p char) do (princ char out)))))))

(defun argsort (x sort)
  (let ((indices (loop for i from 0 below (length x) collect i)))
    (stable-sort indices sort :key (lambda (i) (elt x i)))))

;; [TODO]
;; - einsum is not as optimized as other apis, so we need to optimize it.
;; - Decompose several matmuls https://zenn.dev/termoshtt/articles/einsum-derive#%E5%88%86%E8%A7%A3%E9%A0%86%E5%BA%8F%E3%81%A8%E8%A8%88%E7%AE%97%E9%87%8F
(defun einsum (formula &rest operands)
  (declare (type list formula operands))
  (apply #'verify-formula formula operands)
  ;; [TODO] Einsum notation i s used as verify-formula?
  (warn "Einsum is deprecated")
  ;; [TODO] Optimize+Improve
  (multiple-value-bind (inputs outputs) (apply #'parse-formula formula operands)
    (assert (= (length inputs) (length operands)) () "einsum: The number of input operands is not matched with the formula")
    (assert (= (length outputs) 1) () "einsumg: The number of output operands is zero or one.")
    (let ((letter-vals (make-hash-table :test #'equal)))
      (loop for tensor in operands
            for input in inputs
            do (loop for axis in (map 'list #'princ-to-string input)
                     for shape in (tensor-shape tensor)
                     do (setf (gethash axis letter-vals) shape)))
      (let* ((letter-keys (sort (hash-table-keys letter-vals) #'(lambda (x y) (char-lessp (char x 0) (char y 0)))))
             (lhs (loop for s in inputs
                        for base = (loop for c across s for nth upfrom 0 collect (cons nth c))
                        collect (sort base #'char-lessp :key #'cdr)))
             (xs (loop for x in operands
                       for lh in lhs
                       for order = (map 'list #'car lh)
                       for chars = (map 'list (compose #'princ-to-string #'cdr) lh)
                       collect
                       (!expand
                        (!reshape
                         (!permute x order)
                         (loop for key in letter-keys
                               for val = (gethash key letter-vals)
                               if (find key chars :test #'equalp)
                                 collect val
                               else
                                 collect 1))
                        (loop for key in letter-keys
                              collect (gethash key letter-vals)))))
             (rhs-letter-order (argsort (coerce (car outputs) 'list) #'char-lessp))
             (rhs-order (argsort rhs-letter-order #'<))
             (reduce-axes (loop for axis upfrom 0
                                for letter in letter-keys
                                unless (find (char letter 0) (coerce (car outputs) 'list) :test #'char=)
                                  collect axis))
             (output-shape (loop for key across (car outputs)
                                 collect (gethash (princ-to-string key) letter-vals)))
             (out (!reshape (!sum (apply #'!* xs) :axis reduce-axes) output-shape)))
        (!permute out rhs-order)))))
