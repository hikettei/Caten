(in-package :caten/test-suite)

(deftest shrink+reshape-contiguous
  (testing "Reshape(Slice(...)) should create a copy"
    (ok
     (let ((caten/aasm:*default-order* :row)
           (a (ax+b `(2 2 8) 1 0)))
       (let ((b (!view a t t `(0 4)))
             (c (!view a t t `(4 8))))
         (let ((vals (buffer-value (tensor-buffer (proceed (!add (!reshape b `(2 2 1 4)) (!reshape c `(2 2 1 4))))))))
           (ok (every #'= vals #(4.0 6.0 8.0 10.0 20.0 22.0 24.0 26.0 36.0 38.0 40.0 42.0 52.0 54.0 56.0 58.0)))))))))

(eval-when (:compile-toplevel :execute :load-toplevel)

(defun action->caten-view (bind actions)
  (when (null actions) (return-from action->caten-view bind))
  (trivia:ematch (car actions)
    ((list* :reshape _) `(!reshape ,(action->caten-view bind (cdr actions)) ,@(cdr (car actions))))
    ((list* :permute _) `(!permute ,(action->caten-view bind (cdr actions)) ,@(cdr (car actions))))
    ((list* :broadcast _) `(!view ,(action->caten-view bind (cdr actions))
                                  ,@(loop for i in (cdr (car actions))
                                          if (eql i t)
                                            collect t
                                          else
                                            collect `(list :~ ,i))))
    ((list* :slice _) `(!view ,(action->caten-view bind (cdr actions))
                              ,@(loop for subscript in (cdr (car actions))
                                      if (integerp subscript)
                                        collect subscript
                                      else
                                        collect
                                        (if (and (third subscript) (< (third subscript) 0))
                                            `(list ,(second subscript) ,(car subscript) ,(third subscript))
                                            `(list ,@subscript)))))))

(defun action->npview (bind actions)
  (when (null actions) (return-from action->npview bind))
  (trivia:ematch (car actions)
    ((list* :reshape _) `(np:reshape   ,(action->npview bind (cdr actions)) (list ,@(cdr (car actions)))))
    ((list* :permute _) `(np:transpose ,(action->npview bind (cdr actions)) (list ,@(cdr (car actions)))))
    ((list* :broadcast _)
     `(let ((val ,(action->npview bind (cdr actions))))
        (np:broadcast_to
         val
         (list
          ,@(loop for s in (cdr (car actions))
                  for nth upfrom 0
                  if (eql s t)
                    collect `(nth ,nth (np:shape val))
                  else
                    collect s)))))
    ((list* :slice _) `(chain
                        ,(action->npview bind (cdr actions))
                        ([] ,@(loop for subscript in (cdr (car actions))
                                    if (integerp subscript)
                                      collect subscript
                                    else
                                      collect
                                      (if (and (third subscript) (< (third subscript) 0))
                                          `(slice ,(second subscript) ,(car subscript) ,(third subscript))
                                          `(slice ,@subscript))))))))

) ;; eval-when

(defmacro define-view-test (name shape &rest actions &aux (actions (reverse actions)))
  "Translates actions into Caten/PyTorch codes, checking they have the same result."
  `(deftest ,name
     (testing ,(format nil "Testing: ~(~a~)" (action->caten-view 'x actions))
       (let ((x (linspace ',shape 1 0)))
         (let ((caten (elements (proceed (!contiguous ,(action->caten-view 'x actions) :force t))))
               (numpy
                 (let ((x (->numpy x)))
                   (np:reshape ,(action->npview 'x actions) -1))))
           (ok (and (= (length caten) (length numpy)) (every #'= caten numpy))
               (format nil "~a~%  caten=~a~%  numpy=~a" ',(action->caten-view 'x actions) caten numpy)))))))

(define-view-test permute+reshape (3 3 3)
  (:permute 1 0 2)
  (:reshape 3 3 3)
  (:permute 1 0 2))

(define-view-test permute+slice (5 5)
  (:slice (1 3 -1) (1 3))
  (:permute 1 0)
  (:reshape 4))

(define-view-test bc (1 10)
  (:broadcast 10 t))
;; [TODO] More Compose Case ...

