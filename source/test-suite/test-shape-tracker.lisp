(in-package :caten/test-suite)

(deftest shrink+reshape-contiguous
  (testing "Reshape(Slice(...)) should create a copy"
    (ok
     (let ((caten/aasm:*default-order* :row)
           (a (ax+b `(2 2 8) 1 0)))
       (let ((b (!view a t t `(0 4)))
             (c (!view a t t `(4 8))))
         (let ((vals (buffer-value (tensor-buffer (proceed (!copy (!add (!reshape b `(2 2 1 4)) (!reshape c `(2 2 1 4)))))))))
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
    ((list* :slice _) `(let ((tmp ,(action->npview bind (cdr actions))))
                         (chain
                          tmp
                          ([] ,@(loop for subscript in (cdr (car actions))
                                      if (integerp subscript)
                                        collect subscript
                                      else
                                        collect
                                        (if (and (third subscript) (< (third subscript) 0))
                                            `(slice ,(second subscript) ,(car subscript) ,(third subscript))
                                            `(slice ,@subscript)))))))))

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
           (let ((succeed (and (= (length caten) (length numpy)) (every #'= caten numpy))))
             (if succeed
                 (ok succeed ,(format nil "~a" (action->caten-view 'x actions)))
                 (ok succeed (format nil "~a~%  caten=~a~%  numpy=~a" ',(action->caten-view 'x actions) caten numpy)))))))))

(defmacro define-view-binary-test ((name shapex shapey) (&rest x-actions) (&rest y-actions)
                                   &aux (x-actions (reverse x-actions)) (y-actions (reverse y-actions)))
  `(deftest ,name
     (testing ,(format nil "Testing: ~(~a~) + ~(~a~)" (action->caten-view 'x x-actions) (action->caten-view 'y y-actions))
       (let ((x (linspace ',shapex 1 0))
             (y (linspace ',shapey 1 0)))
         (let ((caten (proceed (!contiguous (!add ,(action->caten-view 'x x-actions) ,(action->caten-view 'y y-actions)) :force t)))
               (numpy
                 (let ((x (->numpy x))
                       (y (->numpy y)))
                   (np:add ,(action->npview 'x x-actions) ,(action->npview 'y y-actions)))))
           (ok (= (length (elements caten)) (length (np:reshape numpy -1)))
               (format nil "Total length: caten=~a numpy=~a" (length (elements caten)) (length (np:reshape numpy -1))))
           (let ((succeed (and (= (length (elements caten)) (length (np:reshape numpy -1))) (every #'= (elements caten) (np:reshape numpy -1)))))
             (if succeed
                 (ok succeed ,(format nil "~a+~a" (action->caten-view 'x x-actions) (action->caten-view 'y y-actions)))
                 (ok succeed (format nil "~a+~a~%  caten=~a~%  numpy=~a" ',(action->caten-view 'x x-actions) ',(action->caten-view 'y y-actions)
                                     caten (->caten (torch.from_numpy numpy)))))))))))
;; [TODO] Add kernel count tests for each define-view-test. The goal is to simplify the convnd jit kernel!
(define-view-test permute+reshape (3 3 3)
  (:permute 1 0 2)
  (:reshape 3 3 3)
  (:permute 1 0 2))

(define-view-test permute+slice (5 5)
  (:slice (1 3 -1) (1 3))
  (:permute 1 0)
  (:reshape 4))

(define-view-test permute+reshape-1 (3 4 5)
  (:permute 2 1 0)
  (:reshape 5 1 4 1 3 1))

(define-view-test permute+reshape-2 (3 4 5)
  (:permute 2 1 0)
  (:reshape 3 1 4 1 5 1))

(define-view-test permute+reshape-3 (3 4 5) ;; small repro for islated-test-for-convnd-failing-4-3
  (:permute 2 0 1)
  (:reshape 1 5 1 3 1 4 1 1 1))

(define-view-test permute+reshape-4 (3 4 5) ;; reshape+permute (not an ascending order) is the cause for isolated-test-for-convnd-failing-4-3?
  (:permute 2 0 1)
  (:reshape 5 3 4))

(define-view-test isolated-test-for-convnd-failing-4-1 (2 3 4 5 1 4 5 1)
  (:reshape 2 3 4 5 4 5))

(define-view-test isolated-test-for-convnd-failing-4-2 (2 3 4 5 1 4 5 1)
  (:reshape 2 3 4 5 4 5)
  (:permute 0 1 3 5 2 4))
;; So permute+reshape
(define-view-test isolated-test-for-convnd-failing-4-3 (2 3 4 5 1 4 5 1)
  (:reshape 2 3 4 5 4 5)
  (:permute 0 1 3 5 2 4)
  (:reshape  2 1 3 1 5 5 4 4))
;; permute+reshape+broadcast
(define-view-test isolated-test-for-convnd-failing-4-4 (2 3 4 5 1 4 5 1)
  (:reshape 2 3 4 5 4 5)
  (:permute 0 1 3 5 2 4)
  (:reshape   2 1 3 1 5 5 4 4)
  (:broadcast t t t 3 t t t t))

(define-view-test isolated-test-for-convnd-failing-4 (2 3 4 5 1 4 5 1)
  (:reshape 2 3 4 5 4 5)
  (:permute 0 1 3 5 2 4)
  (:reshape   2 1 3 1 5 5 4 4)
  (:broadcast t t t 3 t t t t)
  (:permute 0 1 2 5 4 3 6 7))

(define-view-test test-mha-failing-case (10 32 3 64)
  (:permute 0 1 3 2)
  (:reshape 10 32 8 3 8)
  (:permute 3 0 2 1 4))
#+(or nil)(setf rove::*debug-on-error* t) ;; <- C-c C-c to abort on the error
;; You can see the graph by doing:
;; - Disabling compose-views-from-graph (insert nil for the last line)
;; - Running: (->dot (runtime-graph (caten (forward (ConvND 3 2 `(4 4)) (make-tensor `(2 3 8 8))))))

(define-view-binary-test (convnd-failing-1 (2 3 4 4) (1 2 1 3 2 4 2 4))
    ((:reshape   1 2 1 3 1 4 1 4)
     (:broadcast 1 t 1 t 2 t 2 t))
    ())

(define-view-binary-test (convnd-failing-1-rev (1 2 1 3 2 4 2 4) (2 3 4 4))
    ()
    ((:reshape   1 2 1 3 1 4 1 4)
     (:broadcast 1 t 1 t 2 t 2 t)))

(define-view-binary-test (convnd-failing-2 (1 2 1 3 5 8 5 8) (2 3 36 36))
    ((:reshape 2 3 40 40)
     (:slice (0 2) (0 3) (0 36) (0 36)))
    ())

(define-view-binary-test (convnd-failing-3 (2 3 4 5 4 5) (2 3 4 5 1 4 5 1))
    ((:reshape 2 3 4 5 1 4 5 1))
    ())

(define-view-binary-test (convnd-failing-4 (2 3 4 5 1 4 5 1) (2 1 3 5 5 3 4 4))
    ((:reshape 2 3 4 5 4 5)
     (:permute 0 1 3 5 2 4)
     (:reshape   2 1 3 1 5 5 4 4)
     (:broadcast t t t 3 t t t t)
     (:permute 0 1 2 5 4 3 6 7))
    ())

(define-view-binary-test (convnd-failing-5 (3 3 4 4) (2 1 3 5 5 3 4 4))
    ((:reshape 1 1 3 1 1 3 4 4)
     (:permute 4 3 2 1 0 5 6 7))
    ())

(define-view-binary-test (convnd-failing-5-rev (2 1 3 5 5 3 4 4) (3 3 4 4)) ;; case 5 but rhs/lhs are swapped.
    ()
    ((:reshape 1 1 3 1 1 3 4 4)
     (:permute 4 3 2 1 0 5 6 7)))

(define-view-binary-test (convnd-failing-6 (2 1 3 5 5 3 4 4) (2 1 3 5 5 3 4 4))
    () ())

(define-view-binary-test (convnd-failing-7 (2 1 3 5 5 1 1 1) (2 1 3 5 5 1 1 1))
    ((:broadcast t t t t t 1 1 1))
    ())

(define-view-binary-test (convnd-failing-8 (3) (2 1 3 5 5 1 1 1))
    ((:reshape 1 3 1 1)
     (:broadcast 2 t 5 5))
    ((:reshape 2 3 5 5)))
;; Note(hikettei) Previously failing due to memory planner
(define-view-binary-test (mha-failing-case-binary-1 (10 32 3 64) (10 32 64 3))
    ((:permute 0 1 3 2)
     (:reshape 10 32 1 3 64))
    ((:permute 0 1 2 3)
     (:reshape 10 32 1 3 64)))

(deftest shape-tracker-shape-infer-failing-case
  (macrolet ((test (form)
               `(let ((x ,form))
                  (ok (equal (caten/apis::tr-shape (caten/apis::tensor-tr x)) (tensor-shape x))))))
    (test (!matmul (make-tensor `(10 3 32)) (!t (make-tensor `(96 32)))))
    (test (!add (!matmul (make-tensor `(10 3 32)) (!t (make-tensor `(96 32)))) (make-tensor `(96))))))
