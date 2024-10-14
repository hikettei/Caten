(in-package :cl-user)

(defpackage :caten/workflow.test
  (:use :cl :rove :caten/workflow))

(in-package :caten/workflow.test)

(defmacro define-action-test (name &key (action) (lisp action) (inputs) (test 'equal))
  "Tests if `action` and `lisp` returns the same result."
  (let ((action-name (gensym (format nil "~a_ACTION" name)))
        (lisp-name   (gensym (format nil "~a_LISP" name))))
    `(progn
       (defaction ,action-name ,@action)
       (defun ,lisp-name ,@lisp)
       (deftest ,name
         (let* ((device
                  (testing (format nil "Setting the device = ~a" (ctx:getenv :JIT_BACKEND))
                    (caten/ajit:default-device (ctx:getenv :JIT_BACKEND))))
                (action-results
                  (testing "Running the action"
                    (multiple-value-list (run-action device ',action-name ,@inputs))))
                (lisp-results
                  (testing "Running the lisp"
                    (multiple-value-list (,lisp-name ,@inputs)))))
           (testing "Comaring the results"
             (ok (funcall #',test action-results lisp-results))))))))

;; Binary
(macrolet ((def (name op x y)
             `(define-action-test ,name
                :action ((a b)
                         (declare (type :float32 a b))
                         (,op a b))
                :lisp ((a b) (,op a b))
                :inputs (,x ,y))))
  (def binary-add + 1.0 2.0)
  (def binary-sub - 1.0 2.0)
  (def binary-mul * 1.0 2.0)
  (def binary-div / 1.0 2.0))
