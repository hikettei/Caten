#.(progn
    (defparameter *test-components*
      (list
       :graph/test-rewrite
       :api/test-shape
       ))
    (defun generate-components () (loop for component in *test-components* collect `(:file ,(princ-to-string component))))
    `(asdf:defsystem "caten.test-suite"
       :description "This is where unittest occur for Caten."
       :author      "hikettei <ichndm@gmail.com>"
       :depends-on ("rove" "py4cl" "trivia" "cl-ppcre")
       :components ,(generate-components)
       :serial t
       :perform
       (asdf:test-op
        (o s)
        (format t "Running test-suite ...~%")
        ,@(loop for component in *test-components*
                collect
                `(let ((pkg (find-package ,(intern (format nil "CATEN/TEST-SUITE/~a" component) "KEYWORD"))))
                   (format t ,(format nil "Running ~a ...~%" component))
                   (assert pkg () ,(format nil "A package CATEN/TEST-SUITE/~a is not found" component))
                   (uiop:symbol-call :rove :run-suite pkg) ;; style spec is available on latest commit of rove
                   )))))
