(in-package :cl-user)

(defpackage :caten/workflow
  (:use :cl :caten/air :trivia)
  (:import-from
   :caten/lang
   #:Context
   #:ctx-render
   #:ctx-render-function
   #:ctx-compile
   #:ctx-get-code
   #:make-context-from-list
   #:ctx-run
   )
  (:export
   #:defaction
   #:run-action
   ))
