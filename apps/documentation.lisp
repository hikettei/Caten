(defpackage :caten/apps.docs
  (:use :cl :caten/common.documentation))

(in-package :caten/apps.docs)

(define-page ("caten/apps" "packages/caten.apps.md")
  (title "Overview"))

(define-page ("GPT2" "packages/caten.apps.gpt2.md")
  (title "caten/apps.gpt2")) 
