(defpackage :caten/apps.docs
  (:use :cl :caten/common.documentation))

(in-package :caten/apps.docs)

(define-page ("caten/apps" "packages/caten.apps.md")
  (title "Overview")
  (body "
Do you want to integrate Caten's features into your Common Lisp application?

With `caten/apps`, you can effortlessly experiment with inference using ready-made implementations of various deep learning models.

`caten/apps` is a suite of packages that implement pipelines to smoothly perform the following tasks:

- Automatically download pretrained weights

- Automatically optimize and compile models for inference

For instance, text generation inference provided by caten/apps.gpt2 can be accomplished in just two lines:

```lisp
(defparameter *model* (make-gpt2 :gpt2))
(gpt2-generate *model* \"What is the answer to life, and the world?\")
```

If you're considering contributing to Caten, we warmly welcome PRs that add new models to `caten/apps`. If you've implemented a model using Caten, please consider adding it to `caten/apps`! (Feel free to create a PR!)
"))

(define-page ("GPT2" "packages/caten.apps.gpt2.md")
  (title "caten/apps.gpt2")
  (doc/package 'caten/apps.gpt2))
