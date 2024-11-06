(defpackage :caten/polyhedral/documentation
  (:use :cl :caten/common.documentation))

(in-package :caten/polyhedral/documentation)

(define-page ("caten/polyhedral" "packages/caten.polyhedral.md")
  (title "caten/polyhedral")
  (subtitle "Automatic Parameter Scheduler (Coming soon!)")
  (body "caten/polyhedral will provide a customizable polyhedral compiler for various backends

The goal is:
- Auto-Packing for Vectorization
- Finding the best tiling
- auto parallelization

To enable this, set `AUTO_SCHEDULER=1`.
"))
