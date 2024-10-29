(defpackage :caten/polyhedral/documentation
  (:use :cl :caten/common.documentation))

(in-package :caten/polyhedral/documentation)

;; This package is user-extensible

(define-page ("caten/polyhedral" "packages/caten.polyhedral.md")
  (title "caten/polhedral")
  (subtitle "Automatic Parameter Scheduler")
  (body "Optimizes the following thing:

- Auto-Packing for vectorization
- Finding the best tiling
- Tune parallel, unroll

To enable this, set `AUTO_SCHEDULER=1`.
")
  (subtitle "Configuration"))
