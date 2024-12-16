(defpackage :caten/polyhedral/documentation
  (:use :cl :caten/common.documentation))

(in-package :caten/polyhedral/documentation)

(define-page ("caten/polyhedral" "packages/caten.polyhedral.md")
  (title "caten/polyhedral")
  (subtitle "Advanced Auto Scheduler (Coming soon!)")
  (body "caten/polyhedral will provide a customizable polyhedral compiler for various backends

This package will also provide the following advanced auto scheduling algorithm compared to `caten/codegen`.

- [ ] Maximizing the outermost loop coincidence
- [ ] Tiling
- [ ] Packing
- [ ] Classical Loop Transformation (e.g.: Loop Permutation, Loop Interchange, Loop Unrolling, etc
- [ ] Complicated Kernel Fusion (e.g.: Matmul+Normalization, ConvND+Activation+Pooling, etc)

...

To enable this, set `AUTO_SCHEDULER=1`.
"))
