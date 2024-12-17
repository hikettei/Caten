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
")
  (title "References")
  (body "
- TODO: More (PolyTOPS, TensorComprehensions, AKG, Tiramisu etc...)
- https://www.slideshare.net/slideshow/introduction-to-polyhedral-compilation/70482946
- https://inria.hal.science/hal-01253322v1/document
- https://repository.dl.itc.u-tokyo.ac.jp/record/6112/files/48136454.pdf
"))
