(cl:in-package :cl-user)
(defpackage :caten/nn
  (:documentation "Caten Neural Network Frontends
Policy:
  - We only provide the official implementation to established and well used nn modules.
  - You have to ensure that each files works standalone. (Use 1 file, 1 package coding rule at caten/nn)
  - Each module should be tested well (we expected to have a ton of regression tests here); 1 file 1 test-suite.
  - TODO: Add test-helpers.lisp")
  (:use :cl :caten)
  (:export
   
   ))
(in-package :caten/nn)
