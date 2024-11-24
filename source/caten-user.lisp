(defpackage :caten-user
  (:documentation "REPL Playground for Caten")
  (:use :cl :caten :caten/aasm :caten/air :caten/codegen :caten/nn :caten/air
   :caten/avm :caten/common.documentation
   :caten/codegen/scheduler :caten/codegen/shape-inference :caten/codegen/rewriting-rules))
(in-package :caten-user)
