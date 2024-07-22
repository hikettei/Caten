(in-package :caten/api)

(defpattern number (x) `(guard ,x (numberp ,x)))
(defsimplifier
    (fold-constant)
    ((:Load ((:Allocate () :nrank 0)) :value (number x)) -> (:Const (x)))

    ;; 最後にTmpConst -> Load Allocateにする
    )
