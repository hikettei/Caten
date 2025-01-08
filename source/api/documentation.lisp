(in-package :caten/api)

(docs:define-page ("caten/api" "packages/caten.api.md")
  (docs:title "Overview")
  (docs:body "Welcome to Caten/APIs")
  (docs:body "WIP")
  (docs:body "proceed is a function that evaluates a computational graph.")
  (docs:example-repl "(proceed (make-tensor `(3 3) :initial-element 1.0))")
  (docs:body "`ctx:with-contextvar` is a macro that sets the context variable. `BACKEND=CLANG` to use JIT, `JIT_DEBUG=4` to see the generated code. `DOT=1` to debug the pattern matcher on your browser. (need graphviz)")
  (docs:example-repl "(ctx:with-contextvar (:BACKEND \"CLANG\" :jit_debug 4 :dot 0)
  (caten (forward (Embedding 10 10) (make-tensor `(b c)))))"))

(docs:define-page ("Tensor" "packages/caten.api.tensor.md")
  (docs:title "Tensor")
  (docs:doc/struct "Tensor" 'Tensor)
  (docs:doc/function "make-tensor" #'make-tensor)
  (docs:doc/function "make-scalar" #'make-scalar)
  (docs:doc/function "grad" #'grad)
  (docs:doc/function "shape" #'shape)
  (docs:doc/function "ndim" #'ndim)
  (docs:doc/function "dtype-of" #'dtype-of)
  (docs:doc/function "order" #'order)
  (docs:doc/function "caten" #'caten)
  (docs:doc/function "tensor-graph" #'tensor-graph)
  (docs:doc/function "tensor-lowered-graph" #'tensor-lowered-graph)
  (docs:body "
### [method] forward

```
(forward runtime &rest params)
```

Compute the forward pass of the compiled computational graph (GraphRuntime). The params are additional parameters that are used to initialize the GraphRuntime variable table, passed in the following format:


- `(symbol . number)` Loaded as a `*default-int*` scalar tensor, used to determine the shape of dynamic shaped tensors.
- `(symbol . buffer)` or `(symbol . tensor)` Used to assign the initial elements of the tensor specified by the name in `form` in `(make-tensor ... :from x)`

or `(forward runtime place1 value1 place2 value2 ...)` is added recently.

Here's an example.
")
  (docs:subsection "Examples")
  (docs:example-code "
(let ((model (caten (!randn `(:a :b)))))
  (forward model :a 10 :b 10))"
                     :title "Example of forward with dynamic shaped")
  
  (docs:body "
### [method] backward
```
(forward runtime &optional prev-dout)
```

Compute the backward pass of the compiled computational graph (GraphRuntime). Note that the `prev-dout` is ignored. Forward pass must be computed first. Gradients are automatically reset to zero before the forward pass.
")
  (docs:doc/function "proceed" #'proceed)
  (docs:doc/macro "with-no-grad" 'with-no-grad)
  (docs:doc/variable "*inference-mode*" '*inference-mode*)
  (docs:doc/macro "with-inference-mode" 'with-inference-mode)
  
  (docs:section "Examples")
  (docs:subsection "Tensor Creation")
  (docs:example-repl "(make-tensor `(30 30))")
  (docs:subsection "Realize")
  (docs:example-repl "(proceed (make-tensor `(30 30)))")
  (docs:subsection "Creating a computational graph (Lazy)")
  (docs:example-repl "(!add (ax+b `(3 3) 0 1) (ax+b `(3 3) 0 1))")
  (docs:subsection "Evaluating a computational graph.")
  (docs:example-repl "(proceed (!add (ax+b `(3 3) 0 1) (ax+b `(3 3) 0 1)))")
  (docs:subtitle "Floating Features")
  (docs:doc/function "inf" #'inf)
  (docs:example-repl "(inf)")
  (docs:example-repl "(proceed (!full `(3 3) (inf)))")
  (docs:doc/function "-inf" #'-inf)
  (docs:example-repl "(-inf)")
  (docs:example-repl "(proceed (!full `(3 3) (-inf)))")
  (docs:doc/function "nan" #'nan)
  (docs:example-repl "(nan)")
  (docs:example-repl "(proceed (!full `(3 3) (nan)))")
  (docs:doc/function "float-type-of" #'float-type-of))

(docs:define-page ("Differentiable Ops" "packages/caten.api.differentiable_ops.md")
  (docs:title "Func")
  (docs:section "Func Class")
  (docs:doc/class "Func" 'Func)
  (docs:doc/generic "lower" #'lower)
  (docs:doc/generic "forward" #'forward)
  (docs:doc/generic "backward" #'backward)
  (docs:section "Differentiable Ops (built_in)")
  (macrolet ((def (name api example)
               `(progn
                  (docs:doc/function ,name ,api)
                  (docs:example-repl ,example))))
    (def "!identity" #'!identity "(proceed (!identity (make-tensor `(3 3) :initial-element 1.0)))")
    (def "!view" #'!view "(proceed (!contiguous (!view (ax+b `(10 10) 1 0) `(3 6) `(3 6))))")
    (def "!permute" #'!permute "(proceed (!contiguous (!permute (ax+b `(10 10) 1 0) `(1 0))))")
    (def "!t" #'!t "(proceed (!contiguous (!t (ax+b `(10 10) 1 0))))")
    (def "!transpose" #'!transpose "(proceed (!contiguous (!transpose (ax+b `(10 10) 1 0) 1 0)))")
    (def "!contiguous" #'!contiguous "(proceed (!contiguous (ax+b `(10 10) 1 0)))")
    (def "!copy" #'!copy "(proceed (!copy (ax+b `(10 10) 1 0)))")
    (def "!reshape" #'!reshape "(proceed (!reshape (ax+b `(10 10) 1 0) `(5 20)))")
    (def "!flatten" #'!flatten "(proceed (!flatten (make-tensor `(3 3 3 3)) :axis 1))")
    (def "!uprank" #'!uprank "(proceed (!uprank (ax+b `(10 10) 1 0) 2))")
    (def "!repeat" #'!repeat "(proceed (!repeat (ax+b `(1 10) 1 0) 10 1))")
    (def "!expand" #'!expand "(proceed (!expand (ax+b `(1 10) 1 0) `(10 10)))")
    (def "!move" #'!move "(proceed (!move (ax+b `(10 10) 0 0) (ax+b `(10 10) 0 2)))")
    (def "!assign" #'!assign "(proceed (!assign (ax+b `(10 10) 0 0) (ax+b `(10 10) 0 2)))")
    (def "!add" #'!add "(proceed (!add (ax+b `(10 10) 1 0) (ax+b `(10 10) 1 0)))")
    (def "!sub" #'!sub "(proceed (!sub (ax+b `(10 10) 1 0) (ax+b `(10 10) 1 0)))")
    (def "!mul" #'!mul "(proceed (!mul (ax+b `(10 10) 1 0) (ax+b `(10 10) 1 0)))")
    (def "!div" #'!div "(proceed (!div (ax+b `(10 10) 1 1) (ax+b `(10 10) 1 1)))")
    (def "!mod" #'!mod "(proceed (!mod (ax+b `(10 10) 1 1 :dtype :int32) (ax+b `(10 10) 1 1 :dtype :int32)))")
    (def "!idiv" #'!idiv "(proceed (!idiv (ax+b `(10 10) 3 1 :dtype :uint32) (ax+b `(10 10) 0 2 :dtype :uint32)))")
    (def "!maximum" #'!maximum "(proceed (!maximum (rand `(3 3)) (randn `(3 3))))")
    (def "!minimum" #'!minimum "(proceed (!minimum (rand `(3 3)) (randn `(3 3))))")
    (def "!gcd" #'!gcd "(ctx:with-contextvar (:backend \"lisp\") (proceed (!gcd (iconst 8) (iconst 4))))")
    (def "!lcm" #'!lcm "(ctx:with-contextvar (:backend \"lisp\") (proceed (!lcm (iconst 8) (iconst 4))))")
    (def "!exp" #'!exp "(proceed (!exp (ax+b `(10 10) 0.01 0.0)))")
    (def "!log" #'!log "(proceed (!log (ax+b `(10 10) 0.01 0.001)))")
    (def "!sqrt" #'!sqrt "(proceed (!sqrt (ax+b `(10 10) 0.01 0.0)))")
    (def "!neg" #'!neg "(proceed (!neg (ax+b `(10 10) 0.01 0.0)))")
    (def "!recip" #'!recip "(proceed (!recip (ax+b `(10 10) 0.01 0.1)))")
    (def "!square" #'!square "(proceed (!square (ax+b `(10 10) 0.01 0.1)))")
    (def "!rsqrt" #'!rsqrt "(proceed (!rsqrt (ax+b `(10 10) 0.01 0.1)))")
    (def "!signum" #'!signum "(proceed (!signum (ax+b `(10 10) 0.02 -0.1)))")
    (def "!gid" #'!gid "(proceed (!gid (make-tensor `(3 3)) -1))")
    (def "!normalize-axis" #'!normalize-axis "(proceed (!normalize-axis 3 -1))")
    (def "!abs" #'!abs "(proceed (!abs (ax+b `(10 10) 0.02 -0.1)))")
    (def "!>" #'!> "(proceed (!where (!> (rand `(3 3)) (randn `(3 3))) (iconst 1) (iconst 0)))")
    (def "!<" #'!< "(proceed (!where (!< (rand `(3 3)) (randn `(3 3))) (iconst 1) (iconst 0)))")
    (def "!>=" #'!>= "(proceed (!where (!>= (rand `(3 3)) (randn `(3 3))) (iconst 1) (iconst 0)))")
    (def "!<=" #'!<= "(proceed (!where (!<= (rand `(3 3)) (randn `(3 3))) (iconst 1) (iconst 0)))")
    (def "!eq" #'!eq "(proceed (!where (!eq (rand `(3 3)) (randn `(3 3))) (iconst 1) (iconst 0)))")
    (def "!neq" #'!neq "(proceed (!where (!neq (rand `(3 3)) (randn `(3 3))) (iconst 1) (iconst 0)))")
    (def "!and" #'!and "(proceed (!and (iconst 5) (iconst 3)))")
    (def "!xor" #'!xor "(proceed (!xor (iconst 5) (iconst 3)))")
    (def "!or" #'!or "(proceed (!or (iconst 5) (iconst 3)))")
    (def "!where" #'!where "(proceed (!where (!eq (rand `(3 3)) (randn `(3 3))) (iconst 1) (iconst 0)))")
    (def "!const" #'!const "(proceed (!const (make-tensor `(3 3)) 1.0))"))
  (docs:doc/generic "!index-components" #'!index-components)
  (docs:example-repl "(proceed (!index-components (make-tensor `(3 3))))")
  (docs:example-repl "(proceed (!index-components `(1 3)))"))

(docs:define-page ("Module" "packages/caten.api.module.md")
  (docs:title "Module")
  (docs:doc/macro "defmodule" 'defmodule)
  (docs:section "Modules (built_in)")
  (macrolet ((def (name op example)
               `(progn
                  (docs:doc/function ,name ,op)
                  (docs:example-repl ,example))))
    (def "!sum" #'!sum "(proceed (!sum (ax+b `(10 10) 1 0)))")
    (def "!mean" #'!mean "(proceed (!mean (ax+b `(10 10) 1 0)))")
    (def "!max" #'!max "(proceed (!max (ax+b `(10 10) 1 0)))")
    (def "!min" #'!min "(proceed (!min (ax+b `(10 10) 1 0)))")
    (def "!matmul" #'!matmul "(proceed (!matmul (rand `(32 64)) (rand `(64 128))))")
    (def "!sinh" #'!sinh "(proceed (!sinh (randn `(3 3))))")
    (def "!cosh" #'!cosh "(proceed (!cosh (randn `(3 3))))")
    (def "!tanh" #'!tanh "(proceed (!tanh (randn `(3 3))))")
    (def "!cos" #'!cos "(proceed (!cos (randn `(3 3))))")
    (def "!tan" #'!tan "(proceed (!tan (randn `(3 3))))")
    (def "!log2" #'!log2 "(proceed (!log2 (ax+b `(3 3) 1 0.1)))")
    (def "!exp2" #'!exp2 "(proceed (!exp2 (ax+b `(3 3) 1 0.1)))")
    (def "!expt" #'!expt "(proceed (!expt (ax+b `(3 3) 1 0.1) 2))")
    (def "!truncate" #'!truncate "(proceed (!truncate (randn `(3 3))))")
    (def "!ceiling" #'!ceiling "(proceed (!ceiling (randn `(3 3))))")
    (def "!floor" #'!floor "(proceed (!floor (randn `(3 3))))")
    (def "!triu" #'!triu "(proceed (!triu (rand `(3 3))))")
    (def "!tril" #'!tril "(proceed (!tril (rand `(3 3))))")
    (def "!argmax" #'!argmax "(proceed (!argmax (rand `(3 3))))")
    (def "!argmin" #'!argmin "(proceed (!argmin (rand `(3 3))))")
    (def "!clip" #'!clip "(proceed (!clip (randn `(3 3)) -0.3 0.3))")
    (def "!erf" #'!erf "(proceed (!erf (randn `(3 3))))")))

(docs:define-page ("Models" "packages/caten.api.models.md")
  (docs:title "Models")
  (docs:body "TODO")
  (docs:doc/macro "defcall" 'defcall)
  (docs:doc/macro "defsequence" 'defsequence)
  (docs:doc/function "asnode" 'asnode)
  )

(docs:define-page ("Initializers" "packages/caten.api.initializers.md")
  (docs:title "Initializers")
  (docs:doc/function "set-manual-seed" #'set-manual-seed)
  (docs:doc/macro "with-manual-seed" 'with-manual-seed)
  (macrolet ((def (name func example1 example2)
               `(progn
                  (docs:doc/function ,name ,func)
                  (docs:example-repl ,example1 :title "Lazy")
                  (docs:example-repl ,example2 :title "Static"))))
    (def "ax+b" #'ax+b "(proceed (ax+b `(3 3) 2 1))" "(linspace `(3 3) 1 0)")
    (def "!rand" #'!rand "(proceed (!rand `(3 3)))" "(rand `(3 3))")
    (def "!normal" #'!normal "(proceed (!normal `(3 3) :mean 0.0 :std 2.0))" "(normal `(3 3) :mean 0.0 :std 1.0)")
    (def "!randn" #'!randn "(proceed (!randn `(3 3)))" "(randn `(3 3))")
    (def "!uniform" #'!uniform "(proceed (!uniform `(3 3) :low 1.0 :high 2.0))" "(uniform `(3 3) :low 1.0 :high 2.0)")
    (def "!randint" #'!randint "(ctx:with-contextvar (:backend \"clang\") (proceed (!randint `(3 3) :low 1 :high 10)))" "(ctx:with-contextvar (:backend \"clang\") (randint `(3 3) :low 1 :high 10 :dtype :int32))"))
  (docs:doc/function "!full" #'!full)
  (docs:example-repl "(proceed (!full `(3 3) (inf)))")
  (docs:doc/function "xavier-uniform" #'xavier-uniform)
  (docs:doc/function "xavier-gaussian" #'xavier-gaussian))

(docs:define-page ("Facet API" "packages/caten.api.facet_api.md")
  (docs:title "Facet API")
  (docs:body "We are going to see how to access the Tensor as a lisp object.")
  (docs:doc/generic "change-facet" #'change-facet)
  (docs:example-repl "(change-facet 1 :tensor)")
  (docs:example-repl "(change-facet '((1.0 2.0 3.0) (4.0 5.0 6.0)) :tensor)")
  (docs:example-repl "(change-facet #(1 2 3) :tensor)")
  (docs:example-repl "(change-facet (rand `(2 2)) :simple-array)")
  (docs:example-repl "(change-facet (rand `(2 2)) :array)")
  (docs:doc/macro "with-facet" 'with-facet)
  (docs:doc/macro "with-facets" 'with-facets)
  (docs:body "If you want to access an individual element of Tensor, it is wiser to convert it into an Array. The following code snippet initializes the diagonal of a Tensor to 0.0 without creating a copy:")
  (docs:example-repl "(let ((x (rand `(3 3))))
  (with-facet (a (x :direction :array))
    (setf (aref a 0 0) 0.0
          (aref a 1 1) 0.0
          (aref a 2 2) 0.0))
  (print x))"))

(docs:define-page ("ShapeTracker" "packages/caten.api.shapetracker.md")
  (docs:title "ShapeTracker")
  (docs:body "TODO")
  )

(docs:define-page ("State Dict" "packages/caten.api.state-dict.md")
  (docs:title "State-Dict")
  (docs:doc/struct "State-Dict" 'State-Dict)
  (docs:doc/generic "->state-dict" #'->state-dict)
  (docs:doc/function "get-state-dict" #'get-state-dict)
  (docs:doc/function "load-state-dict" #'load-state-dict)
  (docs:subtitle "Example: Transformer")
  (docs:example-repl "(progn (ql:quickload :caten/llm) (get-state-dict (caten/llm:Transformer 32 2 2 1e-5 32)))"))
