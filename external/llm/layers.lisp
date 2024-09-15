(in-package :caten/llm)

(defmodel (FeedForward (dim hidden-dim))
    ((c-fc   (Linear dim hidden-dim))
     (c-proj (Linear hidden-dim dim))))

(defmethod call ((model FeedForward) &rest inputs)
  (multiple-value-bind (x) (apply #'values inputs)
    (with-slots ((c-fc c-fc) (c-proj c-proj)) model
      (call c-proj (!gelu (call c-fc x))))))
