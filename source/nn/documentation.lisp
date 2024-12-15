(in-package :caten/nn)

(docs:define-page ("caten/nn" "packages/caten.nn.md")
  (docs:title "caten/nn"))

(docs:define-page ("Activations" "packages/caten.nn.activations.md")
  (docs:title "Activations")
  (docs:doc/function "!sigmoid" #'!sigmoid)
  (docs:doc/function "!hard-sigmoid" #'!hard-sigmoid)
  (docs:doc/function "!relu" #'!relu)
  (docs:doc/function "!leaky-relu" #'!leaky-relu)
  (docs:doc/function "!log-softmax" #'!log-softmax)
  (docs:doc/function "!elu" #'!elu)
  (docs:doc/function "!relu6" #'!relu6)
  (docs:doc/function "!softmax" #'!softmax)
  (docs:doc/function "!softplus" #'!softplus)
  (docs:doc/function "!softsign" #'!softsign)
  (docs:doc/function "!softshrink" #'!softshrink)
  (docs:doc/function "!celu" #'!celu)
  (docs:doc/function "!silu" #'!silu)
  (docs:doc/function "!logsigmoid" #'!logsigmoid)
  (docs:doc/function "!gelu" #'!gelu)
  (docs:doc/function "!selu" #'!selu)
  (docs:doc/function "!mish" #'!mish)
  (docs:doc/function "!hardswish" #'!hardswish)
  (docs:doc/function "!hardtanh" #'!hardtanh)
  (docs:doc/function "!softmin" #'!softmin))

(docs:define-page ("Convolutions" "packages/caten.nn.convolutions.md")
  (docs:title "Convolutions")
  (docs:doc/class "ConvND" 'convnd)
  (docs:doc/function "!convnd" #'!convnd))

(docs:define-page ("Embeddings" "packages/caten.nn.embeddings.md")
  (docs:title "Embeddings")
  (docs:doc/class "Embedding" 'embedding))

(docs:define-page ("Linear" "packages/caten.nn.linears.md")
  (docs:title "Linear")
  (docs:doc/class "Linear" 'linear))

(docs:define-page ("Normalizations" "packages/caten.nn.normalizations.md")
  (docs:title "Normalizations")
  (docs:doc/class "BatchNorm" 'batchnorm)
  (docs:doc/function "!batch-norm" #'!batch-norm)
  (docs:doc/class "LayerNorm" 'layernorm)
  (docs:doc/function "!layer-norm" #'!layer-norm)
  (docs:doc/class "RMSNorm" 'rmsnorm)
  (docs:doc/function "!rms-norm" #'!rms-norm))

(docs:define-page ("Padding" "packages/caten.nn.padding.md")
  (docs:title "Padding")
  (docs:body "TODO"))

(docs:define-page ("Pooling" "packages/caten.nn.pooling.md")
  (docs:title "Pooling")
  (docs:body "TODO"))

(docs:define-page ("Encoding" "packages/caten.nn.encoding.md")
  (docs:title "Encoding")
  (docs:body "TODO"))

