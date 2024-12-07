(defpackage :caten/apps.gpt2
  (:documentation "
Implements GPT2 text generation from the pre-trained model.

Note: Currently still experimental and may not work as expected!

### [function] make-gpt2

```
(make-gpt2 model-type &key max-seq-len)
```

Creates a compiled GPT2 model from the pre-trained gguf file. The `model-type` must be one of `:gpt2`, `:gpt2-medium`, `:gpt2-large`, or `:gpt2-xl`. The `max-seq-len` is the maximum sequence length for the model. Since GPT2 is a heavy model, you should consider `JIT=1` as a prerequisite.

The pretrained model is downloaded from the following HuggingFace repository:

```lisp
(defun url (model-type) (format nil \"https://huggingface.co/hikettei/gpt2-gguf/resolve/main/~(~a~)-f32.gguf?download=true\" model-type))
```

### [function] gpt2-inference

```
(gpt2-inference model input)
```

Takes a compiled GPT2 model and a string input, and generates a text output.
")
  (:use :cl :caten/apis :caten/llm :caten/gguf :float-features)
  (:export :make-gpt2 :gpt2-generate))

(in-package :caten/apps.gpt2)

(defstruct (GPT2
            (:constructor %make-gpt2 (model tokenizer max-seq-len)))
  (model model :type caten/avm:AVM) (tokenizer tokenizer :type Tokenizer) (max-seq-len max-seq-len :type fixnum))

(defstruct Params n-layers n-heads dim (norm-eps 1e-5) (vocab-size 50257))

(defparameter *gpt2* (make-params :n-layers 12 :n-heads 12 :dim 768))
(defparameter *gpt2-medium* (make-params :n-layers 24 :n-heads 16 :dim 1024))
(defparameter *gpt2-large* (make-params :n-layers 36 :n-heads 20 :dim 1280))
(defparameter *gpt2-xl* (make-params :n-layers 48 :n-heads 25 :dim 1600))

(defun get-param (model-type)
  (ecase model-type
    (:gpt2 *gpt2*)
    (:gpt2-medium *gpt2-medium*)
    (:gpt2-large *gpt2-large*)
    (:gpt2-xl *gpt2-xl*)))

(defun url (model-type) (format nil "https://huggingface.co/hikettei/gpt2-gguf/resolve/main/~(~a~)-f32.gguf?download=true" model-type))

(defparameter *remap-key*
  '(("blk." . "h.")
    ("token_embd.weight" . "wte.weight")
    ("position_embd.weight" . "wpe.weight")
    (".attn_norm.weight" . ".ln_1.affine")
    (".attn_norm.bias" . ".ln_1.bias")
    (".attn_qkv.weight" . ".attn.c_attn.weight")
    (".attn_qkv.bias" . ".attn.c_attn.bias")
    (".ffn_down.weight" . ".mlp.c_proj.weight")
    (".ffn_down.bias" . ".mlp.c_proj.bias")
    (".ffn_norm.weight" . ".ln_2.affine")
    (".ffn_norm.bias" . ".ln_2.bias")
    (".ffn_up.weight" . ".mlp.c_fc.weight")
    (".ffn_up.bias" . ".mlp.c_fc.bias")
    (".attn_output.weight" . ".attn.c_proj.weight")
    (".attn_output.bias" . ".attn.c_proj.bias")
    ("output.weight" . "lm_head.weight")
    ("output_norm.bias" . "ln_f.bias")
    ("output_norm.weight" . "ln_f.affine")))

(defun remap-key (key)
  (loop for (before . after) in *remap-key*
        do (setf key (cl-ppcre:regex-replace-all before key after)))
  key)

(defun remap-state-dict-keys (state-dict)
  (let ((new-entry (make-hash-table :test #'equal)))
    (maphash #'(lambda (k v) (setf (gethash (remap-key k) new-entry) v)) (state-dict-entry state-dict))
    (setf (state-dict-entry state-dict) new-entry)))

(defun make-gpt2 (model-type &key (max-seq-len 1024))
  (declare (type keyword model-type))
  (assert (find model-type `(:gpt2 :gpt2-medium :gpt2-large :gpt2-xl)) () "model-type must be one of :gpt2, :gpt2-medium, :gpt2-large, :gpt2-xl")
  (with-inference-mode ()
    (let* ((param (get-param model-type))
           (gguf (load-gguf-url (url model-type) (format nil "~(~a~)-f32.gguf" model-type)))
           (model (Transformer (params-dim param) (params-n-heads param) (params-n-layers param) (params-norm-eps param) (params-vocab-size param) :max-seq-len max-seq-len))
           (tokenizer (gguf->bpe-tokenizer gguf))
           (state-dict (gguf->state-dict gguf)))
      (remap-state-dict-keys state-dict)
      (load-state-dict model state-dict)
      (%make-gpt2 (caten (forward model (make-tensor `(1 s) :from 'x) (iconst 'n))) tokenizer max-seq-len))))

(defun ->input (list) (change-facet `(,(map 'list #'(lambda (x) (+ 0.0 x)) list)) :tensor))

(defun gpt2-generate (gpt2 input &key (verbose t) (max-length 100) (expected nil))
  (declare (type GPT2 gpt2) (type string input))
  (with-float-traps-masked t
    (with-slots ((model model) (tokenizer tokenizer) (max-seq-len max-seq-len)) gpt2
      (let* ((tokens (encode tokenizer input)) (start-pos 0))
        (loop for i upfrom 0 below max-length
              for in-tokens = (subseq tokens start-pos)
              for out = (forward model `(x . ,(->input in-tokens)) `(s . ,(length in-tokens)) `(n . ,start-pos)) do
                (with-facet (out* (out :direction :simple-array))
                  (setf start-pos (length tokens)) ;; start_pos = previous_total_seq_len
                  (let ((size (array-total-size out*)))
                    (setf tokens (append tokens (list (aref out* (1- size)))))
                    (when verbose (print (decode tokenizer (last tokens)))))))
        (let ((decoded (decode tokenizer tokens)))
          (when expected
            (assert (string= decoded expected) () "Expected output: ~a, but got: ~a" expected decoded))
          decoded)))))
