(defpackage :caten/apps.gpt2
  (:use :cl :caten/apis :caten/llm :caten/gguf)
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
    (".attn_qkv.bias" . ".attn.c_attn.bias")
    (".attn_qkv.weight" . ".attn.c_attn.weight")
    (".ffn_norm.bias" . ".ln_2.bias")
    (".ffn_norm.weight" . ".ln_2.affine")
    (".attn_norm.bias" . ".ln_1.bias")
    (".attn_norm.weight" . ".ln_1.affine")
    (".attn_output.bias" . ".attn.c_proj.bias")
    (".attn_output.weight" . ".attn.c_proj.weight")
    (".ffn_up.bias" . ".mlp.c_fc.bias")
    (".ffn_up.weight" . ".mlp.c_fc.weight")
    (".ffn_down.bias" . ".mlp.c_proj.bias")
    (".ffn_down.weight" . ".mlp.c_proj.weight")
    ("token_embd.weight" . "wte.weight")
    ("output.weight" . "lm_head.weight")
    ("output_norm.bias" . "ln_f.bias")
    ("output_norm.weight" . "ln_f.affine")
    ("position_embd.weight" . "wpe.weight")))

(defparameter *transpose-aot* (caten (!copy (!t (make-tensor `(a b) :from 'x)))))

(defun transpose-tensor (tensor)
  (assert (= (ndim tensor) 2) () "Cannot transpose the tensor ~a" tensor)
  (multiple-value-bind (a b) (apply #'values (shape tensor))
    (let ((out (forward *transpose-aot* `(x . ,tensor) `(a . ,a) `(b . ,b))))
      (setf (tensor-shape out) (list b a))
      out)))

(defun remap-key (key)
  (loop for (before . after) in *remap-key*
        do (setf key (cl-ppcre:regex-replace-all before key after)))
  key)

(defun remap-state-dict-keys (state-dict)
  (let ((new-entry (make-hash-table :test #'equal)))
    (maphash #'(lambda (k v) (setf (gethash (remap-key k) new-entry) v)) (state-dict-entry state-dict))
    (maphash
     #'(lambda (k v)
         (when (cl-ppcre:scan ".weight" k)
           (setf (gethash k new-entry) (transpose-tensor v))))
        new-entry)
    (setf (state-dict-entry state-dict) new-entry)))

(defun make-gpt2 (model-type &key (max-seq-len 1024))
  (declare (type keyword model-type))
  (assert (find model-type `(:gpt2 :gpt2-medium :gpt2-large :gpt2-xl)) () "model-type must be one of :gpt2, :gpt2-medium, :gpt2-large, :gpt2-xl")
  (with-no-grad
    (let* ((caten/llm::*use-kv-cache* nil) ;; todo: use kv-cache once segv is resolved.
           (param (get-param model-type))
           (gguf (load-gguf-url (url model-type) (format nil "~(~a~)-f32.gguf" model-type)))
           (model (Transformer (params-dim param) (params-n-heads param) (params-n-layers param) (params-norm-eps param) (params-vocab-size param) :max-seq-len max-seq-len))
           (avm (caten (forward model (make-tensor `(1 s) :from 'x) (iconst 'n))))
           (tokenizer (gguf->bpe-tokenizer gguf))
           (state-dict (gguf->state-dict gguf)))
      (remap-state-dict-keys state-dict)
      (load-state-dict model state-dict)
      (%make-gpt2 avm tokenizer max-seq-len))))

(defun extend-token (tensor token pos)
  (loop for i upfrom 0 below (nth 1 (shape tensor))
        if (not (= pos i))
          do (setf (aref (caten/avm:buffer-value (tensor-buffer tensor)) i) (aref (caten/avm:buffer-value (tensor-buffer tensor)) i))
        else
          do (setf (aref (caten/avm:buffer-value (tensor-buffer tensor)) i) (+ 0.0 (aref (caten/avm:buffer-value (tensor-buffer token)) 0))))
  tensor)

(defun gpt2-generate (gpt2 input)
  (declare (type GPT2 gpt2) (type string input))
  (with-slots ((model model) (tokenizer tokenizer) (max-seq-len max-seq-len)) gpt2
    (setf max-seq-len 30)
    (let* ((x (linspace `(1 ,max-seq-len) 0 0))
           (outputs))
      (loop for i upfrom 0
            for token in (encode tokenizer input)
            do (setf (aref (caten/avm:buffer-value (tensor-buffer x)) i) (+ 0.0 token)))
      (loop for i upfrom 0 below max-seq-len
            for nth upfrom (length (encode tokenizer input))
            for out = (forward model `(x . ,x) `(s . ,(nth 1 (shape x))) `(n . ,6))
            do (setf x (extend-token x out nth))
               (print (tensor-buffer out))
               (print (tensor-buffer x))
               (push (aref (caten/avm:buffer-value (tensor-buffer out)) i) outputs)
               (print outputs)
               (print (decode tokenizer (reverse outputs))))
      (print "Output:")
      (print (decode tokenizer (reverse outputs))))))
