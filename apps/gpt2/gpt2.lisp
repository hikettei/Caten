(defpackage :caten/apps.gpt2
  (:use :cl :caten/apis :caten/llm)
  (:export
   :make-gpt2
   ;; :predict
   ))

(in-package :caten/apps.gpt2)

(defstruct (GPT2
            (:constructor %make-gpt2 (model tokenizer)))
  (model model) (tokenizer tokenizer))

(defstruct Param n-layers n-heads dim (norm-eps 1e-5) (vocab-size 50257))

(defparameter *gpt2* (make-param :n-layers 12 :n-heads 12 :dim 768))
(defparameter *gpt2-medium* (make-param :n-layers 24 :n-heads 16 :dim 1024))
(defparameter *gpt2-large* (make-param :n-layers 36 :n-heads 20 :dim 1280))
(defparameter *gpt2-xl* (make-param :n-layers 48 :n-heads 25 :dim 1600))

(defun get-param (model-type)
  (ecase model-type
    (:gpt2 *gpt2*)
    (:gpt2-medium *gpt2-medium*)
    (:gpt2-large *gpt2-large*)
    (:gpt2-xl *gpt2-xl*)))

(defun make-gpt2 (model-type &key (max-seq-len 1024))
  (declare (type keyword model-type))
  (assert (find model-type `(:gpt2 :gpt2-medium :gpt2-large :gpt2-xl)) () "model-type must be one of :gpt2, :gpt2-medium, :gpt2-large, :gpt2-xl")
  (let* ((param (get-param model-type))
         (model (Transformer (param-dim param) (param-n-heads param) (param-n-layers param) (param-vocab-size param) (param-norm-eps param) :max-seq-len max-seq-len)))

    ;; [TODO] Fetch the parameter from https://huggingface.co/{model_size}/resolve/main/pytorch_model.bin
    ;; [TODO] Somehow load the weight from ^
    ;; [TODO] Run the inference from CLI
    ;; [TODO] Fix for the MHA, getting GPT2 to work
    ))
         
