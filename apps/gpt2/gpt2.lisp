(defpackage :caten/apps.gpt2
  (:use :cl :caten/apis :caten/llm :caten/gguf)
  (:export
   :make-gpt2
   :gpt2-generate))

(in-package :caten/apps.gpt2)

(defstruct (GPT2
            (:constructor %make-gpt2 (model tokenizer max-seq-len)))
  (model model) (tokenizer tokenizer) (max-seq-len))

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

(defun url (model-type) (format nil "https://huggingface.co/hikettei/gpt2-gguf/blob/main/~(~a~)-f32.gguf?download=true" model-type))

(defun make-gpt2 (model-type &key (max-seq-len 1024))
  (declare (type keyword model-type))
  (assert (find model-type `(:gpt2 :gpt2-medium :gpt2-large :gpt2-xl)) () "model-type must be one of :gpt2, :gpt2-medium, :gpt2-large, :gpt2-xl")
  (with-no-grad
    (let* ((param (get-param model-type))
           (gguf (load-gguf-url (url model-type) (format nil "~(~a~)-f32.gguf" model-type)))
           (model (Transformer (param-dim param) (param-n-heads param) (param-n-layers param) (param-vocab-size param) (param-norm-eps param) :max-seq-len max-seq-len))
           (avm (caten (forward model (make-tensor `(1 ,max-seq-len)) (iconst 'pos)))))
      
      ;; [TODO] Replace the keys
      (load-state-dict model (gguf->state-dict gguf))
      (%make-gpt2 avm nil max-seq-len))))

(defun gpt2-generate (gpt2 input)
  (declare (type GPT2 gpt2) (type string input))
  (with-slots ((model model) (tokenizer tokenizer) (max-seq-len max-seq-len)) gpt2
    (let ((input (proceed (make-tensor `(1 ,max-seq-len) :initial-elements 1.0)))
          (start-pos 0))
      ;; WIP!
      (error "NOT READY!!"))))
