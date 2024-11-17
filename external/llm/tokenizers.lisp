(in-package :caten/llm)

(defclass Tokenizer () nil)

(defgeneric encode (tokenizer string))

(defgeneric decode (tokenizer string))
