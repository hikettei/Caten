(in-package :caten/llm)

(defclass Tokenizer ()
  nil
  (:documentation "
A tokenizer is a class that can encode and decode strings into and from.
- `(encode tokenizer string) -> (list fixnum ...)` encodes a string into a tokenized form.
- `(decode tokenizer (list fixnum ...)) -> string` decodes a tokenized form into a string.
"))

(defgeneric encode (tokenizer string))

(defgeneric decode (tokenizer string))
