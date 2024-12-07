(in-package :caten/test-suite)

(python-exec "
def tiktoken_tokenize(str):
  import tiktoken
  tokenizer = tiktoken.get_encoding(\"gpt2\")
  return tokenizer.encode(str)

def tiktoken_decode(list):
  import tiktoken
  tokenizer = tiktoken.get_encoding(\"gpt2\")
  return tokenizer.decode(list)
")

(import-function "tiktoken_tokenize")
(import-function "tiktoken_decode")

(defun make-bpe-gpt2 ()
  (make-bpe-tokenizer
   (uiop:read-file-string "./source/test-suite/external/assets/gpt2_tokenizer/tokens.txt")
   (uiop:read-file-string "./source/test-suite/external/assets/gpt2_tokenizer/merges.txt")))

(defparameter *gpt2-tokenizer* (make-bpe-gpt2))

(deftest gpt2-tokenizer-test
  (macrolet ((compare (sentence)
               `(progn
                  (let ((x1 (encode *gpt2-tokenizer* ,sentence))
                        (x2 (coerce (tiktoken_tokenize ,sentence) 'list)))
                    (ok (equal x1 x2) (format nil "Encoding: ~a. Caten = ~a, Tiktoken = ~a." ,sentence x1 x2)))
                  (let ((x1 (decode *gpt2-tokenizer* (encode *gpt2-tokenizer* ,sentence)))
                        (x2 (tiktoken_decode (tiktoken_tokenize ,sentence))))
                  (ok (equal x1 x2) (format nil "Decoding: ~a. Caten = ~a, Tiktoken = ~a" ,sentence x1 x2))))))
    (compare "Say hello to me")
    (compare "Hello world")
    (compare "What is the answer to the life, the world, and everything?")))
               
