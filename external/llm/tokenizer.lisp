(in-package :caten/llm)

;; SentencePieceTokenizer?
;; [TODO] Adding String Array, Compiling the tokenizer?

(defclass Tokenizer ()
  ((token2id :type hash-table :initform (make-hash-table :test #'equal) :accessor tokenizer-token2id)
   (id2token :type hash-table :initform (make-hash-table :test #'equal) :accessor tokenizer-id2token)
   (scores   :type hash-table :initform (make-hash-table :test #'equal) :accessor tokenizer-scores)))

(defmethod ->tokens ((tokenizer Tokenizer) (text string))
  (map 'vector #'(lambda (x) (gethash x (tokenizer-token2id tokenizer))) text))

(defmethod encode-text ((tokenizer Tokenizer) (text string))
  (declare (optimize (speed 3)))
  (let ((tokens (->tokens tokenizer text)))
    (declare (type simple-vector tokens))
    (loop named outer
          with best-score single-float = 1e-10
	  with best-id = -1
	  with best-index = -1
	  do (loop for i below (1- (length tokens))
		   for string = (concatenate
                                 'string
                                 (gethash i (tokenizer-id2token tokenizer))
                                 (gethash (1+ i) (tokenizer-id2token tokenizer)))
		   for id = (gethash string (tokenizer-id2token tokenizer))
		   if (and id (> (the single-float (gethash id (tokenizer-scores tokenizer))) best-score))
		     do (setf best-score (gethash id (tokenizer-scores tokenizer))
			      best-id id
			      best-index i))
	     (if (= best-index -1) (return-from outer tokens))
	     (setf (aref tokens best-index) best-id
		   tokens (concatenate
                           'vector
                           (subseq tokens 0 (1+ best-index))
                           (subseq tokens (+ 2 best-index)))))))

;; Workflow and Concepts
;; TODO: Compile the entire graph. Including token sampling, 
;;      Input
;;        |
;;   [Tokenizer]
;;        |
;;        |
;;  |-------------|
;;  | Transformer | <-----------------------|
;;  |-------------|                         |
;;        |                                 | x N
;;        |----[Logits Argmax Concatenate] -|
;;        |
;;    [Output]
;;
