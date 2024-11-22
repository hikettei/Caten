(in-package :caten/llm)
;; https://github.com/hikettei/cl-waffe2/blob/master/examples/gpt-2/tokenizer.lisp
(defparameter *pat* (create-scanner "'s|'t|'re|'ve|'m|'ll|'d| ?\\p{L}+| ?\\p{N}+| ?[^\\s\\p{L}\\p{N}]+|\\s+(?!\\S)|\\s+"))

(defclass BPETokenizer (Tokenizer)
  ((encoder :type hash-table :initarg :encoder :reader bpe-encoder)
   (decoder :type hash-table :initarg :decoder :reader bpe-decoder)
   (merges :type hash-table :initarg :merges :reader bpe-merges)
   (cache :type hash-table :initform (make-hash-table :test #'equal) :reader bpe-cache))
  (:documentation "An implementation of Byte Pair Encoding tokenizer. Initialized by the make-bpe-tokenizer function."))

(defun make-bpe-tokenizer (tokens merges)
  "
```
(make-bpe-tokenizer tokens merges)
```

Creates a new instance of BPETokenizer from given tokens and merges.

- Tokens[string] A string that contains the vocabulary of the tokenizer. This function will split the given string by whitespace and label each token from 0 to N. (e.g.: `hello word a b c` -> `hello:0 word:1 a:2 b:3 c:4`)
- Merges[string] A string that contains the BPE merges of the tokenizer, split by newline, and then space. (e.g.: 'a b\nb c\n' -> `a b:0 b c:1`)
"
  (declare (type string tokens merges) (optimize (speed 3)))
  (let ((encoder (make-hash-table :test #'equal))
        (decoder (make-hash-table))
        (bpe-merges (make-hash-table :test #'equal))
        (bpe-pairs (cdr (loop for mstr in (split "\\n" merges) collect (split " " mstr)))))
    (loop for token in (cdr (split " " tokens))
          for nth fixnum upfrom 0 do
            (setf (gethash token encoder) nth (gethash nth decoder) token))
    (loop for p in bpe-pairs
          for nth fixnum upfrom 0 do
            (setf (gethash p bpe-merges) nth))
    (make-instance 'BPETokenizer :encoder encoder :decoder decoder :merges bpe-merges)))

(declaim (ftype (function (list) list) get-pairs))
(defun get-pairs (token)
  (declare (type list token) (optimize (speed 3)))
  ;; e.g.: HIThere -> ((H I) (I T) (T H) (H E) (E R) (R E))
  (loop for index fixnum upfrom 0 below (1- (length token))
	collect
	(list (nth index token) (nth (1+ index) token))))
;; TODO: When I have a time, Let's rewrite bpe-split using xsubseq for maximize the performance! (+ i think i should refactor this function)
(defun bpe-split (tokenizer token)
  (declare (type string token)
           (optimize (speed 3)))
  (let* ((bpe-merges (bpe-merges tokenizer))
         (word (map 'list #'string (coerce token 'list)))
 	 (out-of-range (+ 2 (hash-table-count bpe-merges)))
	 (pairs (get-pairs word))
         (cache (gethash token (bpe-cache tokenizer))))
    (when cache (return-from bpe-split cache))
    (when (null pairs) (return-from bpe-split token))
    (loop named bpe-iter while t do
      (let* ((smallest (loop for pair in pairs minimize (the fixnum (or (gethash pair bpe-merges) out-of-range))))
	     (bigram   (find smallest pairs :test #'eql :key #'(lambda (x) (gethash x bpe-merges)))))
	(when (or (null bigram) (>= smallest out-of-range)) (return-from bpe-iter)) ;; Break from the loop
	(multiple-value-bind (first second) (apply #'values bigram)
	  (let ((new-word) (i 0))
            (declare (type list new-word))
	    (loop named bpe-word-iter while (< i (length word)) do
              (let ((j (position first word :start i :test #'equal)))
                (when (null j)
                  ;; Appending the rest to the new-word, and break from the inner loop
                  (setf new-word (nconc new-word (subseq word i)))
                  (return-from bpe-word-iter))
                ;; Adding the partial word (i ~ j) to the new-word
                (setf new-word (nconc new-word (subseq word i j))
                      i j) ;; the next starting point is j
                (if (and (equal (nth i word) first)
                         (< i (1- (length word)))
                         (equal (nth (1+ i) word) second))
                    ;; Two words are mergeable
                    (setf new-word (nconc new-word (list (concatenate 'string first second)))
                          i (+ i 2))
                    ;; Keep separating two words
                    (setf new-word (nconc new-word (list (nth i word)))
                          i (1+ i)))))
	    (setf word new-word)
	    (if (= (length word) 1)
		(return-from bpe-iter)
		(setf pairs (get-pairs word)))))))
    (setf (gethash token (bpe-cache tokenizer)) word)
    word))

(defmethod encode ((tokenizer BPETokenizer) text)
  (declare (optimize (speed 3)))
  (check-type text string)
  (let ((encoder (bpe-encoder tokenizer)))
    (let* ((tokens (all-matches-as-strings *pat* text))
           (tokens (if (stringp tokens) (list tokens) tokens))
           (bpe-tokens))
      (declare (type list tokens bpe-tokens))
      (loop for token string in tokens do
        (let ((token
                (with-output-to-string (tmp)
                  (loop for n upfrom 0 below (length token)
                        do (princ (gethash (char-code (aref (the (simple-array character (*)) token) n)) *byte2unicode*) tmp)))))
	  (dolist (bpetoken (bpe-split tokenizer token))
	    (push (or (gethash bpetoken encoder) 0) bpe-tokens))))
      (nreverse bpe-tokens))))

(defmethod decode ((tokenizer BPETokenizer) tokens)
  (declare (optimize (speed 3)))
  (check-type tokens list)
  (let ((decoder (bpe-decoder tokenizer)))
    (let ((text (apply #'concatenate 'string (loop for token in tokens collect (gethash token decoder)))))
      (declare (type string text))
      (with-output-to-string (out)
        (loop for pos fixnum upfrom 0 below (length text) do
	  (let ((code (gethash (char-code (aref text pos)) *byte2unicode*)))
	    (if code
	        (princ code out)
                (princ " " out))))))))
