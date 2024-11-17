(in-package :caten/llm)
;; https://github.com/hikettei/cl-waffe2/blob/master/examples/gpt-2/tokenizer.lisp
;; [TODO] Optimize as far as i go!!!
;; [TODO] Split bpe-merges by 50000 in the gguf level
;; Workload:
;; - [ ] Clean up the impl
;; - [ ] Write a documentation
;; - [ ] Optimize
;; - [ ] Load from GGUF (Convert)
;; - [ ] Testing
;; - [ ] Add: self.cache
;; - [ ] OK (Fetch from URL is working?)
(defparameter *pat* (create-scanner "'s|'t|'re|'ve|'m|'ll|'d| ?\\p{L}+| ?\\p{N}+| ?[^\\s\\p{L}\\p{N}]+|\\s+(?!\\S)|\\s+"))

(defclass BPETokenizer (Tokenizer)
  ((encoder :type hash-table :initarg :encoder)
   (decoder :type hash-table :initarg :decoder)
   (merges :type hash-table :initarg :merges))
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

(declaim (ftype (function (string) list) get-pairs))
(defun get-pairs (token)
  (declare (type (simple-array character (*)) token) (optimize (speed 3)))
  ;; e.g.: HIThere -> ((H I) (I T) (T H) (H E) (E R) (R E))
  (loop for index fixnum upfrom 0 below (1- (length token))
	collect
	(list (string (aref token index)) (string (aref token (1+ index))))))

(defun countup-nth (word token n)
  (declare (type fixnum n))
  (let ((count 0)
	(n (1+ n)))
    (loop for tkn in token
	  for pos upfrom 0
	  if (equal tkn word)
	    do (incf count 1)
	  if (= count n)
	    do (return-from countup-nth pos))))

(defun bpe-split (token bpe-merges)
  (declare (type string token)
           (type hash-table bpe-merges)
           ;; (optimize (speed 3))
           )
  (let ((word (list token))
	(out-of-range (* -1 (+ 1 (hash-table-count bpe-merges))))
	(pairs (get-pairs token)))
    (loop named bpe-iter while t do
      (let* ((smallest (loop for pair in pairs minimize (or (gethash pair bpe-merges) out-of-range)))
	     (bigram   (find smallest pairs :test #'eql :key #'(lambda (x) (gethash x bpe-merges)))))
	(when (null bigram) (return-from bpe-iter))
	(multiple-value-bind (first second) (apply #'values bigram)
	  (let ((new-word) (i 0))
	    (loop named bpe-word-iter while (< i (length word)) do
	      (if (or (null (find first word :test #'equal))
		      (not (< i (count first word :test #'equal))))
		  (progn
		    ;; Break
		    (setq new-word
			  `(,@new-word
			    ,@(subseq word i (length word))))
		    (return-from bpe-word-iter))
		  (let ((j (countup-nth first word i)))
		    (setq new-word
			  `(,@new-word
			    ,@(subseq word i j)))
		    (setq i j)
		    (if (and (equal (nth i word) first)
			     (< i (1- (length word)))
			     (equal (nth (1+ i) word) second))
			(progn
			  (setq new-word
				`(,@new-word
				  ,(concatenate 'string first second)))
			  (incf i 2))
			(progn
			  (setq new-word
				`(,@new-word ,(nth i word)))
			  (incf i 1))))))
	    (setq word new-word)
	    (if (= (length word) 1)
		(return-from bpe-iter)
		(setq pairs (get-pairs word)))))))
    word))

(defmethod encode ((tokenizer BPETokenizer) text)
  (declare (optimize (speed 3)))
  (check-type text string)
  (with-slots ((encoder encoder) (merges merges)) tokenizer
    (let* ((tokens (all-matches-as-strings *pat* text))
           (bpe-tokens))
      (loop for token in tokens do
        (let* ((token
                 (loop for n upfrom 0 below (length token)
                       collect (gethash (char-code (aref token n)) *byte2unicode*)))
	       (token (apply #'concatenate 'string token)))
	  (dolist (bpetoken (bpe-split token merges))
	    (push (or (gethash bpetoken encoder) 0) bpe-tokens))))
      (nreverse bpe-tokens))))

(defmethod decode ((tokenizer BPETokenizer) tokens)
  (check-type tokens list)
  (with-slots ((decoder decoder)) tokenizer
    (let ((text (apply #'concatenate 'string (loop for token in tokens collect (gethash token decoder)))))
      (with-output-to-string (out)
        (loop for pos fixnum upfrom 0 below (length text) do
	  (let ((code (gethash (char-code (aref text pos)) *byte2unicode*)))
	    (if code
	        (princ code out)
                (princ " " out))))))))
