(in-package :caten/gguf)
;; Corresponds to https://github.com/ggerganov/ggml/blob/master/docs/gguf.md#file-structure
(defclass GGUF ()
  ((version :type fixnum :initarg :version :accessor gguf-version)
   (tensor-count :type (unsigned-byte 64) :initarg :tensor-count :accessor gguf-tensor-count)
   (metadata-kv-count :type (unsigned-byte 64) :initarg :metadata-kv-count :accessor gguf-metadata-kv-count)
   (metadata :initarg :metadata :accessor gguf-metadata)
   (tensor-info :initarg :tensor-info :accessor gguf-tensor-info))
  (:documentation "
A class that represents the GGUF file format.

- (gguf-version gguf) returns a fixnum indicating the version of the GGUF file.

- (gguf-tensor-count gguf) returns the number of tensors in the GGUF file.

- (gguf-metadata-kv-count gguf) returns the number of metadata key-value pairs in the GGUF file.

- (gguf-metadata gguf) returns a list of metadata key-value pairs.

- (gguf-tensor-info gguf) returns a list of tensor information.

- (gguf-metadata-get gguf key) to get the corresponding value of the key where key is a string.
"))

(defmethod gguf-metadata-get ((gguf GGUF) key)
  (find key (gguf-metadata gguf) :key #'metadata-key :test #'equal))

(defmethod print-object ((gguf gguf) stream)
  (with-slots ((version version) (tensor-info tensor-info) (metadata metadata)) gguf
    (format stream "<GGUF
  version=~a
  metadata: ~a datum
  tensor-info: ~a tensors
>"
	    version (length metadata) (length tensor-info))))

(defun parse-header (buffer)
  (declare (type input-buffer buffer))
  (multiple-value-bind (g1 g2 u f)
      (values
       (fast-read-byte buffer)
       (fast-read-byte buffer)
       (fast-read-byte buffer)
       (fast-read-byte buffer))
    (let ((header (make-array 4 :element-type '(unsigned-byte 8) :initial-contents (list g1 g2 u f))))
      (babel:octets-to-string header :encoding :utf-8))))

(defun make-gguf (stream)
  "
Creates GGUF from the given stream.

The definition is described in the following link:

- https://github.com/ggerganov/ggml/blob/master/docs/gguf.md#file-structure

In short, this function accepts the following format:
```
[Magic Number (4 Byte)] | [GGUF Version (4 Byte)] | [Tensor_Count (8 Byte)] | [Metadata_KV_Count (8 Byte)] | [Rest_data]
```
"
  (with-fast-input (buffer nil stream)
    (multiple-value-bind (header version tensor-count metadata-kv-count)
	(values
	 (parse-header buffer)
	 (readu32-le buffer)
	 (readu64-le buffer)
	 (readu64-le buffer))
      (declare (type string header) (type (unsigned-byte 64) version tensor-count))
      (assert (string= header "GGUF") () "Expecting the header to be GGUF, but got ~a.~%The given stream is not a gguf format.~%~a" header stream)
      (let* ((metadata (parse-metadata-kv buffer metadata-kv-count))
	     (alignment (find "general.alignment" metadata :key #'metadata-key :test #'equalp))
	     (alignment (if alignment (metadata-value alignment) 32))
	     (tensors (parse-tensor-info buffer tensor-count alignment stream))
	     (gguf (make-instance 'gguf :version version :tensor-count tensor-count :metadata-kv-count metadata-kv-count
					:metadata metadata :tensor-info tensors)))
	(assert (= metadata-kv-count (length metadata)) () "The number of parsed metadatas is invaild. Parsed ~a, but expected ~a" (length metadata) metadata-kv-count)
	(assert (= tensor-count (length tensors)) () "The number of parsed tensor-info is invaild. Parsed ~a, but expected ~a" (length tensors) tensor-count)
	;; Processing [Rest_Data]
	;; Rest_Data is consisted of two parts:
	;; [tensor_info] | [rest_of_the_file]
	gguf))))

(defun load-gguf (pathname)
  "
```
(load-gguf pathname)
```
Creates a gguf file from the given pathname.
"
  (with-open-file (stream pathname :element-type '(unsigned-byte 8))
    (make-gguf stream)))

(defun gguf->state-dict (gguf)
  "
```
(gguf->state-dict gguf)
```
Creates a caten/state-dict from the given gguf file's tensor-info.
"
  (declare (type gguf gguf))
  (let ((dict (make-hash-table :test #'equal)))
    (loop for info in (gguf-tensor-info gguf)
          do (setf (gethash (tensor-info-name info) dict) (tensor-info->tensor info)))
    (caten/apis:make-state-dict :entry dict)))

(defun load-gguf-url (url filename &optional (output-directory "./"))
  "
```
(load-gguf-url url filename &optional output-directory)
```
Creates a gguf file from the given URL. The downloaded file will be saved in the output-directory named as filename. If the file already exists, it will not download the file again."
  (let* ((output-path (merge-pathnames filename (pathname output-directory))))
    (unless (probe-file output-path)
      (caten/common.logger:print-info "Downloading ~a to ~a..." url output-path)
      (trivial-download:download url output-path))
    (load-gguf output-path)))

(defun gguf->bpe-tokenizer (gguf
                            &key
                              (metadata-tokens "tokenizer.ggml.tokens")
                              (metadata-merges "tokenizer.ggml.merges"))
  "
```
(gguf->bpe-tokenizer gguf &key (metadata-tokens \"tokenizer.ggml.tokens\") (metadata-merges \"tokenizer.ggml.merges\"))
```

Creates a BPE tokenizer (which is caten/llm:Tokenizer) from the given gguf file's metadata.
"
  (declare (type gguf gguf))
  (let ((tokens (gguf-metadata-get gguf metadata-tokens))
        (merges (gguf-metadata-get gguf metadata-merges)))
    (assert (and tokens merges) () "The given gguf does not have the metadata for the tokenizers.~%tokens_key: ~a~%merges_key: ~a" metadata-tokens metadata-merges)
    (let ((tokens-split-id (/ (array-total-size (metadata-value tokens)) 2))
          (merges-split-id (/ (array-total-size (metadata-value merges)) 2)))
      (assert (and (integerp tokens-split-id) (integerp merges-split-id)) ()
              "The sizes for tokens/merges are not even.~%tokens: ~a~%merges: ~a"
              (array-total-size (metadata-value tokens)) (array-total-size (metadata-value merges)))
      (let ((tokens
              (with-output-to-string (out)
                (loop for value across (metadata-value tokens)
                      if (and (numberp value) (= tokens-split-id value))
                        do (princ " " out)
                      else
                        do (princ value out))))
            (merges
              (with-output-to-string (out)
                (loop for value across (metadata-value merges)
                      if (and (numberp value) (= merges-split-id value))
                        do (princ #\newline out)
                      else
                        do (princ value out) (princ " " out)))))
        (setf merges (cl-ppcre:regex-replace-all " \\n" merges (format nil "~%")))
        (caten/llm:make-bpe-tokenizer tokens merges)))))
