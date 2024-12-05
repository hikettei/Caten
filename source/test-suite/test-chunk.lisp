(in-package :caten/test-suite)

(python-exec
 "
def torch_chunk_test(x, chunks, dim):
    return x.chunk(chunks, dim)
")
(import-function "torch_chunk_test")

(deftest test-chunk
  (with-given-dtype ((:float32 . "float32"))
    (let* ((batch-size 1)
           (seq-len 32)
           (dim-size 192)
           (chunks 3)
           (dim 2)
           (x (with-manual-seed (0) (rand `(,batch-size ,seq-len ,dim-size)))))
      (multiple-value-bind (lisp-chunk1 lisp-chunk2 lisp-chunk3)
          (!chunk x chunks :dim dim)
        (let ((python-chunks (with-torch (x)
                               (torch_chunk_test x chunks dim))))
          (let ((python-chunk1 (->caten (nth 0 python-chunks)))
                (python-chunk2 (->caten (nth 1 python-chunks)))
                (python-chunk3 (->caten (nth 2 python-chunks))))
            ;; Compare each chunk
            (assert-equal
             (:rtol 1e-5 :atol 1e-5)
             python-chunk1
             (proceed (!contiguous lisp-chunk1)))
            (assert-equal
             (:rtol 1e-5 :atol 1e-5)
             python-chunk2
             (proceed (!contiguous lisp-chunk2)))
            (assert-equal
             (:rtol 1e-5 :atol 1e-5)
             python-chunk3
             (proceed (!contiguous lisp-chunk3)))))))))
