(in-package :caten/nn)
;; TODO: Embedding, PositionalEmbedding, RoPE
;; TODO: FlashAttention2
(defmodel (Embedding (vocab-size embedding-dim))
    ((vocab-size vocab-size)
     (embedding-dim embedding-dim)
     (weight (make-tensor `(,vocab-size ,embedding-dim)))
     (arange (!reshape (ax+b `(,vocab-size) 1 0) `(1 1 ,vocab-size 1)))))

(defmethod call ((op Embedding) &rest inputs)
  (st "A[~] -> A[~]" (inputs))
  (let ((x (car inputs)))
    (with-slots ((vocab-size vocab-size) (embedding-dim embedding-dim) (weight weight) (arange arange)) op
      (let* ((weight-shp `(1 1 ,vocab-size ,embedding-dim))
	     (big-shp (append (shape x) `(,vocab-size ,embedding-dim)))
	     (arange (!expand arange big-shp))
	     (idx (!expand (!reshape x (append (shape x) `(1 1))) big-shp))
	     (vals (!expand (!reshape weight weight-shp) big-shp)))
	(let* ((out (!sum (!mul (!where (!eq arange idx) (!const x 1) (!const x 0)) vals) :axis 2))
	       (shp (append (shape x) (list embedding-dim))))
	  (!reshape out shp))))))
