(in-package :caten/nn)
;; TODO: FlashAttention2
(defmodel (Embedding (vocab-size embedding-dim))
    ((vocab-size vocab-size)
     (embedding-dim embedding-dim)
     (weight (normal `(,vocab-size ,embedding-dim) :mean 0 :std 1 :requires-grad t))))

(defmethod call ((op Embedding) &rest inputs)
  (st "A[~] -> A[~]" (inputs))
  (let ((x (car inputs)))
    (with-slots ((vocab-size vocab-size) (embedding-dim embedding-dim) (weight weight)) op
      (let* ((weight-shp `(1 1 ,vocab-size ,embedding-dim))
	     (big-shp (append (shape x) `(,vocab-size ,embedding-dim)))
	     (arange (!index-components `(1 1 ,vocab-size 1)))
	     (arange (!expand arange big-shp))
	     (idx (!expand (!reshape x (append (shape x) `(1 1))) big-shp))
	     (vals (!expand (!reshape weight weight-shp) big-shp)))
	(let* ((out (!sum (!where (!eq arange idx) vals (!const x 0)) :axis 2))
	       (shp (append (shape x) (list embedding-dim))))
	  (!reshape out shp))))))

(in-package :caten/nn.test)
;; Fuse Index-Components in Multiexpr
