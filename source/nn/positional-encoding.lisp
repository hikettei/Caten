(in-package :caten/nn)

; from https://github.com/ml-explore/mlx/blob/main/python/mlx/nn/layers/positional_encoding.py
;; RoPE
;; PositionalEncoding

(defmodel (RoPE (dims &key (traditional NIL) (base 10000) (scale 1.0) (offset 1.0)))
    ((dims dims)
     (traditional traditional)
     (base base)
     (scale scale)
     (offset offset)))

(defmethod call ((op RoPE) &rest inputs)
  (let* ((x (car inputs))
         (shape (shape x))
         (last-two (last shape 2))
         (n (first last-two))
         (d (second last-two))
         (b (reduce #'* (butlast shape 2)))
         (x (!reshape x (list b n d)))
         (positions (!index-components (list n)))
         ; TODO: handle potential divisions by 0
         (freqs (!exp (!div (!index-components (list (floor d 2)))
                            (!const x (log (- (floor 10000 d) 1))))))
         (positions-reshaped (!reshape positions (list n 1)))  ; (N,1)
         (freqs-reshaped (!reshape freqs (list 1 (floor d 2))))  ; (1,D/2)
         (theta (!mul positions-reshaped freqs-reshaped))  ; (N,D/2)
         (costheta (!cos theta))
         (sintheta (!sin theta))
         (x1 (!view x t t `(0 ,d 2)))
         (x2 (!view x t t `(1 ,d 2)))
         (rx1 (!sub (!mul x1 costheta) (!mul x2 sintheta)))
         (rx2 (!add (!mul x1 sintheta) (!mul x2 costheta)))
         (rx1-expanded (!reshape rx1 (append (shape rx1) (list 1))))
         (rx2-expanded (!reshape rx2 (append (shape rx2) (list 1))))
         (result (!concatenate -1 rx1-expanded rx2-expanded))
         (final-result (!reshape result (list b n d))))
    proceed final-result)))




(defparameter *tensor1* (make-tensor '(10 10 20) :initial-element 1.0))

(let ((instance (make-instance 'RoPE)))
  (call instance *tensor1*))








(in-package :caten/test-suite)

(python-exec
"
def precompute_freqs_cis(dim: int, end: int, theta: float = 10000.0):
    freqs = 1.0 / (theta ** (torch.arange(0, dim, 2)[: (dim // 2)].float() / dim))
    t = torch.arange(end, device=freqs.device)  # type: ignore
    freqs = torch.outer(t, freqs).float()  # type: ignore
    freqs_cis = torch.polar(torch.ones_like(freqs), freqs)  # complex64
    return freqs_cis
")


(import-function "precompute_freqs_cis")




(->caten (precompute_freqs_cis 1 1))
;; [TODO] Fuse in a single kernel (var/std)
;; (deftest test-variance
;;(with-given-dtype ((:float32 . "float32"))
;;  (let ((x (rand `(30 30))))
;;    (assert-equal
;;     (:atol 1e-5 :rtol 1e-6)
;;    (with-torch (x) (->caten (torch.var x :axis -1 :keepdims t :correction 1)))
;;     (proceed (!rope x :axis -1 :correction 1))))));(in-package :caten/nn.test)





(deftest test-rope-tensor ()
  (with-no-grad
    (let* ((model (RoPE 1)))
      (print model)
      (print "test"))))
