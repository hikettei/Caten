(in-package :caten/test-suite)

(defun count-load (value graph)
  (count value (graph-nodes graph) :key #'(lambda (x) (and (eql (node-type x) :LOAD) (getattr x :value)))))
;; [TODO] Doing the same test for JIT.
(deftest stride-reuse-is-working-p
  (testing "Introducing a new symbolic reshape won't create a new node for computing strides."
    (let ((graph-large (ctx:with-contextvar (:BACKEND "LISP") (runtime-graph (caten (!reshape (!matmul (make-tensor `(n a b)) (make-tensor `(n b c))) `(c b n))))))
          (graph-small (ctx:with-contextvar (:BACKEND "LISP") (runtime-graph (caten (!matmul (make-tensor `(n a b)) (make-tensor `(n b c))))))))
      (ok (= (length (graph-nodes graph-large)) (1+ (length (graph-nodes graph-small)))) "Only the difference is :VIEW.")
      (dolist (sym `(a b c n))
        ;; [TODO] Remove :UINT64 allocation
        (ok (<= (count-load sym graph-large) 2) ":UINT64 :INT64 creation is allowed")
        (ok (<= (count-load sym graph-small) 2) ":UINT64 :INT64 creation is allowed")))))

;; [TODO] Schedule Test
;; - [ ]  Write a test for (caten (!add (!matmul (make-tensor `(n a b)) (make-tensor `(n b c))) (make-tensor `(n a c)))), in the store, val_31[(((val_13*_gid0)+(c*_gid1))+_gid2)] = (val_27+val_25[(((_gid0*val_13)+(_gid1*c))+_gid2)]); is rendered?
  
