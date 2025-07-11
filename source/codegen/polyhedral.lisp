(defpackage :caten/codegen/polyhedral
  (:shadow #:set #:space)
  (:shadowing-import-from :cl :map)
  (:use :cl :caten/air :caten/aasm :caten/isl)
  (:export
   #:make-polyhedral-from-blueprint
   #:get-blueprint-from-polyhedral))

(in-package :caten/codegen/polyhedral)

(defun get-blueprint-from-polyhedral (polyhedral)

  )
(defun render-list (list) (apply #'concatenate 'string (butlast (loop for n in list append (list (format nil "~a" n) ", ")))))

(defun render-domain-body-from-group (blueprint &aux (seen) (idx2domain (make-hash-table)))
  (declare (type FastGraph blueprint))
  (values
   (with-output-to-string (out)
     (format out "{~%")
     (labels ((explore (id domains constraints &aux (node (id->value blueprint id)))
                (when (or (null id) (find (node-id node) seen))
                  (return-from explore))
                (case (node-type node)
                  (:FOR
                   (let ((range (id->value blueprint (car (node-reads node)))))
                     (assert (and range (eql (node-type range) :RANGE)))
                     (assert (= 1 (second (node-reads range))) () "The given range is not a scop!.")
                     (assert (numberp (car (node-reads range))))
                     (explore (second (node-reads node)) (append domains (list range)) constraints)))
                  (:IF
                   (let ((constraint (id->value blueprint (car (node-reads node)))))
                     (assert (and constraint (eql :EXPR (node-type constraint))))
                     ;; [TODO] Only render when conditon is related to index computing
                     (let ((constraint (format nil "~a" constraint))) ;; [TODO] render-expr
                       (explore (second (node-reads node)) domains (append constraints (list constraint))))))
                  (:PROGN
                    (loop for item in (node-reads node)
                          do (explore item domains constraints)))
                  (:EXPR
                   (setf (gethash (node-id node) idx2domain)
                         (format nil "~a[~(~a~)]" (node-id node) (render-list (map 'list #'(lambda (x) (getattr x :idx)) domains))))
                   ;; [TODO] Constraints
                   (format out "~a"
                           (butlast
                            (append
                             (loop for dom in domains
                                   collect (format nil "0 <= ~(~a~) and ~(~a~)" (getattr dom :idx) (car (node-reads dom)))
                                   collect " and ")))))
                  (otherwise (error "No Such SCoP Pattern for ~a" (node-type node))))))
       (assert (= 1 (length (graph-outputs blueprint))))
       (explore (car (graph-outputs blueprint)) nil nil)))
   idx2domain))

"
def softmax(input[1280, 1280], output[1280, 1280]):
  allocate temp[1280, 1280]
  for n in parallel(1280):
    val_11 = 0.0
    val_2 = -Inf
    for m in range(1280):
      val_2 = max(val_2, input[1280*n+m]
    for m in range(1280):
      temp[_gid0*n+m] = exp2((input[1280*n+m]-val_2)*1.442695)
      val_11 = val_11 + temp[_gid0*n+m]
    for m in range(1280):
      output[n*1280+m] = val_9[n*1280+m]/val_11

def matmul(X[128, 128], Y[128, 128], OUT[128, 128]):
  for i in range(128):
    for j in range(128):
      acc = 0.0f
      for k in range(128):
          acc = acc + X[128*i+k] * Y[128*k + j]
      OUT[128*i+j] = acc

// CTA タイル = 64×64，Warp タイル = 16×16，lane = vec4
for B_i in blockIdx.y parallel tile_block_ij:64  // 0..1
  for B_j in blockIdx.x parallel tile_block_ij:64
    // 共有メモリキャッシュ
    shared Xs[64][64] @cache_read_shared_X
    shared Ys[64][64] @cache_read_shared_Y

    // Warp グリッド (=4×4 Warps per CTA)
    for W_i in threadIdx.y parallel tile_warp_ij:16  // 0..3
      for W_j in threadIdx.x/32 parallel tile_warp_ij:16  // 0..3
        // レジスタ蓄積
        reg_C[16][16] = 0

        //--------- k 軸分割（split_k_reduce=2）-----------
        for k_outer in range(0, 128, 64):
          // 二重バッファで Xs, Ys を先読み (sw_pipeline_stage2 + async_copy)
          prefetch X, Y tiles → Xs, Ys

          //---------------- 主演算 ------------------------
          for k_inner in unroll_k8 range(0, 64, 8):
            // lane = vec4 で 4 要素ロード
            vectorize(4):
              reg_A = ld4(Xs, W_i*16 + lane, k_inner + s)    // s=0..7
              reg_B = ld4(Ys, k_inner + s, W_j*16 + lane)
            // Tensor Core (16×16×8 FMA) または simdgroup_mad
            tensorcore_mma(reg_C, reg_A, reg_B)
          //------------------------------------------------

        //--------- 出力 (vec4 store) ---------------------
        vectorize(4):
          st4(OUT,
              (B_i*64 + W_i*16 + lane),
              (B_j*64 + W_j*16 + vec4-id),
              reg_C[lane][:])


"

;; No Polyhedral IR!
;; :noopt :reduce :coincident is all you need
(defun make-polyhedral-from-blueprint (blueprint)
  "Constructs Polyhedral IR from blueprint which is a static graph."
  (declare (type Graph blueprint))
  ;; あ ~ Indexingをどうするかの解釈...
  ;; -> 普通に1Dのままで良さそうに見える？
  ;; Reductionのaccess repをどう解釈するか，という話もある
  (print blueprint)
  ;; OptCandidates,
  )
