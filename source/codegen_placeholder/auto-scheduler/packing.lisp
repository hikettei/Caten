(defpackage :caten/codegen/packing
  (:documentation "`caten/codegen/packing` provides a transformation tool to
replace the packed region (defined in auto-scheduler.lisp, class Packing) with the vectorized ops.
The class `Vectorize` will provide the information of the vectorized region, passed via define-auto-scheduler macro.
TensorCore optimization is also implemented as a part of Vectorize.
")
  (:use :cl :caten/air :caten/codegen/polyhedral-ast :caten/codegen/shape-inference :caten/codegen/transform)
  (:import-from
   :caten/codegen/expr
   #:ExprMeta
   #:ExprMeta-comment
   #:copy-expr
   #:expr-graph)
  (:export
   #:Vectorize
   #:TensorCore
   #:ast-rewrite-vectorize)
  (:export
   #:Vectorize-Config
   #:Vectorize-Config-Vectorize
   #:Vectorize-Config-Expr
   #:vectorized-config-simd)
  (:export
   #:Vectorized
   #:vectorized-intrinsic
   #:vectorized-perform
   #:vectorized-cfg)
  (:export
   #:expr-node-wmma-p
   #:expr-rewrite-as-tensorcore
   #:expr-node-simd-load-p
   #:expr-rewrite-as-simd-load
   #:expr-node-simd-store-p
   #:expr-rewrite-as-simd-store
   #:expr-node-simd-upcast-p
   #:expr-rewrite-as-simd-upcast
   #:blueprint-upcast-inference))

(in-package :caten/codegen/packing)
;; ~~ Vectorize ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defstruct (Vectorize
            (:constructor vectorize (name dims &key (applicable-p #'identity) (rewriter nil))))
  (name name :type keyword)
  (dims dims :type list)
  (applicable-p applicable-p :type function)
  (rewriter rewriter :type function))

(defstruct Vectorize-Config
  "applicable-p, rewriter will receive the necessary information via this structure to complete vectorize."
  (simd (error "simd must occur") :type list)
  (vectorize (error "vectorize must occur") :type Vectorize)
  (expr (error "exrp must occur") :type node))

(defun apply-vectorize (user vectorize schedule-item)
  (declare (type user user))
  (when (< (length (user-vectorize user)) (length (vectorize-dims vectorize)))
    (return-from apply-vectorize nil))
  (let ((padded
          (append (loop repeat (- (length (user-vectorize user)) (length (vectorize-dims vectorize))) collect 1)
                  (vectorize-dims vectorize))))
    (assert (= (length padded) (length (user-vectorize user))))
    (assert (every #'zerop (map 'list #'second (user-vectorize user))) () "apply-vectorze: range should start from zero.")
    (let* ((applicable-p (every #'(lambda (x y) (= 0 (mod x y))) (map 'list #'third (user-vectorize user)) padded))
           (unroll (map 'list #'(lambda (x y) (/ x y)) (map 'list #'third (user-vectorize user)) padded))
           (simd (loop for v in (user-vectorize user)
                       for u in unroll
                       collect (cons (car v) (the integer (/ (third v) u)))))
           (env (make-vectorize-config :vectorize vectorize :simd simd :expr (find-user user (user-name user) schedule-item))))
      (when (and
             applicable-p
             ;; [TODO] Judge the elements are contiguous in the memory! (stride must be one if simd)
             (funcall
              (vectorize-applicable-p vectorize)
              env))
        ;; -> will return a new render node which computes vectorize-dims region.
        (let ((new-user (funcall (vectorize-rewriter vectorize) env)))
          (assert (node-p new-user) () "vectorizer-rewrite must retnurn a node, getting ~a. (vectorize-rule=~a)" new-user vectorize)
          (let ((new-name (string-upcase (princ-to-string (node-id new-user)))))
            (assert (not (equalp new-name (user-name user))) () "vectorize-rewriter should return a new node.")
            (setf (user-name user) new-name))
          ;; TODO(hikettei) refactor this code to:
          ;; - Add something like :render-node-pool attribute in Schedule-Item
          ;; - And push the new-user to the pool instead of :blueprint
          ;; - When finding a user, search from the pool instead of :blueprint.
          (push new-user (getattr schedule-item :blueprint))
          (assert (null (user-simd user)) () "Cannot vectorize user for multiple times.")
          (setf (user-simd user)
                (loop for v in (user-vectorize user)
                      for u in unroll
                      collect (cons (car v) (the integer (/ (third v) u)))))
          (late-rewrite-pack->unroll user :unrolled-as unroll))))))

(defun TensorCore (dims &key (name :TensorCore))
  "
Assuming the memory access (notated as +) is contiguous 1d upcast, computes the vectorized region in a single operation e.g.:
```
  A       B
+---    ++++
+---  @ ----
+---    ----
```
"
  (declare (type list dims))
  (assert (and (= (length dims) 3) (every #'integerp dims)) () "TensorCore: dims are three-dimensional list of integers.")
  (Vectorize name dims :applicable-p #'expr-node-wmma-p :rewriter #'expr-rewrite-as-tensorcore))
;; ~~ Vectorizer ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [TODO] find-user has a lot of reuse with the find-user in ast-parser.lisp
(defun find-user (user node-id schedule-item &aux (items (getattr schedule-item :blueprint)))
  (declare (type user user) (type node schedule-item))
  (let ((node (find (princ-to-string node-id) items
                    :key (alexandria:compose #'princ-to-string #'node-id)
                    :test #'equalp)))
    (assert node () "~a is not found in ~a" node-id items)
    (assert (eql (node-type node) :EXPR))
    (setf node (copy-node node))
    (let ((base (getattr node :iterations)))
      (setf (getattr node :iterations) (user-args user))
      (if (and (null (user-args user)) (> (length base) 0))
          (setf (getattr node :iterations) base)
          (progn
            (setf (getattr node :iterations) (caten/codegen/shape-inference:ensure-iteration-space-length (length base) (getattr node :iterations)))
            (assert (= (length (getattr node :iterations)) (length base)) () "Before and after the polyhedral compilation, the rank of iteration space should not be changed. ~a -> ~a" (getattr node :iterations) (user-args user)))))
    node))

(defun try-rules (ast vectorize-rules schedule-item)
  (loop for r in vectorize-rules
        for new-ast = (apply-vectorize ast r schedule-item)
        when new-ast do (return-from try-rules new-ast)))

(defun ast-rewrite-vectorize (ast vectorize-rules schedule-item)
    "Rewrites the packed user in the AST with the vectorize-rules. The earlier rules have the higher priority.
If some users are failed to be vectorized, they are rewritten as unroll."
  (declare (type list vectorize-rules))
  (assert (eql (node-type schedule-item) :Schedule-Item))
  (labels ((handler (ast)
             (etypecase ast
               (User
                ;; Find the first vectorize item which satisfies applicable-p, otherwise unroll.
                (let ((ast (copy-user ast)))
                  (setf (user-args ast) (copy-list (user-args ast))
                        (user-unroll ast) (copy-list (user-unroll ast))
                        (user-vectorize ast) (copy-list (user-vectorize ast))
                        (user-late-unroll-info ast) (copy-list (user-late-unroll-info ast))
                        (user-simd ast) (copy-list (user-simd ast)))
                  (if (null (user-vectorize ast))
                      ast
                      (or (try-rules ast vectorize-rules schedule-item) (late-rewrite-pack->unroll ast)))))
               (AstBlock (make-block (map 'list #'handler (astblock-body ast))))
               (ASTFor
                (let ((new-for (copy-astfor ast)))
                  (setf (astfor-body new-for) (handler (astfor-body ast)))
                  new-for))
               (AstExpr (make-astexpr (astexpr-expr ast) (astexpr-is-defglobal-p ast)))
               (AstIf
                (let ((new-if (copy-astif ast)))
                  (setf (astif-then-node new-if) (handler (astif-then-node ast))
                        (astif-condition new-if) (astif-condition ast))
                  new-if)))))
    (handler ast)))
               
;; ~~ ExprMeta  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Renderer/Codegen is able to know wheter the expr is vectorized or not by checking the ExprMeta.
;; If the Expr is attributed as `Vectorized`, that means the expr is vectorized.
;; In order to compile the vectorized blueprint, the compiler should run pack/unpack inference first.
(defclass Vectorized (ExprMeta)
  ((intrinsic :initarg :intrinsic :type keyword :accessor vectorized-intrinsic)
   (perform :initarg :perform :type (member :pack :unpack :compute) :accessor vectorized-perform)
   (cfg :initarg :cfg :accessor vectorized-cfg)))

(defmethod exprmeta-comment ((expr Vectorized))
  (declare (type Vectorized expr))
  (format nil "simd_~(~a~)" (vectorized-intrinsic expr)))
;; ~~ WMMA Rewriter ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defnode (:TMP :VECTORIZED_PLACEHOLDER)
    ()
    "A placeholder node (only used in the function wmma->vectorized-wmma) to indicate the wmma node is replaced w/ TensorCore"
    :slots ((args :type list)))
;; In the node (MOVE: X <- A, B) A is not rendered so it is legal to remove the path A.
(defsimplifier
    (simplify-move-in-exprgraph :speed 0)
    ((:MOVE (_ B)) -> B))

(defsimplifier
    (wmma->vectorized-wmma :speed 0)
    ((:WMMA ((:AREF () :storage-id x :buffer xb :space xi)
             (:AREF () :storage-id y :buffer yb :space yi)
             (:AREF () :storage-id z :buffer zb :space zi)))
     -> ((node graph)
         ;; Note: TensorCore should assert at least 2d level parallelism.
         ;; [TODO] Assert they are contiguous (no offsets, no extra strides, etc...)
         (when (and
                (>= (length (iteration-space-shape xi)) 2)
                (>= (length (iteration-space-shape yi)) 2)
                (>= (length (iteration-space-shape zi)) 2))
           ;; [TODO] Broadcastの軸の箇所と，PackのサイズからTensorCoreに変換できるか判定
           (make-node
            :TMP :VECTORIZED_PLACEHOLDER (node-writes node) nil
            :args (list (list x xb xi) (list y yb yi) (list z zb zi)))))))

(defun %expr-node-wmma-p (expr)
  "Returns T if the expr is only consisted of WMMA nodes (plus rewritable as TensorCore)"
  (declare (type node expr))
  (assert (eql (node-type expr) :EXPR))
  ;; [TODO] Switch to use wmma-rewriter
  (when (null (getattr expr :reduction))
    (return-from %expr-node-wmma-p nil))
  
  (let ((graph (copy-graph (caten/codegen/expr:expr-graph (getattr expr :EXPR)))))
    (simplify-move-in-exprgraph graph)
    (caten/codegen/rewriting-rules::wmma-rewriter graph)
    (wmma->vectorized-wmma graph)
    (verify-graph graph)
    ;; Note: ensure only WMMA is left. (e.g. WMMA+Sin is not allowed)
    (when (and (= (length (graph-nodes graph)) 1) (eql :VECTORIZED_PLACEHOLDER (node-type (car (graph-nodes graph)))))
      (let ((node (car (graph-nodes graph))))
        (getattr node :args)))))

(defun expr-node-wmma-p (env)
  (declare (type Vectorize-Config env))
  (%expr-node-wmma-p (vectorize-config-expr env)))

(defun expr-rewrite-as-tensorcore (env name)
  (declare (type Vectorize-Config env))
  (expr-node-rewrite-as-simd env #'expr-node-wmma-p name :compute))
;; ~~ Vectorize Rewriters ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defsimplifier
    (rewrite-load-as-simd :speed 0)
    ;; TODO: :LOAD :ALLOCATE (when loading scalar)
    ((:LOAD ((:AREF () :storage-id x :buffer xb :space xi)) :value value)
     ->
     ((node graph)
      (make-node
       :TMP :VECTORIZED_PLACEHOLDER (node-writes node) nil
       :args (list (list x xb xi) value)))))

(defsimplifier
    (rewrite-store-as-simd :speed 0)
    ((:MOVE ((:AREF () :storage-id x :buffer xb :space xi)
             (:AREF () :storage-id y :buffer yb :space yi)))
     ->
     ((node graph)
      (make-node
       :TMP :VECTORIZED_PLACEHOLDER (node-writes node) nil
       :args (list (list x xb xi) (list y yb yi))))))

(defun expr-node-simd-p (env rewriter &key (allow-always nil))
  (declare (type Vectorize-Config env))
  (when (typep (getattr (vectorize-config-expr env) :meta :allow-undefined t) 'Vectorized)
    (return-from expr-node-simd-p nil))
  (when allow-always (return-from expr-node-simd-p t))
  (let ((graph (copy-graph (expr-graph (getattr (vectorize-config-expr env) :EXPR)))))
    (funcall rewriter graph)
    (verify-graph graph)
    (when (and (= (length (graph-nodes graph)) 1)
               (eql :VECTORIZED_PLACEHOLDER (node-type (car (graph-nodes graph)))))
      (getattr (car (graph-nodes graph)) :args))))

(defun expr-node-rewrite-as-simd (env predicate name perform)
  (declare (type Vectorize-Config env))
  (let ((expr (copy-node (vectorize-config-expr env)))
        (cfg  (funcall predicate env)))
    (setf (node-id expr) (gensym "NID"))
    (assert cfg () "expr-rewrite-as-tensorcore: the expr is not wmma.")
    (setf (getattr expr :EXPR) (copy-expr (getattr expr :EXPR))
          (getattr expr :meta) (make-instance 'Vectorized :intrinsic name :perform perform :cfg cfg))
    expr))

(defun expr-node-simd-load-p (env)
  (expr-node-simd-p env #'rewrite-load-as-simd))

(defun expr-rewrite-as-simd-load (env name)
  (declare (type Vectorize-Config env))
  (expr-node-rewrite-as-simd env #'expr-node-simd-load-p name :pack))

(defun expr-node-simd-store-p (env)
  (declare (type Vectorize-Config env))
  (expr-node-simd-p env #'rewrite-store-as-simd))

(defun expr-rewrite-as-simd-store (env name)
  (declare (type Vectorize-Config env))
  (expr-node-rewrite-as-simd env #'expr-node-simd-store-p name :unpack))

(defun expr-node-simd-upcast-p (env)
  (declare (type Vectorize-Config env))
  (expr-node-simd-p env #'identity :allow-always t))

(defun expr-rewrite-as-simd-upcast (env name)
  (declare (type Vectorize-Config env))
  (expr-node-rewrite-as-simd env #'expr-node-simd-upcast-p name :compute))
;; ~~ Pack/Unpack Insertion ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Consider the following blueprint:
;; ```
;; Y <- EXPR1(X, Meta=NIL)
;; Z <- EXPR2(Y, Meta=VECTORIZED)
;; ```
;; EXPR1 returns `unpacked` values and EXPR2 expects `packed` values. Here the compiler should insert pack/unpack nodes.
;; The function `blueprint-upcast-inference` will infer the pack/unpack nodes and insert them into the blueprint.
;; [TODO] Move to caten/aasm.lisp once the specs are concatenated.
(defnode (:SIMD :PACK) ()
         "
Equivalent to doing:
```
simd_load( for(int idx=0; idx < dim; idx+=stride) { array[idx]; }
```
"
         :slots ((idx :type string) (dim :type expr) (stride :type expr)))

(defnode (:SIMD :UNPACK) ()
         "
Equivalent to doing:
```
simd_load( for(int idx=0; idx < dim; idx+=stride) { array[idx]; }
```
"
         :slots ((idx :type string) (dim :type expr) (stride :type expr)))

(defun %pack (name idx dim stride &key (out (gensym "SIMD")))
  (make-node :SIMD :PACK (list out) (list name) :idx idx :dim dim :stride stride))

(defun %unpack (name idx dim stride &key (out (gensym "SIMD")))
  (make-node :SIMD :PACK (list out) (list name) :idx idx :dim dim :stride stride))
;; TODO: make-packする際に，Broadcastを考慮する
;; TODO: TensorCore, Broadcastingの条件を追加する。PACKは100% 1次元
;; TODO: LayerNorm Unroll val_2_0を修正
;; TODO: TensorCore: float4になるように，Broadcastingの条件を追加する。
(defun blueprint-upcast-inference (blueprints schedule-item)
  "Inserts PACK/UNPACK when the EXPR is trying to access the UNPACK/PACK-ed elements respectively."
  (declare (type list blueprints) (type node schedule-item))
  (let ((variable-table (make-hash-table)))
    (labels ((setvar (id type)
               (declare (type (member :packed :unpacked) type))
               (setf (gethash id variable-table) type))
             (readvar (id) (or (gethash id variable-table) :unpacked))
             (infer-expr (node expr meta &aux (vectorized-p (typep meta 'Vectorized)) (new-nodes) (expr-graph (copy-graph (expr-graph expr))))
               ;; Sort a copy of graph first.
               (verify-graph expr-graph)
               ;; Anything assignment to the array is :unpack
               (when (and vectorized-p (not (= -1 (caten/runtime:buffer-nrank (car (relay-writes (read-type-relay node)))))))
                 (setf (vectorized-perform meta) :unpack))
               
               (dolist (n (graph-nodes expr-graph))
                 (dolist (r (node-reads n))
                   (when (and (null vectorized-p) (eql :packed (readvar r)))
                     (print "INSERT :UNPACK1"))
                   (when (and vectorized-p (eql :unpacked (readvar r)))
                     (print "INSERT :PACK1")))
                 (dolist (w (node-writes n))
                   (if vectorized-p (setvar w :packed) (setvar w :unpacked))))
               ;; Some additional constraints for :unpack and :pack
               (when (and vectorized-p (find (vectorized-perform meta) `(:pack :unpack)))
                 ;; The last node of :unpack is unpacked
                 (dolist (r (node-reads (caten/codegen/expr:expr-out expr)))
                   ;; Ensure they are packed status
                   (when (eql (if (eql (vectorized-perform meta) :pack) :packed :unpacked) (readvar r))
                     (if (eql (vectorized-perform meta) :pack)
                         (print "INSERT :UNPACK")
                         (print "INSERT :PACK")))))
               (insert-nodes (expr-graph expr) new-nodes)))
      ;; TODO: Add the concept of scope
      (loop for bp in blueprints do
        (case (node-type bp)
          (:EXPR
           (infer-expr bp (getattr bp :EXPR) (getattr bp :meta :allow-undefined t)))))
      blueprints)))
