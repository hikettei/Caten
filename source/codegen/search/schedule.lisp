(defpackage :caten/codegen/search/schedule
  (:documentation "Provides a set of ISL operation which transforms a schedule theta")
  (:shadow #:set #:space)
  (:shadowing-import-from :cl :map)
  (:use :cl :caten/isl)
  (:export
   #:compute-dependence-relation
   #:compute-schedule-constraints
   #:zero-vector-union-set
   #:schedule-node-band-parallel-legal-p
   #:schedule-is-legal-p
   #:permutations
   #:permute-list
   #:schedule-node-band-get-depth
   #:schedule-node-band-get-coincident
   #:schedule-node-band-permute
   #:domain-dimension-maxima-from-union-map
   #:union-set-add-tiling-isolation-constraint
   #:union-set-list-add-nonempty
   #:schedule-node-band-tile*
   #:union-set-filter-by-dim-name
   #:partial-schedule-get-involved-dims
   #:tiling-size
   #:schedule-node-count-bands
   #:schedule-get-roots
   #:schedule-split-all-band
   #:schedule-fuse-all-band
   #:schedule-gather-path
   #:schedule-node-at-path
   #:schedule-get-non-marked-sequence/set
   #:schedule-remove-all-marks
   #:schedule-node-sequence-check-fusible
   #:schedule-node-sequence-full-fuse
   #:schedule-node-sequence-get-band-sizes
   #:schedule-node-sequence-align-band-size
   #:schedule-node-sequence-apply-flash
   #:schedule-node-band-get-n-chain
   #:schedule-node-band-scoop-up
   #:schedule-node-band-chain-sink
   #:schedule-node-sequence-splice-children
   #:schedule-node-sequence-reorder
   #:schedule-node-sequence-tpsort
   #:schedule-node-sequence-group-sequence
   #:schedule-compute-parallel
   #:umap-get-set-list-on-id
   #:%foreach-map
   #:%foreach-set
   #:align-params/umap
   #:align-params/uset
   #:schedule-detect-coalesce
   #:schedule-compute-dim-equalities-graph
   #:schedule-permute))

(in-package :caten/codegen/search/schedule)

(defun compute-dependence-relation (read write schedule)
  "Compute the classic memory-dependence relation Δ over the iteration domain
with respect to schedule S, using ISL flow analysis.
Inputs:
  read     : isl::union-map R ⊆ D×M  (read accesses)
  write    : isl::union-map W ⊆ D×M  (write accesses)
  schedule : isl::schedule S
Returns:
  isl::union-map Δ ⊆ D×D, where
    Δ = RAW_must ∪ WAW_must ∪ WAR_may .
Each pair (x, y) ∈ Δ denotes a sink instance x and a (must/may) source y such
that y precedes x under S and their memory accesses form flow/output/antidependences accordingly."
  (declare (type isl::union-map read write)
           (type isl::schedule schedule))
    (let* ((access (union-access-info-from-sink read))
           (access (union-access-info-set-must-source (! access) write))
           (access (union-access-info-set-schedule (! access) schedule))
           (flow (union-access-info-compute-flow (! access)))
           (RaW (union-flow-get-must-dependence (! flow)))
           (access (union-access-info-from-sink write))
           (access (union-access-info-set-must-source (! access) write))
           (access (union-access-info-set-may-source (! access) read))
           (access (union-access-info-set-schedule (! access) schedule))
           (flow   (union-access-info-compute-flow (! access)))
           (WaW    (union-flow-get-must-dependence flow))
           (WaR    (union-flow-get-may-dependence (! flow)))
           (dependencies (union-map-union (! (union-map-union WaR RaW)) WaW)))
      (values dependencies RaW WaW WaR)))

(defun compute-schedule-constraints (domain dependencies)
  "Build scheduling constraints C over an iteration domain D given a dependence relation Δ.
Inputs:
  domain        : isl::union-set D
  dependencies  : isl::union-map Δ ⊆ D×D  (e.g., RAW ∪ WAW ∪ WAR)
Returns:
  isl::schedule-constraints C on D with validity(C)=Δ, coincidence(C)=Δ,
  and proximity(C)=Δ. C can be passed to ISL's scheduler to compute a
  schedule S that respects Δ."
  (declare (type isl::union-set domain) (type isl::union-map dependencies))
  (let* ((schedule-constraints
           (schedule-constraints-on-domain domain))
         (schedule-constraints
           (schedule-constraints-set-coincidence
            (! schedule-constraints)
            dependencies))
         (schedule-constraints
           (schedule-constraints-set-validity
            (! schedule-constraints)
            dependencies))
         (schedule-constraints
           (schedule-constraints-set-proximity
            (! schedule-constraints)
            dependencies)))
    schedule-constraints))

(defun zero-vector-union-set (delta-uset)
  "Given a union-set U ⊆ ℤ^d, return the singleton union-set {0⃗} in the
same space as U. This is constructed by creating a zero multi-affine
mapping in the space of (set-from-union-set U) and converting it back to
a union-set.

Inputs:
  delta-uset : isl::union-set U

Returns:
  isl::union-set {0⃗} ⊆ ℤ^d"
  (declare (type isl::union-set delta-uset))
  (let* ((delta-set (set-from-union-set delta-uset))
         (ma (multi-aff-zero (set-get-space delta-set))))
    (union-set-from-set (set-from-multi-aff ma))))

(defun schedule-node-band-parallel-legal-p (node dep)
  "Return T iff the given band node N can be executed in parallel
without violating the dependence relation Δ.

Inputs:
  node : isl::schedule-node N  — a band node in the schedule tree
  dep  : isl::union-map Δ ⊆ D×D — dependence relation (RAW/WAW/WAR)

Semantics:
  The band is parallel-legal iff every dependence vector in Δ has
  zero distance in all loop dimensions carried by the band.

Returns: boolean"
  (declare (type isl::schedule-node-band node) (type isl::union-map dep))
  (when (union-map-is-empty dep) (return-from schedule-node-band-parallel-legal-p t))
  (let* ((map (schedule-node-band-get-partial-schedule-union-map node))
         (domain (union-map-apply-range (union-map-apply-domain dep map) map))
         (delta (union-map-deltas domain))
         (_ (when (union-set-is-empty delta) (return-from schedule-node-band-parallel-legal-p t)))
         (zeros (zero-vector-union-set delta))
         (cmp (union-set-lex-lt-union-set delta zeros))
         (retval (union-set-is-empty cmp))
         (cmp (union-set-lex-gt-union-set delta zeros)))
    (declare (ignore _))
    (and retval (union-set-is-empty cmp))))

(defun schedule-is-legal-p (schedule dep)
  "Return T iff the given schedule S respects all dependences Δ.

Inputs:
  schedule : isl::schedule S   — complete schedule
  dep      : isl::union-map Δ ⊆ D×D — dependence relation (RAW/WAW/WAR)

Semantics:
  The schedule is legally valid iff, for every (x, y) ∈ Δ, S(x) ≥_lex S(y),
  i.e., it does not reverse or violate the partial order induced by Δ.

Returns:
  boolean"
  (declare (type isl::schedule schedule) (type isl::union-map dep))
  (when (union-map-is-empty dep) (return-from schedule-is-legal-p t))
  (let* ((map (schedule-get-map schedule))
         (domain (union-map-apply-domain dep map))
         (domain (union-map-apply-range domain map))
         (delta (union-map-deltas domain))
         (_ (when (union-set-is-empty delta) (return-from schedule-is-legal-p nil)))
         (zeros (zero-vector-union-set delta))
         (le (union-set-lex-le-union-set delta zeros))
         (retval (union-set-is-empty le)))
    (declare (ignore _))
    retval))

(defun permutations (lst)
  "Return Π(L), the set of all permutations of a finite list L = [x₀,…,x_{n−1}].
Inputs:
  lst : proper list L (length n).
Returns:
  list of lists, each of length n; |Π(L)| = n! when all elements are distinct.
Notes:
  If L contains duplicates (under EQUAL), duplicate permutations may appear."
  (declare (optimize (speed 3)))
  (assert (listp lst) () "permutations: L must be a proper list.")
  (if (null lst) (list nil)
      (mapcan (lambda (x) (mapcar (lambda (y) (cons x y)) (permutations (remove x lst :count 1 :test #'equal)))) lst)))

(defun permute-list (op list)
  "Apply an index permutation π to a list X. If X = [x₀,…,x_{m−1}] and
π = [i₀,…,i_{m−1}] is a permutation of {0,…,m−1}, return
[x_{i₀},…,x_{i_{m−1}}].
Inputs:
  op   : list of indices π, length m, integers in [0,m−1], all distinct.
  list : list X, length m.
Returns:
  list X∘π of length m."
  (declare (type list op list))
  (let ((m (length list)))
    (assert (= m (length op)) () "permute-list: length(OP)=~a ≠ length(LIST)=~a." (length op) m)
    (assert (every #'integerp op) () "permute-list: indices must be integers.")
    (assert (every (lambda (i) (and (>= i 0) (< i m))) op) () "permute-list: indices must lie in [0,~a)." m)
    (assert (= (length op) (length (remove-duplicates op))) ()  "permute-list: indices must be all distinct."))
  (loop for nth in op collect (nth nth list)))

(defun schedule-node-band-get-depth (band) (space-dim (schedule-node-band-get-space band) 3))

(defun schedule-node-band-get-coincident (band)
  "Return the coincidence indicator vector χ ∈ {0,1}^d for a band node.
Inputs:
  band : isl::schedule-node-band  — a band with depth d.
Returns:
  list of integers [χ0,…,χ_{d−1}], where χi=1 iff the i-th band member
  is marked coincident by ISL (i.e., carries no dependence and is thus
  eligible for parallel/permutable treatment); otherwise χi=0.
Notes:
  Here d = schedule-node-get-band-depth(band). This is an ISL-level property
  used by schedulers to expose parallelism."
  (loop for i upfrom 0 below (schedule-node-band-get-depth band)
        if (eql :bool-true (isl::%isl-schedule-node-band-member-get-coincident (isl::schedule-node-handle band) i))
          collect 1 else collect 0))

(defun schedule-node-band-permute (band order)
  "Permute the dimensions of a permutable band by a permutation π.
Let d be the band depth. Given π = [i₀,…,i_{d−1}] a permutation of {0,…,d−1},
reorder both the band’s partial schedule components and their coincidence flags
accordingly.
Inputs:
  band  : isl::schedule-node-band (permutable).
  order : list π of length d, a permutation of {0,…,d−1}.
Returns:
  isl::schedule-node (the updated band node)."
  (declare (type isl:schedule-node-band band) (type list order))
  (assert (eql :bool-true (isl::%isl-schedule-node-band-get-permutable (isl::schedule-node-handle band)))
          ()
          "schedule-node-band-permute: band must be permutable.")
  (let ((depth (schedule-node-band-get-depth band)))
    (assert (> depth 0) () "schedule-node-band-permute: band depth must be positive.")
    (assert (= depth (length order)) ()
            "schedule-node-band-permute: |order|=~a ≠ depth=~a." (length order) depth)
    (assert (every #'integerp order) ()
            "schedule-node-band-permute: order must contain integers.")
    (assert (equal (loop for i upfrom 0 below depth collect i)
                   (sort (copy-list order) #'<))
            ()
            "schedule-node-band-permute: order must be a permutation of 0..~a." (1- depth))
    (let* ((mupa (schedule-node-band-get-partial-schedule band)) ;; defensive: mupa should match depth
           (mupa-size (isl::multi-union-pw-aff-size mupa)))
      (assert (= mupa-size depth) ()
              "schedule-node-band-permute: partial schedule size (~a) ≠ band depth (~a)."
              mupa-size depth)
      (let* ((coincidents (schedule-node-band-get-coincident band))
             (upas (loop for i upfrom 0 below depth
                         collect (multi-union-pw-aff-get-union-pw-aff mupa i)))
             (coincidents-new (permute-list order coincidents))
             (upas-new (permute-list order upas)))
        (loop for i upfrom 0 below depth do
              (setf mupa (multi-union-pw-aff-set-union-pw-aff mupa i (nth i upas-new))))
        (setf band (schedule-node-insert-partial-schedule band mupa))
        (loop for i upfrom 0 below depth do
              (setf band (isl::schedule-node-band-member-set-coincident band i (nth i coincidents-new))))
        band))))
;; ~~ DomainMaximaResults ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defvar *domain-maxima-results*)
(cffi:defcfun ("isl_map_domain_tuple_dim" %isl-map-domain-tuple-dim) :int (x :pointer))
(cffi:defcfun ("isl_set_tuple_dim" %isl-set-tuple-dim) :int (x :pointer))
(cffi:defcfun ("isl_set_dim_max_val" %isl-set-dim-max-val) :pointer (x :pointer)  (pos :int))
(cffi:defcallback extract-domain-maxima-bset-cb :int
    ((bset :pointer) (user :pointer))
  (let ((set (isl::%isl-set-from-basic-set bset))) ;; todo: check memory leak
    (dotimes (pos (cffi:mem-ref user :int))
      (let ((cpy (isl::%isl-set-copy set))
            (dname (isl::%isl-basic-set-get-dim-name bset :dim-set pos)))
        (setf (gethash dname *domain-maxima-results*)
              (isl::%make-value (%isl-set-dim-max-val cpy pos))))))
  0)

(cffi:defcallback extract-domain-maxima-map-cb :int
    ((map :pointer) (user :pointer))
  (declare (ignore user))
  (cffi:with-foreign-objects ((size :int))
    (setf (cffi:mem-aref size :int) (%isl-map-domain-tuple-dim map))
    (isl::%isl-set-foreach-basic-set (isl::%isl-map-wrap map) (cffi:callback extract-domain-maxima-bset-cb) size))
  0)

(defun domain-dimension-maxima-from-union-map (umap)
  "Compute per-dimension maxima over the domain of a union map.
Inputs:
  umap : isl::union-map U
Returns:
  hash-table H mapping dim-name (string) ↦ isl::value vmax, where
    vmax = max { x_k | x ∈ Dom(U) } for the k-th (named) domain dimension.
Notes:
  If a dimension is unbounded, the underlying ISL may yield a sentinel
  (e.g., +∞) in the returned isl::value. Only dimensions that carry names
  are recorded."
  (declare (type isl::union-map umap))
  (let ((*domain-maxima-results* (make-hash-table :test 'equal)))
    ;; [TODO]
    ;; - Stop using cffi:defcallback (SBCL/ECL dependant)
    ;; - Stop using *domain-maxima-results* (this is not thread-safe) pass pointers via cffi
    (isl::%isl-union-map-foreach-map
     (isl::union-map-handle umap)
     (cffi:callback extract-domain-maxima-map-cb)
     (cffi:null-pointer))
    *domain-maxima-results*))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun union-set-add-tiling-isolation-constraint (dom width dim-name dom-size is-full-tile-p)
  "Add a tiling isolation inequality on a named domain dimension.
Inputs:
  dom           : isl::union-set U
  width         : isl::value w ∈ ℤ_{>0}  (tile width)
  dim-name      : string k                (domain dimension name)
  dom-size      : isl::value N ∈ ℤ_{>0}   (extent of dimension k)
  is-full-tile-p: boolean                 (T: keep full tiles, NIL: keep tail)
Let B = N − (N mod w). This function adds, for each set in U whose k-th
dimension is named dim-name:
  • if is-full-tile-p = T  :  x_k ≤ B − 1   (keep only full tiles)
  • if is-full-tile-p = NIL:  x_k ≥ B       (keep only the partial tail)
Sets without the named dimension are left unchanged. Returns the union-set
with the added inequality."
  (declare (type isl::union-set dom) (type string dim-name) (type isl::value dom-size width) (type boolean is-full-tile-p))
  (let* ((lst (isl::union-set-get-set-list dom))
         (cnt (isl::set-list-n-set lst))
         (res nil)
         (bound (value- (value- dom-size (value-mod dom-size width)) (value 1)))) ;; bound = (domain_size) - (domain_size mod width)
    (when (not is-full-tile-p)
      (setf bound (value+ bound (value 1))))
    (loop for i from 0 below cnt do
      (let* ((s (isl::set-list-get-at lst i))
             (bsl (isl::set-get-basic-set-list s))
             (b0  (isl::basic-set-list-get-at bsl 0))
             (nd  (isl::basic-set-dim b0 :dim-set))
             (pos (loop for k from 0 below nd
                        for nm = (isl::%isl-basic-set-get-dim-name (isl::basic-set-handle b0) :dim-set k)
                        when (and nm (string= nm (string dim-name))) do (return k))))
        (if pos
            (let* ((sp  (isl::set-get-space s))
                   (ls  (isl::local-space-from-space sp))
                   (ineq (make-inequality-constraint ls))
                   (ineq (isl::set-constant-val ineq (if is-full-tile-p bound (value-neg bound))))
                   (ineq (isl::set-coefficient-si ineq :dim-set pos (if is-full-tile-p -1 1)))
                   (s (isl::set-add-constraint s ineq))
                   (u (isl::union-set-from-set s)))
              (setf res (if res (isl::union-set-union res u) u)))
            (let ((s (isl::union-set-from-set s)))
              (setf res (if res (isl::union-set-union res s) s))))))
    res))

(defun union-set-list-add-nonempty (lst uset)
  "Append a union-set U to a union-set-list L only if U ≠ ∅.
Inputs:
  lst  : isl::union-set-list L
  uset : isl::union-set U (may be NIL)
Returns:
  isl::union-set-list L' = L ⊎ {U} if U is non-null and non-empty;
  otherwise returns L unchanged."
  (if (or (null uset) (isl::union-set-is-empty uset))
      lst
      (isl::union-set-list-add lst uset)))

(defun make-full/partial-filters (subdom tiled-ids maxima width)
  "Construct tile-isolation filters over a subdomain and return (full, tail).
Inputs:
  subdom    : isl::union-set U                        ; subdomain to filter
  tiled-ids : list of strings T = {k}                 ; names of tiled dims
  maxima    : hash-table (string → isl::value N_k)    ; per-dim extents
  width     : isl::value w ∈ ℤ_{>0}                   ; tile width
Semantics:
  For each named dimension k∈T with extent N_k, let B_k := N_k − (N_k mod w).
  The result consists of:
    full = ⋂_k { x ∈ U | x_k ≤ B_k − 1 }   ; keep only full tiles
    tail = ⋂_k { x ∈ U | x_k ≥ B_k }       ; keep only the partial tail
Returns:
  (values full tail) where full, tail are isl::union-set."
  (let ((full subdom) (tail subdom))
    (dolist (name tiled-ids)
      (let ((max (gethash name maxima)))
        (setf full (union-set-add-tiling-isolation-constraint full width name max t))
        (setf tail (union-set-add-tiling-isolation-constraint tail width name max nil))))
    (values full tail)))

(defun partial-schedule-get-involved-dims (mupa)
  "Return the set of named domain dimensions involved in a partial schedule.
Inputs:
  mupa : isl::multi-union-pw-aff Φ  — a (partial) schedule with components Φ_i
Returns:
  a list of strings {k}, the names of set-dim coordinates that any piecewise
  affine component of Φ depends on (detected via isl_pw_aff_involves_dims).
Notes:
  Only dimensions that carry names (ids) are reported; unnamed dims are skipped.
  Duplicates across components are removed with string equality."
  (declare (type isl::multi-union-pw-aff mupa))
  (remove-duplicates
   (loop for i below (isl::multi-union-pw-aff-size mupa) append
         (let* ((upa (multi-union-pw-aff-get-union-pw-aff mupa i))
                (lst (isl::union-pw-aff-get-pw-aff-list upa)))
           (loop for k below (isl::%isl-pw-aff-list-n-pw-aff (isl::pw-aff-list-handle lst)) append
                 (let* ((pwa (isl::pw-aff-list-get-at lst k)) (dom (isl::pw-aff-domain pwa)))
                   (loop for d below (isl::set-dim dom :dim-set)
                         when (eql :bool-true (isl::%isl-pw-aff-involves-dims (isl::pw-aff-handle pwa) :dim-in d 1))
                         collect (isl::identifier-name-str (isl::set-get-dim-id dom :dim-set d)))))))
   :test #'string=))

(defun union-set-filter-by-dim-name (uset dim-name)
  "Filter a union-set by keeping only components that contain a named dimension.
Inputs:
  uset     : isl::union-set U = ⨆_i S_i
  dim-name : string k (dimension name)
Returns:
  isl::union-set U' = ⨆ { S_i ∈ U | k ∈ names(S_i) }.
If no component set S_i has a set-dimension named k, returns the empty
union-set in the space of U. Name matching is by string equality and only
named dimensions are considered."
  (declare (type isl::union-set uset) (type string dim-name))
  (let* ((lst (isl::union-set-get-set-list uset))
         (n   (isl::set-list-n-set lst))
         (acc nil))
    (dotimes (i n)
      (let* ((s  (isl::set-list-get-at lst i))
             (bs (isl::set-get-basic-set-list s))
             (b0 (isl::basic-set-list-get-at bs 0))
             (nd (isl::basic-set-dim b0 :dim-set))
             (hit (loop for k below nd
                        for nm = (isl::%isl-basic-set-get-dim-name (isl::basic-set-handle b0) :dim-set k)
                        thereis (and nm (string= nm dim-name)))))
        (when hit
          (setf acc (if acc (isl::union-set-union acc (isl::union-set-from-set s)) (isl::union-set-from-set s))))))
    (or acc (isl::union-set-empty (isl::union-set-get-space uset)))))

(defun tiling-size (band size)
  "Construct a uniform tiling width vector in the band space.
Inputs:
  band : isl::schedule-node-band B
  size : fixnum w ∈ ℤ_{>0}
Returns:
  isl::multi-val m of length d, where d = dim(space(B), out) and
  m = (w,…,w) ∈ ℤ^d embedded in the band’s schedule space. This is the
  per-dimension tile width vector used by ISL band tiling."
  (declare (type (or isl::value fixnum) size) (type isl::schedule-node-band band))
  ;; [TODO] Support Symbolic Tile
  (let* ((band-space (schedule-node-band-get-space band))
         (dim (space-dim band-space 3)))
    (multi-val-from-val-list band-space (apply #'make-value-list (loop for i upfrom 0 below dim collect size)))))

(defun schedule-node-band-tile* (band size &key (strategy :atomic) (directive) (sink) (scale))
  "Tile a permutable band by width w, optionally isolating partial tiles.
Inputs:
  band     : isl::schedule-node-band
  size     : integer or isl::value w (tile width)
  strategy : one of {:isolate,:padding,:atomic,:guard}
             - :isolate partitions the subtree domain U into {unaffected, full, tail} using per-dimension maxima from the prefix schedule;
             - :atomic uses min/max style
             - :guard
             - :padding
Returns:
  isl::schedule-node — the tiled band"
  (declare (type (or fixnum isl::value) size)
           (type (or null fixnum isl::value) scale))
  (let ((tiled (schedule-node-band-tile band (tiling-size band size))))
    (when scale
      (setf tiled (isl::schedule-node-band-scale tiled (tiling-size band scale))))
    (ecase strategy
      (:isolate
       (let* ((subdom (union-map-domain (isl::schedule-node-get-subtree-expansion tiled)))
              (mupa (schedule-node-band-get-partial-schedule tiled))
              (tiled-ids (partial-schedule-get-involved-dims mupa)))
         (when (null tiled-ids) (return-from schedule-node-band-tile* tiled))
         (let* ((maxima (domain-dimension-maxima-from-union-map (isl::schedule-node-get-prefix-schedule-relation tiled)))
                (width (if (valuep size) size (value size)))
                (affected
                  (reduce
                   #'isl::union-set-union
                   (map 'list #'(lambda (nm) (union-set-filter-by-dim-name subdom nm)) tiled-ids)
                   :initial-value (isl::union-set-from-str "{}")))
                (unaffected (isl::union-set-subtract subdom affected)))
           (multiple-value-bind (full tail)
               (make-full/partial-filters affected tiled-ids maxima width)
             (let* ((lst (isl::union-set-list-alloc 0))
                    (lst (union-set-list-add-nonempty lst unaffected))
                    (lst (union-set-list-add-nonempty lst full))
                    (lst (union-set-list-add-nonempty lst tail))
                    (sched (isl::schedule-node-insert-sequence tiled lst)))
               sched)))))
      (:padding tiled)
      (:atomic  tiled)
      (:guard   tiled))))

(defun schedule-node-band-separate (band size)
  (isl::%isl-options-set-tile-shift-point-loops (isl::context-handle isl::*context*) 1)
  (isl::%isl-options-set-tile-scale-tile-loops (isl::context-handle isl::*context*) 1)
  (let ((size (tiling-size band size)))
    (isl::schedule-node-band-scale-down
     (isl::schedule-node-band-tile band size)
     size)))

(defun schedule-node-count-bands (node)
  "Count how many band nodes exist in the subtree rooted at NODE."
  (declare (type isl::schedule-node node))
  (let ((self (if (eql (schedule-node-get-type node) :schedule-node-band)
                  (schedule-node-band-get-depth node)
                  0)))
    (+ self (reduce #'+ (mapcar #'schedule-node-count-bands (schedule-node-get-children node)) :initial-value 0))))

(defun schedule-get-roots (schedule)
  (declare (type isl::schedule schedule))
  (let ((root (schedule-node-get-child (schedule-get-root schedule) 0)))
    (case (schedule-node-get-type root)
      (:schedule-node-sequence
       (let ((n-child (isl::%isl-schedule-node-n-children (isl::schedule-node-handle root))))
         (loop for i upfrom 0 below n-child
               collect (schedule-node-get-child root i))))
      (otherwise (list root)))))
;; The code below is not TESTED (TODO: Use this to restrict tile dims)
(defun schedule-node-subtree-domain (node)
  "Return the statement iteration domain of the subtree rooted at NODE.
Inputs:
  node : isl::schedule-node
Returns:
  isl::union-set U = Dom(Exp(NODE)), i.e., the union of statement domains
  covered by the subtree expansion of NODE."
  (caten/isl::union-map-domain (caten/isl::schedule-node-get-subtree-expansion node)))

(defun restrict-umap-to-domain (umap uset)
  "Intersect the domain of a union map with a given union set.
Inputs:
  umap : isl::union-map F
  uset : isl::union-set U
Returns:
  isl::union-map F' = F ∩ (U × Range(F)) = intersect_domain(F, U).
If U ∩ Dom(F) = ∅, the result is the empty union map."
  (caten/isl::union-map-intersect-domain umap uset))

(defun union-map-same-address-relation (acc)
  "Build the same-address (alias) relation over iteration points.
Inputs:
  acc : isl::union-map A ⊆ Iter × Buf   ; e.g., read/write access relation
Returns:
  isl::union-map R ⊆ Iter × Iter defined as R = A ∘ A^{-1}.
Semantics:
  (i, j) ∈ R  ⇔  ∃b ∈ Buf s.t. (i, b) ∈ A ∧ (j, b) ∈ A  (i, j access the same address)."
  (let* ((inv (caten/isl::union-map-reverse acc)))
    (caten/isl::union-map-apply-range inv acc)))

(defun has-data-reuse-in-subtree (schedule-node reads-umap)
  "Detect data reuse within the subtree rooted at SCHEDULE-NODE using read accesses.
Inputs:
  schedule-node : isl::schedule-node
  reads-umap    : isl::union-map R ⊆ Iter × Buf   ; read access relation
Returns:
  boolean — T iff the restricted same-address relation over the subtree domain
  is non-empty, i.e., ∃(i, j) within the subtree such that both read the same
  buffer element.
Procedure:
  U := Dom(Exp(schedule-node));
  R' := intersect_domain(R, U);
  return (R' = ∅) ? NIL : (A∘A^{-1} over R' is non-empty)."
  (let* ((subdom (schedule-node-subtree-domain schedule-node))
         (reads  (restrict-umap-to-domain reads-umap subdom)))
    (if (caten/isl::union-map-is-empty reads)
        nil
        (let ((same (union-map-same-address-relation reads)))
          (not (caten/isl::union-map-is-empty same))))))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;   ISL ScheduleTree Rewriting Rules
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun schedule-map (schedule callback &optional (user (cffi:null-pointer)))
  (isl::%make-schedule
   (isl::%isl-schedule-map-schedule-node-bottom-up
    (isl::schedule-handle (copy schedule))
    callback
    user)))

(cffi:defcallback rewrite/split-band :pointer
    ((band :pointer) (user :pointer))
  (declare (ignore user))
  (if (eql (isl::%isl-schedule-node-get-type band) :schedule-node-band)
      (let ((depth (schedule-node-band-get-depth (isl::%%make-schedule-node-band band))))
        (dotimes (i depth)
          (setf band (isl::%isl-schedule-node-band-split band (- depth i))))
        band)
      band))

(cffi:defcallback rewrite/collapse-band :pointer
    ((band :pointer) (user :pointer))
  (declare (ignore user))
  ;; 3*_gid0+_gid1 ==> (_gid0), (_gid1)
  (if (eql (isl::%isl-schedule-node-get-type band) :schedule-node-band)
      (let* ((bn (isl::%make-schedule-node band))
             (new-sched) (found-p)
             (mupa (schedule-node-band-get-partial-schedule bn)))
        (loop for i upfrom 0 below (multi-union-pw-aff-size mupa)
              for upa = (multi-union-pw-aff-get-union-pw-aff mupa i) do
                (%foreach-pwa
                 upa
                 #'(lambda (pwa)
                     (%foreach-piece
                      pwa
                      #'(lambda (dom aff)
                          (declare (ignore dom))
                          (loop for d upfrom 0 below (aff-dim aff :dim-in)
                                for coeff = (aff-get-coefficient-val aff :dim-in d)
                                when (= 1 (value-sign coeff)) do
                                  (when (> (val->int coeff) 1) (setf found-p t))
                                  (let ((aff1 aff))
                                    (dotimes (n (aff-dim aff :dim-in))
                                      (if (= n d)
                                          (setf aff1 (aff-set-coefficient-si aff1 :dim-in n 1))
                                          (setf aff1 (aff-set-coefficient-si aff1 :dim-in n 0))))
                                    (let ((mupa-at-dim (multi-union-pw-aff-from-union-pw-aff (union-pw-aff-from-aff aff1))))
                                      (if new-sched
                                          (setf new-sched (multi-union-pw-aff-flat-range-product mupa-at-dim new-sched))
                                          (setf new-sched mupa-at-dim))))))))))
        (if (and new-sched found-p)
            (let ((band (isl::%isl-schedule-node-delete band)))
              (dotimes (i (multi-union-pw-aff-size new-sched) band)
                (let ((mupa (multi-union-pw-aff-from-union-pw-aff (multi-union-pw-aff-get-union-pw-aff new-sched i))))
                  (setf band (isl::%isl-schedule-node-insert-partial-schedule band (isl::multi-union-pw-aff-handle (copy mupa)))))))
            band))
      band))

(cffi:defcallback rewrite/fuse-band :pointer
    ((band :pointer) (user :pointer))
  (declare (ignore user))
  (if (eql (isl::%isl-schedule-node-get-type band) :schedule-node-band)
      (let ((depth (schedule-node-band-get-n-chain (isl::%%make-schedule-node-band band))))
        (if (= 0 depth)
            band
            (let ((coincidents)
                  (mupa (schedule-node-band-get-partial-schedule (isl::%%make-schedule-node-band band))))
              (dotimes (nth-band (isl::%isl-schedule-node-band-n-member band))
                (push (isl::%isl-schedule-node-band-member-get-coincident band nth-band) coincidents))
              (dotimes (i depth)
                (setf band (isl::%isl-schedule-node-delete band))
                (let ((sched (schedule-node-band-get-partial-schedule (isl::%%make-schedule-node-band band))))
                  (setf mupa (multi-union-pw-aff-flat-range-product mupa sched))
                  (dotimes (nth-band (isl::%isl-schedule-node-band-n-member band))
                    (push (isl::%isl-schedule-node-band-member-get-coincident band nth-band) coincidents))))
              (let ((band (isl::%isl-schedule-node-insert-partial-schedule (isl::%isl-schedule-node-delete band) (isl::multi-union-pw-aff-handle mupa))))
                (setf coincidents (reverse coincidents))
                (dotimes (i (length coincidents))
                  (setf band (isl::%isl-schedule-node-band-member-set-coincident
                              band i
                              (if (eql :bool-true (nth i coincidents)) 1 0))))
                band))))
      band))

(defun schedule-split-all-band (schedule)
  (schedule-map schedule (cffi:callback rewrite/split-band)))

(defun schedule-fuse-all-band (schedule)
  "band+child+band ==> [band+band]"
  (schedule-map schedule (cffi:callback rewrite/fuse-band)))

(defun schedule-collapse-all-band (schedule)
  (schedule-map schedule (cffi:callback rewrite/collapse-band)))
;; ~~~ ILP ShapeTracker Solver ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Prerequisites:
;; - The given schedule is obtained by schedule-split-all-band
;; - No mark is inserted
(defun schedule-node-at-path (node path)
  "PATH = (i0 i1 ... ik) from root->child(0)."
  (labels ((n-children (n)
             (isl::%isl-schedule-node-n-children (isl::schedule-node-handle n)))
           (child (n i)
             (let ((cnt (n-children n)))
               (assert (and (>= i 0) (< i cnt))
                       () "child index ~a out of [0,~a)" i cnt))
             (schedule-node-get-child n i)))
    (dolist (idx path node) (setf node (child node idx)))))

(defun schedule-gather-path (schedule-node f &aux (results))
  (labels ((explore (node path)
             (when (eql (schedule-node-get-type node) :schedule-node-leaf)
               (return-from explore))
             (when (funcall f node path)
               (push path results))
             (let ((n-child (isl::%isl-schedule-node-n-children (isl::schedule-node-handle node))))
               (dotimes (i n-child)
                 (explore (schedule-node-get-child node i) (append path (list i)))))))
    (explore schedule-node nil)
    (nreverse results)))

(defun schedule-get-non-marked-sequence/set (schedule)
  "Returns a list of schedule_node_sequence/set"
  (schedule-gather-path
   (schedule-get-root schedule)
   #'(lambda (node path)
       (declare (ignore path))
       (and
        (find (schedule-node-get-type node) '(:schedule-node-sequence :schedule-node-set))
        (> (isl::%isl-schedule-node-n-children (isl::schedule-node-handle node)) 1)
        (let ((parent (isl::schedule-node-parent node)))
          (not (eql (schedule-node-get-type parent) :schedule-node-mark)))))))

(defun schedule-remove-empty-schedule (schedule)
  (let ((paths
          (schedule-gather-path
           (schedule-get-root schedule)
           #'(lambda (node path)
               (declare (ignore path))
               (or
                (when (eql (schedule-node-get-type node) :schedule-node-band)
                  (let ((p (schedule-node-band-get-partial-schedule node)))
                    (= 0 (space-dim (isl::multi-union-pw-aff-get-space p) :dim-out))))
                (eql (schedule-node-get-type node) :schedule-node-leaf))))))
    (dolist (path paths)
      (setf schedule
            (schedule-node-get-schedule
             (isl::schedule-node-delete
              (schedule-node-at-path (schedule-get-root schedule) path)))))
    schedule))

(defun schedule-remove-all-marks (schedule) ;; todo: optimize
  (let ((paths
          (schedule-gather-path
           (schedule-get-root schedule)
           #'(lambda (node path)
               (declare (ignore path))
               (eql :schedule-node-mark (schedule-node-get-type node))))))
    (when paths
      (setf schedule
            (schedule-node-get-schedule
             (isl::schedule-node-delete
              (schedule-node-at-path (schedule-get-root schedule) (car paths)))))
      (return-from schedule-remove-all-marks (schedule-remove-all-marks schedule)))
    schedule))

(defun schedule-node-sequence-check-fusible (components)
  "Return: T or reason={:filter, :dependencies, :ReorderFirst}"
  (assert (find (schedule-node-get-type components) '(:schedule-node-sequence :schedule-node-set)))
    (let* ((n-child (isl::%isl-schedule-node-n-children (isl::schedule-node-handle components)))
           (node (isl:schedule-node-first-child components))
           (mupa))
      (loop for i upfrom 0 below n-child do
        (let ((filter (isl::schedule-node-filter-get-filter node)))
          (setf node (schedule-node-first-child node))
          (when (not (eql (schedule-node-get-type node) :schedule-node-band))
            (return-from schedule-node-sequence-check-fusible :need-reorder))
          (let* ((tmp (schedule-node-band-get-partial-schedule node))
                 (tmp (isl::multi-union-pw-aff-intersect-domain tmp filter))
                 (tmp (isl::multi-union-pw-aff-reset-tuple-id tmp :dim-out)))
            (if (null mupa)
                (setf mupa tmp)
                (setf mupa (isl::multi-union-pw-aff-union-add mupa tmp)))
            (setf node (schedule-node-delete node)
                  node (isl::schedule-node-parent node))
            (if (= i (1- n-child))
                (setf node (isl::schedule-node-parent node))
                (setf node (isl::schedule-node-next-sibling node))))))
      :valid))

(defun schedule-node-sequence-full-fuse (components)
  "Fuse all children in the given components (schedule_node_sequence or schedule_node_set)"
  (assert (find (schedule-node-get-type components) '(:schedule-node-sequence :schedule-node-set)))
  (let* ((components (isl::schedule-node-first-child (schedule-node-insert-mark components (isl::make-id-from-str "@ApplyOptimization{FULL_FUSE}"))))
         (n-child (isl::%isl-schedule-node-n-children (isl::schedule-node-handle components)))
         (node (isl:schedule-node-first-child components))
         (mupa))
    (loop for i upfrom 0 below n-child do
      (let ((filter (isl::schedule-node-filter-get-filter node)))
        (setf node (schedule-node-first-child node))
        (when (not (eql (schedule-node-get-type node) :schedule-node-band))
          (error "schedule-full-fuse: The children of each filter in sequence should have a schedule_node_band."))
        (let* ((tmp (schedule-node-band-get-partial-schedule node))
               (tmp (isl::multi-union-pw-aff-intersect-domain tmp filter))
               (tmp (isl::multi-union-pw-aff-reset-tuple-id tmp :dim-out)))
          (if (null mupa)
              (setf mupa tmp)
              (setf mupa (isl::multi-union-pw-aff-union-add mupa tmp)))
          (setf node (schedule-node-delete node)
                node (isl::schedule-node-parent node))
          (if (= i (1- n-child))
              (setf node (isl::schedule-node-parent node))
              (setf node (isl::schedule-node-next-sibling node))))))
    (when mupa (setf node (schedule-node-insert-partial-schedule node mupa)))
    ;;(schedule-remove-empty-schedule (schedule-node-get-schedule node))
    (schedule-node-get-schedule node)))

(defun align-params/umap (umap model-space)
  "Return UMAP aligned to MODEL-SPACE (params only). No callbacks."
  (let* ((ml (isl::union-map-get-map-list umap))
         (n  (isl::map-list-size ml))
         (acc nil))
    (dotimes (i n)
      (let* ((m   (isl::map-list-elt ml i))
             (m*  (isl::map-align-params m model-space)))
        (setf acc (if acc
                      (isl::union-map-union acc (isl::map-union-map m*))
                      (isl::map-union-map m*)))))
    (or acc
        (isl::union-map-empty
         (isl::space-align-params
          (isl::union-map-get-space umap) model-space)))))

(defun align-params/uset (uset model-space)
  "Return USET aligned to MODEL-SPACE (params only). No callbacks."
  (let* ((sl (isl::union-set-get-set-list uset))
         (n  (isl::set-list-n-set sl))
         (acc nil))
    (dotimes (i n)
      (let* ((s  (isl::set-list-get-at sl i))
             (s* (isl::set-align-params s model-space)))
        (setf acc (if acc
                      (isl::union-set-union acc (isl::union-set-from-set s*))
                      (isl::union-set-from-set s*)))))
    (or acc
        (isl::union-set-empty
         (isl::space-align-params
          (isl::union-set-get-space uset) model-space)))))

(defun band-range-union-set (band-node user-domain)
  (let* ((prefix (isl::schedule-node-get-prefix-schedule-union-map band-node)) ; Dom -> Prefix
         (part   (isl::schedule-node-band-get-partial-schedule-union-map band-node)) ; Dom -> Band
         (model-space
           (isl::space-align-params
            (isl::union-map-get-space prefix)
            (isl::union-set-get-space user-domain)))
         (prefix* (align-params/umap prefix model-space))
         (part*   (align-params/umap part   model-space))
         (udom*   (align-params/uset user-domain model-space))
         (full    (isl::union-map-flat-range-product prefix* part*)) ; Dom -> (Prefix × Band)
         (full    (isl::union-map-intersect-domain full (copy udom*)))
         (rng     (isl::union-map-range full)))
    rng))

(defun uset-max/min (uset)
  "Assume USAGE: uset = { [i0] : 0 <= i0 <= N } (1-D, single set, constants).
   Return the constant upper bound N as isl::val."
  (let* ((sl (isl::union-set-get-set-list uset))
         (n  (isl::set-list-n-set sl)))
    (unless (= n 1) (error "union-set must contain exactly one set."))
    (let* ((s   (isl::set-list-get-at sl 0))
           (dim (isl::set-dim s :dim-set)))
      ;; (print uset)
      ;; (print dim)
      ;; (unless (= dim 1) (error "set must be 1-dimensional."))
      ;; always refer to the last dimension?
      (values (isl::set-dim-max-val s (1- dim)) (isl::set-dim-min-val s (1- dim))))))

(defun schedule-node-sequence-get-band-sizes (domain components)
  "Enumerates all childrens domain size"
  (assert (find (schedule-node-get-type components) '(:schedule-node-sequence :schedule-node-set)))
  (let* ((n-child (isl::%isl-schedule-node-n-children (isl::schedule-node-handle components)))
         (node (isl:schedule-node-first-child components))
         (sizes))
    (loop for i upfrom 0 below n-child do
      (setf node (schedule-node-first-child node))
      (when (not (eql (schedule-node-get-type node) :schedule-node-band))
        (error "schedule-node-sequence-get-band-sizes: The children of each filter in sequence should have a schedule_node_band."))
      (let ((uset (band-range-union-set node domain)))
        (multiple-value-bind (max min) (uset-max/min uset)
          (assert (value= min (value 0)))
          (push (value+ max (value 1)) sizes)))
      (setf node (schedule-node-delete node) node (isl::schedule-node-parent node))
      (if (= i (1- n-child))
          (setf node (isl::schedule-node-parent node))
          (setf node (isl::schedule-node-next-sibling node))))
    (multiple-value-bind (min max) (values (reduce #'value-min sizes) (reduce #'value-max sizes))
      (values sizes min max (value= min max)
              (every #'(lambda (x) (value= (value 0) (value-mod x min))) sizes)))))

(defun schedule-node-sequence-align-band-size (components sizes)
  (assert (find (schedule-node-get-type components) '(:schedule-node-sequence :schedule-node-set)))
  (let* ((min-band (reduce #'value-min sizes))
         (max-band (reduce #'value-max sizes)))
    (when (value= min-band max-band)
      (return-from schedule-node-sequence-align-band-size components))
    (let* ((n-child (isl::%isl-schedule-node-n-children (isl::schedule-node-handle components)))
           (node components))
      (loop for i upfrom 0 below n-child
            for size = (pop sizes)
            if (not (value= size max-band)) do
              (setf node (schedule-node-get-child node i)
                    node (schedule-node-first-child node))
              (when (not (eql (schedule-node-get-type node) :schedule-node-band))
                (error "schedule-node-sequence-align-band-size: The children of each filter in sequence should have a schedule_node_band."))
              (setf node (schedule-node-band-separate node (value-div max-band size))
                    node (isl::schedule-node-parent (isl::schedule-node-parent node))))
      node)))

(defun schedule-node-band-reshape (band band-size reshape-to)
  (isl::%isl-options-set-tile-shift-point-loops (isl::context-handle isl::*context*) 1)
  (isl::%isl-options-set-tile-scale-tile-loops (isl::context-handle isl::*context*) 1)
  (schedule-node-band-tile*
   band (value-floor (value-div band-size reshape-to))
   :strategy :atomic
   :sink nil
   :scale (value-div reshape-to band-size)))

(defun schedule-node-sequence-apply-flash (domain components domain-size)
  (declare (type isl::schedule-node components) (type isl::union-set domain) (type isl::value domain-size))
  (assert (find (schedule-node-get-type components) '(:schedule-node-sequence :schedule-node-set)))
  (let* ((n-child (isl::%isl-schedule-node-n-children (isl::schedule-node-handle components)))
         (node components))
    (loop for i upfrom 0 below n-child do
      (setf node (schedule-node-get-child node i)
            node (schedule-node-first-child node))
      (when (not (eql (schedule-node-get-type node) :schedule-node-band))
        (error "schedule-node-sequence-apply-flash: The children of each filter in sequence should have a schedule_node_band."))
      (let ((uset (band-range-union-set node domain)))
        (multiple-value-bind (max min) (uset-max/min uset)
          (assert (value= min (value 0)))
          (let ((max (value+ max (value 1))))
            (when (not (value= max domain-size))
              (setf node
                    (isl::schedule-node-parent
                     (schedule-node-band-chain-sink
                      (schedule-node-first-child (schedule-node-band-reshape node max domain-size)))))))))
      (setf node (isl::schedule-node-parent (isl::schedule-node-parent node))))
    node))

(defun schedule-node-band-get-n-chain (band)
  "Counts the number of band chain from band."
  (declare (type isl::schedule-node-band band))
  (let ((depth 0)
        (last-band band)
        (node (schedule-node-first-child band)))
    (loop while (eql (schedule-node-get-type node) :schedule-node-band) do
      (incf depth)
      (setf last-band node
            node (schedule-node-first-child node)))
    (values depth last-band)))

(defun schedule-node-band-scoop-up (band n)
  "Relocates the nth band into the top of band chain.
```
schedule: ... <-------|
  child:              |
    schedule: ... ----| n=1
      child: ...
        xN
```
"
  (declare (type isl::schedule-node-band band) (type fixnum n))
  (multiple-value-bind (depth innermost-band)
      (schedule-node-band-get-n-chain band)
    (assert (and (> n 0) (<= n depth)) () "schedule-node-band-scoop-up: N=~a is out of bound [1, ~a)" n depth)
    (loop for nth downfrom depth downto (1+ n) do
      (setf innermost-band (isl::schedule-node-parent innermost-band)))
    (let ((mupa (schedule-node-band-get-partial-schedule innermost-band))
          (node (schedule-node-delete innermost-band)))
      (loop for nth downfrom n downto 1 do
        (setf node (isl::schedule-node-parent node)))
      (schedule-node-insert-partial-schedule node mupa))))

(defun schedule-node-band-chain-sink (band)
    "Relocates the given band to the innermost of current band chain. This function always returns the top of chain.
```
schedule: ... --------| // Returned
  child:              |
    schedule: ... <---|
```
"
  (declare (type isl::schedule-node-band band))
  ;; The band is already the innermost
  (when (not (eql (schedule-node-get-type (schedule-node-first-child band)) :schedule-node-band))
    (return-from schedule-node-band-chain-sink band))
  (let ((mupa (schedule-node-band-get-partial-schedule band)))
    (multiple-value-bind (depth innermost-band)
        (schedule-node-band-get-n-chain (schedule-node-delete band))
      (let ((top (schedule-node-insert-partial-schedule (schedule-node-first-child innermost-band) mupa)))
        (dotimes (i (1+ depth) top)
          (setf top (isl::schedule-node-parent top)))))))

(defun schedule-node-sequence-splice-children (node)
  (declare (type isl::schedule-node node))
  (assert (eql :schedule-node-sequence (schedule-node-get-type node)))
  (loop for i upfrom 0 below (isl::%isl-schedule-node-n-children (isl::schedule-node-handle node))
        for c = (schedule-node-get-child node i)
        if (eql (schedule-node-get-type (schedule-node-get-child c 0)) :schedule-node-sequence) do
          (return-from schedule-node-sequence-splice-children
            (schedule-node-sequence-splice-children
             (isl::schedule-node-sequence-splice-child node i))))
  node)

(defun schedule-node-sequence-get-filters (components)
  (declare (type isl::schedule-node-sequence components))
  (loop for i upfrom 0 below (isl::%isl-schedule-node-n-children (isl::schedule-node-handle components))
        for filter-node = (schedule-node-get-child components i)
        collect (isl::schedule-node-filter-get-filter filter-node)))

(defun schedule-node-sequence-get-filter-types (components)
  (declare (type isl::schedule-node-sequence components))
  (loop for i upfrom 0 below (isl::%isl-schedule-node-n-children (isl::schedule-node-handle components))
        for filter-node = (schedule-node-get-child components i)
        collect (schedule-node-get-type (schedule-node-first-child filter-node))))

(defun schedule-node-sequence-reorder (components order)
  (declare (type list order))
  (assert (eql :schedule-node-sequence (schedule-node-get-type components)))
  (let ((new-filters (permute-list order (schedule-node-sequence-get-filters components)))
        (new-filter-list (isl::union-set-list-alloc 0)))
    (dolist (f new-filters)
      (setf new-filter-list (isl::union-set-list-add (! new-filter-list) f)))
    (schedule-node-first-child
     (schedule-node-insert-mark
      (isl::schedule-node-insert-sequence
       (! components)
       (! new-filter-list))
      (isl::make-id-from-str (format nil "@ApplyOptimization{REORDER}~a" order))))))

(defun %umap-collect-range-names (umap &key (domain-name nil))
  "Collect distinct range tuple names from UMAP.
If DOMAIN-NAME is provided, only maps whose domain tuple name equals it are used."
  (let* ((ml (union-map-get-map-list umap))
         (n  (map-list-size ml))
         (acc '()))
    (dotimes (i n (nreverse (remove-duplicates acc :test #'string=)))
      (let* ((m (map-list-elt ml i))
             (dn (map-get-tuple-name m :dim-in)))
        (when (or (null domain-name) (and dn (string= dn domain-name)))
          (let ((rn (map-get-tuple-name m :dim-out)))
            (when rn (push rn acc))))))))

(defun read-missing-or-unwritten-p (read-umap written-bufs filter-name)
  (declare (type isl::union-map read-umap)
           (type list written-bufs)
           (type string filter-name))
  (let ((read-bufs (%umap-collect-range-names read-umap :domain-name filter-name)))
    (if (null read-bufs)
        t
        (every #'(lambda (b) (not (member b written-bufs :test #'string=))) read-bufs))))

(defun union-set-single-tuple-name (uset)
  (declare (type isl::union-set uset))
  (let* ((sl (union-set-get-set-list uset))
         (n  (set-list-n-set sl)))
    (unless (= n 1) (error "union-set must contain exactly one set, but got ~a" n))
    (let* ((s (set-list-get-at sl 0))
           (nm (set-get-tuple-name s)))
      (or nm (error "union-set-single-tuple-name: uset has no name")))))

(declaim (ftype (function (isl::schedule-node-sequence isl::union-map isl::union-map) list) schedule-node-sequence-tpsort))
(defun schedule-node-sequence-tpsort (components read-umap write-umap)
  (declare (type isl::schedule-node-sequence components) (type isl::union-map read-umap write-umap))
  (let* ((filters (schedule-node-sequence-get-filters components))
         (filter-types (schedule-node-sequence-get-filter-types components))
         (filter-ids (loop for i upfrom 0 for f in filters collect i))
         (written-bufs (%umap-collect-range-names write-umap :domain-name nil))
         (filter-is-load-list
           (loop for f in filters
                 for ft in filter-types
                 if (eql ft :schedule-node-leaf)
                   collect (read-missing-or-unwritten-p read-umap written-bufs (union-set-single-tuple-name f))
                 else
                   collect nil))
         (new-orders))
    (loop for l in filter-is-load-list
          for id in filter-ids
          if l do (push id new-orders))
    (loop for l in filter-is-load-list
          for id in filter-ids
          if (not l) do (push id new-orders))
    (nreverse new-orders)))

(defun schedule-node-sequence-group-sequence (components)
  (declare (type isl::schedule-node-sequence components))
  (let* ((filters (schedule-node-sequence-get-filters components))
         (filter-types
           (loop for typ in (schedule-node-sequence-get-filter-types components)
                 collect (eql :schedule-node-band typ)))
         (last-filter nil)
         (new-filter-list (isl::union-set-list-alloc 0)))
    (loop for filter in filters for is-band-p in filter-types
          if is-band-p do
            (setf last-filter
                  (if last-filter
                      (union-set-union last-filter filter)
                      filter))
          else do
            (when last-filter
              (setf new-filter-list (isl::union-set-list-add new-filter-list last-filter)
                    last-filter nil))
            (setf new-filter-list (isl::union-set-list-add new-filter-list filter)))
    (when last-filter
      (setf new-filter-list (isl::union-set-list-add new-filter-list last-filter)))
    (isl::schedule-node-insert-sequence
     components
     new-filter-list)))

(cffi:defcallback schedule/compute-parallel :pointer
    ((node :pointer) (user :pointer))
  (when (eql (isl::%isl-schedule-node-get-type node) :schedule-node-band)
    (assert (= 1 (isl::%isl-schedule-node-band-n-member node)) () "schedule/compute-parallel: do not fuse band before computing coincidence")
    (isl::%isl-schedule-node-band-member-set-coincident
     node
     0
     (if (schedule-node-band-parallel-legal-p (isl::%make-schedule-node node) (isl::%make-union-map user))
         1 0)))
  node)

(defun schedule-compute-parallel (schedule deps)
  (declare (type isl::schedule schedule) (type isl::union-map deps))
  (isl::%make-schedule
   (isl::%isl-schedule-map-schedule-node-bottom-up
    (isl::schedule-handle (isl::__isl_take schedule))
    (cffi:callback schedule/compute-parallel)
    (isl::union-map-handle (isl::__isl_take deps)))))

;; ~~ umap-get-set-list-on-id ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defparameter *stmt-pair-result* nil)
(cffi:defcallback push-map-on-id :int
    ((map :pointer) (id :pointer))
  (let ((map (isl::%make-map map))
        (id  (cffi:mem-ref id :string)))
    (let ((dom-name (isl::set-get-tuple-name (isl::map-domain map))))
      (when (string= dom-name id)
        (let* ((expr (isl::%isl-map-to-str (isl::map-handle map)))
               (expr (subseq expr (position (aref "{" 0) expr))) ;; Ignore quasiaffine params (e.g.: [A] -> { ...)
               (pos  (position (aref ">" 0) expr))
               (pos1 (when pos (position (aref "[" 0) expr :start pos)))
               (pos2 (when pos (position (aref "]" 0) expr :start pos)))
               (var   (when (and pos pos1) (subseq expr (+ 2 pos) pos1)))
               (index (when (and pos1 pos2) (subseq expr (1+ pos1) pos2))))
          (assert (and var index) () "umap-get-set-list-on-id: Couldn't extract this map: ~a" map)
          (push (cons var index) *stmt-pair-result*)))))
  0)
(defun umap-get-set-list-on-id (umap filter &key (wrapper-dom #'string-downcase) (wrapper-ran #'(lambda (x) (format nil "[~a]" x))))
  (declare (type string filter) (type isl::union-map umap))
  (let ((*stmt-pair-result*))
    (cffi:with-foreign-object (str* :string)
      (setf (cffi:mem-ref str* :string) filter)
      (isl::%isl-union-map-foreach-map
       (isl::union-map-handle umap)
       (cffi:callback push-map-on-id)
       str*))
    (map 'list #'(lambda (x) (format nil "~a~a" (funcall wrapper-dom (car x)) (funcall wrapper-ran (cdr x)))) *stmt-pair-result*)))

(progn ;; foreach-map
  (defparameter *%foreach-map-fn* nil)  ; dynamic: (map) -> nil
  (cffi:defcallback %each-map-cb :int ((mp :pointer) (user :pointer))
    (declare (ignore user))
    ;; Call user-supplied Lisp function on a wrapped isl_map
    (when *%foreach-map-fn* (funcall *%foreach-map-fn* (isl::%make-map mp)))
    0)
  (defun %foreach-map (umap fn)
    "Iterate with ISL's foreach_map. Requires top-level defcallback."
    (let ((*%foreach-map-fn* fn))
      (isl::%isl-union-map-foreach-map
       (isl::union-map-handle umap)
       (cffi:callback %each-map-cb)
       (cffi:null-pointer)))))

(progn ;; foreach-set
  (defparameter *%foreach-set-fn* nil)  ; dynamic: (map) -> nil
  (cffi:defcallback %each-set-cb :int ((mp :pointer) (user :pointer))
    (declare (ignore user))
    ;; Call user-supplied Lisp function on a wrapped isl_map
    (when *%foreach-set-fn* (funcall *%foreach-set-fn* (isl::%make-set mp)))
    0)
  (defun %foreach-set (uset fn)
    "Iterate with ISL's foreach_map. Requires top-level defcallback."
    (let ((*%foreach-set-fn* fn))
      (isl::%isl-union-set-foreach-set
       (isl::union-set-handle uset)
       (cffi:callback %each-set-cb)
       (cffi:null-pointer)))))

(progn ;; foreach-pwa
  (defparameter *%foreach-pwa-fn* nil)
  (cffi:defcallback %each-pwa-cb :int ((pwa :pointer) (user :pointer))
    (declare (ignore user))
    (when *%foreach-pwa-fn* (funcall *%foreach-pwa-fn* (isl::%make-pw-aff pwa)))
    0)
  (defun %foreach-pwa (upa fn)
    (let ((*%foreach-pwa-fn* fn))
      (isl::%isl-union-pw-aff-foreach-pw-aff
       (isl::union-pw-aff-handle upa)
       (cffi:callback %each-pwa-cb)
       (cffi:null-pointer))))
  ;; foreach piece
  (defparameter *%foreach-piece-fn* nil)
  (cffi:defcallback %each-piece-cb :int ((set :pointer) (aff :pointer) (user :pointer))
    (declare (ignore user))
    (when *%foreach-piece-fn* (funcall *%foreach-piece-fn* (isl::%make-set set) (isl::%make-aff aff)))
    0)
  (defun %foreach-piece (pwa fn)
    (let ((*%foreach-piece-fn* fn))
      (isl::%isl-pw-aff-foreach-piece
       (isl::pw-aff-handle pwa)
       (cffi:callback %each-piece-cb)
       (cffi:null-pointer)))))

;; [TODO]
;; - [ ] Support Symbolic Graph Coalesce
;; - [ ] Conv+Pool Fusion.

;; (let ((tg (tensor-lowered-graph (!matmul (make-tensor `(256 2048)) (!t (!matmul (make-tensor `(10 10 512 1024)) (make-tensor `(10 1024 2048))))))))
;;               (time (caten/codegen/lowerer::codegen tg)))
(defun val->int (val) (isl::%isl-val-get-num-si (isl::value-handle val)))
(defun uset-find (uset name)
  (%foreach-set
   uset
   #'(lambda (set)
       (when (string= (set-get-tuple-name set) name)
         (return-from uset-find set)))))

(defun map-build-preimage-mt (map filter-name)
  (print "MAP")
  (print map)
  (let* ((bmap (map-affine-hull map))
         (bset (basic-map-wrap bmap))
         (cs (basic-set-get-constraint-list bset))
         (dim2aff (make-hash-table :test 'equal)))
    (PRINT "ACCESS_MAP")
    (print bmap)
    (loop for i upfrom 0 below (constraint-list-size cs)
          for c = (constraint-list-get cs i)
          for tgt-dim = nil
          for affs = nil do
            (loop for j upfrom 0 below (basic-set-dim bset :dim-out)
                  for coeff = (get-coefficient-val c :dim-out j)
                  for name = (constraint-get-dim-name (constraint-list-get cs i) :dim-out j) do
                    (cond
                      ((value= coeff (value 1))
                       (assert (null tgt-dim))
                       (setf tgt-dim name)) ;; child側のtile
                      ((= 1 (value-sign coeff))
                       (push (list :TILE coeff name) affs))
                      ((= -1 (value-sign coeff))
                       (push (cons name (value-mul coeff (value -1))) affs))))
            (setf (gethash tgt-dim dim2aff) (reverse affs)))
    (let ((from) (to) (offset 0) (dims))
      ;; should not permute things
      ;; A = Bを見て行って:
      ;; - Constraintがない => NoFuse
      ;; - _gid3=_gid3 => Fuse
      ;; - _gid3=_gid2 => Interchange+Fuse
      ;; - _gid3=_gid2-2_gid3 => Tile+Fuse
      ;; - ScheduleTreeのExploreとして実装する
      (loop for dim upfrom 0 below (map-dim map :dim-out)
            for name = (map-get-dim-name map :dim-out dim) do
              (format t "Fusion For ~a:~%" name)
              (print (Gethash name dim2aff))))))
                    
(defun schedule-pullback-identity (schedule &key (substitute-id) (substitute))
  (let* ((udom (schedule-node-domain-get-domain (schedule-get-root schedule)))
         (sets (union-set-get-set-list udom))
         (upma (union-pw-multi-aff-empty (union-set-get-space udom))))
    (dotimes (i (set-list-size sets))
      (let* ((s   (set-list-elt sets i))
             (sp  (set-get-space s))
             (tn  (space-get-tuple-name sp :dim-out))
             (n   (set-dim s :dim-set))
             (xs  (loop for k below n collect (format nil "_gid~D" k)))
             (pma
               (pw-multi-aff-from-str
                (if (string= tn substitute-id)
                    substitute
                    (format nil "{ ~A[~{~A~^, ~}] -> ~A[~{~A~^, ~}] }" tn xs tn xs)))))
        (setf upma (union-pw-multi-aff-union-add
                    upma (union-pw-multi-aff-from-pw-multi-aff pma)))))
    (schedule-pullback-union-pw-multi-aff schedule upma)))

(defun get-collapsed-domain (domain-maxima dims tgt-id)
  (let ((gids (loop for i upfrom 0 below (length dims) collect i)))
    (union-set-from-str
     (flet ((r (id render)
              (format nil "0 <= _gid~a <= ~a" render (val->int (gethash id domain-maxima)))))
       (format nil "{ ~a[~{_gid~A~^, ~}] : ~{~A~^and ~}}" tgt-id gids (map 'list #'r dims gids))))))
;; [TODO] ここでPermuteまで面倒見る？
(defun schedule-detect-coalesce (merged-schedule child-read-umap parent-write-umap child-write-umap)
  (multiple-value-bind (deps raw waw war) (compute-dependence-relation child-read-umap parent-write-umap merged-schedule)
    (declare (ignore deps waw war))
    (print parent-write-umap)
    (print child-read-umap)
    
    (%foreach-map
     raw
     #'(lambda (map
                &aux
                  ;; [TODO] domain-maxima => user pw-aff for symbolics
                  (tgt-name (map-get-tuple-name map :dim-out))
                  (preimage (map-build-preimage-mt map tgt-name))) ;; CHILD -> PARENT
         (print map)
         (print preimage)
        ; (error "STOP")
         ))
    merged-schedule))
;; [todo] した全部削除
(defun schedule-compute-dim-equalities-graph (merged-schedule child-read-umap parent-write-umap &aux (results))
  ;; Returns a list of valid permutations for K2
  (declare (type isl::schedule merged-schedule) (type isl::union-map child-read-umap parent-write-umap))
  (multiple-value-bind (deps raw waw war) (compute-dependence-relation child-read-umap parent-write-umap merged-schedule)
    (declare (ignore deps waw war))
    (%foreach-map
     raw
     #'(lambda (map
                &aux
                  (ni (map-dim map :dim-in))
                  (no (map-dim map :dim-out))
                  (pairs))
         ;; [TODO] Coalesceが解消できなかった時どうする？
         (print map)
         (dotimes (i ni)
           (dotimes (j no)
             (let ((meq (map-equate map :dim-in i :dim-out j)))
               (when (map-is-equal map meq)
                 (push (cons i j) pairs)))))
         (push (cons pairs (permutations-from-equalities-graph pairs ni no)) results)))
    (if (= 1 (length results))
        (let ((final (car results)))
          (values (cdr final) (car final))) ;; (values equalities-graph permutation)
        (progn
          (warn "schedule-compute-dim-equalities: case for multiple maps is not implemented yet.")
          ;; [TODO] Single Write, Multiple Readsの時，Fusionできるケースがあるはず。
          nil))))

(defun permutations-from-equalities-graph (pairs n-in-k1 n-out-k2)
  (let* ((order (make-array n-out-k2 :initial-element nil))
         (taken (make-hash-table)))
    ;; place paired K2 dims in the order of K1 dims
    (dotimes (i n-in-k1)
      (let* ((j (cdr (find i pairs :key #'car))))
        (when j
          (setf (aref order i) j)
          (setf (gethash j taken) t))))
    ;; append remaining K2 dims (unpaired) after the paired block
    (let ((k n-in-k1))
      (dotimes (j n-out-k2)
        (unless (gethash j taken)
          (setf (aref order k) j)
          (incf k))))
    (coerce order 'list)))

(defun schedule-permute (schedule n perms)
  (let ((root (schedule-node-get-child (schedule-node-first-child (schedule-get-root schedule)) n))
        (sequence)
        (bands))
    (labels ((explore (node)
               (case (schedule-node-get-type node)
                 ((:schedule-node-sequence :schedule-node-set)
                  (assert (null sequence))
                  (setf sequence node)
                  (dotimes (i (isl::%isl-schedule-node-n-children (isl::schedule-node-handle node)))
                    (let ((child (schedule-node-get-child node i)))
                      (when (eql (schedule-node-get-type (schedule-node-first-child child)) :schedule-node-band)
                        (return-from explore (explore child))))))
                 (:schedule-node-band
                  (push (schedule-node-band-get-partial-schedule node) bands)
                  (when (< (length bands) (length perms))
                    (explore (schedule-node-first-child node))))
                 (:schedule-node-leaf node)
                 (otherwise (explore (schedule-node-first-child node))))))
      (explore root)
      (setf bands (permute-list perms (nreverse bands))
            root (schedule-node-delete (schedule-node-first-child root))
            root (schedule-node-cut root))
      ;(print "ROOT")
      ;(print root)
      (dolist (band bands)
        (setf root (schedule-node-first-child (schedule-node-insert-partial-schedule root band))))
      (when sequence
        (let* ((uset-list (union-set-list-alloc 0))
               (filters (schedule-node-sequence-get-filters sequence)))
          (dolist (f filters) (setf uset-list (union-set-list-add uset-list f)))
          (setf root (schedule-node-insert-sequence root uset-list))))
      ;(print root)
      (schedule-node-get-schedule root))))
;; ~~ NOT TESTED CODES ~~~~~~~~~~~~
;; ~~~ PERMUTATIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;; 便利ラッパ（必要なら）
(defun print-global-dim-dependency-graph (schedule reads writes)
  (dolist (e (build-global-dim-dependency-graph schedule reads writes))
    (format t "~A -> ~A~%" (car e) (cdr e)))
  (values))

(defun pts-cost-for-bands (dom schedule reads writes band1 band2 level)
  "
Reference: Meister et al., HPCS 2019 (Permutation Tensor Scheduler).
Return: (values d* shift feasible vmin vmax)."
  (declare (type isl::schedule schedule)
           (type isl::union-map reads writes)
           (type isl::schedule-node-band band1 band2)
           (type fixnum level))
  (print-global-dim-dependency-graph schedule reads writes)
  nil)
;; ~~~ MergeView in Polyhedral Space ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Problem Setting:
;; Given View = {shape, stride, mask}, computes the beneficial loop generation path {view1.view2}
;; - This should work on different ranked views.
(defparameter *stmt-pair-result* nil)
(cffi:defcallback push-umap-pair :int
    ((map :pointer) (user :pointer))
  (declare (ignore user))
  (let ((map (isl::%make-map map)))
    (let ((dom-name (isl::set-get-tuple-name (isl::map-domain map)))
          (ran-name (isl::set-get-tuple-name (isl::map-range map))))
      (when (not (string= dom-name ran-name))
        (push (cons dom-name ran-name) *stmt-pair-result*))))
  0)
(defun umap->stmt-pairs (umap)
  (declare (type isl::union-map umap))
  (let ((*stmt-pair-result*))
    (isl::%isl-union-map-foreach-map
     (isl::union-map-handle umap)
     (cffi:callback push-umap-pair)
     (cffi:null-pointer))
    *stmt-pair-result*))

(defparameter *filter-names-result* nil)
(cffi:defcallback push-filter-name :int
    ((set :pointer) (user :pointer))
  (declare (ignore user))
  (push (isl::%isl-set-get-tuple-name set) *filter-names-result*)
  0)
(defun union-set-get-statements (union-set)
  (let ((*filter-names-result* nil))
    (isl::%isl-union-set-foreach-set
     (isl::union-set-handle union-set)
     (cffi:callback push-filter-name)
     (cffi:null-pointer))
    (nreverse *filter-names-result*)))

(defun compute-scc-pairs (read-umap write-umap sched)
  ;; return ((cons S1 S2)) S1 depends on S2
  (multiple-value-bind (_ raw waw war) (compute-dependence-relation read-umap write-umap sched)
    (declare (ignore _))
    (remove-duplicates
     (append (umap->stmt-pairs raw));; (umap->stmt-pairs waw) (umap->stmt-pairs war))
     :test #'equal)))

(defun compute-scc-pairs-on-sequence (sequence read-umap write-umap sched)
  (let ((pairs (compute-scc-pairs read-umap write-umap sched))
        (n-sched (isl::%isl-schedule-node-n-children (isl::schedule-node-handle sequence))))
    ;; Sort Topologically?
    ))

(defun compute-fuse-pairs-on-sequence (schedule-node-sequence read-umap write-umap sched)
  "Return (list (cons absolute_path_from_seq1 absolute_path_from_seq2))"
  ;; TODO(optimize): pairs are static on each exploration
  (let ((pairs (compute-scc-pairs read-umap write-umap sched))
        (filters (loop for i upfrom 0 below (isl::%isl-schedule-node-n-children (isl::schedule-node-handle schedule-node-sequence))
                       for filter-node = (schedule-node-get-child schedule-node-sequence i)
                       collect (union-set-get-statements (isl::schedule-node-filter-get-filter filter-node))))
        )
    (values
     filters
     (loop for (dst . src) in pairs
           for dst1 = (find dst filters :test #'(lambda (x y) (find x y :test #'string=)))
           for src1 = (find src filters :test #'(lambda (x y) (find x y :test #'string=)))
           if (and dst1 src1)
             collect (cons dst src)))))
          
;    (loop for (dst . src) in pairs
;          for dst-path = (gethash dst filter2path)
;          for src-path = (gethash src filter2path)
;          for min = (min (length dst-path) (length src-path))
;          if (and dst-path src-path (not (equal (subseq dst-path 0 min) (subseq src-path 0 min))))
;            collect (list dst-path src-path))))

(defun schedule-node-band-delete-on-sequence (sequence pos)
  (let ((band (schedule-node-get-child (schedule-node-get-child sequence pos) 0)))
    (unless (eql (schedule-node-get-type band) :schedule-node-band)
      (return-from schedule-node-band-delete-on-sequence sequence))
    (let ((seq (isl::schedule-node-parent (isl::schedule-node-parent (schedule-node-delete band)))))
      (assert (find (schedule-node-get-type seq) '(:schedule-node-sequence :schedule-node-set)))
      seq)))

;; Goal: FlashAttentionに相当する区間をまとめてBEAM SearchしちゃえばFlashAttentionまで探索できる
;; 1. SCCS同士のdst <- srcのPairを求め，そこから計算していく
;; 2.
;; 1DまでFlatにされたLoop二つの融合を考える？
;; 
;; 理論上必要な操作の集合はこれ:
;; - Interchange: (BandのSequenceを上から下へと持っていく)
;; - Expand:      (1次元でCoalesceされたBandを複数のBandへと分割する)
;; - 3次元と2次元
;; (caten (!add (!matmul (make-tensor `(512 512))  (make-tensor `(512 512))) (!matmul (make-tensor `(512 512)) (!t (make-tensor `(512 512))))))
;; Rename: ScheduleFuse => ScheduleFilterMove?
;; [TODO]
;; - Interchange
;; - Reshape (ScheduleBandScale)を探索対象として追加することでCostmodelを使って探索できるように！
;; - 10*10, 10*10のCoalesceされたBand同士もExploreできる？
;; ==> UnitTest


(defun schedule-node-insert-subtree (node subtree)
  (labels ((rollback (schedule size)
             (dotimes (i size schedule)
               (setf schedule (isl::schedule-node-parent schedule))))
           (explore (schedule tree depth)
             (ecase (schedule-node-get-type tree)
               ((:schedule-node-set :schedule-node-sequence)
                (let ((filter (isl::union-set-list-alloc 0))
                      (n-child (isl::%isl-schedule-node-n-children (isl::schedule-node-handle tree)))
                      (f (case (schedule-node-get-type tree)
                           (:schedule-node-set #'isl::schedule-node-insert-set)
                           (:schedule-node-sequence #'isl::schedule-node-insert-sequence))))
                  (loop for i upfrom 0 below n-child do
                    (setf filter (isl::union-set-list-add filter (isl::schedule-node-filter-get-filter (schedule-node-get-child tree i)))))
                  (setf schedule (funcall f schedule filter))
                  (loop for i upfrom 0 below n-child
                        for child = (schedule-node-get-child (schedule-node-get-child tree i) 0)
                        if (not (eql (schedule-node-get-type child) :schedule-node-leaf)) do
                          (multiple-value-bind (new-schedule new-depth)
                              (explore (schedule-node-get-child (schedule-node-get-child schedule i) 0)
                                       (schedule-node-get-child (schedule-node-get-child tree i) 0)
                                       depth)
                            (assert new-depth)
                            (setf schedule (rollback new-schedule (1+ (- depth new-depth))))))
                  schedule))
               (:schedule-node-filter
                (let* ((filter (isl::union-set-list-alloc 0)))
                  (setf filter (isl::union-set-list-add filter (isl::schedule-node-filter-get-filter tree))
                        schedule (schedule-node-get-child (isl::schedule-node-insert-sequence schedule filter) 0))
                  (explore (schedule-node-get-child schedule 0) (schedule-node-get-child tree 0) (+ 2 depth))))
               (:schedule-node-band
                (let* ((mupa (schedule-node-band-get-partial-schedule tree)))
                  (setf schedule (schedule-node-insert-partial-schedule schedule mupa)))
                (explore (schedule-node-get-child schedule 0) (schedule-node-get-child tree 0) (1+ depth)))
               (:schedule-node-leaf (values schedule depth)))))
    (explore node subtree 0)))
;; A minimal subset for finding the "best" fused kernel.
;; - Permute  : ==> Interchange bands to find FusionEdge
;; - MOD      : ==> Create a new band (to find coalesce)
;; - OnTheFly : ==> Combine Multiple Reduction Ops
;; - TODO: CreateFusionConflictGraph (by using AST?)
;; Find the best schedule on Polynomial Time! ==> SOTA
(defun schedule-sequence-pick-up (components filter-dst filter-src)
  "Relocates filter-src into filter-dst, if there's missing dependencies, adds a new tile bands to satisfy them"
  (declare (type isl::schedule-node-filter filter-dst filter-src))
  (assert (find (schedule-node-get-type components) '(:schedule-node-set :schedule-node-sequence)))
  (let ((parent-filter (isl::union-set-list-alloc 0)))
    (print components)
    (setf parent-filter (isl::union-set-list-add parent-filter (isl::schedule-node-filter-get-filter filter-src)))
    ;; parent-filter: filter-dstのfiltersも追加？
    ;; 
    (let ((sched (schedule-node-get-child
                  (schedule-node-get-child (isl::schedule-node-insert-sequence components parent-filter) 0)
                  0))
          (band nil))
      (print sched)
      (loop for i upfrom 0 below (isl::%isl-schedule-node-n-children (isl::schedule-node-handle sched))
            for c = (schedule-node-get-child sched i)
            if (eql (schedule-node-get-type (schedule-node-get-child c 0)) :schedule-node-band) do
              (setf band (schedule-node-get-child c 0)))
      (assert band)
      (loop until (eql :schedule-node-leaf (schedule-node-get-type (schedule-node-get-child band 0))) do
        (setf band (schedule-node-get-child band 0)))
      (setf band (schedule-node-get-child band 0))
      (print "DST")
      (print band)
      (print "Insert TGT")
      (print filter-dst)
      (PRINT "FINAL SCHED")
      (print (schedule-node-insert-subtree band filter-dst)))))

(defun schedule-sort-sequence (components read-umap write-umap)
  (multiple-value-bind (nodes graph)
      (compute-fuse-pairs-on-sequence components read-umap write-umap (schedule-node-get-schedule components))
    (print "SORTING")
    (print nodes)
    (print graph)
    ;; 1. ReadyForPlace Filters ...
    ;; 2. ReadyForPlace Bands   ...
    ;; 3. ReadyForPlace Filters ...
    ;; 4. ReadyForPlace Bands   ...
    ;; LOAD
    ;; COMPUTE
    ;; STORE
    (print "COMPONENTS")
    ;; TODO: GraphのWrite/Readの関係からPickUpする
    (schedule-remove-empty-schedule (schedule-node-get-schedule components))))

(defstruct (VolumeConstraint
            (:constructor make-volume-constraint
              (schedule read-umap write-umap
               &aux (items (multiple-value-list (compute-dependence-relation read-umap write-umap schedule))))))
  (raw (nth 1 items) :type isl::union-map)
  (waw (nth 2 items) :type isl::union-map)
  (war (nth 3 items) :type isl::union-map))

(progn ;; UnionMapDropInfo
  (defparameter *union-map-drop-info-allowed-names* nil)
  (defparameter *union-map-drop-info-allowed-dims* nil)
  (cffi:defcallback rewrite/append-union-map :int
      ((map :pointer) (umap :pointer))
    (when (find
           (isl::%isl-map-get-tuple-name map :dim-in)
           *union-map-drop-info-allowed-names* :test #'string=)
      (let ((n-params (isl::%isl-map-dim map :dim-in)))
        (loop for i upfrom 0 below n-params
              for dim-name = (isl::%isl-map-get-dim-name map :dim-in i)
              if (null (find dim-name *union-map-drop-info-allowed-dims* :test #'string=)) do
                (setf map (isl::%isl-map-fix-val map :dim-in i (isl::value-handle (isl::copy (value 0))))))
        (setf umap (isl::%isl-union-map-union umap (isl::%isl-union-map-from-map map)))))
    0)
  (defun union-map-drop-info (union-map allowed-names allowed-dims)
    (let ((umap (isl::union-map-from-str "{}"))
          (*union-map-drop-info-allowed-names* allowed-names)
          (*union-map-drop-info-allowed-dims* allowed-dims))
      (isl::%isl-union-map-foreach-map
       (isl::union-map-handle union-map)
       (cffi:callback rewrite/append-union-map)
       (isl::union-map-handle umap))
      umap)))

(defun %pw-aff-to-str (pwa)
  (let* ((p (isl::%isl-printer-to-str (isl::context-handle isl::*context*)))
         (p (isl::%isl-printer-set-output-format p 0))
         (p (isl::%isl-printer-print-pw-aff p (isl::pw-aff-handle pwa))))
    (isl::%isl-printer-get-str p)))

(defun schedule-fuse-two-kernel (components read-umap write-umap)
  (assert (find (schedule-node-get-type components) '(:schedule-node-sequence :schedule-node-set)))
  (labels ((get-n-band (band depth &optional (count 0))
             (assert (eql (schedule-node-get-type band) :schedule-node-band))
             (if (= depth count)
                 band
                 (get-n-band (schedule-node-get-child band 0) depth (1+ count))))
           (get-band-depth (band &optional (count 0))
             (let ((child (schedule-node-get-child band 0)))
               (if (eql (schedule-node-get-type child) :schedule-node-band)
                   (get-band-depth child (1+ count))
                   count)))
           (get-root (n)
             (schedule-node-get-child (schedule-node-get-child components n) 0)))
    (let* ((read/write (union-map-union read-umap write-umap))
           (k-src (get-root 0)) (k-dst (get-root 1))
           (src-depth (get-band-depth k-src)) (dst-depth (get-band-depth k-dst))
           (s-relation (alexandria:flatten (umap->stmt-pairs (isl::schedule-node-get-prefix-schedule-relation k-src))))
           (d-relation (alexandria:flatten (umap->stmt-pairs (isl::schedule-node-get-prefix-schedule-relation k-dst))))
           (domain (schedule-node-get-domain components))
           (s-selections) (d-selections))
      (assert (eql :schedule-node-band (schedule-node-get-type k-src)))
      (assert (eql :schedule-node-band (schedule-node-get-type k-dst)))
      (push k-src s-selections)
      (push k-dst d-selections)
      (labels ((band->id (band)
                 (let* ((pa
                         (isl::pw-aff-list-elt
                          (isl::union-pw-aff-get-pw-aff-list
                           (multi-union-pw-aff-get-union-pw-aff (schedule-node-band-get-partial-schedule band) 0))
                          0))
                        (reg (subseq (second (cl-ppcre:split "->" (%pw-aff-to-str pa))) 3))
                        (reg (subseq reg 0 (- (length reg) 4))))
                   (loop for i upfrom 0 below (isl::pw-aff-dim pa :dim-in)
                         for name = (isl::pw-aff-get-dim-name pa :dim-in i)
                         ;; [TODO] Improve the implementation ...
                         if (string= reg name) collect name)))
               (get-area (selections rels)
                 (union-map-drop-info read/write rels (apply #'append (map 'list #'band->id selections))))
               (get-s-area ()
                 ;; 同じ領域を描画するDomainが欲しい！
                 ;; s-selectionsのUnionSetが欲しい
                 (let ((s1 (get-area s-selections s-relation))
                       (d1 (get-area d-selections d-relation)))
                   (union-map-union s1 d1)
                   ))
               (get-d-area ()
                 ;(union-map-intersect-domain (get-area d-selections) domain)
                 ))
        (print src-depth)
        (print dst-depth)
        (print s-relation)
        (print (get-s-area))
        (print (get-d-area))
        ;; 惜しい。任意のDomainでのSetを計算したい。
;        (print (union-set-subtract (union-map-range (get-s-area)) (union-map-range (get-d-area))))
        ;; Problem Setting:
        ;;   Let K_src be multiple of Bs_1 x Bs_2 x ... x Bs_{src_depth}
        ;;   Let K_dst be multiple of Bd_1 x Bd_2 x ... x Bd_{dst_depth}
        ;; 同じ領域を描画するBandの組み合わせを切り出す
        ;; Solution1:
        ;; ==> A = B*C*...
        ;; Solution2:
        ;; ==> Permute and find valid one?
        ;; ConvのBatchとPoolの二番目のループがFuseされてはいけない。
        ;; TileされてFuseされるのか？検証した方がいい ==> Possible
        (print "DOING FUSION")
        ))))
;; -----------------------------------------------------------------------------
;; - [ ] Filter Relocate Concepts
;; - [ ] Maximize Locality Rewriting
;; - [ ] Rebundant Guard Elimination
;; - [ ] Post Tile Fusion
;; --- Small helpers -----------------------------------------------------------
;; - 下のASTについて，どのようにfilterを操作したら上のASTに近いASTを生成できる？(with maximizing bands)
;; - 入力のASTがもっと乱雑だったとして，決定論的に上の完璧なAST/ScheduleTreeを得る方法を考えているんだ
;; What I wanted to do:
;; - Filter Reodering
;; - tadashi_fuse (後で切り出す)
;; -
;; FlashAttentionを常にIn-Memoryで計算する
;; Guardを削除する
;; PostTileFusion
;; LegalなScheduleTreeの集合を持ってるのだから活用しないのは勿体無い... (Aggresstiveに)
;; Simplifier ... BEAM Searchに統合する

;; SequenceのSortでIfの数最小化したい...
;; AST生成がCost
;; filter = EXPR, sequence/set = progn
;; sequence
;; - filter
;; Polyhedral Modelを使ってるんだから，探索空間がもっと広くないといけないし，コスト関数がdeterministic...
;; filterの移動

(defun %node-type (n) (schedule-node-get-type n))

(defun uset-filter-by-stmt-names (universe names)
  "Return union-set containing only tuples whose tuple-name ∈ NAMES."
  (let* ((sl (isl::union-set-get-set-list universe))
         (n  (isl::set-list-n-set sl))
         (acc nil))
    (dotimes (i n)
      (let* ((s (isl::set-list-get-at sl i))
             (nm (or (isl::set-get-tuple-name s) "")))
        (when (find i names);;(member nm names :test #'string=)
          (setf acc (if acc
                        (isl::union-set-union acc (isl::union-set-from-set s))
                        (isl::union-set-from-set s))))))
    (or acc (isl::union-set-empty (isl::union-set-get-space universe)))))

(defun subtree-universe (node)
  (caten/isl::union-map-domain (caten/isl::schedule-node-get-subtree-expansion node)))

(defun order-stmts (schedule path names &key (where :before))
  "At PATH (sequence/set などの祖先) impose order for statements NAMES.
WHERE = :before → NAMES 側が先, :after → NAMES 側が後。
Return new schedule."
  (declare (type isl::schedule schedule))
  (let* ((sched (copy schedule))
         (node  (schedule-node-at-path sched path))
         (univ  (subtree-universe node))
         (flt   (uset-filter-by-stmt-names univ names)))
    (ecase where
      (:before
       (let ((loc (isl::schedule-node-order-before node (copy flt))))
         (declare (ignore loc))
         (isl::schedule-node-get-schedule node)))
      (:after
       (let ((loc (isl::schedule-node-order-after node (copy flt))))
         (declare (ignore loc))
         (isl::schedule-node-get-schedule node))))))

(defun move-stmt-before (schedule path stmt-name)
  (order-stmts schedule path (list stmt-name) :where :before))

(defun move-stmt-after (schedule path stmt-name)
  (order-stmts schedule path (list stmt-name) :where :after))

(defun simplify-schedule (schedule)
  "Problem Setting:
- sequence:
- set:
これの子ノードをどっか別の場所に移動することを考える
"
  (declare (type isl::schedule schedule))
  (move-stmt-after schedule '(0 1 0) 1))


;; for (int i=0; i<60; i++)
;;   S1(i)
;;
;; for (int i=0; i<10; i++)
;;   for (int j=0; j<6; j++)
;;     S2(i, j)
;; 1. Can assert S1 == S2
;; 2. Can tile S1
;; 実際にFuseしたScheduleを用意する必要がある？
;; (defun r () ;; read (pool)
;;  (union-map-from-str "{ S1[_gid0, _gid1, _gid2, _gid3, _gid4_1, _gid5_1] -> X[((((((441*_gid0)+(2646*_gid1))+(42*_gid2))+(2*_gid3))+(21*_gid4_1))+_gid5_1)] : 0 <= _gid0 < 6 and 0 <= _gid1 < 10 and 0 <= _gid2 < 10 and 0 <= _gid3 < 10 and 0 <= _gid4_1 < 2 and 0 <= _gid5_1 < 2 }"))
;;(defun w () ;; write (conv)
;;  (union-map-from-str "{ S1[_gid0, _gid2, _gid3, _gid4] -> X[((((2646*_gid0)+(441*_gid2))+(21*_gid3))+_gid4)] : 0 <= _gid0 < 10 and 0 <= _gid2 < 6 and 0 <= _gid3 < 21 and 0 <= _gid4 < 21 }"))
