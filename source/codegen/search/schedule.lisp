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
   #:schedule-fuse-all-band))
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
           (access (union-access-info-set-must-source access write))
           (access (union-access-info-set-schedule access schedule))
           (flow (union-access-info-compute-flow access))
           (RaW (union-flow-get-must-dependence flow))
           (access (union-access-info-from-sink write))
           (access (union-access-info-set-must-source access write))
           (access (union-access-info-set-may-source access read))
           (access (union-access-info-set-schedule access schedule))
           (flow   (union-access-info-compute-flow access))
           (WaW    (union-flow-get-must-dependence flow))
           (WaR    (union-flow-get-may-dependence flow))
           (dependencies (union-map-union (union-map-union WaR RaW) WaW)))
      dependencies))

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
            schedule-constraints
            dependencies))
         (schedule-constraints
           (schedule-constraints-set-validity
            schedule-constraints
            dependencies))
         (schedule-constraints
           (schedule-constraints-set-proximity
            schedule-constraints
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
         (zeros (zero-vector-union-set delta))
         (le (union-set-lex-le-union-set delta zeros))
         (retval (union-set-is-empty le)))
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
;; [TODO] upasの操作 => Macroにする？
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
  (let ((set (isl::%isl-set-from-basic-set bset))) ;; __isl_give
    (dotimes (pos (cffi:mem-ref user :int))
      (let ((cpy (isl::%isl-set-copy set))
            (dname (isl::%isl-basic-set-get-dim-name bset :dim-set pos)))
        (setf (gethash dname *domain-maxima-results*)
              (isl::%make-value (%isl-set-dim-max-val cpy pos)))
        (isl::%isl-set-free cpy) ;; [TODO] Check Memory Legality
        ))
    (isl::%isl-set-free set) ;; [TODO] Check Memory Legality
    )
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
  (declare (type fixnum size) (type isl::schedule-node-band band))
  ;; [TODO] Support Symbolic Tile
  (let* ((band-space (schedule-node-band-get-space band))
         (dim (space-dim band-space 3)))
    (multi-val-from-val-list band-space (apply #'make-value-list (loop for i upfrom 0 below dim collect size)))))

(defun schedule-node-band-tile* (band size &key (strategy :atomic) (directive) (sink))
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
  (let ((tiled (schedule-node-band-tile band (tiling-size band size))))
    (ecase strategy
      (:isolate
       (let* ((subdom (union-map-domain (isl::schedule-node-get-subtree-expansion tiled)))
              (mupa (schedule-node-band-get-partial-schedule tiled))
              (tiled-ids (partial-schedule-get-involved-dims mupa)))
         (when (null tiled-ids) (return-from schedule-node-band-tile* tiled))
         (let* ((maxima (extract-domain-maxima (isl::schedule-node-get-prefix-schedule-relation tiled)))
                (width  (value size))
                (affected
                  (reduce
                   #'isl::union-set-union
                   (map 'list #'(lambda (nm) (union-set-filter-by-dim-name subdom nm)) tiled-ids)
                   :initial-value (isl::union-set-from-str "{}")))
                (unaffected (isl::union-set-subtract subdom affected)))
           (multiple-value-bind (full tail)
               (%make-full/partial-filters affected tiled-ids maxima width)
             (let* ((lst (isl::union-set-list-alloc 0))
                    (lst (union-set-list-add-nonempty lst unaffected))
                    (lst (union-set-list-add-nonempty lst full))
                    (lst (union-set-list-add-nonempty lst tail))
                    (sched (isl::schedule-node-insert-sequence tiled lst)))
               sched)))))
      (:padding tiled)
      (:atomic  tiled)
      (:guard   tiled))))

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
;; NOT TESTED!!!
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
  (caten/isl::union-map-intersect-domain (caten/isl::copy umap) (caten/isl::copy uset)))

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
;; -----------------------------------------------------------------------------
;;  ISL ScheduleTree Deterministic Optimization
;; -----------------------------------------------------------------------------
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

(cffi:defcallback rewrite/fuse-band :pointer
    ((band :pointer) (user :pointer))
  (declare (ignore user))
  (if (eql (isl::%isl-schedule-node-get-type band) :schedule-node-band)
      (let ((depth (schedule-node-band-get-depth (isl::%%make-schedule-node-band band))))
        ;; [TODO] 
        band)
      band))

(defun schedule-split-all-band (schedule)
  (schedule-map schedule (cffi:callback rewrite/split-band)))

(defun schedule-fuse-all-band (schedule)
  "band+child+band ==> [band+band]"
  (schedule-map schedule (cffi:callback rewrite/fuse-band)))
;; ~~~ MergeView in Polyhedral Space ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Problem Setting:
;; Given View = {shape, stride, mask}, computes the beneficial loop generation path {view1.view2}
;; - This should work on different ranked views.
;; - 事前に解を与える？->後の探索で困ってしまう。
(defun multi-union-pw-aff-reshape-to-nd (mupa n)

  )

(defun align-src/dst (dst-mupa src-mupa)
  (declare (type isl::multi-union-pw-aff dst-mupa src-mupa))

  )

(defun schedule-fuse (schedule dst src)
  "Fuse two filters like:
```
components = schedule.child(0) // schedule_node_sequence
schedule_get_child(components, dst) += schedule_get_child(components, src)
```"
  (declare (type isl::schedule schedule) (type fixnum dst src))
  (print (schedule-get-root schedule))
  (let* ((components (schedule-node-get-child (schedule-get-root schedule) 0))
         (n-child (isl::%isl-schedule-node-n-children (isl::schedule-node-handle components))))
    (assert (find (schedule-node-get-type components) '(:schedule-node-sequence :schedule-node-set))
            ()
            "schedule-fuse: The given schedule should be scheduled w/ :Serialize+:Maximize-Filter-Candidates")
    (assert (and (>= dst 0) (>= src 0)
                 (<= dst n-child) (<= src n-child)
                 (not (= src dst))))
    ;; 1. domainを書き換えないといけない。
    ;; 2. BandSplitの意味はあったのかな？=>ある
    ;; 3. Rootに到達するまで再帰的に探索というのが必要かも
    ;; 4. Outermost loopsから，再起的に任意のポジションのsequenceをfuseを繰り返す...というのができるかも
    (let* ((dst-filter-node (schedule-node-get-child components dst)) ;; ISL asserts this is a filter.
           (src-filter-node (schedule-node-get-child components src))
           (dst-filter (isl::schedule-node-filter-get-filter dst-filter-node))
           (src-filter (isl::schedule-node-filter-get-filter src-filter-node))
           (dst-band (schedule-node-get-child dst-filter-node 0))
           (src-band (schedule-node-get-child src-filter-node 0))
           (dst-sched (schedule-node-band-get-partial-schedule dst-band))
           (src-sched (schedule-node-band-get-partial-schedule src-band))
           (dst-i (isl::multi-union-pw-aff-reset-tuple-id
                   (isl::multi-union-pw-aff-intersect-domain dst-sched dst-filter)
                   :dim-out))
           (src-i (isl::multi-union-pw-aff-reset-tuple-id
                   (isl::multi-union-pw-aff-intersect-domain src-sched src-filter)
                   :dim-out))
           (new-sequence (isl::union-set-list-alloc 0)))
      (dotimes (i n-child)
        (cond
          ((= i dst) (setf new-sequence (isl::union-set-list-add new-sequence (isl::union-set-union dst-filter src-filter))))
          ((= i src))
          (T
           (setf new-sequence
                 (isl::union-set-list-add
                  new-sequence
                  (isl::schedule-node-filter-get-filter
                   (schedule-node-get-child components i)))))))
      ;; 数理的には二つの部分スケジュールの出力空間 (Arity, 順序，基底)を一致させる写像fを見つける操作をする。
      ;; Transform SRC matching to DST space.
      ;; Result = DST<MUPA> + f(SRC<MUPA>)
      ;; Find best θ s.t.: schedule_is_valid_p(Result, D)
      ;; θ is a list of:
      ;; - Interchange
      ;; - Reshape
      ;; - Padding
      ;; もっと単純に解けない？
      ;; これはPerformanceをMeasureする。
      ;; 全パターンのValidなスケジュールをリストとして返して，一番評価が高いやつを選ぶ，というのでもいい。
      (print dst-i)
      (print src-i)
      ;; ここでSpaceのPadding, Reshape, Coalesceを考え，Validなものを求める..
      ;; DomainをMergeしないと。。。
      (print new-sequence)
      (let ((fused-band
              (schedule-node-get-child
               (schedule-node-get-child
                (isl::schedule-node-insert-sequence components new-sequence)
                dst)
               0))
            (new-mupa (isl::multi-union-pw-aff-union-add dst-i src-i)))
        (schedule-node-get-schedule
         (print
          (schedule-node-insert-partial-schedule
           fused-band
           new-mupa)))))))
;; Goal
;;   Given a schedule S over an iteration domain D and memory accesses
;;   (R: reads, W: writes), rewrite S into S' that is *never worse* and
;;   often better for SRAM↔DRAM traffic, while preserving all dependences Δ.
;;
;; Inputs / Outputs
;;   Simplify(S, R, W) → S'
;;   where Δ := RAW_must(R,W,S) ∪ WAW_must(W,S) ∪ WAR_may(R,W,S).
;;
;; Core Idea
;;   We identify a “group” G (a node and its subtree) that is heavy and
;;   its dependent front/back neighbors, then *deterministically* reorder
;;   them by inserting a sequence at a band: sequence([affected, unaffected])
;;   or its reverse. The subtree schedules are restricted by union-set
;;   filters, so no explicit stmt splitting is required—ISL limits domains.
;;
;; Heavy / Front / Back (automatic)
;;   Let T = schedule map of S (Dom(T)=D). A statement s is “heavy” if
;;     ∃(x→y) ∈ Δ_s.t. T(x) ≠ T(y)  (i.e., the time delta Δt ≠ 0).
;;   Define:
;;     front := { s | ∃(s→h) ∈ Δ for some heavy h },
;;     back  := { s | ∃(h→s) ∈ Δ for some heavy h }.
;;   affected := heavy ∪ front ∪ back, unaffected := D_subtree \ affected.
;;
;; Legality
;;   We only accept rewrites that keep S' legal:
;;     ∀(x→y) ∈ Δ,  T'(x) ≥_lex T'(y).
;;   This is checked with schedule-is-legal-p over Δ.
;;
;; Benefit (communication proxy via reuse/fusion)
;;   Use *same-address* relation over reads:
;;     SameAddr(R) := R ∘ R^{-1}  ⊆ D × D.
;;   For a pair of stmt sets A,B, restrict domain/range of SameAddr(R) and
;;   embed into time using T:
;;     Δ_reuse(A→B) := deltas( T ∘ (SameAddr|_{A×B}) ∘ T ).
;;   We say “perfect fusion” holds if:
;;     Δ_reuse ≠ ∅  and  Δ_reuse ⊆ {0⃗}.
;;   The objective here is a discrete “fusion score”:
;;     score := 1{front→heavy fused} + 1{heavy→back fused} ∈ {0,1,2}.
;;   We prefer the rewrite (affected-first or -last) that maximizes score.
;;
;; Search Strategy (complete but local)
;;   Enumerate all band nodes; at each band, try two candidates:
;;     sequence([affected,unaffected])  and  sequence([unaffected,affected]).
;;   Evaluate score for each candidate; pick the single global best gain.
;;   If no candidate improves the score, return the original S (monotone).
;;
;; Why it helps (e.g., FlashAttention)
;;   Heavy kernels (e.g., GEMM/WMMA) surrounded by pre/post transforms gain
;;   when reuse becomes *same-time* (Δt=0), keeping tiles hot in SRAM and
;;   removing redundant DRAM round-trips. This pass enforces such proximity
;;   without guessing cache sizes or tile factors.
;;
;; Extensibility
;;   - Replace the discrete score with a richer cost (e.g., L₁-distance of
;;     Δ_reuse to 0⃗, or weighted reuse across dimensions).
;;   - Generalize placement from {front/back} to n-way sibling reordering.
;;   - Combine with later tiling/parallel passes once proximity is improved.
;;
;; Invariants
;;   (1) Dependence legality is never violated.  (2) Score never decreases.
;;   Hence, Simplify is a safe, deterministic pre-optimization for beam search.
;; -----------------------------------------------------------------------------
;; - [ ] Filter Relocate Concepts
;; - [ ] Maximize Locality Rewriting
;; - [ ] Rebundant Guard Elimination
;; - [ ] Post Tile Fusion
;; --- Small helpers -----------------------------------------------------------
;; 少し議論しよう。上のScheduleNode/ASTと下のScheduleNode/ASTの違いはなんだろう？どうして違うASTを生成して，下のASTはメモリアクセスが遅いのだろう？
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

(defun schedule-node-at-path (schedule path)
  "PATH = (i0 i1 ... ik) from root->child(0)."
  (labels ((n-children (n)
             (isl::%isl-schedule-node-n-children (isl::schedule-node-handle n)))
           (child (n i)
             (let ((cnt (n-children n)))
               (assert (and (>= i 0) (< i cnt))
                       () "child index ~a out of [0,~a)" i cnt))
             (schedule-node-get-child n i)))
    (let ((node (schedule-node-get-child (schedule-get-root schedule) 0)))
      (dolist (idx path node) (setf node (child node idx))))))

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
          (print "Reorder ID")
          (print nm)
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
;; Rescheduleを削除する？
;; - その代わり，Sequenceの位置を移動する探索を可能にする
;; - OptFuse
;; - OptFission
;; - OptShift
;; - OptJump

;; - Coincident付与
;; - BandFusion
;; - Late Fission
;; - 極論, serialize-sccs ==> FlashAttentionが組み立てられたらいい
;; - permutableは使わない

(defun ->str (sched)
  (let* ((p     (isl::%isl-printer-to-str (isl::context-handle isl::*context*)))
         (ast   (caten/codegen/search/ast::compute-ast-from-schedule sched))
         (p     (isl::%isl-printer-set-output-format p 4)) ;; 4 == Clang
         (q     (isl::%isl-printer-print-ast-node p (isl::ast-node-handle ast)))
         (str   (isl::%isl-printer-get-str q)))
    str))
(defparameter *sched* "")

(defun test () (->str (isl::schedule-read-from-str *sched*)))
