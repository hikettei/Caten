(defpackage :caten/fcg
  (:shadow #:set #:space)
  (:shadowing-import-from :cl :map)
  (:use :cl :caten/isl)
  (:import-from
   :caten/codegen/search/schedule
   #:compute-dependence-relation
   #:union-set-get-statements
   #:zero-vector-union-set
   #:umap->stmt-pairs)
  (:export
   #:build-fcg
   #:fcg
   #:fcg-clustered-p
   #:fcg-vertices
   #:fcg-edges
   #:fcg-attrs))

(in-package :caten/fcg)

;;; References (kept brief; see original papers):
;;; - Uday Bondhugula et al., "A Practical Automatic Polyhedral Parallelizer and Locality Optimizer", PLDI 2008 (PLuTo).
;;;   * Fusion legality via dependence (distance >= 0) checks across pairs (S,T).
;;;   * Conflict graph construction for loop fusion/ordering.
;;; - Sven Verdoolaege, "isl: Integer Set Library", for precise map/set operations.
;;; Our build-fcg follows these: self permute-preventing, inter-pair fuse-preventing,
;;; parallelism-preserving (typed outer-parallel), intra-component exclusivity, nonreachability edges.
(defun %tuple-name-first (x &key (role :domain))
  "Return the tuple-name (string) of the first component of X.
X may be a string/symbol, isl::set/union-set, isl::map/union-map.
ROLE=:domain or :range only matters for map/union-map."
  (cond
    ((stringp x) x)
    ((symbolp x) (symbol-name x))
    ((typep x 'isl::set)
     (or (isl::set-get-tuple-name x) ""))
    ((typep x 'isl::union-set)
     (let* ((sl (isl::union-set-get-set-list x))
            (n  (isl::set-list-n-set sl)))
       (if (> n 0)
           (or (isl::set-get-tuple-name (isl::set-list-get-at sl 0)) "")
           "")))
    ((typep x 'isl::map)
     (let ((s (ecase role
                (:domain (isl::map-domain (copy x)))
                (:range  (isl::map-range  (copy x))))))
       (or (isl::set-get-tuple-name s) "")))
    ((typep x 'isl::union-map)
     (let* ((ml (isl::union-map-get-map-list x))
            (n  (isl::map-list-size ml)))
       (if (> n 0)
           (let* ((m (isl::map-list-elt ml 0))
                  (s (ecase role
                       (:domain (isl::map-domain (copy m)))
                       (:range  (isl::map-range  (copy m))))))
             (or (isl::set-get-tuple-name s) ""))
           "")))
    (t "")))

(defstruct fcg
  (clustered-p t)
  ;; vertices: key => t
  ;;   clustered:   key = (list :scc scc-id dim)
  ;;   unclustered: key = (list :stmt stmt-name dim)
  (vertices (make-hash-table :test 'equal))
  ;; edges: key => hash-set of neighbors (undirected)
  ;;        self-edge is recorded as key :self in neighbor set
  (edges    (make-hash-table :test 'equal))
  ;; per-vertex attributes (hash): e.g.,
  ;;   (:serial-p t) / (:parallel-p t) / (:reason <list-of-symbols>)
  (attrs    (make-hash-table :test 'equal)))

(defun %edges-get (ht k)
  (or (gethash k ht)
      (setf (gethash k ht) (make-hash-table :test 'equal))))

(defun fcg-add-vertex (g key)
  (setf (gethash key (fcg-vertices g)) t))

(defun fcg-attr-push (g key kw val)
  (let ((a (or (gethash key (fcg-attrs g))
               (setf (gethash key (fcg-attrs g)) (make-hash-table :test 'equal)))))
    (setf (gethash kw a) val)))

(defun fcg-add-edge (g u v &key (reason nil))
  "Add an undirected conflict edge {u,v}. If u=v, record a self-edge.
When REASON is non-NIL and u=v, append reason to (:reasons ...) attr."
  (when (equal u v)
    ;; self-edge prevents coloring this vertex at the current level
    (setf (gethash :self (%edges-get (fcg-edges g) u)) t)
    (when reason
      (let ((ah (%edges-get (fcg-attrs g) u)))
        (push reason (gethash :reasons ah)))))
  (unless (equal u v)
    (setf (gethash v (%edges-get (fcg-edges g) u)) t)
    (setf (gethash u (%edges-get (fcg-edges g) v)) t))
  g)

;;;; ============================================================
;;;; Basic graph/ISL helpers
;;;; ============================================================

(defun schedule-domain (schedule)
  "Dom(Schedule) as isl::union-set."
  (union-map-domain (schedule-get-map schedule)))

(defun %uset-of-stmt (domain stmt-name)
  "Return union-set component for a single statement tuple-name."
  (let* ((sets (isl::union-set-get-set-list domain))
         (n    (isl::set-list-n-set sets))
         (acc  nil))
    (dotimes (i n)
      (let* ((s  (isl::set-list-get-at sets i))
             (nm (or (isl::set-get-tuple-name s) "")))
        (when (string= nm stmt-name)
          (setf acc (if acc (isl::union-set-union acc (isl::union-set-from-set s))
                        (isl::union-set-from-set s))))))
    (or acc (isl::union-set-empty (isl::union-set-get-space domain)))))

(defun %restrict-delta-pair (delta uset-src uset-dst)
  "Return Δ ∩ (uset-src × uset-dst)."
  (let* ((d1 (union-map-intersect-domain (copy delta) (copy uset-src))))
    (union-map-intersect-range d1 (copy uset-dst))))

(defun %build-ddg (delta)
  "DDG adjacency: stmt -> (unique list of successors)."
  (let ((pairs (umap->stmt-pairs delta))
        (adj   (make-hash-table :test 'equal)))
    (dolist (p pairs)
      (push (car p)  (gethash (cdr p) adj))  ;; edge src→dst was stored as (dst . src)
      (gethash (cdr p) adj))
    (maphash (lambda (k v) (setf (gethash k adj) (remove-duplicates v :test #'equal))) adj)
    adj))


(defun %ddg-scc (adj)
  "Tarjan SCC. Return (values id-of (list of sccs))."
  (let ((index 0) (stack '()) (onstack (make-hash-table :test 'equal))
        (idx (make-hash-table :test 'equal))
        (low (make-hash-table :test 'equal))
        (id-of (make-hash-table :test 'equal))
        (sccs '()))
    (labels
        ((strongconnect (v)
           (setf (gethash v idx) index
                 (gethash v low) index)
           (incf index)
           (push v stack)
           (setf (gethash v onstack) t)
           (dolist (w (gethash v adj))
             (cond
               ((not (gethash w idx))
                (strongconnect w)
                (setf (gethash v low) (min (gethash v low) (gethash w low))))
               ((gethash w onstack)
                (setf (gethash v low) (min (gethash v low) (gethash w idx))))))
           (when (= (gethash v low) (gethash v idx))
             (let ((comp '()))
               (block pop-loop
                 (loop for w = (pop stack) do
                   (setf (gethash w onstack) nil)
                   (push w comp)
                   (when (equal w v)
                     (return-from pop-loop nil))))
               (let ((cid (length sccs)))
                 (dolist (x comp) (setf (gethash x id-of) cid))
                 (push (nreverse comp) sccs))))))
      (maphash (lambda (v _succs)
                 (declare (ignore _succs))
                 (unless (gethash v idx) (strongconnect v)))
               adj))
    (values id-of (nreverse sccs))))

;;;; ============================================================
;;;; Robust legality checks on Δ (no φ construction).
;;;;   Per-map computation of indices; guard by tuple-names.
;;;; ============================================================
(progn
  (defparameter *check-one-map* nil)
  (cffi:defcallback %cb-map-viol :int ((m :pointer) (user :pointer))
    (declare (ignore user))
    (funcall *check-one-map* (isl::%make-map m))
    0)
  (defun %map-any-violates?/pair (umap s-name s-dim t-name t-dim &key (violate-kind :lt))
    "Return T iff ∃(x∈S, y∈T) in UMAP that violates:
   :lt ⇒ y[t] - x[s] ≤ -1;  :gt ⇒ x[s] - y[t] ≤ -1
   Only maps whose (domain,range) tuple-names equal (s-name,t-name) are considered."
    (let* ((sname (%tuple-name-first s-name :role :domain))
           (tname (%tuple-name-first t-name :role :range))
           (hit   nil))
      (labels ((check-one-map (m)
                 (let* ((dom   (isl::map-domain (copy m)))
                        (ran   (isl::map-range  (copy m)))
                        (dname (or (isl::set-get-tuple-name dom) ""))
                        (rname (or (isl::set-get-tuple-name ran) "")))
                   (unless (and (string= dname sname) (string= rname tname))
                     (return-from check-one-map nil))
                   (let* ((domd (isl::set-dim dom :dim-set))
                          (rand (isl::set-dim ran :dim-set)))
                     (when (or (>= s-dim domd) (>= t-dim rand))
                       (return-from check-one-map nil))
                     (let* ((set  (isl::map-wrap (copy m)))
                            (ls   (local-space-from-space (set-get-space set)))
                            (ineq (make-inequality-constraint ls))
                            (dpos s-dim)
                            (rpos (+ domd t-dim)))
                       ;; Build inequality
                       (ecase violate-kind
                         (:lt
                          (setf ineq (isl::set-constant-val ineq (value -1)))
                          (setf ineq (isl::set-coefficient-si ineq :dim-set dpos +1))
                          (setf ineq (isl::set-coefficient-si ineq :dim-set rpos -1)))
                         (:gt
                          (setf ineq (isl::set-constant-val ineq (value -1)))
                          (setf ineq (isl::set-coefficient-si ineq :dim-set rpos +1))
                          (setf ineq (isl::set-coefficient-si ineq :dim-set dpos -1))))
                       (let ((viol (isl::set-add-constraint set ineq)))
                         (unless (isl::set-is-empty viol)
                           (setf hit t))))))))
        (let ((*check-one-map* #'check-one-map))
          (isl::%isl-union-map-foreach-map
           (isl::union-map-handle umap)
           (cffi:callback %cb-map-viol)
           (cffi:null-pointer))))
      hit)))

(defun %outer-parallel?/pair (umap s-name s-dim t-name t-dim)
  "Return T iff distance in (s-dim,t-dim) is always 0 for all (x∈S,y∈T) in UMAP."
  (and (not (%map-any-violates?/pair umap s-name s-dim t-name t-dim :violate-kind :lt))
       (not (%map-any-violates?/pair umap s-name s-dim t-name t-dim :violate-kind :gt))))

(defun %nonnegative-distance?/pair (umap s-name s-dim t-name t-dim)
  "Return T iff ∀(x∈S,y∈T):  y[t]-x[s] ≥ 0."
  (not (%map-any-violates?/pair umap s-name s-dim t-name t-dim :violate-kind :lt)))

(defun %outer-parallel?/self (umap name dim)
  (%outer-parallel?/pair umap name dim name dim))

(defun %nonnegative-distance?/self (umap name dim)
  (%nonnegative-distance?/pair umap name dim name dim))

(defun %stmt->rank (domain)
  "Return alist (stmt-name . rank). Rank = set-dim of the tuple."
  (let ((names (union-set-get-statements domain))
        (acc   '()))
    (dolist (nm names (nreverse acc))
      (let* ((uset (%uset-of-stmt domain nm))
             (sl   (isl::union-set-get-set-list uset))
             (s    (isl::set-list-get-at sl 0))
             (rank (isl::set-dim s :dim-set)))
        (push (cons nm rank) acc)))))

;;;; ============================================================
;;;; Vertex enumeration
;;;; ============================================================

(defun %enumerate-vertices (g domain clustered scc-of sccs)
  (if clustered
      ;; per-SCC: vertex = (:scc cid dim), dim ∈ [0 .. max-rank(SCC)-1]
      (loop for cid from 0 below (length sccs) do
            (let* ((stmts (nth cid sccs))
                   (mr (reduce #'max
                               (mapcar (lambda (nm)
                                         (isl::set-dim
                                          (isl::set-list-get-at
                                           (isl::union-set-get-set-list (%uset-of-stmt domain nm)) 0)
                                          :dim-set))
                                       stmts)
                               :initial-value 0)))
              (dotimes (d mr) (fcg-add-vertex g (list :scc cid d)))))
      ;; per-statement
      (dolist (pr (%stmt->rank domain))
        (dotimes (d (cdr pr))
          (fcg-add-vertex g (list :stmt (car pr) d)))))
  g)

;;;; ============================================================
;;;; Edge construction
;;;; ============================================================

(defun %add-intra-exclusive-edges (g domain &key (clustered t) sccs)
  "Add exclusivity edges inside the same SCC (or same statement) so that
two distinct dimensions of the same component cannot share a color:
∀ i≠j in a component C, add edge (C,i)-(C,j)."
  (if clustered
      (dotimes (cid (length sccs))
        (let* ((stmts (nth cid sccs))
               (mr (reduce #'max
                           (mapcar (lambda (nm)
                                     (isl::set-dim
                                      (isl::set-list-get-at
                                       (isl::union-set-get-set-list (%uset-of-stmt domain nm)) 0)
                                      :dim-set))
                                   stmts)
                           :initial-value 0)))
          (dotimes (i mr)
            (dotimes (j mr)
              (when (< i j)
                (fcg-add-edge g (list :scc cid i) (list :scc cid j) :reason :same-component)))))) 
      ;; unclustered: per statement
      (dolist (pr (%stmt->rank domain))
        (let ((nm (car pr)) (rk (cdr pr)))
          (dotimes (i rk)
            (dotimes (j rk)
              (when (< i j)
                (fcg-add-edge g (list :stmt nm i) (list :stmt nm j) :reason :same-statement))))))))

(defun %add-nonreachability-edges (g adj domain &key (clustered t) sccs)
  "If two components are mutually unreachable in the DDG, they should not be fused:
add full bipartite edges across all their dimensions."
  (labels ((reach? (src dst)
             (let ((found nil)
                   (seen (make-hash-table :test 'equal)))
               (labels ((dfs (v)
                          (when found (return-from dfs))
                          (when (equal v dst) (setf found t) (return-from dfs))
                          (unless (gethash v seen)
                            (setf (gethash v seen) t)
                            (dolist (w (gethash v adj)) (dfs w)))))
                 (dfs src))
               found)))
    (if clustered
        (let ((n (length sccs)))
          (dotimes (a n)
            (dotimes (b n)
              (when (/= a b)
                (let* ((Sa (nth a sccs))
                       (Sb (nth b sccs))
                       (ab (some (lambda (x) (some (lambda (y) (reach? x y)) Sb)) Sa))
                       (ba (some (lambda (x) (some (lambda (y) (reach? x y)) Sa)) Sb)))
                  (when (and (not ab) (not ba))
                    (let* ((ra (reduce #'max
                                       (mapcar (lambda (nm)
                                                 (isl::set-dim
                                                  (isl::set-list-get-at
                                                   (isl::union-set-get-set-list (%uset-of-stmt domain nm)) 0)
                                                  :dim-set))
                                               Sa)
                                       :initial-value 0))
                           (rb (reduce #'max
                                       (mapcar (lambda (nm)
                                                 (isl::set-dim
                                                  (isl::set-list-get-at
                                                   (isl::union-set-get-set-list (%uset-of-stmt domain nm)) 0)
                                                  :dim-set))
                                               Sb)
                                       :initial-value 0)))
                      (dotimes (i ra)
                        (dotimes (j rb)
                          (fcg-add-edge g (list :scc a i) (list :scc b j) :reason :nonreachable))))))))))
        ;; unclustered
        (let ((stmts (union-set-get-statements domain)))
          (dolist (sa stmts)
            (dolist (sb stmts)
              (when (and (not (string= sa sb))
                         (not (reach? sa sb))
                         (not (reach? sb sa)))
                (let* ((ra (isl::set-dim (isl::set-list-get-at
                                          (isl::union-set-get-set-list (%uset-of-stmt domain sa)) 0)
                                         :dim-set))
                       (rb (isl::set-dim (isl::set-list-get-at
                                          (isl::union-set-get-set-list (%uset-of-stmt domain sb)) 0)
                                         :dim-set)))
                  (dotimes (i ra)
                    (dotimes (j rb)
                      (fcg-add-edge g (list :stmt sa i) (list :stmt sb j) :reason :nonreachable)))))))))))

(defun %add-self-edges (g delta domain &key (clustered t) sccs typed)
  "Add self permute-preventing edges for every dimension that cannot be
moved to the outer level without violating Δ. If TYPED, also mark :serial-p
when the dimension is not outer-parallel (distance ≠ 0)."
  (if clustered
      (dotimes (cid (length sccs))
        (let* ((stmts (nth cid sccs))
               (mr (reduce #'max
                           (mapcar (lambda (nm)
                                     (isl::set-dim
                                      (isl::set-list-get-at
                                       (isl::union-set-get-set-list (%uset-of-stmt domain nm)) 0)
                                      :dim-set))
                                   stmts)
                           :initial-value 0)))
          (dotimes (d mr)
            ;; permute-preventing: if any statement in SCC violates nonnegativity on this dim
            (let ((ok
                    (every (lambda (nm)
                             (let* ((S (%uset-of-stmt domain nm))
                                    (sub (%restrict-delta-pair delta S S)))
                               (or (union-map-is-empty sub)
                                   (%nonnegative-distance?/self sub nm d))))
                           stmts)))
              (unless ok
                (fcg-add-edge g (list :scc cid d) (list :scc cid d) :reason :permute-preventing)
                (fcg-attr-push g (list :scc cid d) :serial-p t)))
            (when typed
              ;; outer-parallel? (distance==0)
              (let ((par?
                      (every (lambda (nm)
                               (let* ((S (%uset-of-stmt domain nm))
                                      (sub (%restrict-delta-pair delta S S)))
                                 (or (union-map-is-empty sub)
                                     (%outer-parallel?/self sub nm d))))
                             stmts)))
                (unless par?
                  (fcg-attr-push g (list :scc cid d) :serial-p t)))))))
      ;; unclustered
      (dolist (pr (%stmt->rank domain))
        (let ((nm (car pr)) (rk (cdr pr)))
          (dotimes (d rk)
            (let* ((S   (%uset-of-stmt domain nm))
                   (sub (%restrict-delta-pair delta S S)))
              (unless (or (union-map-is-empty sub)
                          (%nonnegative-distance?/self sub nm d))
                (fcg-add-edge g (list :stmt nm d) (list :stmt nm d) :reason :permute-preventing)
                (fcg-attr-push g (list :stmt nm d) :serial-p t))
              (when typed
                (unless (or (union-map-is-empty sub)
                            (%outer-parallel?/self sub nm d))
                  (fcg-attr-push g (list :stmt nm d) :serial-p t)))))))))

(defun %add-pair-edges (g delta domain &key (clustered t) sccs typed)
  "Add inter-component fuse/permute preventing edges.
Typed: also add parallelism-preserving edges (outer-parallel must be kept)."
  (if clustered
      ;; --- clustered (per-SCC) ---
      (let ((n (length sccs)))
        (dotimes (a n)
          (dotimes (b n)
            (let* ((Sa (nth a sccs))
                   (Sb (nth b sccs))
                   (ra (reduce #'max
                               (mapcar (lambda (nm)
                                         (isl::set-dim
                                          (isl::set-list-get-at
                                           (isl::union-set-get-set-list (%uset-of-stmt domain nm)) 0)
                                          :dim-set))
                                       Sa)
                               :initial-value 0))
                   (rb (reduce #'max
                               (mapcar (lambda (nm)
                                         (isl::set-dim
                                          (isl::set-list-get-at
                                           (isl::union-set-get-set-list (%uset-of-stmt domain nm)) 0)
                                          :dim-set))
                                       Sb)
                               :initial-value 0)))
              (dotimes (i ra)
                (dotimes (j rb)
                  (when (or (/= a b) (/= i j))
                    ;; Fuse legality (non-negative distance both directions)
                    (let ((ok
                            (every (lambda (s1)
                                     (every (lambda (s2)
                                              (let* ((S1 (%uset-of-stmt domain s1))
                                                     (S2 (%uset-of-stmt domain s2))
                                                     (ab (%restrict-delta-pair delta S1 S2))
                                                     (ba (%restrict-delta-pair delta S2 S1)))
                                                (and
                                                 (or (union-map-is-empty ab)
                                                     (%nonnegative-distance?/pair ab s1 i s2 j))
                                                 (or (union-map-is-empty ba)
                                                     (%nonnegative-distance?/pair ba s2 j s1 i)))))
                                            Sb))
                                   Sa)))
                      (unless ok
                        (fcg-add-edge g (list :scc a i) (list :scc b j) :reason :fuse-preventing)))
                    ;; Typed: outer-parallel must be preserved (distance==0 possible)
                    (when typed
                      (let ((par?
                              (some (lambda (s1)
                                      (some (lambda (s2)
                                              (let* ((S1 (%uset-of-stmt domain s1))
                                                     (S2 (%uset-of-stmt domain s2))
                                                     (ab (%restrict-delta-pair delta S1 S2))
                                                     (ba (%restrict-delta-pair delta S2 S1)))
                                                (and
                                                 (or (union-map-is-empty ab)
                                                     (%outer-parallel?/pair ab s1 i s2 j))
                                                 (or (union-map-is-empty ba)
                                                     (%outer-parallel?/pair ba s2 j s1 i)))))
                                            Sb))
                                    Sa)))
                        (unless par?
                          (fcg-add-edge g (list :scc a i) (list :scc b j)
                                        :reason :parallelism-preventing)))))))))))
      ;; --- unclustered (per-statement) ---
      (let ((stmts (union-set-get-statements domain)))
        (dolist (sa stmts)
          (dolist (sb stmts)
            (let* ((ra (isl::set-dim (isl::set-list-get-at
                                      (isl::union-set-get-set-list (%uset-of-stmt domain sa)) 0)
                                     :dim-set))
                   (rb (isl::set-dim (isl::set-list-get-at
                                      (isl::union-set-get-set-list (%uset-of-stmt domain sb)) 0)
                                     :dim-set))
                   (SA (%uset-of-stmt domain sa))
                   (SB (%uset-of-stmt domain sb))
                   (ab (%restrict-delta-pair delta SA SB))
                   (ba (%restrict-delta-pair delta SB SA)))
              (dotimes (i ra)
                (dotimes (j rb)
                  (when (or (not (string= sa sb)) (/= i j))
                    (unless (and
                             (or (union-map-is-empty ab)
                                 (%nonnegative-distance?/pair ab sa i sb j))
                             (or (union-map-is-empty ba)
                                 (%nonnegative-distance?/pair ba sb j sa i)))
                      (fcg-add-edge g (list :stmt sa i) (list :stmt sb j) :reason :fuse-preventing))
                    (when typed
                      (unless (and
                               (or (union-map-is-empty ab)
                                   (%outer-parallel?/pair ab sa i sb j))
                               (or (union-map-is-empty ba)
                                   (%outer-parallel?/pair ba sb j sa i)))
                        (fcg-add-edge g (list :stmt sa i) (list :stmt sb j)
                                      :reason :parallelism-preventing))))))))))))
;;;; ============================================================
;;;; Top-level FCG builder
;;;; ============================================================

(defun build-fcg (schedule read-umap write-umap &key (clustered t) (typed nil))
  "Construct the Fusion Conflict Graph F from:
  • S : schedule,
  • R : read accesses (UMap),
  • W : write accesses (UMap).
Options:
  :clustered T ⇒ vertices correspond to (SCC-id,dim);
              NIL ⇒ vertices correspond to (stmt,dim).
  :typed     T ⇒ add parallelism-preserving (outer-parallel) constraints
                  as edges/attrs (shift/scale conflicts are handled later).

Edges encode:
  - permute-preventing self-edges (cannot be outermost at this stage),
  - fuse-preventing edges across components,
  - parallelism-preventing (typed) edges,
  - nonreachability edges between mutually-unreachable components,
  - exclusivity edges within the same component (two dims never same color)."
  (multiple-value-bind (deps _raw _waw _war)
      (compute-dependence-relation read-umap write-umap schedule)
    (declare (ignore _raw _waw _war))
    (let* ((domain (schedule-domain schedule))
           (ddg    (%build-ddg deps))
           (g      (make-fcg :clustered-p clustered)))
      (multiple-value-bind (scc-of sccs)
          (%ddg-scc ddg)
        ;; 1) vertices
        (%enumerate-vertices g domain clustered scc-of sccs)
        ;; 2) exclusivity inside same component (SCC or stmt)
        (%add-intra-exclusive-edges g domain :clustered clustered :sccs sccs)
        ;; 3) edges due to nonreachability in DDG
        (%add-nonreachability-edges g ddg domain :clustered clustered :sccs sccs)
        ;; 4) self permute-preventing (+ typed serial mark)
        (%add-self-edges g deps domain :clustered clustered :sccs sccs :typed typed)
        ;; 5) inter-component fuse/permute (+ typed parallelism-preserving)
        (%add-pair-edges g deps domain :clustered clustered :sccs sccs :typed typed)
        g))))

;;;; ============================================================
;;;; (Optional) Greedy convex coloring utilities
;;;;   — kept minimal; FCG itself is the primary artifact.
;;;; ============================================================
(defun %has-self-edge-p (g v)
  (let ((nb (gethash v (fcg-edges g))))
    (and nb (gethash :self nb))))

(defun %conflicts-with-set-p (g v chosen)
  "Conflict if v has a self-edge, or an edge to any u in CHOSEN."
  (or (%has-self-edge-p g v)
      (some (lambda (u)
              (let ((nu (gethash u (fcg-edges g)))
                    (nv (gethash v (fcg-edges g))))
                (or (and nu (gethash v nu))
                    (and nv (gethash u nv)))))
            chosen)))

;; Note: A full-fledged reconstruction across levels (removing satisfied
;; deps and rebuilding the FCG) belongs to the scheduling phase. Here we
;; keep a minimal greedy coloring helper for clients that want a quick
;; convex partition on the constructed FCG itself.
