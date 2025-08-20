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
   #:assign-shifts-and-skews
   #:color-fcg
   #:build-fcg
   #:fcg
   #:fcg-clustered-p
   #:fcg-vertices
   #:fcg-edges
   #:fcg-attrs))

(in-package :caten/fcg)

(defstruct fcg
  (sccs (make-hash-table) :type hash-table) ;; scc-id -> scc
  ;; vertices: key => t
  (vertices (make-hash-table :test 'equal))
  (colours (make-hash-table))) ;; key => value where (list color-id x\mathinvertices)

(defmethod print-object ((g fcg) stream)
  (labels ((ref (id) (or (gethash id (fcg-sccs g)) "<UNKNOWN>"))
           (vertex->string (v)
             (etypecase v
               (list
                (destructuring-bind (tag a i) v
                  (format nil "<(~a) : [~A] -> [grid(~A)]>" tag (ref a) i)))
               (t (princ-to-string v)))))
    (print-unreadable-object (g stream :type t :identity t)
      (format stream "~%  vertices:")
      (maphash
       #'(lambda (v attrs)
         (format stream "~%    - ~A ~@[~A~]"
                 (vertex->string v)
                 (when attrs (format nil " ~S" attrs))))
       (fcg-vertices g)))))

(defun fcg-add-vertex (g key) (setf (gethash key (fcg-vertices g)) t))
;; ~~ ISL Utilities ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun %build-ddg (delta)
  "DDG adjacency: stmt -> (unique list of successors)."
  (let ((pairs (umap->stmt-pairs delta))
        (adj   (make-hash-table :test 'equal)))
    (dolist (p pairs) (push (car p) (gethash (cdr p) adj)))  ;; edge src→dst was stored as (dst . src)
    (maphash #'(lambda (k v) (setf (gethash k adj) (remove-duplicates v :test #'equal))) adj)
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
      (maphash #'(lambda (v _succs)
                 (declare (ignore _succs))
                 (unless (gethash v idx) (strongconnect v)))
               adj))
    (values id-of (nreverse sccs))))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun stmt-get-dim (stmt domain)
  (isl::set-dim (isl::set-list-get-at (isl::union-set-get-set-list (%uset-of-stmt domain stmt)) 0) :dim-set))

(defun %enumerate-vertices (g domain sccs)
  "Initialize vertices in g"
  (declare (type FCG g) (type isl::union-set domain) (type list sccs))
  (loop for stmts in sccs for cid upfrom 0
        for mr = (reduce #'max (map 'list #'(lambda (nm) (stmt-get-dim nm domain)) stmts) :initial-value 0) do
          (dotimes (d mr) (fcg-add-vertex g (list :scc cid d)))))

(defmethod fcg-polyhedron-on-dims ((fcg fcg) dom-id dims)
  ;; cached
  )

(defun build-fcg (schedule domain read-umap write-umap)
  "Construct the extended Fusion Conflict Graph F from:
- Schedule[ISL:Schedule]
- Domain[ISL:UnionSet]
- ReadUmap[ISL:UnionMap]
- WriteUmap[ISL:UnionMap]

Caten's fusion conflict graph is extended to 1 vs N edges to handle coalesce.

Edges encode:
  - permute-preventing self-edges (cannot be outermost at this stage),
  - fuse-preventing edges across components,
  - parallelism-preventing (typed) edges,
  - nonreachability edges between mutually-unreachable components,
  - exclusivity edges within the same component (two dims never same color)."
  (declare (type isl::schedule schedule) (type isl::union-set domain)
           (type isl::union-map read-umap write-umap))
  (multiple-value-bind (deps _raw _waw _war) (compute-dependence-relation read-umap write-umap schedule)
    (declare (ignore _raw _waw _war))
    (let* ((ddg    (%build-ddg deps))
           (g      (make-fcg)))
      (multiple-value-bind (scc-of sccs) (%ddg-scc ddg)
        (setf (fcg-sccs g)
              (alexandria:alist-hash-table
               (map 'list #'(lambda (a) (cons (cdr a) (car a))) (alexandria:hash-table-alist scc-of))))
        ;; Approachは同じで，同じ多面体を描画するBandの集合をColoringして，同じLevelに配置したい。
        ;; あまりが出たらModでSeparateする？
        ;; 1) vertices
        (%enumerate-vertices g domain sccs)
        ;; 2) exclusivity inside same component (SCC or stmt)
        ;; (%add-intra-exclusive-edges g domain sccs) ??
        ;; 3) edges due to nonreachability in DDG
        ;; (%add-nonreachability-edges g ddg domain :clustered clustered :sccs sccs)
        ;; 4) self permute-preventing (+ typed serial mark)
        ;; (%add-self-edges g deps domain :clustered clustered :sccs sccs :typed typed)
        ;; 5) inter-component fuse/permute (+ typed parallelism-preserving)
        ;; (%add-pair-edges g deps domain :clustered clustered :sccs sccs :typed typed)
        (print g)))))



;;; ~~~~~~ OLD IMPL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

(defun fcg-attr-push (g key kw val)
  (let ((a (or (gethash key (fcg-attrs g))
               (setf (gethash key (fcg-attrs g)) (make-hash-table :test 'equal)))))
    (setf (gethash kw a) val)))

;;;; ============================================================
;;;; Basic graph/ISL helpers
;;;; ============================================================

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
;;;; ============================================================
;;;; Edge construction
;;;; ============================================================
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
;;;; ============================================================
;;;; (Optional) Greedy convex coloring utilities
;;;;   — kept minimal; FCG itself is the primary artifact.
;;;; ============================================================
(defun %fcg-all-edges (g)
  "Return list of (src dst attr). See print-object for tolerant expansion."
  (let ((res '()))
    (maphash
     (lambda (k v)
       (cond
         ((and (consp k) (= (length k) 2))
          (push (list (first k) (second k) v) res))
         ((or (keywordp k) (listp k))
          (cond
            ((hash-table-p v)
             (maphash (lambda (dst attr) (push (list k dst attr) res)) v))
            ((listp v)
             (dolist (dst v) (push (list k dst nil) res)))
            (t (when v (push (list k v nil) res)))))
         (t)))
     (fcg-edges g))
    (nreverse res)))

(defun %fcg-neighbors (g)
  "Return two hash-tables:
   out[u] => (set of v), undirected[u] => (set of v)."
  (flet ((add (ht a b)
           (let ((s (or (gethash a ht) (make-hash-table :test 'equal))))
             (setf (gethash b s) t)
             (setf (gethash a ht) s))))
    (let ((out (make-hash-table :test 'equal))
          (und (make-hash-table :test 'equal)))
      (dolist (e (%fcg-all-edges g))
        (destructuring-bind (u v _attr) e
          (declare (ignore _attr))
          (add out u v)
          (add und u v)
          (add und v u)))
      (values out und))))

(defun %fcg-self-edge-p (g v)
  (let ((edges (fcg-edges g)))
    (or (gethash (list v v) edges)
        (let ((val (gethash v edges)))
          (cond
            ((hash-table-p val) (gethash v val))
            ((listp val) (member v val :test #'equal))
            (t nil))))))

(defun %vertex-scc-id (v)
  (and (consp v) (eql (first v) :scc) (second v)))

(defun %vertex-dim (v)
  (and (listp v) (third v)))

(defun %vertex-stmt (v)
  (and (consp v) (eql (first v) :stmt) (second v)))

(defun %topo-order-groups (edges group-of)
  "groups: set of group-ids found via (group-of v).
   edges : list of (u v _). Build DAG on groups and topo-sort.
   If cyclic (shouldn't), fall back to numeric/name order."
  (let ((nodes (make-hash-table :test 'equal))
        (adj   (make-hash-table :test 'equal))
        (indeg (make-hash-table :test 'equal)))
    (labels ((add-node (g)
               (unless (gethash g nodes)
                 (setf (gethash g nodes) t)
                 (setf (gethash g indeg) 0)))
             (add-edge (a b)
               (when (and a b (not (equal a b)))
                 (let ((s (or (gethash a adj)
                              (make-hash-table :test 'equal))))
                   (unless (gethash b s)
                     (setf (gethash b s) t)
                     (incf (gethash b indeg 0)))
                   (setf (gethash a adj) s)))))
      ;; build
      (dolist (e edges)
        (destructuring-bind (u v _attr) e
          (declare (ignore _attr))
          (let ((gu (funcall group-of u))
                (gv (funcall group-of v)))
            (add-node gu) (add-node gv)
            (add-edge gu gv))))
      ;; Kahn topo
      (let ((q '())
            (res '()))
        (maphash (lambda (g _)
                   (when (zerop (gethash g indeg 0))
                     (push g q)))
                 nodes)
        (loop while q do
          (let ((x (pop q)))
            (push x res)
            (let ((s (gethash x adj)))
              (when s
                (maphash (lambda (y _)
                           (declare (ignore _))
                           (decf (gethash y indeg))
                           (when (zerop (gethash y indeg)) (push y q)))
                         s)))))
        (let ((n (hash-table-count nodes)))
          (if (= (length res) n)
              (nreverse res)
              ;; fallback: 行儀よく並べる
              (let (all)
                (maphash (lambda (k _) (push k all)) nodes)
                (sort all #'string< :key #'princ-to-string))))))))

;;; --- main: greedy coloring -------------------------------------------------
(defun color-fcg (g &key (clustered (fcg-clustered-p g)) (verbose nil))
  "Assign loop levels as a sequence of convex independent sets.
Store into g.attrs: :levels (vector), :level-of (hash v -> level)."
  (multiple-value-bind (out und) (%fcg-neighbors g)
    (declare (ignore out))
    (let ((vs '()))
      (maphash (lambda (v _attrs) (push v vs)) (fcg-vertices g))
      (setf vs (nreverse vs))
      (let ((levels '())
            (level-of (make-hash-table :test 'equal)))
        (labels
            ((conflict-p (a b)
               (let ((s (gethash a und)))
                 (and s (gethash b s))))
             (degree (v)
               (hash-table-count (or (gethash v und)
                                     (make-hash-table :test 'equal))))
             (greedy-mis (candidates)
               ;; 次数昇順の貪欲最大独立集合近似
               (let ((queue (stable-sort (copy-list candidates) #'< :key #'degree))
                     (picked '()))
                 (dolist (v queue)
                   (unless (some (lambda (u) (conflict-p u v)) picked)
                     (push v picked)))
                 (nreverse picked)))
             (uncolored ()
               (remove-if (lambda (v) (gethash v level-of)) vs))
             (by-scc (xs)
               (let ((m (make-hash-table :test 'equal)))
                 (dolist (v xs)
                   (let ((id (%vertex-scc-id v)))
                     (push v (gethash id m))))
                 m))
             (by-stmt (xs)
               (let ((m (make-hash-table :test 'equal)))
                 (dolist (v xs)
                   (let ((nm (%vertex-stmt v)))
                     (push v (gethash nm m))))
                 m)))
          (loop
            with l = 0
            for first = t then nil
            while (uncolored) do
              (let ((Lv '()))
                (if clustered
                    ;; --- clustered: SCC トポ順で貪欲 ---
                    (let* ((es    (%fcg-all-edges g))
                           (order (%topo-order-groups es #'%vertex-scc-id))
                           (bucket (by-scc (uncolored))))
                      (dolist (gid order)
                        (let* ((cand  (nreverse (gethash gid bucket)))
                               (cand* (if first
                                          (remove-if (lambda (v) (%fcg-self-edge-p g v)) cand)
                                          cand)))
                          (when cand*
                            (setf Lv (nconc Lv (greedy-mis cand*)))))))
                    ;; --- non-clustered: 文トポ順で貪欲 ---
                    (let* ((es    (%fcg-all-edges g))
                           (order (%topo-order-groups es #'%vertex-stmt))
                           (bucket (by-stmt (uncolored))))
                      (dolist (nm order)
                        (let ((cand (nreverse (gethash nm bucket))))
                          (when cand
                            (setf Lv (nconc Lv (greedy-mis cand))))))))
                ;; 停滞回避（レベル0自己辺禁止で詰まる等）
                (when (null Lv)
                  (let* ((cand (uncolored))
                         (picked (if cand (greedy-mis cand) '())))
                    (setf Lv picked)))
                ;; レベル確定
                (dolist (v Lv) (setf (gethash v level-of) l))
                (push Lv levels)
                (incf l)))
          ;; 保存
          (let* ((levels* (nreverse levels))
                 (vec     (coerce levels* 'vector)))
            (setf (gethash :levels  (fcg-attrs g)) vec)
            (setf (gethash :level-of (fcg-attrs g)) level-of)
            (when verbose
              (format t "~&[color] levels=~D~%~a" (length vec) vec))
            vec))))))

(defun %build-delta-from-rw (read-umap write-umap)
  "Build dependence relation Δ = W ∘ R^{-1} as an ISL union-map.
READ-UMAP, WRITE-UMAP are CATEN/ISL:UNION-MAPs:
  stmt_iters -> memory_subscript_space"
  (let* ((rinv (caten/isl::union-map-reverse read-umap))
         (delta (caten/isl::union-map-apply-range write-umap rinv)))
    delta))

(defun assign-shifts-and-skews (g domain read-umap write-umap &key (verbose nil))
  "Given colored FCG, assign per-vertex shifts and per-statement simple skew.
Stores :shift-of (v->int), :skew-of (stmt->list of (j :plus i))."
  (declare (ignore domain))
  (let* ((levels   (gethash :levels  (fcg-attrs g)))
         (level-of (gethash :level-of (fcg-attrs g)))
         (shift-of (make-hash-table :test 'equal))
         (skew-of  (make-hash-table :test 'equal))
         (delta (%build-delta-from-rw read-umap write-umap)))
    (labels
        ((level-vertices (l) (aref levels l))
         (vertex< (a b) (string< (princ-to-string a) (princ-to-string b)))
         (stmt&dim (v)
           (etypecase v
             (list
              (destructuring-bind (tag a i &rest rest) v
                (declare (ignore rest))
                (ecase tag
                  (:stmt (values a i))
                  (:scc  (values (format nil "@scc-~A" a) i)))))))
         (negative-self-dep? (stmt i j)
           ;; Δ(S,i -> S,j) に負距離が存在？
           (let ((sname (if (stringp stmt) stmt (princ-to-string stmt))))
             (%map-any-violates?/pair delta sname i sname j :violate-kind :lt))))
      ;; (1) per-level shifts（独立集合なのでデフォルト 0）
      (dotimes (l (length levels))
        (let ((Lv (stable-sort (copy-list (level-vertices l)) #'vertex<)))
          (dolist (v Lv)
            (setf (gethash v shift-of) 0))))
      ;; (2) simple skew per statement: j := j + i
      (let ((by-stmt (make-hash-table :test 'equal)))
        ;; 頂点→文ごとにグルーピング
        (maphash
         (lambda (v _)
           (multiple-value-bind (s i) (stmt&dim v)
             (push (list v s i (gethash v level-of))
                   (gethash s by-stmt))))
         (fcg-vertices g))
        ;; 各文で (外側i, 内側j) をチェック
        (maphash
         (lambda (s items)
           (let ((sorted (sort items #'< :key #'fourth)))
             (dolist (a sorted)
               (destructuring-bind (_va _sa ia la) a
                 (declare (ignore _va _sa))
                 (dolist (b sorted)
                   (when (> (fourth b) la)
                     (destructuring-bind (_vb _sb jb lb) b
                       (declare (ignore _vb _sb lb))
                       (when (negative-self-dep? s ia jb)
                         (push (list jb :plus ia)
                               (gethash s skew-of))))))))))
         by-stmt)))
    ;; 保存
    (setf (gethash :shift-of (fcg-attrs g)) shift-of)
    (setf (gethash :skew-of  (fcg-attrs g)) skew-of)
    (when verbose
      (format t "~&[shift/skew] shifts=~D, skews=~D~%"
              (hash-table-count shift-of)
              (hash-table-count skew-of)))
    (values shift-of skew-of)))

;;; ---------- helper ----------


(defun %reason-cell-push (cell reason)
  "Return a reason-set cell with REASON included."
  (let ((rs (or cell (make-hash-table :test 'eq))))
    (when reason (setf (gethash reason rs) t))
    rs))

;;; ---------- add edge (理由を保持) ----------
(defun fcg-add-edge (g u v &key (reason nil))
  "Add an undirected conflict edge {u,v}. If u=v, record a self-edge with reasons.
REASON is a symbol like :fuse-preventing, :parallelism-preventing, etc."
  (if (equal u v)
      ;; self-edge: neighbors[:self] に理由集合を積む
      (let* ((ne (%edges-get (fcg-edges g) u))
             (cell (gethash :self ne)))
        (setf (gethash :self ne) (%reason-cell-push cell (or reason :permute-preventing))))
      ;; undirected edge: 双方向に理由集合を積む
      (let* ((neu (%edges-get (fcg-edges g) u))
             (nev (%edges-get (fcg-edges g) v))
             (cell-uv (gethash v neu))
             (cell-vu (gethash u nev)))
        (setf (gethash v neu) (%reason-cell-push cell-uv (or reason :unspecified)))
        (setf (gethash u nev) (%reason-cell-push cell-vu (or reason :unspecified)))))
  g)

;;; ---------- self-edge 判定（:self を見る） ----------
(defun %fcg-self-edge-p (g v)
  (let ((neighbors (gethash v (fcg-edges g))))
    (cond
      ((hash-table-p neighbors) (and (gethash :self neighbors) t))
      ((listp neighbors)        (member :self neighbors :test #'eq))
      (t nil))))

;;; ---------- エッジ列挙（:self を (u u …) に展開し理由を返す） ----------
;;; ---------- 近隣集合（自己辺は無視して無向化） ----------
(defun %fcg-neighbors (g)
  "Return two hash-tables: out[u] => (set of v), undirected[u] => (set of v)."
  (flet ((add (ht a b)
           (let ((s (or (gethash a ht) (make-hash-table :test 'equal))))
             (setf (gethash b s) t)
             (setf (gethash a ht) s))))
    (let ((out (make-hash-table :test 'equal))
          (und (make-hash-table :test 'equal)))
      (dolist (e (%fcg-all-edges g))
        (destructuring-bind (u v _reasons) e
          (unless (equal u v)            ; self-edge は隣接には入れない
            (add out u v)
            (add und u v)
            (add und v u))))
      (values out und))))

;;; ---------- 表示を理由付きに（print-object のローカル collect-edges 相当を置換） ----------
