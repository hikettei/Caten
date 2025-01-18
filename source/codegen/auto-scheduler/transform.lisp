(defpackage :caten/codegen/transform
  (:documentation "Provides various polyhedral transformation for the code transformation")
  (:shadow #:set #:space)
  (:shadowing-import-from :cl :map)
  (:use :cl :caten/aasm :caten/air :caten/codegen/polyhedral :cl-ppcre :caten/isl)
  (:export #:get-zeros-on-union-set #:check-legality-parallel #:check-legality #:schedule-node-band-get-depth)
  ;; Directive
  (:export
    #:Directive #:directive-type #:directive-amount #:directive-visible
    #:directive->str #:directive->id #:str->directive)
  ;; Transforms
  (:export
   #:apply-tile #:apply-unroll #:apply-pack #:%apply-tile #:apply-multi-tile
   #:apply-parallel #:apply-global #:apply-coalesce))

(in-package :caten/codegen/transform)
;; ~~ Legality Computations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun get-zeros-on-union-set (delta-uset)
  (declare (type isl::union-set delta-uset))
  (let* ((delta-set (set-from-union-set delta-uset))
         (ma (multi-aff-zero (set-get-space delta-set))))
    (union-set-from-set (set-from-multi-aff ma))))

(defun check-legality-parallel (node dep)
  "
```
(check-legality-parallel node dep)
```
Returns T if the band node is legal to be parallelized with respect to the dep.
Reference: https://github.com/hikettei/tadashi/blob/main/src/legality.c#L91-L122"
  (declare (type isl::schedule-node node) (type isl::union-map dep))
  (when (union-map-is-empty dep) (return-from check-legality-parallel t))
  (let* ((map (schedule-node-band-get-partial-schedule-union-map node))
         (domain (union-map-apply-range (union-map-apply-domain dep map) map))
         (delta (union-map-deltas domain))
         (_ (when (union-set-is-empty delta) (return-from check-legality-parallel t)))
         (zeros (get-zeros-on-union-set delta))
         (cmp (union-set-lex-lt-union-set delta zeros))
         (retval (union-set-is-empty cmp))
         (cmp (union-set-lex-gt-union-set delta zeros)))
    (declare (ignore _))
    (and retval (union-set-is-empty cmp))))

(defun check-legality (schedule dep)
  "
```
(check-legality schedule dep)
```
Returns T if the current schedule does not break any dependences in dep."
  (declare (type isl::schedule schedule) (type isl::union-map dep))
  (when (union-map-is-empty dep) (return-from check-legality t))
  (let* ((map (schedule-get-map schedule))
         (domain (union-map-apply-domain dep map))
         (domain (union-map-apply-range domain map))
         (delta (union-map-deltas domain))
         (zeros (get-zeros-on-union-set delta))
         (le (union-set-lex-le-union-set delta zeros))
         (retval (union-set-is-empty le)))
    retval))
;; ~~ Directive ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass Directive ()
  ((type :initarg :type :accessor directive-type)
   (amount :initarg :amount :accessor directive-amount)
   (visible :initarg :visible :accessor directive-visible))
  (:documentation "Directive will effect how the schedule-node-band is interpreted by ast-parser.lisp.
Usually, it is inserted just before schedule-node-band, and one schedule-node-band has a one or zero directive.
This class is transformable/loadable to/from string/schedule-node-mark."))

(defun directive (type amount visible)
  "Creates a new directive."
  (declare (type string type) (type (or list fixnum) amount) (type boolean visible))
  (make-instance 'Directive :type type :amount (if (numberp amount) (list amount) amount) :visible visible))

(defmethod print-object ((directive Directive) stream)
  (print-unreadable-object (directive stream)
    (format stream "~a" (directive->str directive))))

(defmethod directive->str ((directive Directive))
  (with-output-to-string (out)
    (format out "@DIRECTIVE(")
    (loop with slots = (c2mop:class-slots (class-of directive))
          for slot-def in slots
          for slot-name = (c2mop:slot-definition-name slot-def)
          for value     = (slot-value directive slot-name)
          for idx upfrom 0 do
            (format out "~a=~a" (string-upcase (princ-to-string slot-name)) value)
            (when (< idx (1- (length slots))) (format out ",")))
    (format out ")")))

(defmethod directive->id ((directive directive)) (isl::make-id-from-str (directive->str directive)))

(defun split-key-and-value (str)
  (let ((pos (position #\= str)))
    (assert pos)
    (let ((key (intern (subseq str 0 pos) "KEYWORD"))
          (value (subseq str (1+ pos))))
      (list
       key
       (case key
         (:TYPE value)
         (:AMOUNT (read-from-string value))
         (:VISIBLE (string= (string-upcase value) "T"))
         (otherwise value))))))

(defun split-directive-string (str)
  (let ((res '()) (start 0) (len (length str)))
    (loop for pos = (position #\, str :start start)
          do (cond
               ((null pos)
                (push (subseq str start len) res)
                (return-from split-directive-string (map 'list #'split-key-and-value (nreverse res))))
               (t
                (push (subseq str start pos) res)
                (setf start (1+ pos)))))))

(defmethod str->directive ((string string))
  ;; @DIRECTIVE(...) is a valid format.
  (unless (and (uiop:string-prefix-p "@DIRECTIVE(" string) (char= (char string (1- (length string))) #\))) (error "Invalid directive string: ~S" string))
  (let* ((content (subseq string #.(length "@DIRECTIVE(") (1- (length string)))))
    (apply #'make-instance 'Directive (apply #'append (split-directive-string content)))))
;; ~~ Primitive Transformations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Here, we provide a set of polyhedral space schedule transformations. The goal here is just proving the validity of the transformations.
;; Final kernel is tweaked when pasing the ISL AST (see: parse-isl-mark in caten/codegen/auto-scheduler/ast-parser.lisp).
;; Everything you should know to read this code is:
;; - Everything is %apply-tile and insert-mark
(defun schedule-node-band-get-depth (band)
  "Returns the depth of the target band."
  (declare (type isl::schedule-node-band band))
  (space-dim (schedule-node-band-get-space band) 3))

(defun broadcast-tile-size (band obj &aux (n-depth (schedule-node-band-get-depth band)))
  (declare (type (or list fixnum) obj))
  (if (listp obj)
      (progn
        (assert (<= (length obj) n-depth) () "The tile dims must be less than the band dims(=~a). getting ~a" n-depth obj)
        obj)
      (loop for i upfrom 0 below n-depth collect obj)))

(defun tiling-sizes (band dims)
  (declare (type list dims) (type list dims))
  (let* ((band-space (schedule-node-band-get-space band))
         (dim (schedule-node-band-get-depth band)))
    (assert (= dim (length dims)) () "Tiling-Sizes: The depth and dims are not matched! getting ~a but length should be ~a." dims dim)
    (multi-val-from-val-list
     band-space
     (apply #'make-value-list dims))))

(defun %apply-tile (band amount directive-parent directive-child)
  "
The function %apply-tile tiles the given schedule-node with amount. e.g.:
```
for (int i=0; i<100; i++) {
  S1(i)
}
```
is transformed into:
```
@DirectiveParent(amount=amount)
for (int i=0; i<100; i+=amount) {
  @DirectiveChild(amount=amount)
  for (int j=0; j<min(i, 4); j++) {
    i' = i + j
    S1(i') 
  }
}
```
"
  (declare (type isl::schedule-node band) (type (or null Directive) directive-parent directive-child) (type list amount))
  (assert (eql (schedule-node-get-type band) :schedule-node-band) () "Only schedule-node-band is tilable!")
  (let* ((dims (schedule-node-band-get-depth band)) ;; Split the band if amount is smaller than dims.
         (band (if (= (length amount) dims) band (schedule-node-band-split band (length amount))))
         (tiled-schedule (if directive-parent
                             (schedule-node-get-child
                              (schedule-node-insert-mark band (directive->id directive-parent))
                              0)
                             band))
         (tiled-schedule (schedule-node-band-tile tiled-schedule (tiling-sizes band amount))))
    (if directive-child
        (let ((tiled-schedule (schedule-node-insert-mark (schedule-node-get-child tiled-schedule 0) (directive->id directive-child))))
          ;; If directive-visible is NIL, the band is oblige to have more optimizations, the returned schedule should be pointed to mark.
          (if (directive-visible directive-child)
              tiled-schedule
              (schedule-node-get-child tiled-schedule 0)))
        tiled-schedule)))
;; ~~ Tile Transformations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun apply-tile (band tile-size)
  "Applies 1D Tiling to the given schedule-node-band with tile-size.
(TODO: this function should accept a symolic tile-size to finetune an optimal tilesize without recompling the code.)"
  (declare (type isl::schedule-node band) (type (or fixnum list) tile-size))
  ;; %apply-tile transform itself is apply-tile. no additional transformation by caten is required. so set directive=nil.
  (%apply-tile band (broadcast-tile-size band tile-size) nil nil))

(defun apply-unroll (band unroll-by)
  "Unrolls the given schedule-node-band with unroll-by."
  (declare (type isl::schedule-node band) (type (or fixnum list) unroll-by))
  (let ((unroll-by (broadcast-tile-size band unroll-by)))
    (%apply-tile band unroll-by (directive "UNROLL_OUTER" unroll-by t) (directive "UNROLL_INNER" unroll-by nil))))

(defun apply-pack (band pack-by)
  "Packs the given schedule-node-band with pack-by which must be constant.
Packed schedule-items has a chance to be upcasted/vectorized by the caten compiler."
  (declare (type isl::schedule-node band) (type (or fixnum list) pack-by))
  (let ((pack-by (broadcast-tile-size band pack-by)))
    (%apply-tile band pack-by (directive "PACKED_OUTER" pack-by t) (directive "PACKED_INNER" pack-by nil))))
;; ~~ Coincidence ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun apply-parallel (schedule-node)
  ;; note: num of cores optimization is beyond caten!
  ;; note: @collapse(2) is 100% effective, apply this automatically...
  (schedule-node-insert-mark schedule-node (directive->id (directive "PARALLEL" 0 T))))

(defun apply-global (poly band ls)
  "Globalizes the schedule-node-band is it satisfies the leagality-parallel.
When considering globalizing S1:
```
for (int i=0; i<100; i++) S1(i)
```
This is equivalent to tile this band with ls.
```
@DIRECTIVE(type=GLOBAL)
for (int i=0; i<100; i+=ls) {
  @DIRECTIVE(type=LOCAL)
  for (int ii=0; ii<min(ls, i);ii++)
    S1(i+ii)
```
Here, we apply some transformation in the ast level:
- 1. If `min` is scheduled, rewrite it with the equivalent `AstIf`
- 2. Remove away @DIRECTIVE(type=GLOBAL), @DIRECTIVE(type=LOCAL) bands with AstExpr: const idx = blockIdx.x * ls + threadIdx.x;
And finally, the gpu kernel is generated:
```
const i = blockIdx.x * ls + threadIdx.x
S1(i+ii)
```

The band should be a plain node just generated by the scop.lisp tp keep simplicity of the generated kernel.
Do not feed the tiled band otherwise the generated kernel would be messed! This thing is not checked by the compiler!"
  (declare (type isl::schedule-node-band band) (type (or list fixnum) ls))
  (assert (eql (schedule-node-get-type band) :schedule-node-band) ())
  ;; (assert (check-legality-parallel band (poly-dependencies poly)))
  (let ((ls (broadcast-tile-size band ls)))
    (%apply-tile band ls (directive "GLOBAL" ls NIL) (directive "LOCAL" ls NIL))))
;; ~~ Legality Based Transformations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun apply-coalesce (band1)
  "Merges the band with the undernearth band if possible.
The basic idea is that:
```
(dotimes (i 10)
  (dotimes (j 10)
    (print (+ (* 10 i) j))))
```
This is equivalent to:
```
(dotimes (i 100)
  (print (+ (* 10 (floor i 10)) (mod i 10))))
```
We assume band1 and band2 are contiguous and mergeable dimension by the shape tracker.
(we only want this optimization under the PARALLEL/GLOBAL Nodes)"
  (declare (type isl::schedule-node band1))
  (assert (eql :schedule-node-band (schedule-node-get-type band1)))
  ;; 単純にこうやって書き換えるだけにする。
  ;; TODO: @DIRECTIVEを使ってAST Levelでやる？
  ;;
  ;; Coalesceにはそれを踏まえてSchedule Node Bandを書き換えるようにしたい。。。
  ;; 1. Remove the band I
  ;; 2. Remove the band J
  ;; 3. Compute the stride of I (but how? by rendering the ast partially?)
  ;; 4. Insert a new schedule band with the stride of I

  ;; softmax -> band depthが同じの複数あるとParallelizeできないルールにする
  ;; const idx1 const idx1でどうせ重複する
  (let* ((sched-a (schedule-node-band-get-partial-schedule band1))
         (band2 (schedule-node-get-child band1 0))
         (_ (assert (eql :schedule-node-band (print (schedule-node-get-type band2))))) ;; [TODO] skip mark ...
         (sched-b (schedule-node-band-get-partial-schedule band2))
         (merged (multi-union-pw-aff-flat-range-product sched-a sched-b)))
    (declare (ignore _))
    ;; Assume you have 4 schedules:
    ;; for (int i=0;i<M;i++16) @GLOBAL(X)
    ;;   for (int j=0;j<min(16, i);j++) @LOCAL(X)
    ;;     for(int k=0;k<N;k++) @GLOBAL(Y)
    ;;       for (int l=0;l<min(16, i);l++) @LOCAL(Y)
    ;;         S(i+j, k+l)
    ;; The goal here is merge J and L in a single loop in a equivalent form.
    ;; for (int i=0;i<M;i++16) @GLOBAL(X)
    ;;   for(int k=0;k<N;k++16) @GLOBAL(Y)
    ;;     for (int j=0;j<min(16, m - i - 1);j++) @LOCAL(X)
    ;;       for (int l=0;l<min(16, n - k - 1);l++) @LOCAL(Y)
    ;;         S(i+j, k+l)
    ;; ==>
    ;; for (int i=0;i<M;i++16) @GLOBAL(X)
    ;;   for(int k=0;k<N;k++16) @GLOBAL(Y)
    ;;     for(..) @LOCAL(
    ;;     for (int j=0;j<min(16, m - i - 1);j++) @LOCAL(X)
    ;;       for (int l=0;l<min(16, n - k - 1);l++) @LOCAL(Y)
    ;;         S(i+j, k+l)
    band1))

(defun apply-cache-blocking ()
  "Applies Shared Memory Cache-Blocking Optimization to the given schedule-node-band (expecting the code is generated for the gpu)
TODO: How to insert the barrier?
"
  ;; TODO: This is just a tiling
  )
