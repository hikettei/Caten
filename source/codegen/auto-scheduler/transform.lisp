(defpackage :caten/codegen/transform
  (:documentation "Provides various polyhedral transformation for the code transformation")
  (:shadow #:set #:space)
  (:shadowing-import-from :cl :map)
  (:use :cl :caten/aasm :caten/air :caten/codegen/polyhedral :cl-ppcre :caten/isl)
  (:export #:get-zeros-on-union-set #:check-legality-parallel #:check-legality)
  ;; Directive
  (:export
    #:Directive #:directive-type #:directive-amount #:directive-visible
    #:directive->str #:directive->id #:str->directive)
  ;; Transforms
  (:export
    #:apply-interchange #:apply-tile #:apply-unroll #:apply-pack #:%apply-tile)
  ;; Marks
  (:export
    #:apply-parallel
    #:apply-global))

(in-package :caten/codegen/transform)
;; ~~ Legality Computations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
  (:documentation "Directive is an instruction to schedule-node-band. This class is dumpable as a string to interoperate with ISL."))

(defun directive (type amount visible)
  (declare (type string type) (type fixnum amount) (type boolean visible))
  (make-instance 'Directive :type type :amount amount :visible visible))

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
         (:AMOUNT (parse-integer value))
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
;; ~~ Tile Based Transformations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun tiling-sizes (band &key (size-default 32) (dims))
  (declare (type list dims) (type fixnum size-default))
  (let* ((band-space (schedule-node-band-get-space band))
         (dim (space-dim band-space 3)))
    (multi-val-from-val-list
     band-space
     (apply #'make-value-list (loop for i upfrom 0 below dim collect (or (nth i dims) size-default))))))

(defun %apply-tile (band amount directive-parent directive-child)
  (declare (type isl::schedule-node band) (type Directive directive-parent directive-child))
  (assert (eql (schedule-node-get-type band) :schedule-node-band))
  (let* ((tiled-schedule (schedule-node-insert-mark band (directive->id directive-parent)))
         (tiled-schedule (schedule-node-get-child tiled-schedule 0))
         (tiled-schedule (schedule-node-band-tile tiled-schedule (tiling-sizes band :size-default amount)))
         (tiled-schedule (schedule-node-insert-mark (schedule-node-get-child tiled-schedule 0) (directive->id directive-child))))
    ;; Do not reschedule mark/tiled band
    (if (directive-visible directive-child)
        tiled-schedule ;; applying further optimizations ...
        (schedule-node-get-child tiled-schedule 0))))

(defun apply-tile (band tile-size)
  (declare (type isl::schedule-node band) (type fixnum tile-size))
  (%apply-tile band tile-size (directive "TILE_OUTER" tile-size t) (directive "TILE_INNER" tile-size t)))

(defun apply-unroll (band unroll-by)
  (declare (type isl::schedule-node band) (type fixnum unroll-by))
  (%apply-tile band unroll-by (directive "UNROLL_OUTER" unroll-by t) (directive "UNROLL_INNER" unroll-by nil)))

(defun apply-pack (band pack-by)
  (declare (type isl::schedule-node band) (type fixnum pack-by))
  (%apply-tile band pack-by (directive "PACKED_OUTER" pack-by t) (directive "PACKED_INNER" pack-by nil)))
;; ~~ Legality Transformations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun apply-interchange (poly band1 idx)
  "Returns a new schedule of band1 with band1 and idx'th band node interchanged. Returns nil if the operation breaks the dependencies."
  (declare (type polyhedral-ir poly) (type isl::schedule-node band1) (type fixnum idx))
  (assert (eql (schedule-node-get-type band1) :schedule-node-band))
  (let* ((mupa (schedule-node-band-get-partial-schedule band1))
         (node (schedule-node-delete band1))
         (n-child (isl::%isl-schedule-node-n-children (isl::schedule-node-handle node)))
         (_ (when (= 0 n-child) (return-from apply-interchange nil))) ;; Nothing to interchange
         (node (schedule-node-get-band-from-relative-idx node idx))
         (__ (assert node () "IDX=~a does not exists in the schedule:~%~A" idx band1))
         (node (schedule-node-insert-partial-schedule node mupa)))
    (declare (ignore _ __))
    (when (check-legality (schedule-node-get-schedule node) (poly-dependencies poly))
      ;(schedule-node-insert-mark node (directive->id (directive "INTERCHANGE" idx t)))
      node)))
;; ~~ Insert Marks ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun apply-parallel (schedule-node)
  (schedule-node-insert-mark schedule-node (directive->id (directive "PARALLEL" 0 T))))

(defun apply-global (schedule-node n-local)
  (schedule-node-insert-mark schedule-node (directive->id (directive "GLOBAL" n-local T))))
