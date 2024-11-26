(defpackage :caten/common.documentation
  (:use :cl)
  (:export
   #:build-docs
   #:define-page)
  ;; Components
  (:export
   #:title
   #:subtitle
   #:section
   #:subsection
   #:body
   #:keybind
   #:highlight
   #:math
   #:syntax-highlight
   #:example-repl
   #:example-code)
  (:export
   #:doc/package
   #:doc/function
   #:doc/class
   #:doc/struct
   #:doc/macro
   #:doc/generic
   #:doc/variable))
(in-package :caten/common.documentation)
;; TODO: Create a fork of lantana, which supports English
(defparameter *pages* (make-hash-table :test 'equal))
(defparameter *editing* nil)

(defun build-docs ()
  (maphash
   #'(lambda (pathname builder)
       (caten/common.logger:print-info "Building: ~a" pathname)
       (with-open-file (stream (format nil "docs/~a" pathname) :direction :output :if-exists :supersede :if-does-not-exist :create)
         (format stream "~a" (let ((*package* (find-package :caten-user))) (funcall (cdr builder))))))
   *pages*)
  (caten/common.logger:print-info "Done"))

(defmacro define-page ((section-name filepath) &body body)
  "Section-Name | filepath. Pages are sorted by level"
  (declare (type string section-name filepath))
  `(setf (gethash ,filepath *pages*)
         (cons
          ,section-name
          #'(lambda ()
              (with-output-to-string (*editing*)
                ,@body)))))

;; ~~ Sections ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun title (something)
  "# Title"
  (assert *editing*)
  (format *editing* "# ~a~%~%" something))

(defun subtitle (something)
  "## Subtitle"
  (assert *editing*)
  (format *editing* "## ~a~%~%" something))

(defun section (something)
  "### Section"
  (assert *editing*)
  (format *editing* "### ~a~%~%" something))

(defun subsection (something)
  "#### Subsection"
  (assert *editing*)
  (format *editing* "#### ~a~%~%" something))

(defun body (&rest things)
  (assert *editing*)
  (dolist (thing things)
    (princ thing *editing*)
    (princ " " *editing*))
  (format *editing* "~%~%"))

(defun syntax-highlight (lang something)
  (assert *editing*)
  (format *editing* "```~a~%~a~%```~%~%" lang something))

(defun example-repl (code &key (title "lisp"))
  "Running the code under caten-user"
  (assert *editing*)
  (format *editing* "~%```lisp title=\"~a\"~%CATEN-USER> ~a~%```~%~%" title code)
  (let ((result (eval (read-from-string code))))
    (format *editing* "<details>
<summary>Result</summary>
```title=\"Result\"
~a
```
</details>~%~%" result)))

(defun example-code (code &key (title "Example"))
  (assert *editing*)
  (format *editing* "~%```lisp title=\"~a\"~%~a~%```~%~%" title code)
  (let ((result (eval (read-from-string code))))
    (format *editing* "<details>
<summary>Result</summary>
```title=\"Result\"
~a
```
</details>~%~%" result)))
;; ~~ Aliases ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun keybind (something)
  "++Something++ (e.g.: (keybind \"C-x C-f\""
  (format nil "++~a++" something))

(defun highlight (something)
  "Highlight something"
  (format nil "==~a==" something))

(defun math (something)
  "Katex"
  (format nil "$$~a$$" something))

(defun alert (tag title thing)
  "Docs are here https://lantana.wsoft.ws/cheatsheet/markdown/"
  (assert *editing*)
  (format *editing* "!!! ~(~a~) \"~a\"~%" tag
          (or
           title
           (ecase tag
             (:note "Note")
             (:Abstract "Abstract")
             (:info "Info")
             (:tip "Tip")
             (:Success "Success")
             (:Question "Question")
             (:Warning "Warning")
             (:Failure "Failure")
             (:Danger "Danger")
             (:Bug "Bug")
             (:Example "Example")
             (:Quote "Quote"))))
  (format *editing* "  ~a~%~%" thing))

(defun doc/function (name func)
  (assert *editing*)
  (format *editing* "### [function] ~a~%~%~a~%~%" name (documentation func 'function)))

(defun doc/class (name class)
  (assert *editing*)
  (format *editing* "### [class] ~a~%~%~a~%~%" name (documentation (find-class class) 't)))

(defun doc/struct (name struct)
  (assert *editing*)
  (format *editing* "### [struct] ~a~%~%~a~%~%" name (documentation (find-class struct) 't)))

(defun doc/macro (name macro)
  (assert *editing*)
  (format *editing* "### [macro] ~a~%~%~a~%~%" name (documentation (macro-function macro) 't)))

(defun doc/generic (name generic)
  (assert *editing*)
  (format *editing* "### [generic] ~a~%~%~a~%~%" name (documentation generic 't)))

(defun doc/variable (name variable)
  (assert *editing*)
  (format *editing* "### [variable] ~a~%~%~a~%~%" name (documentation variable 'variable)))

(defun doc/package (name)
  (assert *editing*)
  (format *editing* "~a" (documentation (find-package name) t)))
