(in-package :caten/lang)

(defun read-delimited-stream (stream opener delimiter)
  (let ((chars '()) (depth 0))
    (loop for c = (read-char stream nil nil) for nth upfrom 0
          while c do
            (when (= nth 0) (assert (char= c opener)))
            (push c chars)
            (when (char= c opener) (incf depth))
            (when (char= c delimiter)
              (decf depth)
              (when (zerop depth) (return))))
    (assert (char= delimiter (car chars)) () "The brackets are not closed.")
    (coerce (nreverse chars) 'string)))

(defun consume-whitespace-until (stream expect &aux (whitespace #.(aref " " 0)))
  (loop named bracket
        for nxt = (peek-char nil stream nil nil) while nxt do
          (case nxt
            ((#\Space #\Newline))
            (otherwise
             (when (char= nxt expect) (return-from bracket))
             (unless (char= whitespace nxt)
               (error "@caten macro parse error:~%  Unexpected character '~a' while skipping whitespace. Only space or newline may appear before the code block." nxt))))
          (read-char stream)))

(defun read-caten-directive (stream char &aux (definition "CATEN."))
  "Parse a reader macro of the following form:
```lisp
@caten.<feat_name>(params){
PROGRAM
}
```
Returns three values: OPERATION, PARAMS, and CODE. If not matched, returns the original s-expression unchanged."
  (declare (ignore char))
  (let* ((operator (read stream nil nil t)) (opname (string-upcase (symbol-name operator)))
         (caten-p (and (symbolp operator) (>= (length opname) (length definition)) (equalp (subseq opname 0 (length definition)) definition))))
    ;; Verify that the operator symbol begins with 'caten.', indicating a caten directive
    (unless caten-p (return-from read-caten-directive operator))
    (let* ((dot-pos (position #\. opname))
           (feat-name (subseq opname (1+ dot-pos)))
           (next1 (progn (consume-whitespace-until stream #\() (peek-char nil stream nil nil))))
      (unless (char= next1 #\()
        (error "@caten macro parse error: After reading operator '~a', expected '(' but found '~a'. Please follow the syntax: @caten.<feat_namen>(params){...}" operator next1))
      (let ((params (read-delimited-stream stream #\( #\))))
        (consume-whitespace-until stream #\{)
        (unless (char= (peek-char nil stream nil nil) #\{) ;; Ensure the next character is the opening brace '{' that begins the code block
          (error "@caten macro parse error: Expected '{' after reading parameters, but none was found.
Please ensure the directive follows the syntax: @caten.<feat_name>(params){...}
"))
        (let* ((code (read-delimited-stream stream #\{ #\}))
               (code (subseq code 1 (1- (length code)))) ;; Trim { } from the code
               (params (handler-case (read-from-string params)
                         (error (c) (error "@caten macro parse error: Cannot parse params \"~a\" due to~%:~a" params c)))))
          ;; Return three values: operation, parameters, and code block content
          (apply (get-feature-expander-macro (intern feat-name "KEYWORD")) (append (list (make-directive-context :code code :params params)) params)))))))

(named-readtables:defreadtable caten
  (:merge :standard)
  (:macro-char #\@ #'read-caten-directive t)
  (:dispatch-macro-char #\# #\{ #'with-c-syntax.core::read-in-c-syntax))

(defmacro in-caten-toplevel ()
  `(named-readtables:in-readtable caten))


;; We have a C -> Lisp Compiler
;; i.e.: We can translate Lisp -> Blueprint Compiler
;; And we have a both of C, Lisp, Frontend
;; Readmeに乗っける
;; Export to JSON (Compiled Code)

;; lang.lisp: APIs for constructing blueprint
;; TODO:
;; https://github.com/ruricolist/infix-math
;; TileLangLike
;; FlashAttention Benchmark
;; Reduction Resolver
;; Create caten/lang
  ;; Not in References
  ;;
