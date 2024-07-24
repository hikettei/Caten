(in-package :caten)

(defpattern sym (to-what) `(and (type symbol) (satisfies (lambda (x) (equalp (symbol-name x) ,to-what)))))
