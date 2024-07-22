(in-package :cl-user)

(defpackage :caten/air
  (:use :cl :alexandria :trivia)
  (:export #:Node #:node-class
   #:node-id #:node-type #:node-writes
   #:node-reads #:node-attrs)
  (:export #:make-node)
  (:export #:lower #:mutate)
  (:export #:Graph #:Graph-nodes #:id->value #:id->users #:remnode #:verify-graph)
  (:export #:defsimplifier))
