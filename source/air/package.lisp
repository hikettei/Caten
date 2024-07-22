(in-package :cl-user)

(defpackage :caten/air
  (:use :cl :alexandria :trivia)
  (:export #:Node #:node-class #:node-p #:node->id #:id->node
   #:node-id #:node-type #:node-writes
   #:node-reads #:node-attrs)
  (:export #:make-node)
  (:export #:lower #:mutate)
  (:export #:Graph #:make-graph #:graph-p #:graph-tops #:Graph-nodes #:id->value #:id->users #:remnode #:verify-graph)
  (:export #:getattrs #:getattr)
  (:export #:defsimplifier))
