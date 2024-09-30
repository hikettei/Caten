(in-package :cl-user)

(defpackage :caten/air
  (:use :cl :alexandria :trivia :caten/common.documentation)
  (:export #:Node #:node-class #:node-p #:node->id #:id->node
   #:node-id #:node-type #:node-writes
   #:node-reads #:node-attr #:print-node #:get-output-to)
  (:export #:make-node #:copy-node)
  (:export #:lower #:mutate)
  (:export #:Graph #:FastGraph #:make-graph #:copy-graph #:graph-p #:graph-seen #:graph-outputs #:Graph-nodes #:id->value #:id->users #:remnode #:verify-graph
	   #:insert-nodes #:->graph #:->fast-graph #:%graph-nodes-table #:graph-weakly-connected-p)
  (:export #:getattrs #:getattr #:remattr)
  (:export #:defsimplifier)
  (:export #:Attribute #:defnode #:debug/render-defined-nodes #:debug/attrs-by-module #:node-build-documentation-by-class #:verify-args #:dump-into-list)
  (:export #:->dot))
