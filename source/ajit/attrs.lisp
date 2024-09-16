(in-package :caten/ajit)

(defnode (:JIT :JIT_KERNEL) ()
	 "The node :JIT_KERNEL is an instruction that calls a jit-compiled kernel from the VM."
	 :slots ((fname :type string)
		 (jit-info :type JIT-Info)))

(defnode (:Render :FOR) () "
RenderGraph:
```
for(int idx=upfrom, below, by)
```
"
	 :slots ((idx)
		 (upfrom)
		 (below)
		 (by)))

(defnode (:Render :ENDFOR) () "
RenderGraph:
```
} / idx
```"
	 :slots ((idx)))

(defnode (:Render :FUNCALL) () "
RenderGraph:
```
FUNCALL(...)
```"
	 :slots ((name) (args) (idx) (unroll-offsets)
		 (_packed) (_unrolled) (_metadata) (scope :type (member :global :Local))
		 (coincident) (permutable)))

(defnode (:Render :IF) () "
RenderGraph:
```
IF (condition) {
```"
	 :slots ((condition)))

(defnode (:Render :ELSE) () "
RenderGraph:
```
} ELSE {
```")
(defnode (:Render :ENDIF) () "
RenderGraph:
```
} // endif
```")

;; Temporary Nodes
(defnode (:IR :IR/FOR) () "" :slots ((_scalar_p)))
(defnode (:IR :IR/ENDFOR) () "")
(defnode (:TIME :GRAPH) () "" :slots ((id)))
