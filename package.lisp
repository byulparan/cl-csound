;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2025.11.05 byulparan@gmail.com
;; 
;; 

(defpackage #:csnd
  (:use #:cl)
  (:export #:*debug-mode*
	   
	   #:fltfy
	   #:run-csound
	   #:quit-csound
	   #:with-render

	   #:now
	   #:clock-add
	   #:quant
	   
	   #:dispatch-queue
	   #:dispatch-queue-async
	   
	   #:definst
	   #:inst
	   
	   #:global
	   #:set!
	   #:assign
	   #:array
	   #:aget
	   #:aset
	   #:label
	   #:if-then
	   #:if-igoto
	   #:if-kgoto
	   #:if-goto

	   #:make-control-channel
	   #:channel-value

	   #:?
	   #:load-sample
	   #:proxy
	   #:stop
	   #:with-timout))
