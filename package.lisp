;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2025.11.05 byulparan@gmail.com
;; 
;; 

(defpackage #:csound
  (:use #:cl)
  (:shadow #:array #:space)
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


(defpackage #:csound-user
  (:use #:cl #:csound)
  (:shadow cl:array cl:space))


