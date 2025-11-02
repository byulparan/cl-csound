
(defpackage #:csnd
  (:use #:cl)
  (:export #:*debug-mode*
	   #:now
	   #:callback
	   #:quant
	   #:fltfy
	   #:definst
	   #:csnd
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
	   #:?
	   #:load-sample
	   #:run-csound
	   #:quit-csound
	   #:with-render
	   #:make-control-channel
	   #:channel-value

	   #:proxy
	   #:stop
	   #:with-timout))
