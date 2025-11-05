;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2025.11.05 byulparan@gmail.com
;; 
;; 

(defpackage #:csound
  (:use #:cl)
  (:shadow #:array #:space)
  (:export #:*debug-mode*

	   #:+note-amplitude-messages+
	   #:+samples-out-of-range-message+
	   #:+warning-messages+
	   #:+benchmark-information+

	   #:*run-hooks*
	   #:*stop-hooks*
	   
	   #:run-csound
	   #:quit-csound
	   #:with-render
	   #:get-csound

	   #:now
	   #:clock-add
	   #:clock-quant
	   #:quant
	   #:bpm

	   #:fltfy
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

	   #:midicps
	   #:rrand
	   #:exp-rand
	   #:sinr
	   #:cosr
	   #:once
	   #:nth-beat
	   #:beat-count
	   #:latch
	   #:coin
	   #:schedule
	   #:with-lambda
	   
	   #:?
	   #:load-sample
	   #:proxy
	   #:stop
	   #:with-timout))


(defpackage #:csound-user
  (:use #:cl #:csound)
  (:shadow cl:array cl:space))


