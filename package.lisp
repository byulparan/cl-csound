;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2025.11.05 byulparan@gmail.com
;; 
;; 

(eval-when (:compile-toplevel :load-toplevel :execute)
  (named-readtables:defreadtable :csound
    (:merge
     :common-lisp)))


(defpackage #:csound
  (:use #:cl)
  (:shadow #:array #:space)
  (:export #:*debug-mode*

 	   #:+csoundinit-no-signal-handler+
	   #:+csoundinit-no-atexit+  
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
	   #:get-csound-performance-thread

	   #:now
	   #:clock-add
	   #:clock-quant
	   #:quant
	   #:bpm

	   #:fltfy
	   #:definstr
	   #:instr
	   #:ir
	   #:kr
	   #:ar
	   
	   #:stop
	   #:num-instance
	   
	   #:global
	   #:set!
	   #:assign
	   #:fillarray
	   #:fillarray*
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
	   #:with-timout))


(defpackage #:csound-user
  (:use #:cl #:csound)
  (:shadow cl:array cl:space))


(in-package :csound-user)
(named-readtables:in-readtable :csound)


(let ((rpar (get-macro-character #\))))
  (set-macro-character #\] rpar)
  (set-macro-character #\[ (lambda (stream char1)
			     (declare (ignore char1))
			     (apply (lambda (&rest rest) (cons 'list rest))
				    (read-delimited-list #\] stream t)))))





