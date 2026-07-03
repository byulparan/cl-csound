
(ql:quickload :cl-csound)

(in-package :csound-user)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; running and quit csound
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(run-csound :software-bufsize 2048)

;; If you want quit to csound
;; (quit-csound)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setup global environment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; If you want to declare a variable in the global scope, use the `global' function.


;; `ftgen' creates an i-rate variable.
(defparameter *sine-table* (global (ftgen 1 0 8192 10 1)))
(defparameter *saw-table* (global (ftgen 2 0 512 7 -1 512 1)))


;; `init' create a-rate variable defaulty.
(defparameter *reverb-bus* (global (init 0)))

;; If you want to specify the rate explicitly, use the `ar' or `kr' functions.
(defparameter *frequency* (global (kr (init (midicps 48)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define instrument
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(definstr simple-osc (ifreq ipan ifn)  ;; arguments should be have `i-' prefix
  ;; Local variable names within a `let binding` must have an appropriate prefix to specify the rate (i, k, a, etc.).
  (let* ((kenv (line .1 :p3 .0))  ;; access `p3'  
	 (asig (oscil kenv ifreq ifn))
	 ;; Opcodes with multiple outputs can be used as follows.
	 ((ar al) (pan2 asig ipan)))
    (out ar al)))


;; Call an instrument
(instr 'simple-osc (now) 1 440 .5 *sine-table*)
(instr 'simple-osc (now) 1 440 .5 *saw-table*)



;; If the rate cannot be inferred, an error occurs. In this case, you must specify the rate explicitly.
(definstr simple-osc2 (ifreq)
  (out (ar (oscil (kr (line .1 :p3 .0)) ifreq))
       (ar (oscil (kr (line .1 :p3 .0)) (+ 1 ifreq)))))

;; Call an instrument
(instr 'simple-osc2 (now) 1 440)




;; You can use it-then macro like cond
(definstr condition-osc (ifreq)
  (let* ((ifn (init 0)))
    (if-then ((< ifreq 400) (set! ifn *sine-table*))
	     (:else (set! ifn *saw-table*)))
    (let* ((asig (oscil .1 ifreq ifn)))
      (out asig asig))))


(instr 'condition-osc (now) 1 320)  ;; listen sine wave
(instr 'condition-osc (now) 1 420)  ;; listen saw wave



;; If you want to see how your Lisp code is converted to Csound code, set `*debug-mode*' to T.
;; If `*debug-mode*' is set to T, you can only view the code; it will not run.


;; Each defined instrument is assigned a random number. If you want to specify the number yourself, use the following method.
(definstr (reverb 99) ()
  (let* (((al ar) (reverbsc *reverb-bus* *reverb-bus* .95 12000)))
    (clear *reverb-bus*)
    (out al ar)))

;; Call an instrument, infinitely
(instr 'reverb (now) -1)


(definstr simple-osc3 (ifreq ipan ifn ireverb)
  (let* ((kenv (line .1 :p3 .0))  ;; access `p3'  
	 (asig (oscil kenv ifreq ifn))
	 ((ar al) (pan2 asig ipan)))
    (vincr *reverb-bus* (* asig ireverb))
    (out ar al)))


(instr 'simple-osc3 (now) .1 440 .5 *sine-table* .2)






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; play instrument in realtime
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This uses a scheduler I wrote. This scheduler does not rely heavily on csound.
;; If you can implement a scheduler that is more useful and better suited to your needs, use that instead.


;; set bpm
(bpm 72)

(defun play-simple-osc (beat dur n)
  (when (> n 0)
    (instr 'simple-osc beat (* dur .5) (rrand 440 1320) (rrand -1.0 1.0) (rrand (list *sine-table* *saw-table*)))
    (let* ((next-beat (+ beat dur)))
      (clock-add next-beat 'play-simple-osc next-beat dur (- n 1)))))


;; Start at the next quantized beat point
(play-simple-osc (clock-quant 1) .5 8)




;; You can use scheudle macro.
(schedule test-tone (1)
  (with-lambda (1/4)
    (instr 'simple-osc3 beat .4 (rrand 440 2000) (rrand -1.0 1.0) *saw-table* (rrand .2))))


;; If you want to stop the currently running scheduler, call `stop'.
(stop)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; export csd
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; If you wish, you can create a csd file that contains the current state of Csound.
;; Since this file may contain several temporary variables that were created during development but are no longer in use,
;; We recommend restarting Lisp to initialize the environment, executing only the necessary code, and then outputting the results.


(with-render ("~/Desktop/simple.csd" :output "simple.wav" :pad 120)  ;; `pad' is end beat of score. Be sure to specify this to prevent infinite loops.
  (instr 'reverb 0 -1)
  (schedule test-tone (0)
    (with-lambda (1/4)
      (instr 'simple-osc3 beat .4 (rrand 440 2000) (rrand -1.0 1.0) *saw-table* (rrand .2)))))



