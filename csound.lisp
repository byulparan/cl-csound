;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2025.11.05 byulparan@gmail.com
;; 
;; 

(in-package #:csound)

(defvar *streams* t
  "Lisp's Opcode objects translate to Csound Orchestra expression via this stream in definst.")

(defvar *opcodes* nil
  "Temporary place, that created opcode object are stored.")

(defvar *debug-mode* nil
  "if *debug-mode* is T, definst is not compile by CsoundCompileOrc().
 just translate to csound orchestra expression. and print.
 It useful to debug your definst syntax.")

(defvar *run-csound-hooks* nil
  "After booting the csound engine by #'make-csound,
 the variable that stores a function to be called.
 Default, define instrument to other instrument terminate.")

(defvar *stop-hooks* nil)


(defvar *csound-instr-count* 100
  "cl-csound not support Named(string) Instrument. So every time you define instrument,
 this count will increase by one. and this number is insnum of defined instrument.
 This count start at 100.")

(defvar *csound-all-instrs* nil
  "All instr number of defined instruments.")

(defvar *csound-instr-table* nil
  "Same instrument's name(lisp's symbol), the same insnum is given.
 This HashTable is pair of lisp's symbol and insnum.")

(defvar *csound-global-variables* nil
  "csound's global commands(zakinit, ftgen, turnoff2, etc..),
 is stored this place. That use rendering your instruments and score functions.")

(defvar *csound-orchestra* nil
  "Translated orchestra expression is sotred this place. That use rendering your instruments and score functions.")

(defvar *pushed-orchestra-p* t
  "If this special variable is Nil, not store your orchestra expression to *csound-orchestra*.")

(defvar *render-stream* nil
  "cl-csound support realtime, rendering both. in rendering mode, you will create csd file.
 This special variable is stored that csd file's stream.")

(defvar *channels*)



;;;;;;;;;;;;;;;;;;
;;  Run Csound  ;;
;;;;;;;;;;;;;;;;;;

(let ((csound nil)
      (csound-perform-thread nil)
      (csound-scheduler nil)
      (csound-running-p nil))
  (defun run-csound (&key (sr 48000)
		       (ksmps 64)
		       (nchnls 2)
		       (software-buffer-size 128)
		       (hardware-buffer-size 256)
		       (dac "dac")
		       (rtaudio #+darwin "AuHal")
		       rtmidi
		       (midi-device 0)
		       (message-level +warning-messages+))
    "Bootup csound engine and initialize to many global variables.
 cl-csound only support one csound instance. 0dbfs set 1."
    (when csound (error "csound already running"))
    (setf *csound-instr-count* 100
	  *csound-all-instrs* nil
	  *csound-instr-table* (make-hash-table)
	  *csound-orchestra* (make-hash-table)
	  *csound-global-variables* (make-hash-table)
	  *channels* nil)
    (trivial-main-thread:call-in-main-thread 
     (lambda ()
       (float-features:with-float-traps-masked (:invalid :overflow :divide-by-zero)
	 (csound-initialize 1)
	 (setf csound-running-p t)
	 (setf csound (csound-create (cffi-sys:null-pointer)))
	 (csound-set-message-level csound message-level)
	 (csound-compile-orc csound (format nil "sr = ~d~%ksmps = ~d~%nchnls = ~d~%0dbfs = 1~%" sr ksmps nchnls))
	 (csound-set-option csound "--realtime")
	 (csound-set-option csound (format nil "-o~a" dac))
	 (csound-set-option csound (format nil "-b~d" software-buffer-size))
	 (csound-set-option csound (format nil "-B~d" hardware-buffer-size))
	 (when rtaudio
	   (csound-set-option csound (format nil "-+rtaudio=~a" rtaudio)))
	 (when (and rtmidi midi-device)
	   (csound-set-option csound (format nil "-+rtmidi=~a" rtmidi))
	   (csound-set-option csound (format nil "-M~d" midi-device)))
	 (csound-start csound)
	 (setf csound-perform-thread
	   (bt:make-thread
	    (lambda ()
	      (loop while (and (zerop (csound-perform-ksmps csound))
			       csound-running-p)))
	    :name "Csound_Perform_Thread"))
	 (setf csound-scheduler (make-instance 'tempo-clock 
				  :timestamp #'(lambda () (csound-get-score-time csound))))
	 (tempo-clock-run csound-scheduler)
	 (dolist (hook *run-csound-hooks*)
	   (funcall hook))))
     :blocking t)
    nil)
  (defun quit-csound ()
    "shutdown to csound engine."
    (unless csound (error "csound not playing"))
    (tempo-clock-stop csound-scheduler)
    (csound-stop csound)
    (setf csound-running-p nil)
    (bt:join-thread csound-perform-thread)
    (csound-destroy csound)
    (setf csound nil
	  csound-perform-thread nil
	  csound-scheduler nil))
  (defun get-csound ()
    "get pointer of csound instance."
    csound)
  (defun get-csound-scheduler ()
    csound-scheduler))



;;cleanup
(labels ((cleanup-csound ()
	   (when (get-csound)
	     (quit-csound))))
  #+ccl (push #'cleanup-csound ccl::*lisp-cleanup-functions*)
  #+sbcl (push #'cleanup-csound sb-ext:*exit-hooks*))




;;;;;;;;;;;;;;;;;;;;;
;;  scheduler API  ;;
;;;;;;;;;;;;;;;;;;;;;

(defun now ()
  (tempo-clock-beats (get-csound-scheduler)))


(defun clock-add (beat function &rest args)
  (tempo-clock-add (get-csound-scheduler) beat (lambda () (apply function args))))


(defun clock-quant (quant)
  (tempo-clock-quant (get-csound-scheduler) quant))


(defun stop (&rest instrs)
  "Stop function use to terminate instruments. If you just call (stop), all scheduling events are clear, and
 instruments(insnum >= 100) terminate immediately. If you call (stop 60) or (stop 'foo 'bar), specified instruments release."
  (when (get-csound)
    (if instrs (loop for synth in instrs do (csound-kill-instance (get-csound) (fltfy synth) (cffi:null-pointer) 0 1))
      (progn
	(tempo-clock-clear (get-csound-scheduler))
	(dolist (synth (remove-if-not (lambda (instr) (>= instr 100)) *csound-all-instrs*))
	  (csound-kill-instance (get-csound) (fltfy synth) (cffi:null-pointer) 0 0))
	(dolist (hook *stop-hooks*)
	  (funcall hook))))))

 


;;;;;;;;;;;;;;;;;;;;;
;;  scheduler API  ;;
;;;;;;;;;;;;;;;;;;;;;


(defgeneric fltfy (object)
  (:documentation "CsoundAPI use MYFLT type. This function convert from Lisp objects to MYFLT.
 That lisp objects are Symbol(may be instrument's name), Number, GenRoutine."))

(defmethod fltfy ((object symbol))
  (let ((insnum (gethash object *csound-instr-table*)))
    (if insnum (fltfy insnum)
	(error "can't fltfy this symbol ~a" object))))

(defmethod fltfy ((object number))
  (coerce object *myflt*))


;;;;;;;;;;;;;;;;;;;;;;;;;
;;  compile orchestra  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro csnd-binding (let letform &body body)
  "binding for csound's local variables. Don't use it directly. It use internal of slet,slet*."
  (let* ((names nil))
    `(,let ,(mapcan (lambda (pair)
		      (destructuring-bind (name form) pair
			(if (atom name) (list `(,name (with-sigrate-by-name (,name)
							(setf (var ,form) ,(string-downcase name)))))
			      (let ((varnames (intern (string-upcase (format nil "~{~a~^_~}" name)))))
				(push varnames names)
				(append
				 `((,varnames
				    (with-sigrate-by-name (,varnames)
				      (setf (var ,form) ,(cons 'list (mapcar #'string-downcase name))))))
				 (loop for n in name
				       do (push n names)
				       collect (list n (alexandria:make-keyword n))))))))
	     letform)
       (declare (ignorable ,@names))
       ,@body)))

(defmacro slet (letform &body body)
  `(csnd-binding let ,letform ,@body))

(defmacro slet* (letform &body body)
  `(csnd-binding let* ,letform ,@body))

;;;
;;; 
(defmacro parse-params (params &body body)
  "default, Csound instruments have 3 parameters. p1: insnum, p2: start-time, p3: duration.
 This function make that duration parameter then binding to local variables idur."
  (let ((count 3))
    `(let ,(mapcar (lambda (name)
		      (list name
			    `(make-instance 'param :name ,(format nil "p~d" (incf count))
						   :var ,(string-downcase (format nil "~a" name)))))
		    params)
       (declare (ignorable ,@params))
       ,@body)))

(defun convert-code-table (atom)
  (case atom
    (let 'slet)
    (let* 'slet*)
    (pi '*pi~*)
    (+ 'add)
    (- 'sub)
    (* 'mul)
    (/ 'div)
    (> 'gt)
    (>= 'ge)
    (< 'lt)
    (<= 'le)
    (= '=~)
    (/= '!=)
    (not '~)
    (cos 'cos~)
    (cosh 'cosh~)
    (abs 'abs~)
    (exp 'exp~)
    (floor 'floor~)
    (log 'log~)
    (max 'max~)
    (min 'min~)
    (pop 'pop~)
    (print 'print~)
    (push 'push~)
    (random 'random)
    (round 'round~)
    (signum 'signum~)
    (sin 'sin~)
    (sinh 'sinh~)
    (sqrt 'sqrt~)
    (tan 'tah~)
    (tanh 'tanh~)
    (expt '^)
    (mod '%)
    (t atom)))

(defun convert-code (form)
  "before, translate from lisp code to csound orchestra, serveral symbols in lisp code convert to other symbols.
 lisp and csound have conflict symbol names(+,-,*,/,sqrt,mod... core/math function). in definst context, your lisp's
 core function convert to csound's core function."
  (cond ((null form) nil)
	((atom form) (convert-code-table form))
	((eql (car form) 'lisp) (setf (car form) 'progn) form)
	(t (cons (convert-code (car form))
		 (convert-code (cdr form))))))





(defmacro definst (name params &body body)
  "defined instruments. in this context, many core lisp functions are convert to other functions.
 examples)  + -> +~ , * -> *~ , let -> slet, let* -> slet*......
 If *debug-mode* is Nil, definst code is translate to csound orchestra expression, then compile by CsoundCompileOrc()."
  (let* ((body (replace-body-on-cound-readtable body)))
    (alexandria:with-gensyms (form insnum ins result)
      `(let* ((,insnum (if (get-csound) ,(if (atom name) `(alexandria:if-let ((,ins (gethash ',name *csound-instr-table*))) ,ins
							    (setf (gethash ',name *csound-instr-table*) (incf *csound-instr-count*)))
					     `(setf (gethash ',(car name) *csound-instr-table*) ,(second name)))
			   100)))
	 (let* ((,form
		  (let* ((*streams* (make-string-output-stream))
			 (*opcodes* nil))
		    (format *streams* "~&instr ~d" ,insnum)
		    (parse-params ,params
		      ,@(convert-code body))
		    (dolist (opcode (nreverse *opcodes*))
		      (build opcode))
		    (format *streams* "~&endin")
		    (get-output-stream-string *streams*))))
	   (if (and (get-csound) (not *debug-mode*))
	       (let* ((,result nil))
		 (when (not (zerop (csound-compile-orc (get-csound) ,form)))
		   (error "Error Defintion Instrument \"~a\"" ',name))
		 (pushnew ,insnum *csound-all-instrs*)
		 (when *pushed-orchestra-p*
		   (setf (gethash ',(if (atom name) name (car name)) *csound-orchestra*) ,form)))
	       ,form))))))




(defun inst (name beat dur &rest args)
  (let* ((insnum (gethash name *csound-instr-table*))
	 (len (length args)))
    (cffi:with-foreign-objects ((pfield 'myflt (+ len 3)))
      (setf (cffi:mem-aref pfield 'myflt 0) (coerce insnum *myflt*)
	    (cffi:mem-aref pfield 'myflt 1) 0.0d0
	    (cffi:mem-aref pfield 'myflt 2) (coerce dur *myflt*))
      (dotimes (i len)
	(setf (cffi:mem-aref pfield 'myflt (+ i 3)) (coerce (nth i args) *myflt*)))
      (csound-score-event-absolute (get-csound) (char-code #\i) pfield (+ len 3) (beats-to-secs (get-csound-scheduler) beat)))))




;; (defmacro with-render ((output-filename &key (sr 44100) (ksmps 10) (chnls 2) pad keep-csd-file-p) &body body)
;;   "Make csound csd file from your lisp code. then rendering that file."
;;   (alexandria:with-gensyms (tmp-csd-file)
;;     `(progn
;;        (unless (or ,body ,pad) (error "nothing csound score event!"))
;;        (let ((cb::*scheduling-mode* :step)
;; 	     (,tmp-csd-file (make-pathname :directory (pathname-directory ,output-filename)
;; 					   :name (pathname-name ,output-filename)
;; 					   :type "csd")))
;; 	 (unwind-protect (progn
;; 			   (with-open-file (*render-stream* ,tmp-csd-file
;; 							    :direction :output
;; 							    :if-exists :supersede)
;; 			     (format *render-stream* "<CsoundSynthesizer>~%")
;; 			     (format *render-stream* "<CsInstruments>~%~%")
;; 			     (format *render-stream* "sr = ~d~%" ,sr)
;; 			     (format *render-stream* "ksmps = ~d~%" ,ksmps)
;; 			     (format *render-stream* "nchnls = ~d~%" ,chnls)
;; 			     (format *render-stream* "0dbfs = 1~%~%")
;; 			     (dolist (var (alexandria:hash-table-values *csound-global-variables*))
;; 			       (format *render-stream* "~a~%" var))
;; 			     (terpri *render-stream*)
;; 			     (dolist (orc (alexandria:hash-table-values *csound-orchestra*))
;; 			       (format *render-stream* "~a~%~%" orc))
;; 			     (terpri *render-stream*)
;; 			     (format *render-stream* "</CsInstruments>~%")
;; 			     (format *render-stream* "<CsScore>~%")
;; 			     ,@body
;; 			     (terpri *render-stream*)
;; 			     (when ,pad
;; 			       (format *render-stream* "e ~f" ,pad))
;; 			     (terpri *render-stream*)
;; 			     (format *render-stream* "</CsScore>~%")
;; 			     (format *render-stream* "</CsoundSynthesizer>~%"))
;; 			   (uiop/run-program:run-program (format nil "csound -o ~a ~a" ,output-filename ,tmp-csd-file)
;; 							 :output t :error-output t))
;; 	   (unless ,keep-csd-file-p
;; 	     (delete-file ,tmp-csd-file)))))))

