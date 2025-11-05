;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2025.11.06 byulparan@gmail.com
;; 
;; 

(in-package :csound)

(defun midicps (note)
  (flet ((inner (n)
	   (* 440 (expt 2 (/ (- n 69) 12.0)))))
    (let* ((freqs (mapcar #'inner (alexandria:ensure-list note)) ))
      (if (= (length freqs) 1) (car freqs) freqs))))


(defun rrand (n &optional p)
  (cond (p (let* ((min (min n p))
		  (max (max n p)))
	     (+ min (random (- max (- min (if (every #'integerp (list n p)) 1 0)))))))
	((numberp n) (rrand 0 n))
	((listp n) (alexandria:random-elt n))))


(defun exp-rand (lo hi &optional (alpha 1.0))
  "Exponential random between lo and hi, slope controlled by alpha.
   alpha=1 → normal ExpRand
   alpha<1 → bias towards hi
   alpha>1 → bias towards lo"
  (unless (> hi lo)
    (error "hi must be greater than lo"))
  (when (<= lo 0)
    (error "lo must be greater than 0 for ExpRand"))
  (when (< alpha 0)
    (error "alpha must not be less than 0 for ExpRand"))
  (let* ((u (random 1.0))
         (biased-u (expt u alpha)))
    (* lo (expt (/ hi lo) biased-u))))


(defmacro sinr (lo hi rate &optional (offset 0.0))
  `(lin-lin (sin (* pi 2 (+ ,offset ,(alexandria:symbolicate "BEAT")) ,rate)) -1.0 1.0 ,lo ,hi))

(defmacro cosr (lo hi rate &optional (offset 0.0))
  `(lin-lin (cos (* pi 2 (+ ,offset ,(alexandria:symbolicate "BEAT")) ,rate)) -1.0 1.0 ,lo ,hi))


(defmacro once (form)
  (let* ((result (eval form)))
    `(quote ,result)))

(defmacro nth-beat (dur list)
  (let ((sym-beat (alexandria:symbolicate "BEAT")))
    (alexandria:once-only (list)
      `(nth (mod (floor ,sym-beat ,dur) (length ,list)) ,list))))

(defmacro beat-count (&optional (len most-positive-fixnum) dur)
  (let* ((sym-beat (alexandria:symbolicate "BEAT"))
	 (sym-dur (alexandria:symbolicate "DUR")))
    `(mod (round ,sym-beat ,(if dur dur sym-dur)) ,len)))

(defstruct box result)

(defmacro latch (b form &optional (default form))
  (let* ((box (make-box :result (eval default)))
	 (beat (alexandria:symbolicate "BEAT"))
	 (l (gensym)))
    `(let* ((,l ,box))
       (when (zerop (mod ,beat ,b))
	 (setf (box-result ,l) ,form))
       (box-result ,l))))


(defun coin (n &optional (if-true-val t) (if-false-val nil)
		 (state *random-state*))
  (if (< (random 1.0 state) n) if-true-val if-false-val))



(defvar *schedule-object* (make-hash-table))

(flet ((reset-sched-object ()
	 (setf *schedule-object* (make-hash-table))))
  (pushnew
   #'reset-sched-object
   *stop-hooks*))

(defstruct schedule-object
  time running-p)


(defmacro schedule (name (quant &key (ahead 0) (count sb-ext:double-float-positive-infinity)) &optional function)
  (alexandria:with-gensyms (func execute next-time sched-time obj sched-obj q-time sym-beat sym-dur sym-count body-fun halt)
    `(let* ((,halt t))
       (flet ((,(alexandria:symbolicate "SCHED-STOP") ()
		(setf ,halt nil)
		(setf (schedule-object-running-p (gethash ',name *schedule-object*)) nil)))
	 (declare (ignorable (function ,(alexandria:symbolicate "SCHED-STOP"))))
	 (let* ((,obj (make-schedule-object))
		(,body-fun ,function)
		(,func ,(when function
			  `(lambda (,sym-beat)
			     #+sbcl (declare (sb-ext:muffle-conditions style-warning))
			     (labels ((,execute (,sym-beat ,sym-count)
					(declare (ignorable ,sym-beat ,sym-count))
					(let* ((,sched-obj (gethash ',name *schedule-object*))
					       (,sched-time (schedule-object-time ,sched-obj)))
					  (if (< ,sym-count ,count)
					      (when (or (eql ,sched-obj ,obj)
							(< ,sym-beat ,sched-time))
						(let* ((,sym-dur (rationalize (funcall ,body-fun ,sym-beat ,sym-count))))
						  (let* ((,next-time (+ ,sym-beat ,sym-dur)))
						    (when (and ,halt
							       (or (eql ,sched-obj ,obj)
								   (< ,next-time ,sched-time)))
						      (clock-add (- ,next-time ,ahead) #',execute ,next-time (+ ,sym-count 1))))))
					    (setf (schedule-object-running-p ,sched-obj) nil)))))
			       (,execute ,sym-beat 0))))))
	   (declare (ignorable ,func))
	   (let* ((,q-time (clock-quant ,quant)))
	     (setf (schedule-object-time ,obj) ,q-time)
	     (setf (gethash ',name *schedule-object*) ,obj)
	     ,@(when function
		 `((setf (schedule-object-running-p ,obj) t)
		   (clock-add (- ,q-time ,ahead) ,func (rationalize ,q-time))))
	     ',name))))))


(defmacro with-lambda ((dur) &body body)
  (let* ((sym-beat (alexandria:symbolicate "BEAT"))
	 (sym-dur (alexandria:symbolicate "DUR"))
	 (sym-count (alexandria:symbolicate "N"))
	 (sym-tick (alexandria:symbolicate "TICK")))
    `(lambda (,sym-beat ,sym-count)
       (let* ((,sym-dur ,dur)
	      (,sym-tick (beat-count)))
	 ,@body
	 ,sym-dur))))

(defun schedule-status ()
  (loop for key being the hash-key of *schedule-object*
	  using (hash-value obj)
	when (schedule-object-running-p obj)
	  collect key))


