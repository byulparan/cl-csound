;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2025.11.05 byulparan@gmail.com
;; 
;; 

(in-package :csnd)

#-windows
(cffi:defcstruct sched-param
  (priority :int))

#+darwin
(cffi:defcenum sched-policy
  (:sched_other 1)
  (:sched_rr 2)
  (:sched_fifo 4))

#+(or linux freebsd)
(cffi:defcenum sched-policy
  (:sched_other 0)
  (:sched_fifo 1)
  (:sched_rr 2))

#-windows
(defun set-thread-realtime-priority ()
  "This function is made high priority to calling thread, and sched-policy set SCHED_RR."
  (cffi:with-foreign-objects ((param '(:pointer (:struct sched-param))))
    (let* ((max-priority (cffi:foreign-funcall "sched_get_priority_max" :int (cffi:foreign-enum-value 'sched-policy :sched_rr)
									:int)))
      (cffi:with-foreign-slots ((priority dummy) param (:struct sched-param))
	(setf priority max-priority)))
    (cffi:foreign-funcall "pthread_setschedparam" :pointer (cffi:foreign-funcall "pthread_self" :pointer)
						  :int (cffi:foreign-enum-value 'sched-policy :sched_rr)
						  :pointer param)))

#-windows
(defun get-thread-priority ()
  "Get the thread-info of calling thread. If you want get thread-info of *main-scheduler*,
 eval the '(callback (now) #'get-thread-priority)."
  (cffi:with-foreign-objects ((param '(:pointer (:struct sched-param)))
			      (policy :int))
    (cffi:foreign-funcall "pthread_getschedparam" :pointer (cffi:foreign-funcall "pthread_self" :pointer)
						  :pointer policy
						  :pointer param)
    (format t "~&policy: ~d~%priority: ~d" (let ((policy (cffi:mem-ref policy :int)))
					     (cffi:foreign-enum-keyword 'sched-policy policy))
	    (cffi:with-foreign-slots ((priority dummy) param (:struct sched-param))
	      priority))))

#+windows
(defun set-thread-realtime-priority ()
  "Not implements windows,yet"
  (values))

#+windows
(defun get-thread-priority ()
  "Not implements windows,yet"
  (values))



;; ================================================================================
;; threading util

#+ecl
(defmacro with-recursive-lock-held ((lock) &body body)
  `(if (eql (bt:current-thread) (mp:lock-owner ,lock))
       (progn
	 ,@body)
     (bt:with-lock-held (,lock)
       ,@body)))

#-ecl
(defmacro with-recursive-lock-held ((lock) &body body)
  `(bt:with-recursive-lock-held (,lock)
     ,@body))

#+ecl
(defun condition-wait (condition-variable lock &key timeout)
  (let* ((success (bt:condition-wait condition-variable lock :timeout timeout)))
    (when (not success)
      (bt:acquire-lock lock))
    success))

#-ecl
(setf (symbol-function 'condition-wait) #'bt:condition-wait)





(defstruct sched-event timestamp task)

(defclass tempo-clock ()
  ((bpm
    :initarg :bpm
    :initform 60.0d0)
   (status
    :initform :stop
    :accessor sched-status)
   (sched-thread
    :initform nil
    :accessor sched-thread)
   (ahead :initarg :ahead :initform .2d0 :accessor ahead)
   (base-seconds)
   (base-beats :initarg :base-beats :initform 0 :reader base-beats)
   (beat-dur)
   (mutex
    :reader mutex)
   (condition-var
    :initform (bt:make-condition-variable)
    :reader condition-var)
   (in-queue
    :initform (pileup:make-heap #'<= :size 100 :key #'sched-event-timestamp)
    :reader in-queue)
   (timestamp
    :initarg :timestamp
    :reader timestamp
    :documentation
    "This Function is get current scheduler time. That must based on seconds.")))


(defmethod initialize-instance :after ((self tempo-clock) &key)
  (with-slots (mutex in-queue bpm base-beats beat-dur base-seconds) self
    (setf mutex (slot-value in-queue 'pileup::lock))
    (setf beat-dur (/ 60.0d0 bpm)
	  base-seconds (funcall (timestamp self)))))


(defun sched-time (scheduler)
  (funcall (timestamp scheduler)))


(defmethod beats-to-secs ((tempo-clock tempo-clock) beats)
  (with-slots (base-beats beat-dur base-seconds) tempo-clock
    (+ (* (- beats base-beats) beat-dur) base-seconds)))

(defmethod secs-to-beats ((tempo-clock tempo-clock) secs)
  (with-slots (base-seconds bpm base-beats) tempo-clock
    (+ (* (- secs base-seconds) (/ bpm 60.0)) base-beats)))


(defmethod tempo-clock-run ((tempo-clock tempo-clock))
  (when (eql (sched-status tempo-clock) :stop)
    (setf (sched-thread tempo-clock)
      (bt:make-thread
       (lambda ()
         (setf *random-state* (make-random-state t))
         (labels ((run ()
                    (handler-case
                        (let* ((run-p t))
                          (loop while run-p do
                            (loop :while (pileup:heap-empty-p (in-queue tempo-clock))
                                  :do (condition-wait (condition-var tempo-clock) (mutex tempo-clock)))
                            (loop :while (not (pileup:heap-empty-p (in-queue tempo-clock)))
                                  :do (let ((timeout (- (- (beats-to-secs tempo-clock (sched-event-timestamp (pileup:heap-top (in-queue tempo-clock))))
                                                           (ahead tempo-clock))
							(sched-time tempo-clock))))
                                        (unless (plusp timeout)
                                          (return))
                                        (condition-wait (condition-var tempo-clock) (mutex tempo-clock) :timeout timeout)))
                            (loop :while (and (not (pileup:heap-empty-p (in-queue tempo-clock)))
                                              (>= (sched-time tempo-clock)
                                                  (- (beats-to-secs tempo-clock (sched-event-timestamp (pileup:heap-top (in-queue tempo-clock))))
                                                     (ahead tempo-clock))))
                                  :do (when (eql 'ensure-scheduler-stop-quit ;; it's magic code. it seems chagne..
                                                 (funcall (sched-event-task (pileup:heap-pop (in-queue tempo-clock)))))
                                        (setf run-p nil)
                                        (return)))))
                      (error (c) (format t "~&Error \"~a\" in TempoClock thread~%" c)
                        (finish-output)
                        (run)))))
           (set-thread-realtime-priority)
           (bt:with-lock-held ((mutex tempo-clock))
             (setf (sched-status tempo-clock) :running)
             (run))))
       :name "Csound Scheduler thread"))
    :running))

(defmethod tempo-clock-beats ((tempo-clock tempo-clock))
  (secs-to-beats tempo-clock (sched-time tempo-clock)))


(defmethod tempo-clock-add ((tempo-clock tempo-clock) beats f &rest args)
  (with-recursive-lock-held ((mutex tempo-clock))
    (pileup:heap-insert (make-sched-event :timestamp beats
					  :task (lambda () (apply f args)))
			(in-queue tempo-clock))
    (bt:condition-notify (condition-var tempo-clock)))
  (values))


(defmethod tempo-clock-stop ((tempo-clock tempo-clock))
  (with-slots (beat-dur) tempo-clock
    (when (eql (sched-status tempo-clock) :running)
      (tempo-clock-add tempo-clock (+ (* (ahead tempo-clock) .5 (reciprocal beat-dur))
				      (tempo-clock-beats tempo-clock))
		       (lambda () 'ensure-scheduler-stop-quit))
      (bt:join-thread (sched-thread tempo-clock))
      (tempo-clock-clear tempo-clock)
      (setf (sched-status tempo-clock) :stop))))


(defmethod tempo-clock-set-bpm ((tempo-clock tempo-clock) new-bpm)
  (with-recursive-lock-held ((mutex tempo-clock))
    (with-slots (base-seconds base-beats bpm beat-dur) tempo-clock
      (let* ((in-beats (tempo-clock-beats tempo-clock)))
	(setf base-seconds (beats-to-secs tempo-clock in-beats)
	      base-beats in-beats
	      bpm new-bpm
	      beat-dur (/ 60.0d0 new-bpm))))
    (bt:condition-notify (condition-var tempo-clock))))

(defmethod tempo-clock-bpm ((tempo-clock tempo-clock) &optional new-bpm)
  (if new-bpm (tempo-clock-set-bpm tempo-clock new-bpm)
    (with-slots (bpm) tempo-clock
      bpm)))

(defmethod tempo-clock-clear ((tempo-clock tempo-clock))
  (with-recursive-lock-held ((mutex tempo-clock))
    (with-slots (in-queue) tempo-clock
      (loop :until (pileup:heap-empty-p in-queue)
	    :do (pileup:heap-pop in-queue)))
    (bt:condition-notify (condition-var tempo-clock))))

(defmethod tempo-clock-quant ((tempo-clock tempo-clock) quant)
  (let* ((beats (secs-to-beats tempo-clock (+ (ahead tempo-clock) (sched-time tempo-clock)))))
    (+ beats (- quant (mod beats quant)))))







