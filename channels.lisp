;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2025.11.05 byulparan@gmail.com
;; 
;; 

(in-package #:csound)

(defclass channel ()
  ((name :initarg :name :accessor name)
   (direction :initarg :direction :reader direction)))

(defmethod channel-size ((channel channel))
  (/ (csound-get-channel-datasize (get-csound) (name channel)) (csound-size-of-myflt)))

(defmethod initialize-instance :before ((self channel) &key name)
  (when (find (ppcre:regex-replace-all "-" name "_") *channels* :test #'string=)
    (error "channel ~a is already exists!" name)))

(defclass control-channel (channel)
  ((itype :initarg :type :reader itype)
   (init-value :initarg :init-value :reader init-value)
   (min-value :initarg :min-value :reader min-value)
   (max-value :initarg :max-value :reader max-value)))

(defmethod initialize-instance :after ((self control-channel) &key)
  (with-slots (name direction itype init-value min-value max-value) self
    (assert (and (> max-value min-value)
		 (>= max-value init-value)
		 (>= init-value min-value)) nil
		 "channel value invalid. must be max-value(~a) >= init-value(~a) >=min-value(~a)"
		 max-value init-value min-value)
    (setf name (ppcre:regex-replace-all "-" name "_"))
    (csound-compile-orc
     (get-csound)
     (format nil "gk~a init ~f~%gk~a chnexport ~s, ~d, ~d, ~a, ~f, ~f"
	     name init-value name name
	     (ecase direction (:input 1) (:output 2) (:io 3))
	     (ecase itype (:default 0) (:int-only 1) (:linear 2) (:exponential 3))
	     (format nil "i(gk~a)" name)
	     min-value max-value))
    (push name *channels*)))

(defmethod get-form ((arg control-channel))
  (format nil "gk~a" (name arg)))

(defmethod channel-value ((channel control-channel))
  (csound-get-control-channel (get-csound) (name channel)))

(defmethod (setf channel-value) ((value number) (channel control-channel))
  (with-slots (min-value max-value) channel
    (let ((value (alexandria:clamp value min-value max-value)))
      (csound-set-control-channel (get-csound) (name channel) (fltfy value)))))

(defun make-control-channel (name direction &key (type :default) init min max)
  (assert (and init min max))
  (make-instance 'control-channel
		 :name name
		 :direction direction
		 :type type
		 :init-value init
		 :min-value min
		 :max-value max))


;; (defclass audio-channel (channel)
;;   ())

;; (defmethod initialize-instance :after ((self audio-channel) &key)
;;   (with-slots (name direction) self
;;     (csound-compile-orc
;;        (get-csound)
;;        (format nil "ga~a chnexport ~s, ~d"
;; 	       name name (ecase direction (:input 1) (:output 2) (:io 3))))))

;; (defmethod get-form ((arg audio-channel))
;;   (format nil "ga~a" (name arg)))

;; (defmethod channel-value ((channel audio-channel))
;;   (let* ((chan-len (/ (channel-size channel) (csound-size-of-myflt)))
;; 	 (lisp-array (make-array chan-len :element-type *myflt*)))
;;     (cffi:with-foreign-objects ((samples 'myflt chan-len))
;;       (csound-get-audio-channel (get-csound) (name channel) samples)
;;       (dotimes (i chan-len)
;; 	(setf (aref lisp-array i) (cffi:mem-aref samples 'myflt i)))
;;       lisp-array)))

;; (defmethod (setf channel-value) ((value array) (channel audio-channel))
;;   (let* ((chan-len (/ (channel-size channel) (csound-size-of-myflt))))
;;     (assert (= (length value) chan-len) nil "length of value not ~d" chan-len)
;;     (cffi:with-foreign-objects ((samples 'myflt chan-len))
;;       (dotimes (i chan-len)
;; 	(setf (cffi:mem-aref samples 'myflt i) (aref value i)))
;;       (csound-set-audio-channel (get-csound) (name channel) samples))))

;; (defun make-audio-channel (name direction)
;;   (make-instance 'audio-channel
;; 		 :name name
;; 		 :direction direction))

