(in-package #:csound)

(defopcode lenarray func (array))

(defclass csnd-array (ugen)
  ())

(defmethod build ((opcode csnd-array))
  (unless (var opcode) (setf (var opcode) (make-unique-name (rate opcode))))
  (format *streams* "~&~{~a~^,~}[]~20t~10a ~{~a~^, ~}~:[~;, ~]~{~@[~a~^, ~]~}"
	  (alexandria:ensure-list (var opcode))
	  (name opcode)
	  (mapcar #'get-form (args opcode))
	  (and (args opcode) (opt-args opcode))
	  (mapcar #'get-form (opt-args opcode))))

(defun fillarray (&rest args)
  (make-instance 'csnd-array :name "fillarray" :args args))

(defun fillarray* (lst)
  (apply #'fillarray lst))

(defun aget (array index)
  (unless (var array) (setf (var array) (make-unique-name *default-sigrate*)))
  (make-instance 'opcode :var (format nil "~a[~d]" (var array) (get-form index))))

(defun aset (array index value)
  (set! (aget array index) value))



