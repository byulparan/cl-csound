(in-package :csnd)

;;; dynamic variable for definition instruments. ------------------
(defvar *default-sigrate* :ar)

(let ((number 0))
  (defun get-unique-number ()
    (incf number)))

(defun make-unique-name (sig-rate)
  (format nil "~a_~a"
	  (alexandria:make-gensym
	   (ecase sig-rate
	     (:ar "a")
	     (:kr "k")
	     (:ir "i")
	     (:fr "f")))
	  (get-unique-number)))

(defmacro with-sigrate-by-name ((name) &body body)
  `(let ((*default-sigrate* ,(case (aref (string-downcase name) 0)
			       (#\a :ar)
			       (#\k :kr)
			       (#\i :ir)
			       (#\f :fr))))
     ,@body))

(defgeneric ar (ugen))
(defgeneric kr (ugen))
(defgeneric ir (ugen))
;;; (defgeneric fr (ugen))  ; <- is it need?

(defparameter *csound-readtable* (copy-readtable *readtable*))
(make-dispatch-macro-character #\k nil *csound-readtable*)
(make-dispatch-macro-character #\a nil *csound-readtable*)
(make-dispatch-macro-character #\i nil *csound-readtable*)

;;; define macro-character
(macrolet ((set-char-macro (char func)
	     `(set-dispatch-macro-character
	       ,char #\.
	       (lambda (stream char1 char2)
		 (declare (ignore char1 char2))
		 (let ((first-char (read-char stream nil nil)))
		   (if (char= first-char #\space) nil
		       (unread-char first-char stream)))
		 (let ((body-form (read stream nil nil)))
		   (list ',func body-form)))
	       *csound-readtable*)))
  (set-char-macro #\k kr)
  (set-char-macro #\a ar)
  (set-char-macro #\i ir))

(defun replace-body-on-cound-readtable (body)
  (let* ((*readtable* *csound-readtable*))
    (read-from-string
     (ppcre:regex-replace-all
       "I\\. "
       (ppcre:regex-replace-all
	"A\\. "
	(ppcre:regex-replace-all
	 "K\\. "
	 (format nil "~s" body)
	 "k.")
	"a.")
       "i."))))


(defmacro csnd (&body form)
  (let ((form (replace-body-on-cound-readtable form))
	(opcode (gensym))
	(build-form (gensym)))
    `(let* ((*opcodes* nil)
	    (*streams* (make-string-output-stream))
	    (,opcode ,@form))
       (when (typep ,opcode 'ugen)
	 (with-slots (var) ,opcode
	   (setf var (concatenate 'string "g" (if var var (make-unique-name (rate ,opcode)))))))
       (build ,opcode)
       (let ((,build-form (get-output-stream-string *streams*)))
       	 (when (get-csound)
       	   (when (or (zerop (length ,build-form))
		     (not (zerop (csound-compile-orc (get-csound) ,build-form))))
       	     (error "error! ~a" ,build-form))
	   (when (typep ,opcode 'ugen)
	     (setf (gethash (intern (string-upcase (var ,opcode)) :keyword)
			    *csound-global-variables*)
		   ,build-form))))
       ,opcode)))

;;; ----------------------------------------------------------------

(defun set! (ugen value &rest ugens)
  (car
   (last
    (cons
     (make-instance 'ugen :name "=" :args (list value) :var (get-form ugen))
     (loop for (u v) on ugens by #'cddr
	   collect (make-instance 'ugen :name "=" :args (list v) :var (get-form u)))))))

(defun assign (value)
  (make-instance 'ugen :name "=" :args (list value)))

;;; ----------------------------------------------------------------

(defclass opcode ()
  ((args :initarg :args :initform nil :reader args)
   (opt-args :initarg :opt-args :initform nil :reader opt-args)
   (var :initarg :var :initform nil :reader var)
   (name :initarg :name :reader name)))

(defmethod initialize-instance :after ((self opcode) &key)
  (when (boundp '*opcodes*)
    (push self *opcodes*)))

(defmethod (setf var) (name (opcode opcode))
  (if (var opcode) (set! (alexandria:make-keyword (string-upcase name)) opcode)
      (with-slots (var) opcode
	(setf var (let ((v (mapcar (lambda (n) (ppcre:regex-replace-all "-" n "_")) (alexandria:ensure-list name))))
		    (if (= (length v) 1) (car v) v)))
	opcode)))


(defmethod (setf var) (name (opcode number))
  (set! (alexandria:make-keyword (string-upcase name)) opcode))

(defmethod (setf var) (name (opcode symbol))
  (set! (alexandria:make-keyword (string-upcase name)) opcode))

(defmethod (setf var) (name opcode)
  (declare (ignore name))
  opcode)

(defmethod get-form ((arg opcode))
  (var arg))

(defmethod get-form ((arg number))
  arg)

(defmethod get-form ((arg single-float))
  (let ((int (round arg)))
    (if (> .001 (- arg int)) (format nil "~,6f" arg)
	(format nil "~f" arg))))

(defmethod get-form ((arg double-float))
  (format nil "~,6f" arg))

(defmethod get-form ((arg string))
  (format nil "\"~a\"" arg))

(defmethod get-form ((arg symbol))
  (if (keywordp arg) (ppcre:regex-replace-all "-" (string-downcase arg) "_")
      (get-form (fltfy arg))))

(defmethod build :before ((opcode opcode))
  (alexandria:when-let ((names (alexandria:ensure-list (var opcode))))
    (dolist (name names)
      (let ((char-code (elt name 0)))
	(when (char= char-code #\g) (setf char-code (elt name 1)))
	(ecase char-code
	  ((#\a #\k #\i #\f #\t)))))))

(defmethod build ((opcode opcode))
  ())

(defmethod build ((opcode number))
  opcode)

;;; 
(defclass param (opcode)
  ())

(defmethod build ((opcode param))
  (format *streams* "~&~a~20t=~31t~10a" (var opcode) (name opcode)))

;;; 
(defclass ugen (opcode)
  ((rate :initarg :rate :initform *default-sigrate* :accessor rate)))


(defmethod get-form ((opcode ugen))
  (unless (var opcode) (setf (var opcode) (make-unique-name (rate opcode))))
  (var opcode))

(defmethod build ((opcode ugen))
  (unless (var opcode) (setf (var opcode) (make-unique-name (rate opcode))))
  (format *streams* "~&~{~a~^,~}~20t~10a ~{~a~^, ~}~:[~;, ~]~{~@[~a~^, ~]~}"
	  (alexandria:ensure-list (var opcode))
	  (name opcode)
	  (mapcar #'get-form (args opcode))
	  (and (args opcode) (opt-args opcode))
	  (mapcar #'get-form (opt-args opcode))))

(defmethod ar ((ugen ugen))
  (unless (eql *default-sigrate* :ar)
    (setf (var ugen) (make-unique-name :ar)))
  ugen)

(defmethod kr ((ugen ugen))
  (unless (eql *default-sigrate* :kr)
    (setf (var ugen) (make-unique-name :kr)))
  ugen)

(defmethod ir ((ugen ugen))
  (unless (eql *default-sigrate* :ir)
    (setf (var ugen) (make-unique-name :ir)))
  ugen)

;;;
(defclass gen-routine (ugen)
  ((rate :initform :ir :reader rate)
   (ifn :initarg :ifn :reader ifn)
   (chanls :initform 1 :accessor chanls)))


(defmethod fltfy ((opcode gen-routine))
  (fltfy (ifn opcode)))

(defmethod get-form ((opcode gen-routine))
  (get-form (fltfy opcode)))




;;; 
(defclass command (opcode)
  ())

(defmethod (setf var) (name (opcode command))
  (declare (ignore name))
  opcode)

(defmethod build ((opcode command))
  (format *streams* "~&~20t~10a ~{~a~^, ~}~:[~;, ~]~{~@[~a~^, ~]~}"
	  (name opcode)
	  (mapcar #'get-form (args opcode))
	  (and (args opcode) (opt-args opcode))
	  (mapcar #'(lambda (op) (when op (get-form op))) (opt-args opcode))))

(defclass label (command)
  ())

(defmethod build ((opcode label))
  (format *streams* "~&~a:" (name opcode)))

;;; 
(defclass func (opcode)
  ())

(defmethod get-form ((arg func))
  (if (var arg) (var arg)
      (format nil "~a(~{~a~^, ~})" (name arg)  (mapcar #'get-form (args arg)))))

(defmethod build ((opcode func))
  (when (var opcode)
    (format *streams* "~&~20a=~31t~a(~{~a~^,  ~})" (var opcode) (name opcode) (mapcar #'get-form (args opcode)))))

;;; 

(defclass operator (opcode)
  ())

(defmethod get-form ((arg operator))
  (if (var arg) (var arg)
      (with-output-to-string (str)
	(format str "(")
	(dolist (x (butlast (args arg)))
	  (format str "~@[~a ~]~a " (if x (get-form x)) (name arg)))
	(format str "~a)" (get-form (car (last (args arg))))))))

(defmethod build ((opcode operator))
  (when (var opcode)
    (format
     *streams*
     "~&~a"
     (with-output-to-string (str)
       (format str "~20a=~31t" (var opcode))
       (dolist (x (butlast (args opcode)))
	 (format str "~@[~a ~]~a " (if x (get-form x)) (name opcode)))
       (format str "~a" (get-form (car (last (args opcode)))))))))

(defclass 3r-operator (operator)
  ((2nd-op :initarg :2nd-op :reader 2nd-op)))

(defmethod get-form ((arg 3r-operator))
  (if (var arg) (var arg)
      (with-output-to-string (str)
	(destructuring-bind (a b c)
	    (args arg)
	  (format str "(~a ~a ~a ~a ~a)" (get-form a) (name arg) (get-form b) (2nd-op arg) (get-form c))))))

(defmethod build ((opcode 3r-operator))
  (when (var opcode)
    (format
     *streams*
     "~&~a"
     (with-output-to-string (str)
       (format str "~20a=~31t" (var opcode))
       (destructuring-bind (a b c)
	   (args opcode)
	 (format str "~a ~a ~a ~a ~a" (get-form a) (name opcode) (get-form b) (2nd-op opcode) (get-form c)))))))

;;; 
(defmacro defopcode (name type args &optional (op-name (string-downcase name)))
  (let* ((delimiter (find-if #'(lambda (arg)(or (eql arg '&optional) (eql arg '&rest))) args))
	 (parse-args (split-sequence:split-sequence delimiter args)))
    `(progn (defun ,name ,args
	      (make-instance ',type :args ,(cons 'list (car parse-args))
				    :opt-args (remove nil ,(if (eql delimiter '&rest) (car (second parse-args))  (cons 'list (second parse-args))))
				    :name ,op-name))
	    (export ',name))))

