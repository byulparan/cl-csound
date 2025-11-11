;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2025.11.05 byulparan@gmail.com
;; 
;; 

(in-package :csound)


(defvar *default-sigrate* :ar)

(let ((number 0))
  (defun get-unique-number ()
    (incf number)))

(defun make-unique-name (sig-rate)
  (format nil "~a_~a"
	  (alexandria:make-gensym sig-rate)
	  (get-unique-number)))


(defgeneric ar (ugen))
(defgeneric kr (ugen))
(defgeneric ir (ugen))
;;; (defgeneric fr (ugen))  ; <- is it need?



;;;;;;;;;;;;;;
;;  global  ;;
;;;;;;;;;;;;;;

(defmacro global (&body form)
  (let ((opcode (gensym))
	(build-form (gensym)))
    `(let* ((*opcodes* nil)
	    (*streams* (make-string-output-stream))
	    (,opcode ,@form))
       (when (typep ,opcode 'ugen)
	 (with-slots (var) ,opcode
	   (setf var (concatenate 'string "g" (if var var
						(let* ((rate (rate ,opcode)))
						  (make-unique-name (if rate rate "a"))))))))
       (build ,opcode)
       (let ((,build-form (get-output-stream-string *streams*)))
	 (when (or *debug-mode* (not (get-csound)))
	   (print ,build-form))
       	 (when (get-csound)
	   (when (or (zerop (length ,build-form))
		     (not (zerop (csound-compile-orc (get-csound) ,build-form 0))))
       	     (error "error! ~a" ,build-form))
	   (when (typep ,opcode 'ugen)
	     (setf (gethash (intern (string-upcase (var ,opcode)) :keyword)
			    *csound-global-variables*)
		   ,build-form))))
       ,opcode)))



;;;;;;;;;;;;;;
;;  assign  ;;
;;;;;;;;;;;;;;

(defun set! (name value)
  (make-instance 'assign  :var (get-form name) :args value))



;;;;;;;;;;;;;;
;;  opcode  ;;
;;;;;;;;;;;;;;

(defclass opcode ()
  ((name :initarg :name :reader name)
   (var :initarg :var :initform nil :reader var)
   (args :initarg :args :initform nil :reader args)
   (opt-args :initarg :opt-args :initform nil :reader opt-args)))

(defmethod initialize-instance :after ((self opcode) &key)
  (when (boundp '*opcodes*)
    (push self *opcodes*)))



;; 
;; var - opcode 를 저장하는 변수 이름
;; 
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


;;
;; get-form - opcode 가 다른 옵코드나 함수의 인자로 들어갈때 보여지는 이름
;; 
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


;;
;; build - opcode 가 실제 코드 상에 어떻게 표기 될 것인가
;; 

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




;;;;;;;;;;;;;;
;;  assign  ;;
;;;;;;;;;;;;;;

(defclass assign (opcode)
  ())


(defmethod build ((opcode assign))
  (format *streams* "~&  ~a = ~a" (var opcode) (get-form (args opcode))))


;;;;;;;;;;;;;
;;  param  ;;
;;;;;;;;;;;;;

(defclass param (opcode)
  ())

(defmethod build ((opcode param))
  (format *streams* "~&  ~a = ~a" (var opcode) (name opcode)))


;;;;;;;;;;;;
;;  ugen  ;;
;;;;;;;;;;;;

(defclass ugen (opcode)
  ((rate :initarg :rate :initform nil :accessor rate)))


(defmethod get-form ((opcode ugen))
  (if (var opcode) (var opcode)
    (format nil "~a(~{~a~^, ~}~:[~;, ~]~{~@[~a~^, ~]~})"
	    (if (rate opcode) (format nil "~a:~a" (name opcode) (rate opcode))
	      (name opcode))
	    (mapcar #'get-form (args opcode))
 	    (and (args opcode) (opt-args opcode))
	    (mapcar #'get-form (opt-args opcode)))))


(defmethod build ((opcode ugen))
  (when (var opcode)
    (format *streams* "~&  ~{~a~^,~} = ~a( ~{~a~^, ~}~:[~;, ~]~{~@[~a~^, ~]~} )"
	    (alexandria:ensure-list (var opcode))
	    (name opcode)
	    (mapcar #'get-form (args opcode))
	    (and (args opcode) (opt-args opcode))
	    (mapcar #'get-form (opt-args opcode)))))


(defmethod ar ((ugen ugen))
  (setf (rate ugen) "a")
  ugen)

(defmethod kr ((ugen ugen))
  (setf (rate ugen) "k")
  ugen)

(defmethod ir ((ugen ugen))
  (setf (rate ugen) "i")
  ugen)



;;;;;;;;;;;;;;;;;;
;;  GenRoutine  ;;
;;;;;;;;;;;;;;;;;;

(defclass gen-routine (ugen)
  ((rate :initform "i" :reader rate)
   (ifn :initarg :ifn :reader ifn)
   (chanls :initform 1 :accessor chanls)))


(defmethod fltfy ((opcode gen-routine))
  (fltfy (ifn opcode)))

(defmethod get-form ((opcode gen-routine))
  (get-form (fltfy opcode)))



;;;;;;;;;;;;;;;
;;  command  ;;
;;;;;;;;;;;;;;;

(defclass command (opcode)
  ())

(defmethod (setf var) (name (opcode command))
  (declare (ignore name))
  opcode)

(defmethod build ((opcode command))
  (format *streams* "~&  ~a( ~{~a~^, ~}~:[~;, ~]~{~@[~a~^, ~]~} )"
	  (name opcode)
	  (mapcar #'get-form (args opcode))
	  (and (args opcode) (opt-args opcode))
	  (mapcar #'(lambda (op) (when op (get-form op))) (opt-args opcode))))

;;;;;;;;;;;;;
;;  label  ;;
;;;;;;;;;;;;;

(defclass label (command)
  ())

(defmethod build ((opcode label))
  (format *streams* "~&~a:" (name opcode)))



;;;;;;;;;;;;
;;  func  ;;
;;;;;;;;;;;;

(defclass func (opcode)
  ())

(defmethod get-form ((arg func))
  (if (var arg) (var arg)
      (format nil "~a(~{~a~^, ~})" (name arg)  (mapcar #'get-form (args arg)))))

(defmethod build ((opcode func))
  (when (var opcode)
    (format *streams* "~&  ~a = ~a(~{~a~^,  ~})" (var opcode) (name opcode) (mapcar #'get-form (args opcode)))))


;;;;;;;;;;;;;;;;
;;  operator  ;;
;;;;;;;;;;;;;;;;

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
       (format str "  ~a = " (var opcode))
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
       (format str " ~a = " (var opcode))
       (destructuring-bind (a b c)
	   (args opcode)
	 (format str "~a ~a ~a ~a ~a" (get-form a) (name opcode) (get-form b) (2nd-op opcode) (get-form c)))))))




;;;;;;;;;;;;;;;;;;;;;
;;  define-opcode  ;;
;;;;;;;;;;;;;;;;;;;;;

(defmacro defopcode (name type args &optional (op-name (string-downcase name)))
  (let* ((delimiter (find-if #'(lambda (arg)(or (eql arg '&optional) (eql arg '&rest))) args))
	 (parse-args (split-sequence:split-sequence delimiter args)))
    `(progn (defun ,name ,args
	      (make-instance ',type :args ,(cons 'list (car parse-args))
				    :opt-args (remove nil ,(if (eql delimiter '&rest) (car (second parse-args))  (cons 'list (second parse-args))))
				    :name ,op-name))
	    (export ',name))))

