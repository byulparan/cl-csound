(in-package #:csnd)

#+sbcl (eval-when (:compile-toplevel :load-toplevel :execute)
	 (sb-ext:unlock-package :cl))

(defparameter *pi~*
  (make-instance 'ugen :var "$M_PI"))

(defun add (&rest args)
  (labels ((inner (a b)
	     (if (every #'numberp (list a b)) (+ a b)
		 (make-instance 'operator :name "+" :args (list a b)))))
    (reduce #'inner args)))

(defun sub (&rest args)
  (labels ((inner (a b)
	     (if (every #'numberp (list a b)) (- a b)
		 (make-instance 'operator :name "-" :args (list a b)))))
    (reduce #'inner args)))

(defun mul (&rest args)
  (labels ((inner (a b)
	     (if (every #'numberp (list a b)) (* a b)
		 (make-instance 'operator :name "*" :args (list a b)))))
    (reduce #'inner args)))

(defun div (&rest args)
  (labels ((inner (a b)
	     (if (every #'numberp (list a b)) (* 1.0 (/ a b))
		 (make-instance 'operator :name "/" :args (list a b)))))
    (reduce #'inner args)))

(defun gt (a b)
  (make-instance 'operator :name ">" :args (list a b)))

(defun ge (a b)
  (make-instance 'operator :name ">=" :args (list a b)))

(defun lt (a b)
  (make-instance 'operator :name "<" :args (list a b)))

(defun le (a b)
  (make-instance 'operator :name "<=" :args (list a b)))

(defun =~ (a b)
  (make-instance 'operator :name "==" :args (list a b)))

(defun != (a b)
  (make-instance 'operator :name "!=" :args (list a b)))

(defun ^ (a b)
  (make-instance 'operator :name "^" :args (list a b)))

(defun % (a b)
  (make-instance 'operator :name "%" :args (list a b)))

(defun ~ (a)
  (make-instance 'operator :name "~" :args (list nil a)))

(defun ? (condition true false)
  (make-instance '3r-operator :name "?" :2nd-op ":" :args (list condition true false)))



;; (defmacro if-then (&rest args)
;;   `(push
;;     (lambda ()
;;       (let* ((*opcodes* nil)
;; 	     (data (get-form ,(first (car args)))))
;; 	(dolist (op (nreverse *opcodes*)) (build op))
;; 	(format *streams* "~&if ~a then" data))
;;       (let ((*opcodes* nil)) ,@(cdr (car args))
;; 	(dolist (op (nreverse *opcodes*)) (build op)))
;;       ,@(loop for form in (cdr args)
;; 	      append
;; 	      `(,(if (eql :else (first form)) `(format *streams* "~&else")
;; 		     `(format *streams* "~&elseif ~a then" (get-form ,(first form))))
;; 		(let ((*opcodes* nil)) ,@(cdr form)
;; 		  (dolist (op (nreverse *opcodes*)) (build op)))))
;;       (format *streams* "~&endif"))
;;     *opcodes*))

(defmacro if-then (&rest args)
  `(push
    (lambda ()
      (let* ((*opcodes* nil)
	     (stream (make-string-output-stream)))
	(format stream "~%~&if ~a then" (get-form ,(first (car args))))
	(let ((*opcodes* nil)
	      (*streams* stream))
	  ,@(cdr (car args))
	  (dolist (op (nreverse *opcodes*)) (build op)))
	,@(loop for form in (cdr args)
		append
		`(,(if (eql :else (first form)) `(format stream "~&else")
		       `(format stream "~&elseif ~a then" (get-form ,(first form))))
		  (let ((*opcodes* nil)
			(*streams* stream))
		    ,@(cdr form)
		    (dolist (op (nreverse *opcodes*)) (build op)))))
	(format stream "~&endif")
	(dolist (op (nreverse *opcodes*))
	  (build op))
	(format *streams* (get-output-stream-string stream))))
    *opcodes*))



(defmacro build-if-goto (&rest commands)
  (cons 'progn
	(loop for cmd in commands
	      collect
	      `(defun ,(intern (string-upcase (format nil "if-~a" cmd))) (condition label)
		 (let ((cmd ,(string-downcase cmd)))
		   (push
		    #'(lambda () (format *streams* "~&if ~a ~a ~a" (get-form condition) cmd (get-form label)))
		    *opcodes*))))))

(build-if-goto igoto kgoto goto)

(defmethod build ((opcode function))
  (funcall opcode))


(defun label (name)
  (make-instance 'label :name (get-form name)))


;; (defmacro with-reinit ((init-form update-form condition) &body body)
;;   (alexandria:with-gensyms (reset inits contin)
;;     (let ((reset (alexandria:make-keyword (format nil "~a_~a" reset (random 1000))))
;; 	  (inits (alexandria:make-keyword (format nil "~a_~a" inits (random 1000))))
;; 	  (contin (alexandria:make-keyword (format nil "~a_~a" contin (random 1000)))))
;;       `(progn
;; 	 (slet* (,init-form) 
;; 		(label ,reset)
;; 		(slet* (,update-form)
;; 		       (assigns ,(car init-form) (add ,(car init-form) ,(car update-form)))
;; 		       (if~ ,condition :kgoto ,inits)
;; 		       (kgoto ,contin)
;; 		       (label ,inits)
;; 		       (reinit ,reset)
;; 		       (label ,contin)
;; 		       ,@body))))))
