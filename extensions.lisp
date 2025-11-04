;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2025.11.05 byulparan@gmail.com
;; 
;; 

(in-package #:csnd)

(setf *make-csound-hook*
      (lambda ()
	(csound-compile-orc
	 (get-csound)
	 (format nil
		 "instr ~d
                turnoff2 p4, 0, p5
                turnoff
                 endin"
		 *stop-synth-insnum*))))


(defmacro proxy (key &optional body &key (fade-time 4))
  (let ((name (intern (format nil "~a-TEMP-SYNTH" key))))
    (alexandria:with-gensyms ()
      `(progn
	 (when (find ',name (alexandria:hash-table-keys *csound-insnum-hash*))
	   (stop ',name))
	 ,(when body
	    `(let ((*pushed-orchestra-p* nil))
	       (definst ,name ()
		 (let* ((asig ,body)
			(kenv (linsegr 0 ,fade-time 1 ,fade-time 0))
			(aout (* asig kenv)))
			(outs aout aout)))
	       (csnd (turnon ',name))))))))



(defun full-pathname (path)
  "returning absoulte full-pathname of path"
  #+ccl (namestring (ccl:full-pathname path))
  #-ccl
  (labels ((absolute-dir (dir)
	     (if (eql (car dir) :absolute) (if (find :home dir)
					       (append
						(pathname-directory (user-homedir-pathname))
						(cdr (member :home dir)))
					       dir)
		 (let* ((default-dir
			  (pathname-directory (truename ""))))
		   (when (find :up dir)
		     (setf dir (cdr dir))
		     (setf default-dir (butlast default-dir)))
		   (append default-dir (cdr dir))))))
    (namestring (make-pathname :directory (absolute-dir (pathname-directory path)) :name (pathname-name path) :type (pathname-type path)))))


;; (defun load-sample (ifn path &key (skip-time 0.0) (ch :left))
;;   (let ((full-path (full-pathname path)))
;;     (sf:with-open-sndfile (snd full-path)
;;       (let* ((chanls (sf:chanls snd))
;; 	     (frames (sf:frames snd)))
;; 	(when (and (= chanls 2) (eql ch :both))
;; 	  (setf frames (* chanls frames)))
;; 	(let ((gen-routine (ftgen ifn 0 (expt 2 (ceiling (log frames 2))) -1 full-path skip-time 0
;; 				      (if (= chanls 1) 0
;; 					  (ecase ch
;; 					    (:both 0)
;; 					    (:left 1)
;; 					    (:right 2))))))
;; 	  (prog1 gen-routine
;; 	    (setf (chanls gen-routine) chanls)))))))


(defmacro with-timout (bindings reinit-forms &body body)
  (let ((label (intern (format nil "LABEL_~a" (get-unique-number)) :keyword))
	(itime (intern (string-upcase (make-unique-name :ir)))))
    `(slet ((,itime :p2))
       (label ,label)
       (slet* ,bindings
	 (let* ((timeout ,reinit-forms))
	   (set! ,itime (add ,itime timeout)))
	 (if-then ((ge (kr (times)) ,itime) (reinit ,label)))
	 ,@body))))

(defun mix (signal-lst)
  (apply #'sum signal-lst))





