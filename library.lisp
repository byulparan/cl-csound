;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2025.11.05 byulparan@gmail.com
;; 
;; 

(in-package #:csnd)

#-windows
(pushnew "/usr/local/lib/" cffi:*foreign-library-directories*)

#+windows
(pushnew "c:/program files/csound6/bin/" cffi:*foreign-library-directories*)

(cffi:define-foreign-library csoundlib
  (:darwin (:framework "CsoundLib64"))
  (:unix (:or "libcsound64.so"))
  (:windows "csound64.dll"))

(cffi:use-foreign-library csoundlib)

(cffi:defcfun ("csoundGetSizeOfMYFLT" csound-size-of-myflt) :int)

(defvar *myflt*)

(ecase (csound-size-of-myflt)
  (8 (cffi:defctype myflt :double
	   "base type of csound's internal data.")
   (setf *myflt* 'double-float))
  (4 (cffi:defctype myflt :float
	   "base type of csound's internal data.")
   (setf *myflt* 'single-float)))

