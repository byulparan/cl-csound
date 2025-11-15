;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;.11.05 byulparan@gmail.com
;; 
;; 

(asdf/defsystem:defsystem #:cl-csound
  :name "cl-csound"
  :author "Sungmin Park. byulparan@gmail.com"
  :description "Binding Csound7 API for Common Lisp"
  :licence "Public Domain / 0-clause MIT"
  :version "2025.11.15"
  :serial t
  :depends-on (#:cffi
	       #:cffi-libffi
	       #:named-readtables
	       #:bordeaux-threads
	       #:trivial-main-thread
	       #:float-features
	       #:split-sequence
	       #:cl-ppcre
	       #:alexandria
	       #:pileup)
  :components ((:file "package")
	       (:file "library")
	       (:file "cffi")
	       (:file "scheduler")
	       (:file "csound")
	       (:file "channels")
	       (:file "opcode")
	       (:file "opcodes/op")
	       (:file "opcodes/array")
	       (:file "opcodes/abc")
	       (:file "opcodes/def")
	       (:file "opcodes/ghi")
	       (:file "opcodes/jkl")
	       (:file "opcodes/mno")
	       (:file "opcodes/pqr")
	       (:file "opcodes/stu")
	       (:file "opcodes/vwxyz")
	       (:file "extensions/extensions")
	       (:file "extensions/util")
	       (:file "extensions/pc")))
