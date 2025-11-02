
(asdf/defsystem:defsystem #:cl-csound
  :serial t
  :depends-on (#:cffi
	       #:bordeaux-threads
	       #:bt-semaphore
	       #:split-sequence
	       #:cl-ppcre
	       #:alexandria
	       ;; #:scheduler
	       ;;#:cl-sndfile
	       )
  :components ((:file "package")
	       (:file "library")
	       (:file "cffi")
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
	       (:file "extensions")))
