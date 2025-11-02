(in-package #:csnd)

(cffi:defcfun ("csoundGetVersion" csound-version) :int)

(cffi:defcfun ("csoundGetAPIVersion" csound-api-version) :int)

(cffi:defcfun ("csoundInitialize" csound-initialize) :int
  (flags :int))

(cffi:defcfun ("csoundCreate" csound-create) :pointer
  (args :pointer))

(cffi:defcfun ("csoundCompile" csound-compile) :int
  (csound :pointer)
  (argc :int)
  (argv :pointer))

(cffi:defcfun ("csoundDestroy" csound-destroy) :void
  (csound :pointer))

(cffi:defcfun ("csoundPerformKsmps" csound-perform-ksmps) :int
  (csound :pointer))

(cffi:defcfun ("csoundPerform" csound-perform) :int
  (csound :pointer))

(cffi:defcfun ("csoundReset" csound-reset) :void
  (csound :pointer))

(cffi:defcfun ("csoundStop" csound-stop) :void
  (csound :pointer))

(cffi:defcfun ("csoundGetSr" csound-sr) myflt
  (csound :pointer))

(cffi:defcfun ("csoundGetKr" csound-kr) myflt
  (csound :pointer))

(cffi:defcfun ("csoundGetKsmps" csound-ksmps) :uint32
  (csound :pointer))

(cffi:defcfun ("csoundGetNchnls" csound-nchnls) :uint32
  (csound :pointer))

(cffi:defcfun ("csoundGetNchnlsInput" csound-nchnls-input) :uint32
  (csound :pointer))

(cffi:defcfun ("csoundGet0dBFS" csound-0dbfs) myflt
  (csound :pointer))

(cffi:defcfun ("csoundGetInputBufferSize" csound-input-buffer-size) :long
  (csound :pointer))

(cffi:defcfun ("csoundGetOutputBufferSize" csound-output-buffer-size) :long
  (csound :pointer))

(cffi:defcfun ("csoundGetSpout" csound-spout) :pointer
  (csound :pointer))

(cffi:defcfun ("csoundGetOutputName" csound-output-name) :string
  (csound :pointer))

(cffi:defcfun ("csoundStart" csound-start) :void
  (csound :pointer))

(cffi:defcfun ("csoundSetOption" csound-set-option) :int
  (csound :pointer)
  (option :string))

(cffi:defcfun ("csoundCompileOrc" csound-compile-orc) :int
  (csound :pointer)
  (orc :string))

(cffi:defcfun ("csoundReadScore" csound-read-score) :int
  (csound :pointer)
  (score :string))

(cffi:defcfun ("csoundGetScoreTime" csound-score-time) :double
  (csound :pointer))

(cffi:defcfun ("csoundScoreEvent" csound-score-event) :int
  (csound :pointer)
  (type :char)
  (pfields :pointer)
  (num-fields :long))

(cffi:defcfun ("csoundScoreEventAbsolute" csound-score-event-absolute) :int
  (csound :pointer)
  (type :char)
  (pfields :pointer)
  (num-fields :long)
  (time-ofs :double))

(cffi:defcfun ("csoundSetScoreOffsetSeconds" csound-set-score-offset-seconds) :void
  (csound :pointer)
  (seconds myflt))

(cffi:defcfun "csoundGetMessageLevel" :int
  (csound :pointer))

(cffi:defcfun "csoundSetMessageLevel" :void
  (csound :pointer)
  (message-level :int))


(cffi:defcfun ("csoundGetTable" csound-get-table) :int
  (csound :pointer)
  (table-ptr :pointer)
  (table-num :int))

(cffi:defcfun ("csoundTableLength" csound-table-length) :int
  (csound :pointer)
  (table :int))

(cffi:defcfun ("csoundTableGet" csound-table-get) myflt
  (csound :pointer)
  (table :int)
  (index :int))

(cffi:defcfun ("csoundTableSet" csound-table-set) :void
  (csound :pointer)
  (table :int)
  (index :int)
  (value myflt))

(cffi:defcfun ("csoundGetRandomSeedFromTime" csound-random-seed-from-time) :uint32)

(cffi:defcfun ("csoundGetChannelDatasize" csound-get-channel-datasize) :int
  (csound :pointer)
  (name :string))

(cffi:defcfun ("csoundGetControlChannel" csound-get-control-channel) myflt
  (csound :pointer)
  (name :string))

(cffi:defcfun ("csoundSetControlChannel" csound-set-control-channel) :void
  (csound :pointer)
  (name :string)
  (val myflt))

(cffi:defcfun ("csoundGetAudioChannel" csound-get-audio-channel) :void
  (csound :pointer)
  (name :string)
  (samples :pointer))

(cffi:defcfun ("csoundSetAudioChannel" csound-set-audio-channel) :void
  (csound :pointer)
  (name :string)
  (samples :pointer))

(cffi:defcfun "csoundSetInputChannelCallback" :void
  (csound :pointer)
  (input-channel-callback :pointer))

(cffi:defcfun "csoundGetChannelPtr" :int
  (csound :pointer)
  (myflt :pointer)
  (name :string)
  (type :int))



;;; channels 
(defconstant +csound-control-channel+ 1)
(defconstant +csound-audio-channel+ 2)
(defconstant +csound-string-channel+ 3)
(defconstant +csound-input-channel+ 16)
(defconstant +csound-output-channel+ 32)

;;; message level
(defconstant +note-amplitude-messages+ 1)
(defconstant +samples-out-of-range-message+ 2)
(defconstant +warning-messages+ 4)
(defconstant +benchmark-information+ 128)
