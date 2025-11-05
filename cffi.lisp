;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2025.11.05 byulparan@gmail.com
;; 
;; 

(in-package #:csound)

;;;;;;;;;;;;;;;;;;;;;;
;; Data Structures  ;;
;;;;;;;;;;;;;;;;;;;;;;

(cffi:defcstruct csound-params
  (debug-mode :int)
  (buffer-frames :int)
  (hardware-buffer-frames :int)
  (displays :int)
  (ascii-graphs :int)
  (postscript-graphs :int)
  (message-level :int)
  (tempo :int)
  (ring-bell :int)
  (use-cscore :int)
  (terminate-on-midi :int)
  (heartbeat :int)
  (defer-gen01-load :int)
  (midi-key :int)
  (midi-key-cps :int)
  (midi-key-oct :int)
  (midi-key-pch :int)
  (midi-velocity :int)
  (midi-velocity-amp :int)
  (no-default-paths :int)
  (number-of-threads :int)
  (syntax-check-only :int)
  (csd-line-counts :int)
  (compute-weights :int)
  (realtime-mode :int)
  (sample-accurate :int)
  (sample-rate-override myflt)
  (control-rate-override myflt)
  (nchnls-override :int)
  (nchnls-i-override :int)
  (e0dbfs-override myflt)
  (daemon :int)
  (ksmps-override :int)
  (FFT-library :int))


(cffi:defcstruct control-channel-hints
  (behav :int)
  (dflt myflt)
  (min myflt)
  (max myflt)
  (x :int)
  (y :int)
  (width :int)
  (height :int)
  (attributes :pointer))


;;;;;;;;;;;;
;;  enum  ;;
;;;;;;;;;;;;

;; channel-behavior
(defconstant +csound-control-channel-no-hints+ 0)
(defconstant +csound-control-channel-int+ 1)
(defconstant +csound-control-channel-lin+ 2)
(defconstant +csound-control-channel-exp+ 3)


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


;;;;;;;;;;;;;;;;;
;;  functions  ;;
;;;;;;;;;;;;;;;;;


(defmacro define-cfun (name return-type document &body body)
  "binding C foreign function and export it"
  `(progn
     (cffi:defcfun ,name ,return-type
       ,document
       ,@body)
     (export ',(second name))))



;; Debugger

;; Instantiation

(define-cfun ("csoundCreate" csound-create) :pointer
  "Creates an instance of Csound.
Returns an opaque pointer that must be passed to most Csound API functions. The hostData parameter can be NULL, or it can be a pointer to any sort of data; this pointer can be accessed from the Csound instance that is passed to callback routines."
  (host-data :pointer))


(define-cfun ("csoundDestroy" csound-destroy) :void
  "Destroys an instance of Csound."
  (csound :pointer))


(define-cfun ("csoundGetAPIVersion" csound-get-api-version) :int
  "Returns the API version number times 100 (1.00 = 100).")


(define-cfun ("csoundGetVersion" csound-get-version) :int
  "Returns the version number times 1000 (5.00.0 = 5000).")


(define-cfun ("csoundInitialize" csound-initialize) :int
  "Initialise Csound library with specific flags.
This function is called internally by csoundCreate(), so there is generally no need to use it explicitly unless you need to avoid default initilization that sets signal handlers and atexit() callbacks. Return value is zero on success, positive if initialisation was done already, and negative on error."
  (flags :int))


(define-cfun ("csoundLoadPlugins" csound-load-plugins) :int
  "Loads all plugins from a given directory."
  (csound :pointer)
  (dir :string))


(define-cfun ("csoundSetOpcodeDir" csound-set-opcode-dir) :void
  "Sets an opcodedir override for csoundCreate()"
  (s :string))


;; Performance

(define-cfun ("csoundCleanup" csound-cleanup) :int
  "Prints information about the end of a performance, and closes audio and MIDI devices.
Note: after calling csoundCleanup(), the operation of the perform functions is undefined."
  (csound :pointer))


(define-cfun ("csoundCompile" csound-compile) :int
  "Compiles Csound input files (such as an orchestra and score, or CSD) as directed by the supplied command-line arguments, but does not perform them.
Returns a non-zero error code on failure. This function cannot be called during performance, and before a repeated call, csoundReset() needs to be called. In this (host-driven) mode, the sequence of calls should be as follows: /code csoundCompile(csound, argc, argv); while (!csoundPerformBuffer(csound)); csoundCleanup(csound); csoundReset(csound); /endcode Calls csoundStart() internally. Can only be called again after reset (see csoundReset())"
  (csound :pointer)
  (args :int)
  (argv :pointer))


(define-cfun ("csoundCompileArgs" csound-compile-args) :int
  "Read arguments, parse and compile an orchestra, read, process and load a score."
  (csound :pointer)
  (argc :int)
  (argv :pointer))


(define-cfun ("csoundCompileCsd" csound-compile-csd) :int
  "Compiles a Csound input file (CSD, .csd file), but does not perform it.
Returns a non-zero error code on failure.
If csoundStart is called before csoundCompileCsd, the <CsOptions> element is ignored (but csoundSetOption can be called any number of times), the <CsScore> element is not pre-processed, but dispatched as real-time events; and performance continues indefinitely, or until ended by calling csoundStop or some other logic. In this \"real-time\" mode, the sequence of calls should be:

csoundSetOption(\"-an_option\");
csoundSetOption(\"-another_option\");
csoundStart(csound);
csoundCompileCsd(csound, csd_filename);
while (1) {
   csoundPerformBuffer(csound);
   // Something to break out of the loop
   // when finished here...
}
csoundCleanup(csound);

NB: this function can be called repeatedly during performance to replace or add new instruments and events.

But if csoundCompileCsd is called before csoundStart, the <CsOptions> element is used, the <CsScore> section is pre-processed and dispatched normally, and performance terminates when the score terminates, or csoundStop is called. In this \"non-real-time\" mode (which can still output real-time audio and handle real-time events), the sequence of calls should be:

csoundCompileCsd(csound, csd_filename);
csoundStart(csound);
while (1) {
   int finished = csoundPerformBuffer(csound);
   if (finished) break;
}
csoundCleanup(csound);
csoundReset(csound);
"
  (csound :pointer)
  (csd-filename :string))


(define-cfun ("csoundCompileCsdText" csound-compile-csd-text) :int
  "Behaves the same way as csoundCompileCsd, except that the content of the CSD is read from the csd_text string rather than from a file.
This is convenient when it is desirable to package the csd as part of an application or a multi-language piece."
  (csound :pointer)
  (csd-text :string))


(define-cfun ("csoundCompileOrc" csound-compile-orc) :int
  "Parse, and compile the given orchestra from an ASCII string, also evaluating any global space code (i-time only) this can be called during performance to compile a new orchestra.

/code char *orc = \"instr 1 \n a1 rand 0dbfs/4 \n out a1 \n\"; csoundCompileOrc(csound, orc); /endcode"
  (csound :pointer)
  (str :string))


(define-cfun ("csoundCompileOrcAsync" csound-compile-orc-async) :int
  "Async version of csoundCompileOrc().
The code is parsed and compiled, then placed on a queue for asynchronous merge into the running engine, and evaluation. The function returns following parsing and compilation."
  (csound :pointer)
  (str :string))


(define-cfun ("csoundCompileTree" csound-compile-tree) :int
  "Compile the given TREE node into structs for Csound to use this can be called during performance to compile a new TREE."
  (csound :pointer)
  (root :pointer))


(define-cfun ("csoundCompileTreeAsync" csound-compile-tree-async) :int
  "Asynchronous version of csoundCompileTree()"
  (csound :pointer)
  (root :pointer))


(define-cfun ("csoundDeleteTree" csound-delete-tree) :void
  "Free the resources associated with the TREE *tree This function should be called whenever the TREE was created with csoundParseOrc and memory can be deallocated."
  (csound :pointer)
  (tree :pointer))


(define-cfun ("csoundEvalCode" csound-eval-code) myflt
  "Parse and compile an orchestra given on an string, evaluating any global space code (i-time only).

On SUCCESS it returns a value passed to the 'return' opcode in global space /code char *code = \"i1 = 2 + 2 \n return i1 \n\"; MYFLT retval = csoundEvalCode(csound, code); /endcode"
  (csound :pointer)
  (str :string))


(define-cfun ("csoundInitializeCscore" csound-initialize-cscore) :int
  "Prepares an instance of Csound for Cscore processing outside of running an orchestra (i.e.
\"standalone Cscore\"). It is an alternative to csoundCompile(), and csoundPerform*() and should not be used with these functions. You must call this function before using the interface in \"cscore.h\" when you do not wish to compile an orchestra. Pass it the already open FILE* pointers to the input and output score files. It returns CSOUND_SUCCESS on success and CSOUND_INITIALIZATION or other error code if it fails."
  (insco :pointer)
  (outsco :pointer))


(define-cfun ("csoundParseOrc" csound-parse-orc) :pointer
  "Parse the given orchestra from an ASCII string into a TREE.
This can be called during performance to parse new code."
  (csound :pointer)
  (str :string))


(define-cfun ("csoundPerform" csound-perform) :int
  "Senses input events and performs audio output until the end of score is reached (positive return value), an error occurs (negative return value), or performance is stopped by calling csoundStop() from another thread (zero return value).
Note that csoundCompile() or csoundCompileOrc(), csoundReadScore(), csoundStart() must be called first. In the case of zero return value, csoundPerform() can be called again to continue the stopped performance. Otherwise, csoundReset() should be called to clean up after the finished or failed performance."
  (csound :pointer))


(define-cfun ("csoundPerformBuffer" csound-perform-buffer) :int
  "Performs Csound, sensing real-time and score events and processing one buffer's worth (-b frames) of interleaved audio.
Note that csoundCompile must be called first, then call csoundGetOutputBuffer() and csoundGetInputBuffer() to get the pointer to csound's I/O buffers. Returns false during performance, and true when performance is finished"
  (csound :pointer))


(define-cfun ("csoundPerformKsmps" csound-perform-ksmps) :int
  "Senses input events, and performs one control sample worth (ksmps) of audio output.
Note that csoundCompile() or csoundCompileOrc(), csoundReadScore(), csoundStart() must be called first. Returns false during performance, and true when performance is finished. If called until it returns true, will perform an entire score. Enables external software to control the execution of Csound, and to synchronize performance with audio input and output."
  (csound :pointer))


(define-cfun ("csoundReset" csound-reset) :void
  "Resets all internal memory and state in preparation for a new performance.
Enables external software to run successive Csound performances without reloading Csound. Implies csoundCleanup(), unless already called."
  (csound :pointer))


(define-cfun ("csoundStart" csound-start) :int
  "Prepares Csound for performance.
Normally called after compiling a csd file or an orc file, in which case score preprocessing is performed and performance terminates when the score terminates.
However, if called before compiling a csd file or an orc file, score preprocessing is not performed and \"i\" statements are dispatched as real-time events, the <CsOptions> tag is ignored, and performance continues indefinitely or until ended using the API."
  (csound :pointer))


(define-cfun ("csoundStop" csound-stop) :void
  "Stops a csoundPerform() running in another thread.
Note that it is not guaranteed that csoundPerform() has already stopped when this function returns."
  (csound :pointer))


;; UDP server

;; Attributes

(define-cfun ("csoundGet0dBFS" csound-get-0dbfs) myflt
  "Returns the 0dBFS level of the spin/spout buffers."
  (csound :pointer))


(define-cfun ("csoundGetA4" csound-get-a4) myflt
  "Returns the A4 frequency reference."
  (csound :pointer))


(define-cfun ("csoundGetCurrentTimeSamples" csound-get-current-time-samples) :int64
  "Return the current performance time in samples."
  (csound :pointer))


(define-cfun ("csoundGetDebug" csound-get-debug) :int
  "Returns whether Csound is set to print debug messages sent through the DebugMsg() internal API function.
Anything different to 0 means true."
  (csound :pointer))


(define-cfun ("csoundGetHostData" csound-get-host-data) :pointer
  "Returns host data."
  (csound :pointer))


(define-cfun ("csoundGetKr" csound-get-kr) myflt
  "Returns the number of control samples per second."
  (csound :pointer))


(define-cfun ("csoundGetKsmps" csound-get-ksmps) :unsigned-int
  "Returns the number of audio sample frames per control sample."
  (csound :pointer))


(define-cfun ("csoundGetNchnls" csound-get-nchnls) :unsigned-int
  "Returns the number of audio output channels.
Set through the nchnls header variable in the csd file."
  (csound :pointer))


(define-cfun ("csoundGetNchnlsInput" csound-get-nchnls-input) :unsigned-int
  "Returns the number of audio input channels.
Set through the nchnls_i header variable in the csd file. If this variable is not set, the value is taken from nchnls."
  (csound :pointer))


(define-cfun ("csoundGetParams" csound-get-params) :void
  "Get the current set of parameters from a CSOUND instance in a CSOUND_PARAMS structure.
See csoundSetParams()."
  (csound :pointer)
  (p :pointer))


(define-cfun ("csoundGetSizeOfMYFLT" csound-get-size-of-myflt) :int
  "Return the size of MYFLT in bytes.")


(define-cfun ("csoundGetSr" csound-get-sr) myflt
  "Returns the number of audio sample frames per second."
  (csound :pointer))


(define-cfun ("csoundSetDebug" csound-set-debug) :void
  "Sets whether Csound prints debug messages from the DebugMsg() internal API function.
Anything different to 0 means true."
  (csound :pointer)
  (debug :int))


(define-cfun ("csoundSetHostData" csound-set-host-data) :void
  "Sets host data."
  (csound :pointer)
  (host-data :pointer))


(define-cfun ("csoundSetOption" csound-set-option) :int
  "Set a single csound option (flag).
Returns CSOUND_SUCCESS on success. NB: blank spaces are not allowed"
  (csound :pointer)
  (option :string))


(define-cfun ("csoundSetParams" csound-set-params) :void
  "Configure Csound with a given set of parameters defined in the CSOUND_PARAMS structure.
These parameters are the part of the OPARMS struct that are configurable through command line flags. The CSOUND_PARAMS structure can be obtained using csoundGetParams(). These options should only be changed before performance has started."
  (csound :pointer)
  (p :pointer))


(define-cfun ("csoundSystemSr" csound-system-sr) myflt
  "If val > 0, sets the internal variable holding the system HW sr.
Returns the stored value containing the system HW sr."
  (csound :pointer)
  (val myflt))



;; General Input / Output

(define-cfun ("csoundGetInputName" csound-get-input-name) :string
  "Returns the audio input name (-i)."
  (csound :pointer))


(define-cfun ("csoundGetOutputFormat" csound-get-output-format) :void
  "Get output type and format.
type should have space for at least 5 chars excluding termination, and format should have space for at least 7 chars. On return, these will hold the current values for these parameters."
  (csound :pointer)
  (type :pointer)
  (format :pointer))


(define-cfun ("csoundGetOutputName" csound-get-output-name) :string
  "Returns the audio output name (-o)."
  (csound :pointer))


(define-cfun ("csoundSetFileOpenCallback" csound-set-file-open-callback) :void
  "Sets an external callback for receiving notices whenever Csound opens a file.
The callback is made after the file is successfully opened. The following information is passed to the callback: char* pathname of the file; either full or relative to current dir int a file type code from the enumeration CSOUND_FILETYPES int 1 if Csound is writing the file, 0 if reading int 1 if a temporary file that Csound will delete; 0 if not
Pass NULL to disable the callback. This callback is retained after a csoundReset() call."
  (p :pointer)
  (func :pointer))


(define-cfun ("csoundSetInput" csound-set-input) :void
  "Set input source."
  (csound :pointer)
  (name :string))


(define-cfun ("csoundSetMIDIFileInput" csound-set-midi-file-input) :void
  "Set MIDI file input name."
  (csound :pointer)
  (name :string))


(define-cfun ("csoundSetMIDIFileOutput" csound-set-midi-file-output) :void
  "Set MIDI file output name."
  (csound :pointer)
  (name :string))


(define-cfun ("csoundSetMIDIInput" csound-set-midi-input) :void
  "Set MIDI input device name/number."
  (csound :pointer)
  (name :string))


(define-cfun ("csoundSetMIDIOutput" csoud-set-midi-output) :void
  "Set MIDI output device name/number."
  (csound :pointer)
  (name :string))


(define-cfun ("csoundSetOutput" csound-set-output) :void
  "Set output destination, type and format type can be one of \"wav\",\"aiff\", \"au\",\"raw\", \"paf\", \"svx\", \"nist\", \"voc\", \"ircam\",\"w64\",\"mat4\", \"mat5\", \"pvf\",\"xi\", \"htk\",\"sds\",\"avr\",\"wavex\",\"sd2\", \"flac\", \"caf\",\"wve\",\"ogg\",\"mpc2k\",\"rf64\", or NULL (use default or realtime IO).
format can be one of \"alaw\", \"schar\", \"uchar\", \"float\", \"double\", \"long\", \"short\", \"ulaw\", \"24bit\", \"vorbis\", or NULL (use default or realtime IO). For RT audio, use device_id from CS_AUDIODEVICE for a given audio device."
  (csound :pointer)
  (name :string)
  (type :string)
  (format :string))



;; Realtime Audio I/O

(define-cfun ("csoundAddSpinSample" csound-add-spin-sample) :void
  "Adds the indicated sample into the audio input working buffer (spin); this only ever makes sense before calling csoundPerformKsmps().
The frame and channel must be in bounds relative to ksmps and nchnls. NB: the spin buffer needs to be cleared at every k-cycle by calling csoundClearSpinBuffer()."
  (csound :pointer)
  (frame :int)
  (channel :int)
  (sample myflt))


(define-cfun ("csoundClearSpin" csound-clear-spin) :void
  "Clears the input buffer (spin).")


(define-cfun ("csoundGetAudioDevList" csound-get-audio-dev-list) :int
  "This function can be called to obtain a list of available input or output audio devices.
If list is NULL, the function will only return the number of devices (isOutput=1 for out devices, 0 for in devices). If list is non-NULL, then it should contain enough memory for one CS_AUDIODEVICE structure per device. Hosts will typically call this function twice: first to obtain a number of devices, then, after allocating space for each device information structure, pass an array of CS_AUDIODEVICE structs to be filled:

int i,n = csoundGetAudioDevList(csound,NULL,1);
CS_AUDIODEVICE *devs = (CS_AUDIODEVICE *)
    malloc(n*sizeof(CS_AUDIODEVICE));
csoundGetAudioDevList(csound,devs,1);
for(i=0; i < n; i++)
    csound->Message(csound, \" %d: %s (%s)\n\",
          i, devs[i].device_id, devs[i].device_name);
free(devs);"
  (csound :pointer)
  (list :pointer)
  (is-output :int))


(define-cfun ("csoundGetInputBuffer" csound-get-input-buffer) :pointer
  "Returns the address of the Csound audio input buffer.
Enables external software to write audio into Csound before calling csoundPerformBuffer."
  (csound :pointer))


(define-cfun ("csoundGetInputBufferSize" csound-input-buffer-size) :long
  "Returns the number of samples in Csound's input buffer."
  (csound :pointer))


(define-cfun ("csoundGetModule" csound-get-module) :int
  "retrieves a module name and type (\"audio\" or \"midi\") given a number Modules are added to list as csound loads them returns CSOUND_SUCCESS on success and CSOUND_ERROR if module number was not found

char *name, *type;
int n = 0;
while(!csoundGetModule(csound, n++, &name, &type))
     printf(\"Module %d:  %s (%s) \n\", n, name, type);"
  (csound :pointer)
  (number :int)
  (name :pointer)
  (type :pointer))


(define-cfun ("csoundGetOutputBuffer" csound-get-output-buffer) :pointer
  "Returns the address of the Csound audio output buffer.
Enables external software to read audio from Csound after calling csoundPerformBuffer."
  (csound :pointer))


(define-cfun ("csoundGetOutputBufferSize" csound-get-output-buffer-size) :long
  "Returns the number of samples in Csound's output buffer."
  (csound :pointer))


(define-cfun ("csoundGetRtPlayUserData" csound-get-rt-play-user-data) :pointer
  "Return pointer to user data pointer for real time audio output."
  (csound :pointer))


(define-cfun ("csoundGetRtRecordUserData" csound-get-rt-record-user-data) :pointer
  "Return pointer to user data pointer for real time audio input."
  (csound :pointer))


(define-cfun ("csoundGetSpin" csound-get-spin) :pointer
  "Returns the address of the Csound audio input working buffer (spin).
Enables external software to write audio into Csound before calling csoundPerformKsmps."
  (csound :pointer))


(define-cfun ("csoundGetSpout" csound-get-spout) :pointer
  "Returns the address of the Csound audio output working buffer (spout).
Enables external software to read audio from Csound after calling csoundPerformKsmps."
  (csound :pointer))


(define-cfun ("csoundGetSpoutSample" csound-get-spout-sample) myflt
  "Returns the indicated sample from the Csound audio output working buffer (spout); only ever makes sense after calling csoundPerformKsmps().
The frame and channel must be in bounds relative to ksmps and nchnls."
  (csound :pointer)
  (frame :int)
  (channel :int))


(define-cfun ("csoundSetAudioDeviceListCallback" csound-set-audio-device-list-callback) :void
    "Sets a function that is called to obtain a list of audio devices.
This should be set by rtaudio modules and should not be set by hosts. (See csoundGetAudioDevList())"
  (csound :pointer)
  (audiodevlist_ :pointer))


(define-cfun ("csoundSetHostImplementedAudioIO" csound-set-host-implemented-audio-io) :void
    "Calling this function with a non-zero 'state' value between csoundCreate() and the start of performance will disable all default handling of sound I/O by the Csound library, allowing the host application to use the spin/spout/input/output buffers directly.
For applications using spin/spout, bufSize should be set to 0. If 'bufSize' is greater than zero, the buffer size (-b) in frames will be set to the integer multiple of ksmps that is nearest to the value specified."
  (csound :pointer)
  (state :int)
  (buf-size :int))


(define-cfun ("csoundSetPlayopenCallback" csound-set-playopen-callback) :void
  "Sets a function to be called by Csound for opening real-time audio playback."
  (csound :pointer)
  (playopen_ :pointer))


(define-cfun ("csoundSetRecopenCallback" csound-set-recopen-callback) :void
  "Sets a function to be called by Csound for opening real-time audio recording."
  (csound :pointer)
  (recopen_ :pointer))


(define-cfun ("csoundSetRTAudioModule" csound-set-rt-audio-module) :void
  "Sets the current RT audio module."
  (csound :pointer)
  (module :string))


(define-cfun ("csoundSetRtcloseCallback" csound-set-rtclose-callback) :void
  "Sets a function to be called by Csound for closing real-time audio playback and recording."
  (csound :pointer)
  (rtclose_ :pointer))


(define-cfun ("csoundSetRtplayCallback" csound-set-rtplay-callback) :void
  "Sets a function to be called by Csound for performing real-time audio playback."
  (csound :pointer)
  (rtplay_ :pointer))


(define-cfun ("csoundSetRtrecordCallback" csound-set-rtrecord-callback) :void
  "Sets a function to be called by Csound for performing real-time audio recording."
  (csound :pointer)
  (rtrecord_ :pointer))


(define-cfun ("csoundSetSpinSample" csound-set-spin-sample) :void
  "Sets the audio input working buffer (spin) to the indicated sample this only ever makes sense before calling csoundPerformKsmps().
The frame and channel must be in bounds relative to ksmps and nchnls."
    (csound :pointer)
  (frame :int)
  (channel :int)
  (sample myflt))



;; Realtime Midi I/O

(define-cfun ("csoundGetMIDIDevList" csound-get-midi-dev-list) :int
  "This function can be called to obtain a list of available input or output midi devices.
If list is NULL, the function will only return the number of devices (isOutput=1 for out devices, 0 for in devices). If list is non-NULL, then it should contain enough memory for one CS_MIDIDEVICE structure per device. Hosts will typically call this function twice: first to obtain a number of devices, then, after allocating space for each device information structure, pass an array of CS_MIDIDEVICE structs to be filled. (see also csoundGetAudioDevList())"
  (csound :pointer)
  (list :pointer)
  (is-output :int))


(define-cfun ("csoundSetExternalMidiErrorStringCallback" csound-set-external-midi-error-string-callback) :void
  "Sets callback for converting MIDI error codes to strings."
  (csound :pointer)
  (func :pointer))


(define-cfun ("csoundSetExternalMidiInCloseCallback" csound-set-external-midi-in-close-callback) :void
  "Sets callback for closing real time MIDI input."
  (csound :pointer)
  (func :pointer))


(define-cfun ("csoundSetExternalMidiInOpenCallback" csound-set-external-midi-in-open-callback) :void
  "Sets callback for opening real time MIDI input."
  (csound :pointer)
  (func :pointer))



(define-cfun ("csoundSetExternalMidiOutCloseCallback" csound-set-external-midi-out-close-callback) :void
  "Sets callback for closing real time MIDI output."
  (csound :pointer)
  (func :pointer))


(define-cfun ("csoundSetExternalMidiOutOpenCallback" csound-set-external-midi-out-open-callback) :void
  "Sets callback for opening real time MIDI output."
  (csound :pointer)
  (func :pointer))


(define-cfun ("csoundSetExternalMidiReadCallback" csound-set-external-midi-read-callback) :void
  "Sets callback for reading from real time MIDI input."
  (csound :pointer)
  (func :pointer))


(define-cfun ("csoundSetExternalMidiWriteCallback" csound-set-external-midi-write-callback) :void
  "Sets callback for writing to real time MIDI output."
  (csound :pointer)
  (func :pointer))


(define-cfun ("csoundSetHostImplementedMIDIIO" csound-set-host-implemented-midi-io) :void
  "call this function with state 1 if the host is implementing MIDI via the callbacks below."
  (csound :pointer)
  (state :int))


(define-cfun ("csoundSetMIDIDeviceListCallback" csound-set-midi-device-list-callback) :void
  "Sets a function that is called to obtain a list of MIDI devices.
This should be set by IO plugins, and should not be used by hosts. (See csoundGetMIDIDevList())"
  (csound :pointer)
  (mididevlist_ :pointer))


(define-cfun ("csoundSetMIDIModule" csound-set-midi-module) :void
  "Sets the current MIDI IO module."
  (csound :pointer)
  (module :string))



;; Score Handling

(define-cfun ("csoundGetScoreOffsetSeconds" csound-get-score-offset-seconds) myflt
  "Returns the score time beginning at which score events will actually immediately be performed (see csoundSetScoreOffsetSeconds())."
  (csound :pointer))


(define-cfun ("csoundGetScoreTime" csound-get-score-time) :double
  "Returns the current score time in seconds since the beginning of performance."
  (csound :pointer))


(define-cfun ("csoundIsScorePending" csound-is-score-pending) :int
  "Sets whether Csound score events are performed or not, independently of real-time MIDI events (see csoundSetScorePending())."
  (csound :pointer))


(define-cfun ("csoundReadScore" csound-read-score) :int
  "Read, preprocess, and load a score from an ASCII string It can be called repeatedly, with the new score events being added to the currently scheduled ones.
Referenced by CsoundTextEvent::operator()()."
  (csound :pointer)
  (str :string))


(define-cfun ("csoundReadScoreAsync" csound-read-score-async) :void
  "Asynchronous version of csoundReadScore()."
  (csound :pointer)
  (str :string))


(define-cfun ("csoundRewindScore" csound-rewind-score) :void
  "Rewinds a compiled Csound score to the time specified with csoundSetScoreOffsetSeconds()."
  (csound :pointer))


(define-cfun ("csoundScoreExtract" csound-score-extract) :int
  "Extracts from 'inFile', controlled by 'extractFile', and writes the result to 'outFile'.
The Csound instance should be initialised before calling this function, and csoundReset() should be called after score extraction to clean up. The return value is zero on success."
  (csound :pointer)
  (in-file :pointer)
  (out-file :pointer)
  (extract-file :pointer))


(define-cfun ("csoundScoreSort" csound-score-sort) :int
  "Sorts score file 'inFile' and writes the result to 'outFile'.
The Csound instance should be initialised before calling this function, and csoundReset() should be called after sorting the score to clean up. On success, zero is returned."
  (csound :pointer)
  (in-file :pointer)
  (out-file :pointer))


(define-cfun ("csoundSetCscoreCallback" csound-set-score-callback) :void
  "Sets an external callback for Cscore processing.
Pass NULL to reset to the internal cscore() function (which does nothing). This callback is retained after a csoundReset() call."
  (csound :pointer)
  (cscore-callback :pointer))


(define-cfun ("csoundSetScoreOffsetSeconds" csound-set-score-offset-seconds) :void
  "Csound score events prior to the specified time are not performed, and performance begins immediately at the specified time (real-time events will continue to be performed as they are received).
Can be used by external software, such as a VST host, to begin score performance midway through a Csound score, for example to repeat a loop in a sequencer, or to synchronize other events with the Csound score."
  (csound :pointer)
  (time myflt))


(define-cfun ("csoundSetScorePending" csound-set-score-pending) :void
  "Sets whether Csound score events are performed or not (real-time events will continue to be performed).
Can be used by external software, such as a VST host, to turn off performance of score events (while continuing to perform real-time events), for example to mute a Csound score while working on other tracks of a piece, or to play the Csound instruments live."
  (csound :pointer)
  (pending :int))



;; Message and Text

(define-cfun ("csoundCreateMessageBuffer" csound-create-message-buffer) :void
  "Creates a buffer for storing messages printed by Csound.
Should be called after creating a Csound instance andthe buffer can be freed by calling csoundDestroyMessageBuffer() before deleting the Csound instance. You will generally want to call csoundCleanup() to make sure the last messages are flushed to the message buffer before destroying Csound. If 'toStdOut' is non-zero, the messages are also printed to stdout and stderr (depending on the type of the message), in addition to being stored in the buffer. Using the message buffer ties up the internal message callback, so csoundSetMessageCallback should not be called after creating the message buffer."
  (csound :pointer)
  (to-std-out :int))


(define-cfun ("csoundDestroyMessageBuffer" csound-destroy-message-buffer) :void
  "Releases all memory used by the message buffer."
  (csound :pointer))


(define-cfun ("csoundGetFirstMessage" csound-get-first-message) :string
  "Returns the first message from the buffer."
  (csound :pointer))


(define-cfun ("csoundGetFirstMessageAttr" csound-get-first-message-attr) :int
  "Returns the attribute parameter (see msg_attr.h) of the first message in the buffer."
  (csound :pointer))


(define-cfun ("csoundGetMessageCnt" csound-get-message-cnt) :int
  "Returns the number of pending messages in the buffer."
  (csound :pointer))


(define-cfun ("csoundGetMessageLevel" csound-get-message-level) :int
  "Returns the Csound message level (from 0 to 231)."
  (csound :pointer))


;; csoundMessage
;; csoundMessageS
;; csoundMessageV

(define-cfun ("csoundPopFirstMessage" csound-pop-first-message) :void
  "Removes the first message from the buffer."
  (csound :pointer))


(define-cfun ("csoundSetDefaultMessageCallback" csound-set-default-message-callback) :void
  ""
  (csound-message-callback_ :pointer))


(define-cfun ("csoundSetMessageCallback" csound-set-message-callback) :void
  "Sets a function to be called by Csound to print an informational message.
This callback is never called on –realtime mode"
  (csound :pointer)
  (csound-message-callback_ :pointer))


(define-cfun ("csoundSetMessageLevel" csound-set-message-level) :void
  "Sets the Csound message level (from 0 to 231)."
  (csound :pointer)
  (message-level :int))


(define-cfun ("csoundSetMessageStringCallback" csound-set-message-string-callback) :void
  "Sets an alternative function to be called by Csound to print an informational message, using a less granular signature.
This callback can be set for –realtime mode. This callback is cleared after csoundReset"
  (csound :pointer)
  (csound-message-str-callback :pointer))



;; Channels, Control and Events

(define-cfun ("csoundDeleteChannelList" csound-delete-channel-list) :void
  "Releases a channel list previously returned by csoundListChannels()."
  (csound :pointer)
  (lst :pointer))


(define-cfun ("csoundGetAudioChannel" csound-get-audio-channel) :void
  "copies the audio channel identified by *name into array *samples which should contain enough memory for ksmps MYFLTs"
  (csound :pointer)
  (name :string)
  (samples :pointer))


(define-cfun ("csoundGetChannelDatasize" csound-get-channel-datasize) :int
  "returns the size of data stored in a channel; for string channels this might change if the channel space gets reallocated Since string variables use dynamic memory allocation in Csound6, this function can be called to get the space required for csoundGetStringChannel()"
  (csound :pointer)
  (name :string))


(define-cfun ("csoundGetChannelLock" csound-get-channel-lock) :pointer
  "Recovers a pointer to a lock for the specified channel called 'name'.
The returned lock can be locked/unlocked with the csoundSpinLock() and csoundSpinUnLock() functions.

Returns
   the address of the lock or NULL if the channel does not exist"
  (csound :pointer)
  (name :string))


(define-cfun ("csoundGetChannelPtr" csound-get-channel-ptr) :int
  "Stores a pointer to the specified channel of the bus in *p, creating the channel first if it does not exist yet.

'type' must be the bitwise OR of exactly one of the following values, CSOUND_CONTROL_CHANNEL control data (one MYFLT value) CSOUND_AUDIO_CHANNEL audio data (csoundGetKsmps(csound) MYFLT values) CSOUND_STRING_CHANNEL string data (MYFLT values with enough space to store csoundGetChannelDatasize() characters, including the NULL character at the end of the string) and at least one of these: CSOUND_INPUT_CHANNEL CSOUND_OUTPUT_CHANNEL If the channel already exists, it must match the data type (control, audio, or string), however, the input/output bits are OR'd with the new value. Note that audio and string channels can only be created after calling csoundCompile(), because the storage size is not known until then.

Return value is zero on success, or a negative error code, CSOUND_MEMORY there is not enough memory for allocating the channel CSOUND_ERROR the specified name or type is invalid or, if a channel with the same name but incompatible type already exists, the type of the existing channel. In the case of any non-zero return value, *p is set to NULL. Note: to find out the type of a channel without actually creating or changing it, set 'type' to zero, so that the return value will be either the type of the channel, or CSOUND_ERROR if it does not exist.

Operations on **p are not thread-safe by default. The host is required to take care of threadsafety by 1) with control channels use __atomic_load() or __atomic_store() gcc atomic builtins to get or set a channel, if available. 2) For string and audio channels (and controls if option 1 is not available), retrieve the channel lock with csoundGetChannelLock() and use csoundSpinLock() and csoundSpinUnLock() to protect access to **p. See Top/threadsafe.c in the Csound library sources for examples. Optionally, use the channel get/set functions provided below, which are threadsafe by default."
  (csound :pointer)
  (p :pointer)
  (name :string)
  (type :int))


(define-cfun ("csoundGetControlChannel" csound-get-control-channel) myflt
  "retrieves the value of control channel identified by *name.
If the err argument is not NULL, the error (or success) code finding or accessing the channel is stored in it."
  (csound :pointer)
  (name :string)
  (err :pointer))


(define-cfun ("csoundGetControlChannelHints" csound-get-control-channel-hints) :int
  "Returns special parameters (assuming there are any) of a control channel, previously set with csoundSetControlChannelHints() or the chnparams opcode.
If the channel exists, is a control channel, the channel hints are stored in the preallocated controlChannelHints_t structure. The attributes member of the structure will be allocated inside this function so it is necessary to free it explicitly in the host.
The return value is zero if the channel exists and is a control channel, otherwise, an error code is returned."
  (csound :pointer)
  (name :string)
  (hints :pointer))


(define-cfun ("csoundGetPvsChannel" csound-get-pvs-channel) :int
  "Receives a PVSDAT fout from the pvsout opcode (f-rate) at channel 'name' Returns zero on success, CSOUND_ERROR if the index is invalid or if fsig framesizes are incompatible.
CSOUND_MEMORY if there is not enough memory to extend the bus"
  (csound :pointer)
  (fout :pointer)
  (name :string))


(define-cfun ("csoundGetStringChannel" csound-get-string-channel) :void
  "copies the string channel identified by *name into *string which should contain enough memory for the string (see csoundGetChannelDatasize() below)"
  (csound :pointer)
  (name :string)
  (string :pointer))


(define-cfun ("csoundInputMessage" csound-input-message) :void
  "nput a NULL-terminated string (as if from a console), used for line events."
  (csound :pointer)
  (message :string))


(define-cfun ("csoundInputMessageAsync" csound-input-message-async) :void
  "Asynchronous version of csoundInputMessage()."
  (csound :pointer)
  (message :string))


(define-cfun ("csoundKeyPress" csound-key-press) :void
  "Set the ASCII code of the most recent key pressed.
This value is used by the 'sensekey' opcode if a callback for returning keyboard events is not set (see csoundRegisterKeyboardCallback())."
  (csound :pointer)
  (c :char))


(define-cfun ("csoundKillInstance" csound-kill-instance) :int
  "Kills off one or more running instances of an instrument identified by instr (number) or instrName (name).
If instrName is NULL, the instrument number is used. Mode is a sum of the following values: 0,1,2: kill all instances (1), oldest only (1), or newest (2) 4: only turnoff notes with exactly matching (fractional) instr number 8: only turnoff notes with indefinite duration (p3 < 0 or MIDI) allow_release, if non-zero, the killed instances are allowed to release."
  (csound :pointer)
  (instr myflt)
  (instr-name :pointer)
  (mode :int)
  (allow-release :int))


(define-cfun ("csoundListChannels" csound-list-channels) :int
  "Returns a list of allocated channels in *lst.
A controlChannelInfo_t structure contains the channel characteristics. The return value is the number of channels, which may be zero if there are none, or CSOUND_MEMORY if there is not enough memory for allocating the list. In the case of no channels or an error, *lst is set to NULL. Notes: the caller is responsible for freeing the list returned in *lst with csoundDeleteChannelList(). The name pointers may become invalid after calling csoundReset()."
  (csound :pointer)
  (lst :pointer))


(define-cfun ("csoundRegisterKeyboardCallback" csound-register-keyboard-callback) :int
  "Registers general purpose callback functions that will be called to query keyboard events.
These callbacks are called on every control period by the sensekey opcode. The callback is preserved on csoundReset(), and multiple callbacks may be set and will be called in reverse order of registration. If the same function is set again, it is only moved in the list of callbacks so that it will be called first, and the user data and type mask parameters are updated. 'typeMask' can be the bitwise OR of callback types for which the function should be called, or zero for all types. Returns zero on success, CSOUND_ERROR if the specified function pointer or type mask is invalid, and CSOUND_MEMORY if there is not enough memory.
The callback function takes the following arguments: void *userData the \"user data\" pointer, as specified when setting the callback void *p data pointer, depending on the callback type unsigned int type callback type, can be one of the following (more may be added in future versions of Csound): CSOUND_CALLBACK_KBD_EVENT CSOUND_CALLBACK_KBD_TEXT called by the sensekey opcode to fetch key codes. The data pointer is a pointer to a single value of type 'int', for returning the key code, which can be in the range 1 to 65535, or 0 if there is no keyboard event. For CSOUND_CALLBACK_KBD_EVENT, both key press and release events should be returned (with 65536 (0x10000) added to the key code in the latter case) as unshifted ASCII codes. CSOUND_CALLBACK_KBD_TEXT expects key press events only as the actual text that is typed. The return value should be zero on success, negative on error, and positive if the callback was ignored (for example because the type is not known)."
  (csound :pointer)
  (func :pointer)
  (user-data :pointer)
  (type :unsigned-int))


(define-cfun ("csoundRegisterSenseEventCallback" csound-register-sense-event-callback) :int
  "Register a function to be called once in every control period by sensevents().
Any number of functions may be registered, and will be called in the order of registration. The callback function takes two arguments: the Csound instance pointer, and the userData pointer as passed to this function. This facility can be used to ensure a function is called synchronously before every csound control buffer processing. It is important to make sure no blocking operations are performed in the callback. The callbacks are cleared on csoundCleanup(). Returns zero on success."
  (csound :pointer)
  (func :pointer)
  (user-data :pointer))


(define-cfun ("csoundRemoveKeyboardCallback" csound-remove-keyboard-callback) :void
  "Removes a callback previously set with csoundRegisterKeyboardCallback()."
  (csound :pointer)
  (func :pointer))


(define-cfun ("csoundScoreEvent" csound-score-event) :int
  "Send a new score event.
'type' is the score event type ('a', 'i', 'q', 'f', or 'e'). 'numFields' is the size of the pFields array. 'pFields' is an array of floats with all the pfields for this event, starting with the p1 value specified in pFields[0].
Referenced by CsoundScoreEvent::operator()()."
  (csound :pointer)
  (type :char)
  (p-fields :pointer)
  (num-fields :long))


(define-cfun ("csoundScoreEventAbsolute" csound-score-event-absolute) :int
  "Like csoundScoreEvent(), this function inserts a score event, but at absolute time with respect to the start of performance, or from an offset set with time_ofs."
  (csound :pointer)
  (type :char)
  (p-fields :pointer)
  (num-fields :long)
  (time-ofs :double))


(define-cfun ("csoundScoreEventAbsoluteAsync" csound-score-event-absolute-async) :void
  "Asynchronous version of csoundScoreEventAbsolute()."
  (csound :pointer)
  (type :char)
  (p-fields :pointer)
  (num-fields :long)
  (time-ofs :double))


(define-cfun ("csoundScoreEventAsync" csound-score-event-async) :void
  "Asynchronous version of csoundScoreEvent()."
  (csound :pointer)
  (type :char)
  (p-fields :pointer)
  (num-fields :long))


(define-cfun ("csoundSetAudioChannel" csound-set-audio-channel) :void
  "sets the audio channel identified by *name with data from array *samples which should contain at least ksmps MYFLTs"
  (csound :pointer)
  (name :string)
  (samples :pointer))


(define-cfun ("csoundSetControlChannel" csound-set-control-channel) :void
  "sets the value of control channel identified by *name"
  (csound :pointer)
  (name :string)
  (val myflt))


(define-cfun ("csoundSetControlChannelHints" csound-set-control-channel-hints) :int
  "Set parameters hints for a control channel.
These hints have no internal function but can be used by front ends to construct GUIs or to constrain values. See the controlChannelHints_t structure for details. Returns zero on success, or a non-zero error code on failure: CSOUND_ERROR: the channel does not exist, is not a control channel, or the specified parameters are invalid CSOUND_MEMORY: could not allocate memory"
  (csound :pointer)
  (name :string)
  (hints (:struct control-channel-hints)))


(define-cfun ("csoundSetInputChannelCallback" csound-set-input-channel-callback) :void
  "Sets the function which will be called whenever the invalue opcode is used."
  (csound :pointer)
  (input-channel-callback :pointer))


(define-cfun ("csoundSetOutputChannelCallback" csound-set-output-channel-callback) :void
  "Sets the function which will be called whenever the outvalue opcode is used."
  (csound :pointer)
  (output-channel-callback :pointer))


(define-cfun ("csoundSetPvsChannel" csound-set-pvs-channel) :int
  "Sends a PVSDATEX fin to the pvsin opcode (f-rate) for channel 'name'.
Returns zero on success, CSOUND_ERROR if the index is invalid or fsig framesizes are incompatible. CSOUND_MEMORY if there is not enough memory to extend the bus."
  (csound :pointer)
  (fin :pointer)
  (name :string))


(define-cfun ("csoundSetStringChannel" csound-set-string-channel) :void
  "sets the string channel identified by *name with *string"
  (csound :pointer)
  (name :string)
  (string :pointer))


;; Tables

(define-cfun ("csoundGetNamedGEN" csound-get-named-gen) :void
  "Gets the GEN name from a number num, if this is a named GEN The final parameter is the max len of the string (excluding termination)"
  (csound :pointer)
  (num :int)
  (name :pointer)
  (len :int))


(define-cfun ("csoundGetTable" csound-get-table) :int
  "Stores pointer to function table 'tableNum' in *tablePtr, and returns the table length (not including the guard point).
If the table does not exist, *tablePtr is set to NULL and -1 is returned."
  (table-ptr :pointer)
  (table-num :int))


(define-cfun ("csoundGetTableArgs" csound-get-table-args) :int
  "Stores pointer to the arguments used to generate function table 'tableNum' in *argsPtr, and returns the number of arguments used.
If the table does not exist, *argsPtr is set to NULL and -1 is returned. NB: the argument list starts with the GEN number and is followed by its parameters. eg. f 1 0 1024 10 1 0.5 yields the list {10.0,1.0,0.5}"
  (csound :pointer)
  (args-ptr :pointer)
  (table-num :int))


(define-cfun ("csoundIsNamedGEN" csound-is-named-gen) :int
  "Checks if a given GEN number num is a named GEN if so, it returns the string length (excluding terminating NULL char) Otherwise it returns 0."
  (csound :pointer)
  (num :int))


(define-cfun ("csoundTableCopyIn" csound-table-copy-in) :void
  "Copy the contents of an array *src into a given function table The table number is assumed to be valid, and the table needs to have sufficient space to receive all the array contents."
  (csound :pointer)
  (table :int)
  (src :pointer))


(define-cfun ("csoundTableCopyInAsync" csound-table-copy-in-async) :void
  "Asynchronous version of csoundTableCopyIn()"
  (csound :pointer)
  (table :int)
  (src :pointer))


(define-cfun ("csoundTableCopyOut" csound-table-copy-out) :void
  "Copy the contents of a function table into a supplied array *dest The table number is assumed to be valid, and the destination needs to have sufficient space to receive all the function table contents."
  (csound :pointer)
  (table :int)
  (dest :pointer))


(define-cfun ("csoundTableCopyOutAsync" csound-table-copy-out-async) :void
  "Asynchronous version of csoundTableCopyOut()"
  (csound :pointer)
  (table :int)
  (dest :pointer))


(define-cfun ("csoundTableGet" csound-table-get) myflt
  "Returns the value of a slot in a function table.
The table number and index are assumed to be valid."
  (csound :pointer)
  (table :int)
  (index :int))


(define-cfun ("csoundTableLength" csound-table-length) :int
  "Returns the length of a function table (not including the guard point), or -1 if the table does not exist."
  (csound :pointer)
  (table :int))


(define-cfun ("csoundTableSet" csound-table-set) :void
  "Sets the value of a slot in a function table.
The table number and index are assumed to be valid."
  (csound :pointer)
  (table :int)
  (index :int)
  (value myflt))


;; Function table display

;; Opcodes

(define-cfun ("csoundAppendOpcode" csound-append-opcode) :int
  "Appends an opcode implemented by external software to Csound's internal opcode list.
The opcode list is extended by one slot, and the parameters are copied into the new slot. Returns zero on success."
  (csound :pointer)
  (opname :string)
  (dsblksz :int)
  (flags :int)
  (thread :int)
  (outypes :string)
  (intypes :string)
  (iopadr :pointer)
  (kopadr :pointer)
  (aopdar :pointer))


(define-cfun ("csoundDisposeOpcodeList" csound-dispose-opcode-list) :void
  "Releases an opcode list."
  (csound :pointer)
  (opcodelist :pointer))


(define-cfun ("csoundGetNamedGens" csound-get-named-gens) :pointer
  "Finds the list of named gens."
  (csound :pointer))


(define-cfun ("csoundNewOpcodeList" csound-new-opcode-list) :int
  "Gets an alphabetically sorted list of all opcodes.
Should be called after externals are loaded by csoundCompile(). Returns the number of opcodes, or a negative error code on failure. Make sure to call csoundDisposeOpcodeList() when done with the list."
  (csound :pointer)
  (opcodelist :pointer))


;; Threading and concurrency

;; Miscellaneous functions
(define-cfun ("csoundRunCommand" csound-run-command) :long
  "Runs an external command with the arguments specified in 'argv'.
argv[0] is the name of the program to execute (if not a full path file name, it is searched in the directories defined by the PATH environment variable). The list of arguments should be terminated by a NULL pointer. If 'noWait' is zero, the function waits until the external program finishes, otherwise it returns immediately. In the first case, a non-negative return value is the exit status of the command (0 to 255), otherwise it is the PID of the newly created process. On error, a negative value is returned."
  (argv :pointer)
  (no-wait :int))


(define-cfun ("csoundRunUtility" csound-run-utility) :int
  "Run utility with the specified name and command line arguments.
Should be called after loading utility plugins. Use csoundReset() to clean up after calling this function. Returns zero if the utility was run successfully."
  (csound :pointer)
  (name :string)
  (argc :int)
  (argv :pointer))


(define-cfun ("csoundSeedRandMT" csound-seed-rand-mt) :void
  "Initialise Mersenne Twister (MT19937) random number generator, using 'keyLength' unsigned 32 bit values from 'initKey' as seed.
If the array is NULL, the length parameter is used for seeding."
  (p :pointer)
  (init-key :pointer)
  (key-length :unsigned-int))


(define-cfun ("csoundSetGlobalEnv" csound-set-global-env) :int
  "Set the global value of environment variable 'name' to 'value', or delete variable if 'value' is NULL.
It is not safe to call this function while any Csound instances are active. Returns zero on success."
  (name :string)
  (value :string))


(define-cfun ("csoundSetLanguage" csound-set-language) :void
  "Set language to 'lang_code' (lang_code can be for example CSLANGUAGE_ENGLISH_UK or CSLANGUAGE_FRENCH or many others, see n_getstr.h for the list of languages).
This affects all Csound instances running in the address space of the current process. The special language code CSLANGUAGE_DEFAULT can be used to disable translation of messages and free all memory allocated by a previous call to csoundSetLanguage(). csoundSetLanguage() loads all files for the selected language from the directory specified by the CSSTRNGS environment variable."
  (lang-code :int))


(define-cfun ("csoundWriteCircularBuffer" csound-write-circurlar-buffer) :int
  "Write to circular buffer.

Parameters
  csound  This value is currently ignored.
  p	  pointer to an existing circular buffer
  inp	  buffer with at least items number of elements to be written into circular buffer
  items	  number of samples to write
Returns
  the actual number of items written (0 <= n <= items)"
  (csound :pointer)
  (p :pointer)
  (inp :pointer)
  (items :int))


