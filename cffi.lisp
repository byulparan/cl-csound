;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2025.11.05 byulparan@gmail.com
;; 
;; 

(in-package #:csound)

;;;;;;;;;;;;;;;;;;;;;;
;; Data Structures  ;;
;;;;;;;;;;;;;;;;;;;;;;

(cffi:defcstruct csound-params
  (odebug :int)
  (sfread :int)
  (sfwrite :int)
  (filetyp :int)
  (inbufsamps :int)
  (outbufsamps :int)
  (informat :int)
  (outformat :int)
  (sfsampsize :int)
  (displays :int)
  (graphsoff :int)
  (postscript :int)
  (msglevel :int)
  (Beatmode :int)
  (oMaxLag :int)
  (Linein :int)
  (RTevents :int)
  (Midiin :int)
  (FMidiin :int)
  (RMidiin :int)
  (ringbell :int)
  (termifend :int)
  (rewrt-hdr :int)
  (heartbeat :int)
  (gen01defer :int)
  (cmdTempo :double)
  (sr-override myflt)
  (kr-override myflt)
  (nchnls-override :int)
  (nchnls-i-override :int)
  (infilename :pointer)
  (outfilename :pointer)
  (Linename :pointer)
  (Midiname :pointer)
  (FMidiname :pointer)
  (Midioutname :pointer)
  (FMidioutname :pointer)
  (midiKey :int)
  (midiKeyCps :int)
  (midiKeyOct :int)
  (midiKeyPch :int)
  (midiVelocity :int)
  (midiVelocityAmp :int)
  (noDefaultPaths :int)
  (numThreads :int)
  (syntaxCheckOnly :int)
  (runUnitTests :int)
  (useCsdLineCounts :int)
  (sampleAccurate :int)
  (realtime :int)
  (e0dbfs-override myflt)
  (daemon :int)
  (quality :double)
  (ksmps-override :int)
  (fft-lib :int)
  (echo :int)
  (limiter myflt)
  (sr-default myflt)
  (kr-default myflt)
  (mp3-mode :int)
  (redef :int)
  (error-deprecated :int))


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

;; Flags for csoundInitialize().
(defconstant +csoundinit-no-signal-handler+ 1)
(defconstant +csoundinit-no-atexit+         2)


;; channel-behavior
(defconstant +csound-control-channel-no-hints+ 0)
(defconstant +csound-control-channel-int+ 1)
(defconstant +csound-control-channel-lin+ 2)
(defconstant +csound-control-channel-exp+ 3)


;;; channel type
(defconstant +csound-control-channel+ 1)
(defconstant +csound-audio-channel+ 2)
(defconstant +csound-string-channel+ 3)
(defconstant +csound-pvs-channel+ 4)
(defconstant +csound-var-channel+ 5)
(defconstant +csound-array-channel+ 6)
(defconstant +csound-channel-type-mask+ 15)
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



;; @defgroup INSTANTIATION Instantiation

(define-cfun ("csoundInitialize" csound-initialize) :int
  "Initialise Csound library with specific flags. This function is called internally by csoundCreate(), so there is generally no need to use it explicitly unless you need to avoid default initilization that sets signal handlers and atexit() callbacks. Return value is zero on success, positive if initialisation was done already, and negative on error."
  (flags :int))



(define-cfun ("csoundCreate" csound-create) :pointer
  "Creates an instance of Csound. Returns an opaque pointer that must be passed to most Csound API functions. The hostData parameter can be NULL, or it can be a pointer to any sort of data; this pointer can be accessed from the Csound instance that is passed to callback routines. If not NULL the opcodedir parameter sets an override for the plugin module/opcode directory search"
  (host-data :pointer)
  (opcodedir :pointer))


(define-cfun ("csoundDestroy" csound-destroy) :void
  "Destroys an instance of Csound."
  (csound :pointer))


;; @defgroup ATTRIBUTES Attributes

(define-cfun ("csoundGetVersion" csound-get-version) :int
  "Returns the version number times 1000 (5.00.0 = 5000).")

(define-cfun ("csoundGetSr" csound-get-sr) myflt
  "Returns the number of audio sample frames per second."
  (csound :pointer))


(define-cfun ("csoundGetKr" csound-get-kr) myflt
  "Returns the number of control samples per second."
  (csound :pointer))

(define-cfun ("csoundGetKsmps" csound-get-ksmps) :unsigned-int
  "Returns the audio vector size in frames (= sr/kr)"
  (csound :pointer))


(define-cfun ("csoundGetChannels" csound-get-channels) :unsigned-int
  "Returns the number of audio channels in the Csound instance. If isInput = 0, the value of nchnls is returned, otherwise nchnls_i. If this variable is not set, the value is always taken from nchnls."
  (csound :pointer)
  (is-input :int))


(define-cfun ("csoundGet0dBFS" csound-get-0dbfs) myflt
  "Returns the 0dBFS level of the spin/spout buffers."
  (csound :pointer))


(define-cfun ("csoundGetA4" csound-get-a4) myflt
  "Returns the A4 frequency reference."
  (csound :pointer))


(define-cfun ("csoundGetCurrentTimeSamples" csound-get-current-time-samples) :int64
  "Return the current performance time in sample frames"
  (csound :pointer))


(define-cfun ("csoundGetSizeOfMYFLT" csound-get-size-of-myflt) :int
  "Return the size of MYFLT in bytes.")

 
(define-cfun ("csoundGetHostData" csound-get-host-data) :pointer
  "Returns host data."
  (csound :pointer))


(define-cfun ("csoundSetHostData" csound-set-host-data) :void
  "Sets host data."
  (csound :pointer)
  (host-data :pointer))


(define-cfun ("csoundErrCnt" csound-err-cnt) :int
  "Returns the total error count of the current performance."
  (csound :pointer))


(define-cfun ("csoundGetEnv" csound-get-env) :pointer
  "Get pointer to the value of environment variable 'name', searching in this order: local environment of 'csound' (if not NULL), variables set with csoundSetGlobalEnv(), and system environment variables. If 'csound' is not NULL, should be called after csoundCompile(). Return value is NULL if the variable is not set."
  (csound :pointer)
  (name :string))


(define-cfun ("csoundSetGlobalEnv" csound-set-global-env) :int
  "Set the global value of environment variable 'name' to 'value', or delete variable if 'value' is NULL.
It is not safe to call this function while any Csound instances are active. Returns zero on success."
  (name :string)
  (value :string))


(define-cfun ("csoundSetOption" csound-set-option) :int
  "Set a single csound option (flag). Returns CSOUND_SUCCESS on success. This needs to be called after csoundCreate() and before any code is compiled. Multiple options are allowed in one string. Returns zero on success."
  (csound :pointer)
  (option :string))


(define-cfun ("csoundGetParams" csound-get-params) :pointer
  "Get the current set of parameters from a CSOUND instance in a struct CSOUND_PARAMS structure."
  (csound :pointer))


(define-cfun ("csoundGetDebug" csound-get-debug) :int
  "Returns whether Csound is set to print debug messages sent through the DebugMsg() internal API function. Anything different to 0 means true."
  (csound :pointer))


(define-cfun ("csoundSetDebug" csound-set-debug) :void
  "Sets whether Csound prints debug messages from the DebugMsg() internal API function. Anything different to 0 means true."
  (csound :pointer)
  (debug :int))


(define-cfun ("csoundSystemSr" csound-system-sr) myflt
  "If val > 0, sets the internal variable holding the system HW sr. Returns the stored value containing the system HW sr."
  (csound :pointer)
  (val myflt))


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


(define-cfun ("csoundGetMIDIDevList" csound-get-midi-dev-list) :int
  "This function can be called to obtain a list of available input or output midi devices.
If list is NULL, the function will only return the number of devices (isOutput=1 for out devices, 0 for in devices). If list is non-NULL, then it should contain enough memory for one CS_MIDIDEVICE structure per device. Hosts will typically call this function twice: first to obtain a number of devices, then, after allocating space for each device information structure, pass an array of CS_MIDIDEVICE structs to be filled. (see also csoundGetAudioDevList())"
  (csound :pointer)
  (list :pointer)
  (is-output :int))


(define-cfun ("csoundGetMessageLevel" csound-get-message-level) :int
  "Returns the Csound message level (from 0 to 231)."
  (csound :pointer))


(define-cfun ("csoundSetMessageLevel" csound-set-message-level) :void
  "Sets the Csound message level (from 0 to 231)."
  (csound :pointer)
  (message-level :int))



;; @defgroup PERFORMANCE Compilation and performance

(define-cfun ("csoundCompile" csound-compile) :int
  "Compiles Csound input files (such as an orchestra and score, or CSD) as directed by the supplied command-line arguments, but does not perform them.
Returns a non-zero error code on failure. In this mode, the sequence of calls should be as follows:

csoundCompile(csound, argc, argv);
csoundStart(csound);
while (!csoundPerformBuffer(csound));
soundReset(csound);"
  (csound :pointer)
  (args :int)
  (argv :pointer))


(define-cfun ("csoundCompileOrc" csound-compile-orc) :int
  "Parse, and compile the given orchestra from an ASCII string, also evaluating any global space code (i-time only) in synchronous or asynchronous (async = 1) mode.

char *orc = \"instr 1 \n a1 rand 0dbfs/4 \n out a1 \n\";
csoundCompileOrc(csound, orc, 0);"
  (csound :pointer)
  (str :string)
  (async :int))


(define-cfun ("csoundEvalCode" csound-eval-code) myflt
  "Parse and compile an orchestra given on a string, synchronously, evaluating any global space code (i-time only). On SUCCESS it returns a value passed to the 'return' opcode in global space

char *code =
 \"i1 = 2 + 2 \n\"
 \"return i1 \n\";
 MYFLT retval = csoundEvalCode(csound, code);"
  (csound :pointer)
  (str :string))


(define-cfun ("csoundCompileCSD" csound-compile-csd) :int
  "Compiles a Csound input file (CSD, .csd file) or a tx string containing the CSD code, in synchronous or asynchronous (async = 1) mode. Returns a non-zero error code on failure. If csoundStart is called before csoundCompileCsd, the <CsOptions> element is ignored (but csoundSetOption can be called any number of times), the <CsScore> element is dispatched as score events (e.g. as it is done by csoundEventString())

csoundSetOption(\"-an_option\");
csoundSetOption(\"-another_option\");
csoundStart(csound);
csoundCompileCSD(csound, csd_filename, 0);
while (1) {
   csoundPerformKsmps(csound);
   // Something to break out of the loop
   // when finished here...
}

NB: this function can be called repeatedly during performance to replace or add new instruments and events.

But if csoundCompileCsd is called before csoundStart, the <CsOptions> element is used, the <CsScore> section is pre-processed and dispatched normally, and performance terminates when the score terminates.

csoundCompileCSD(csound, csd_filename, 0);
csoundStart(csound);
while (1) {
   int32_t finished = csoundPerformKsmps(csound);
   if (finished) break;
}

if mode = 1, csd contains a full CSD code (rather than a filename). This is convenient when it is desirable to package the csd as part of an application or a multi-language piece."
  (csound :pointer)
  (csd :string)
  (mode :int)
  (async :int))


(define-cfun ("csoundStart" csound-start) :int
  "Prepares Csound for performance. Normally called after compiling a csd file or an orc file, in which case score preprocessing is performed and performance terminates when the score terminates. However, if called before compiling a csd file or an orc file, score preprocessing is not performed and \"i\" statements are dispatched as real-time events, the <CsOptions> tag is ignored, and performance continues indefinitely or until ended using the API."
  (csound :pointer))


(define-cfun ("csoundPerformKsmps" csound-perform-ksmps) :int
  "Senses input events, and performs one block of audio output containing ksmps frames. csoundStart() must be called first. Returns false during performance, and true when performance is finished. If called until it returns true, will perform an entire score. Enables external software to control the execution of Csound, and to synchronize performance with audio input and output."
  (csound :pointer))


(define-cfun ("csoundRunUtility" csound-run-utility) :int
  "Run utility with the specified name and command line arguments. Should be called after loading utility plugins. Use csoundReset() to clean up after calling this function. Returns zero if the utility was run successfully."
  (csound :pointer)
  (name :string)
  (argc :int)
  (argv :pointer))


(define-cfun ("csoundReset" csound-reset) :void
  "Resets all internal memory and state in preparation for a new performance. Enables external software to run successive Csound performances without reloading Csound."
  (csound :pointer))



;; @defgroup AUDIOIO Audio I/O

(define-cfun ("csoundGetSpin" csound-get-spin) :pointer
  "Returns the address of the Csound audio input working buffer (spin). Enables external software to write audio into Csound before calling csoundPerformKsmps."
  (csound :pointer))


(define-cfun ("csoundGetSpout" csound-get-spout) :pointer
  "Returns the address of the Csound audio output working buffer (spout). Enables external software to read audio from Csound after calling csoundPerformKsmps."
  (csound :pointer))



;; @defgroup MESSAGES Csound Messages and Text

;; csoundMessage
;; csoundMessageS
;; csoundMessageV

(define-cfun ("csoundSetDefaultMessageCallback" csound-set-default-message-callback) :void
  ""
  (csound-message-callback_ :pointer))


(define-cfun ("csoundSetMessageCallback" csound-set-message-callback) :void
  "Sets a function to be called by Csound to print an informational message. This callback is never called on –realtime mode"
  (csound :pointer)
  (csound-message-callback_ :pointer))


(define-cfun ("csoundSetMessageStringCallback" csound-set-message-string-callback) :void
  "Sets an alternative function to be called by Csound to print an informational message, using a less granular signature. This callback can be set for –realtime mode. This callback is cleared after csoundReset"
  (csound :pointer)
  (csound-message-str-callback :pointer))


(define-cfun ("csoundCreateMessageBuffer" csound-create-message-buffer) :void
  "Creates a buffer for storing messages printed by Csound.
Should be called after creating a Csound instance andthe buffer can be freed by calling csoundDestroyMessageBuffer() before deleting the Csound instance. You will generally want to call csoundCleanup() to make sure the last messages are flushed to the message buffer before destroying Csound. If 'toStdOut' is non-zero, the messages are also printed to stdout and stderr (depending on the type of the message), in addition to being stored in the buffer. Using the message buffer ties up the internal message callback, so csoundSetMessageCallback should not be called after creating the message buffer."
  (csound :pointer)
  (to-std-out :int))


(define-cfun ("csoundGetFirstMessage" csound-get-first-message) :string
  "Returns the first message from the buffer."
  (csound :pointer))


(define-cfun ("csoundGetFirstMessageAttr" csound-get-first-message-attr) :int
  "Returns the attribute parameter (see msg_attr.h) of the first message in the buffer."
  (csound :pointer))


(define-cfun ("csoundPopFirstMessage" csound-pop-first-message) :void
  "Removes the first message from the buffer."
  (csound :pointer))


(define-cfun ("csoundGetMessageCnt" csound-get-message-cnt) :int
  "Returns the number of pending messages in the buffer."
  (csound :pointer))


(define-cfun ("csoundDestroyMessageBuffer" csound-destroy-message-buffer) :void
  "Releases all memory used by the message buffer."
  (csound :pointer))



;; @defgroup CONTROLEVENTS Channels, Control and Events

(define-cfun ("csoundGetChannelPtr" csound-get-channel-ptr) :int
  "Stores a pointer to the specified channel of the bus in *p, creating the channel first if it does not exist yet. 'type' must be the bitwise OR of exactly one of the following values,
  CSOUND_CONTROL_CHANNEL
     control data (one MYFLT value) - (MYFLT **) pp
  CSOUND_AUDIO_CHANNEL
     audio data (csoundGetKsmps(csound) MYFLT values) - (MYFLT **) pp
  CSOUND_STRING_CHANNEL
     string data as a STRINGDAT structure - (STRINGDAT **) pp
    (see csoundGetStringData() and csoundSetStringData())
  CSOUND_ARRAY_CHANNEL
    array data as an ARRAYDAT structure - (ARRAYDAT **) pp
    (see csoundArrayData***(), csoundSetArrayData(),
    csoundGetArrayData(), and csoundInitArrayData())
  CSOUND_PVS_CHANNEL
    pvs data as a PVSDATEXT structure - (PVSDAT **) pp
   (see csoundPvsData***(), csoundSetPvsData(),
    csoundGetPvsData(), and csoundInitPvsData())
 and at least one of these:
  CSOUND_INPUT_CHANNEL
  CSOUND_OUTPUT_CHANNEL
If the channel already exists, it must match the data type (control, audio, or string), however, the input/output bits are OR'd with the new value. Note that audio and string channels can only be created after calling csoundCompile(), because the storage size is not known until then.

Return value is zero on success, or a negative error code, CSOUND_MEMORY there is not enough memory for allocating the channel CSOUND_ERROR the specified name or type is invalid or, if a channel with the same name but incompatible type already exists, the type of the existing channel. In the case of any non-zero return value, *p is set to NULL. Note: to find out the type of a channel without actually creating or changing it, set 'type' to zero, so that the return value will be either the type of the channel, or CSOUND_ERROR if it does not exist.

Operations on **p are not thread-safe by default. The host is required to take care of threadsafety by
  1) with control channels use __atomic_load() or __atomic_store() gcc atomic builtins to get or set a channel, if available.
  2) For string and audio channels (and controls if option 1 is not available), use csoundLockChannel() and csoundUnlockChannel() when accessing/modifying channel data at **p.

See Top/threadsafe.c in the Csound library sources for examples. Optionally, use the channel get/set functions provided below, which are threadsafe by default."
  (csound :pointer)
  (p :pointer)
  (name :string)
  (type :int))


(define-cfun ("csoundGetChannelVarTypeName" csound-get-channel-var-type-name) :char
  "Returns the var type for a channel name or NULL if the channel was not found. Currently supported channel var types are 'k' (control), 'a' (audio), 'S' (string), 'f' (pvs), and '[' (array)."
  (csound :pointer)
  (name :string))


(define-cfun ("csoundListChannels" csound-list-channels) :int
  "Returns a list of allocated channels in *lst. A controlChannelInfo_t structure contains the channel characteristics. The return value is the number of channels, which may be zero if there are none, or CSOUND_MEMORY if there is not enough memory for allocating the list. In the case of no channels or an error, *lst is set to NULL. Notes: the caller is responsible for freeing the list returned in *lst with csoundDeleteChannelList(). The name pointers may become invalid after calling csoundReset()."
  (csound :pointer)
  (lst :pointer))


(define-cfun ("csoundDeleteChannelList" csound-delete-channel-list) :void
  "Releases a channel list previously returned by csoundListChannels()."
  (csound :pointer)
  (lst :pointer))


(define-cfun ("csoundSetControlChannelHints" csound-set-control-channel-hints) :int
  "Set parameters hints for a control channel. These hints have no internal function but can be used by front ends to construct GUIs or to constrain values. See the controlChannelHints_t structure for details. Returns zero on success, or a non-zero error code on failure:
  CSOUND_ERROR: the channel does not exist, is not a control channel, or the specified parameters are invalid
  CSOUND_MEMORY: could not allocate memory"
  (csound :pointer)
  (name :string)
  (hints (:struct control-channel-hints)))


(define-cfun ("csoundGetControlChannelHints" csound-get-control-channel-hints) :int
  "Returns special parameters (assuming there are any) of a control channel, previously set with csoundSetControlChannelHints() or the chnparams opcode. If the channel exists, is a control channel, the channel hints are stored in the preallocated controlChannelHints_t structure. The attributes member of the structure will be allocated inside this function so it is necessary to free it explicitly in the host. The return value is zero if the channel exists and is a control channel, otherwise, an error code is returned."
  (csound :pointer)
  (name :string)
  (hints :pointer))


(define-cfun ("csoundLockChannel" csound-lock-channel) :void
  "locks access to the channel allowing access to data in a threadsafe manner"
  (csound :pointer)
  (channel :string))


(define-cfun ("csoundUnlockChannel" csound-unlock-channel) :void
  "unlocks access to the channel, allowing access to data from elsewhere."
  (csound :pointer)
  (channel :string))


(define-cfun ("csoundGetControlChannel" csound-get-control-channel) myflt
  "retrieves the value of control channel identified by *name. If the err argument is not NULL, the error (or success) code finding or accessing the channel is stored in it."
  (csound :pointer)
  (name :string)
  (err :pointer))


(define-cfun ("csoundSetControlChannel" csound-set-control-channel) :void
  "sets the value of control channel identified by *name"
  (csound :pointer)
  (name :string)
  (val myflt))


(define-cfun ("csoundGetAudioChannel" csound-get-audio-channel) :void
  "copies the audio channel identified by *name into array *samples which should contain enough memory for ksmps MYFLTs"
  (csound :pointer)
  (name :string)
  (samples :pointer))


(define-cfun ("csoundSetAudioChannel" csound-set-audio-channel) :void
  "sets the audio channel identified by *name with data from array *samples which should contain at least ksmps MYFLTs"
  (csound :pointer)
  (name :string)
  (samples :pointer))


(define-cfun ("csoundGetStringChannel" csound-get-string-channel) :void
  "copies the string channel identified by *name into *string which should contain enough memory for the string (see csoundGetChannelDatasize() below)"
  (csound :pointer)
  (name :string)
  (string :pointer))


(define-cfun ("csoundSetStringChannel" csound-set-string-channel) :void
  "sets the string channel identified by *name with *string"
  (csound :pointer)
  (name :string)
  (string :pointer))


(define-cfun ("csoundInitArrayChannel" csound-init-array-channel) :pointer
"Create and initialise an array channel with a given array type
  - \"a\" (audio sigs): each item is a ksmps-size MYFLT array
  - \"i\" (init vars): each item is a MYFLT
  - \"S\" (strings): each item is a STRINGDAT (see csoundGetStringData() and csoundSetStringData())
  - \"k\" (control sigs): each item is a MYFLT
  dimensions - number of array dimensions
  sizes - sizes for each dimension
 returns the ARRAYDAT for the requested channel or NULL on error
    NB: if the channel exists and has already been initialised,
    this function is a non-op."
  (csound :pointer)
  (name :string)
  (type :string)
  (dimensions :int)
  (sizes :pointer))


(define-cfun ("csoundArrayDataType" csound-array-data-type) :pointer
  "Get the type of data the ARRAYDAT adat, returning
  - \"a\" (audio sigs): each item is a ksmps-size MYFLT array
  - \"i\" (init vars): each item is a MYFLT
  - \"S\" (strings): each item is a STRINGDAT (see csoundGetStringData() and csoundSetStringData())
  - \"k\" (control sigs): each item is a MYFLT"
  (adat :pointer))


(define-cfun ("csoundArrayDataDimensions" csound-array-data-dimensions) :int
  "Get the dimensions of the ARRAYDAT adat."
  (adat :pointer))


(define-cfun ("csoundArrayDataSizes" csound-array-data-sizes) :pointer
  "Get the sizes of each dimension of the ARRAYDAT adat;"
  (adat :pointer))


(define-cfun ("csoundSetArrayData" csound-set-array-data) :void
  "Set the data in the ARRAYDAT adat"
  (adat :pointer)
  (data :pointer))


(define-cfun ("csoundGetArrayData" csound-get-array-data) :pointer
  "Get the data from the ARRAYDAT adat"
  (adat :pointer))


(define-cfun ("csoundGetStringData" csound-get-string-data) :pointer
  "Get a null-terminated string from a STRINGDAT structure"
  (csound :pointer)
  (sdata :pointer))


(define-cfun ("csoundSetStringData" csound-set-string-data) :void
  "Set a STRINGDAT structure with a null-terminated string"
  (csound :pointer)
  (sdata :pointer)
  (str :string))


(define-cfun ("csoundInitPvsChannel" csound-init-pvs-channel) :pointer
  "Create/initialise an Fsig channel with
  size - FFT analysis size
  overlap - analysis overlap size
  winsize - analysis window size
  wintype - analysis window type (see pvsdat types enumeration)
  format - analysis data format (see pvsdat format enumeration)
returns the PVSDAT for the requested channel or NULL on error  NB: if the channel exists and has already been initialised, this function is a non-op."
  (csound :pointer)
  (name :string)
  (size :int)
  (overlap :int)
  (winsize :int)
  (wintype :int)
  (format :int))


(define-cfun ("csoundPvsDataFFTSize" csound-pvs-data-fft-size) :int
  "Get the analysis FFT size used by the PVSDAT pvsdat"
  (pvsdat :pointer))


(define-cfun ("csoundPvsDataOverlap" csound-pvs-data-overlap) :int
  "Get the analysis overlap size used by the PVSDAT pvsdat"
  (pvsdat :pointer))


(define-cfun ("csoundPvsDataWindowSize" csound-pvs-data-window-size) :int
  "Get the analysis window size used by the PVSDAT pvsdat"
  (pvsdat :pointer))


(define-cfun ("csoundPvsDataFormat" csound-pvs-data-format) :int
  "Get the analysis data format used by the PVSDAT pvsdat"
  (pvsdat :pointer))


(define-cfun ("csoundPvsDataFramecount" csound-pvs-data-framecount) :unsigned-int
  "Get the current framecount from PVSDAT pvsdat"
  (pvsdat :pointer))


(define-cfun ("csoundGetPvsData" csound-get-pvs-data) :pointer
  "Get the analysis data frame from the PVSDAT pvsdat"
  (pvsdat :pointer))


(define-cfun ("csoundSetPvsData" csound-set-pvs-data) :void
  "Set the analysis data frame in the PVSDAT pvsdat"
  (pvsdat :pointer)
  (frame :pointer))


(define-cfun ("csoundGetChannelDatasize" csound-get-channel-datasize) :int
  "returns the size of data stored in a channel; for string channels this might change if the channel space gets reallocated Since string variables use dynamic memory allocation in Csound6, this function can be called to get the space required for csoundGetStringChannel()"
  (csound :pointer)
  (name :string))


(define-cfun ("csoundSetInputChannelCallback" csound-set-input-channel-callback) :void
  "Sets the function which will be called whenever the invalue opcode is used."
  (csound :pointer)
  (input-channel-callback :pointer))


(define-cfun ("csoundSetOutputChannelCallback" csound-set-output-channel-callback) :void
  "Sets the function which will be called whenever the outvalue opcode is used."
  (csound :pointer)
  (output-channel-callback :pointer))


(define-cfun ("csoundEvent" csound-event) :void
  "Schedule a new realtime event. 'type' is the event type
  type 0 - instrument instance     CS_INSTR_EVENT
  type 1 - function table instance CS_TABLE_EVENT
  type 2 - end event               CS_END_EVENT
  event parameters is nparams MYFLT array with the event parameters (p-fields)
  optionally run asynchronously (async = 1)
  NB: This is non-op before csoundStart() is called."
  (csound :pointer)
  (type :int)
  (params :pointer)
  (nparams :int)
  (async :int))


(define-cfun ("csoundEventString" csound-event-string) :void
  "Schedule new score or realtime event(s) as a NULL-terminated string Two operation modes are supported:
  - Score events: any calls before csoundStart() add the string events to the score (before pre-processing) (async should be set to 0).
  - Realtime events: after the engine starts, string events are added to
    the realtime event queue.

Multiple events separated by newlines are possible and score preprocessing (carry, etc) is applied. optionally run asynchronously (async = 1)"
  (csound :pointer)
  (message :string)
  (async :int))


(define-cfun ("csoundGetInstrNumber" csound-get-instr-number) :int
  "Set the ASCII code of the most recent key pressed. This value is used by the 'sensekey' opcode if a callback for returning keyboard events is not set (see csoundRegisterKeyboardCallback())."
  (csound :pointer)
  (name :string))


(define-cfun ("csoundKeyPress" csound-key-press) :void
  "Set the ASCII code of the most recent key pressed.
This value is used by the 'sensekey' opcode if a callback for returning keyboard events is not set (see csoundRegisterKeyboardCallback())."
  (csound :pointer)
  (c :char))


(define-cfun ("csoundRegisterKeyboardCallback" csound-register-keyboard-callback) :int
  "Registers general purpose callback functions that will be called to query keyboard events.
These callbacks are called on every control period by the sensekey opcode. The callback is preserved on csoundReset(), and multiple callbacks may be set and will be called in reverse order of registration. If the same function is set again, it is only moved in the list of callbacks so that it will be called first, and the user data and type mask parameters are updated. 'typeMask' can be the bitwise OR of callback types for which the function should be called, or zero for all types. Returns zero on success, CSOUND_ERROR if the specified function pointer or type mask is invalid, and CSOUND_MEMORY if there is not enough memory.

The callback function takes the following arguments:
  void *userData
    the \"user data\" pointer, as specified when setting the callback
  void *p
    data pointer, depending on the callback type unsigned
  uint32_t type
    callback type, can be one of the following (more may be added in future versions of Csound):
     CSOUND_CALLBACK_KBD_EVENT
     CSOUND_CALLBACK_KBD_TEXT
        called by the sensekey opcode to fetch key codes. The data pointer is a pointer to a single value of type 'int', for returning the key code, which can be in the range 1 to 65535, or 0 if there is no keyboard event. For CSOUND_CALLBACK_KBD_EVENT, both key press and release events should be returned (with 65536 (0x10000) added to the key code in the latter case) as unshifted ASCII codes. CSOUND_CALLBACK_KBD_TEXT expects key press events only as the actual text that is typed.

The return value should be zero on success, negative on error, and positive if the callback was ignored (for example because the type is not known)."
  (csound :pointer)
  (func :pointer)
  (user-data :pointer)
  (type :unsigned-int))


(define-cfun ("csoundRemoveKeyboardCallback" csound-remove-keyboard-callback) :void
  "Removes a callback previously set with csoundRegisterKeyboardCallback()."
  (csound :pointer)
  (func :pointer))



;; @defgroup TABLE Tables


(define-cfun ("csoundTableLength" csound-table-length) :int
  "Returns the length of a function table (not including the guard point), or -1 if the table does not exist."
  (csound :pointer)
  (table :int))


(define-cfun ("csoundGetTable" csound-get-table) :int
  "Stores pointer to function table 'tableNum' in *tablePtr, and returns the table length (not including the guard point).
If the table does not exist, *tablePtr is set to NULL and -1 is returned. NB: this function and the tablePtr returned are not threadsafe"
  (csound :pointer)
  (table-ptr :pointer)
  (table-num :int))


(define-cfun ("csoundGetTableArgs" csound-get-table-args) :int
  "Stores pointer to the arguments used to generate function table 'tableNum' in *argsPtr, and returns the number of arguments used. If the table does not exist, *argsPtr is set to NULL and -1 is returned. NB: the argument list starts with the GEN number and is followed by its parameters. eg. f 1 0 1024 10 1 0.5 yields the list {10.0,1.0,0.5}
This function and the argsPtr returned are not threadsafe"
  (csound :pointer)
  (args-ptr :pointer)
  (table-num :int))


(define-cfun ("csoundTableCopyIn" csound-table-copy-in) :void
  "Copies an array stored in ptable to the function table number given by table, which should exist in the engine. The input array should be at least as long as the table size plus one (guard point required).
This function is threadsafe and can also be run asynchronously"
  (csound :pointer)
  (table :int)
  (ptable :pointer)
  (async :int))


(define-cfun ("csoundTableCopyOut" csound-table-copy-out) :void
  "Copies a function table number given by table, which should exist in the engine, into the array ptable, and have enough space to accommodate the array size.
This function is threadsafe and can also be run asynchronously"
  (csound :pointer)
  (table :int)
  (ptable :pointer)
  (async :int))



;; @defgroup SCOREHANDLING Score Handling


(define-cfun ("csoundGetScoreTime" csound-get-score-time) :double
  "Returns the current score time in seconds since the beginning of performance."
  (csound :pointer))


(define-cfun ("csoundIsScorePending" csound-is-score-pending) :int
  "Sets whether Csound score events are performed or not, independently of real-time MIDI events (see csoundSetScorePending())."
  (csound :pointer))


(define-cfun ("csoundSetScorePending" csound-set-score-pending) :void
  "Sets whether Csound score events are performed or not (real-time events will continue to be performed). Can be used by external software, such as a VST host, to turn off performance of score events (while continuing to perform real-time events), for example to mute a Csound score while working on other tracks of a piece, or to play the Csound instruments live."
  (csound :pointer)
  (pending :int))


(define-cfun ("csoundGetScoreOffsetSeconds" csound-get-score-offset-seconds) myflt
  "Returns the score time beginning at which score events will actually immediately be performed (see csoundSetScoreOffsetSeconds())."
  (csound :pointer))


(define-cfun ("csoundSetScoreOffsetSeconds" csound-set-score-offset-seconds) :void
  "Csound score events prior to the specified time are not performed, and performance begins immediately at the specified time (real-time events will continue to be performed as they are received). Can be used by external software, such as a VST host, to begin score performance midway through a Csound score, for example to repeat a loop in a sequencer, or to synchronize other events with the Csound score."
  (csound :pointer)
  (time myflt))


(define-cfun ("csoundRewindScore" csound-rewind-score) :void
  "Rewinds a compiled Csound score to the time specified with csoundSetScoreOffsetSeconds()."
  (csound :pointer))


(define-cfun ("csoundScoreSort" csound-score-sort) :int
  "Sorts score file 'inFile' and writes the result to 'outFile'.
The Csound instance should be initialised before calling this function, and csoundReset() should be called after sorting the score to clean up. On success, zero is returned."
  (csound :pointer)
  (in-file :pointer)
  (out-file :pointer))


(define-cfun ("csoundScoreExtract" csound-score-extract) :int
  "Extracts from 'inFile', controlled by 'extractFile', and writes the result to 'outFile'.
The Csound instance should be initialised before calling this function, and csoundReset() should be called after score extraction to clean up. The return value is zero on success."
  (csound :pointer)
  (in-file :pointer)
  (out-file :pointer)
  (extract-file :pointer))


(define-cfun ("csoundSleep" csound-sleep) :void
  "Waits for at least the specified number of milliseconds, yielding the CPU to other threads."
  (milliseconds :size))



;; @defgroup OPCODES Opcodes

(define-cfun ("csoundLoadPlugins" csound-load-plugins) :int
  "Loads all plugins from a given directory. Generally called immediately after csoundCreate() to make new opcodes/modules available for compilation and performance."
  (csound :pointer)
  (dir :string))


(define-cfun ("csoundAppendOpcode" csound-append-opcode) :int
  "Appends an opcode implemented by external software to Csound's internal opcode list. The opcode list is extended by one slot, and the parameters are copied into the new slot. Returns zero on success."
  (csound :pointer)
  (opname :string)
  (dsblksz :int)
  (flags :int)
  (outypes :string)
  (intypes :string)
  (init :pointer)
  (perf :pointer)
  (deinit :pointer))





;;
;; CS_PERF_THREAD
;; 

(define-cfun ("csoundCreatePerformanceThread" csound-create-performance-thread) :pointer
  "Runs Csound in a separate thread. The playback (which is paused by default) is stopped by calling stop(), or if an error occurs. The constructor takes a Csound instance pointer as argument; it assumes that ctcsound.compile_() was called successfully before creating the performance thread. Once the playback is stopped for one of the above mentioned reasons, the performance thread return"
  (csound :pointer))


(define-cfun ("csoundDestroyPerformanceThread" csound-destroy-performance-thread) :void
  "Destroys a Csound perfomance thread object"
  (pt :pointer))


(define-cfun ("csoundPerformanceThreadIsRunning" csound-performance-thread-is-running) :int
  "Returns true if the performance thread is running, false otherwise."
  (pt :pointer))


(define-cfun ("csoundPerformanceThreadGetProcessCB" csound-performance-thread-get-process-cb) :pointer
  "Returns the process callback."
  (pt :pointer))


(define-cfun ("csoundPerformanceThreadSetProcessCB" csound-performance-thread-set-process-cb) :void
  "Sets the process callback."
  (pt :pointer)
  (callback :pointer)
  (cb-data :pointer))


(define-cfun ("csoundPerformanceThreadGetCsound" csound-performance-thread-get-csound) :pointer
  "Returns the Csound instance pointer"
  (pt :pointer))


(define-cfun ("csoundPerformanceThreadGetStatus" csound-performance-thread-get-status) :int
  "Returns the current status. Zero if still playing, positive if the end of score was reached or performance was stopped, and negative if an error occured."
  (pt :pointer))


(define-cfun ("csoundPerformanceThreadPlay" csound-performance-thread-play) :void
  "Starts/Continues performance if it was paused"
  (pt :pointer))


(define-cfun ("csoundPerformanceThreadPause" csound-performance-thread-pause) :void
  "Pauses performance"
  (pt :pointer))


(define-cfun ("csoundPerformanceThreadTogglePause" csound-performance-thread-toggle-pause) :void
  "Toggles performance depending on its state (playing, paused)"
  (pt :pointer))


(define-cfun ("csoundPerformanceThreadStop" csound-performance-thread-stop) :void
  "Stops performance fully."
  (pt :pointer))


(define-cfun ("csoundPerformanceThreadRecord" csound-performance-thread-record) :void
  "Starts recording the output from Csound. The sample rate and number of channels are taken directly from the running Csound instance."
  (pt :pointer)
  (filename :string)
  (samplebits :int)
  (numbufs :int))


(define-cfun ("csoundPerformanceThreadStopRecord" csound-performance-thread-stop-record) :void
  "Stops recording and closes audio file."
  (pt :pointer))


(define-cfun ("csoundPerformanceThreadScoreEvent" csound-performance-thread-score-event) :void
  "Sends an event. The event has type opcod (e.g. 'i' for a note event). pFields is tuple, a list, or an ndarray of MYFLTs with all the pfields for this event, starting with the p1 value specified in pFields[0]. If absp2mode is non-zero, the start time of the event is measured from the beginning of performance, instead of the default of relative to the current time."
  (pt :pointer)
  (absp2mode :int)
  (opcod :char)
  (pcnt :int)
  (p :pointer))


(define-cfun ("csoundPerformanceThreadInputMessage" csound-performance-thread-input-message) :void
  "Sends an event as a string"
  (pt :pointer)
  (s :string))


(define-cfun ("csoundPerformanceThreadSetScoreOffsetSeconds" csound-performance-thread-set-score-offset-seconds) :void
  "Sets the playback time pointer to the specified value (in seconds)"
  (pt :pointer)
  (time-val :double))


(define-cfun ("csoundPerformanceThreadCompileOrc" csound-performance-thread-compile-orc) :void
  "Compiles the given orchestra code"
  (pt :pointer)
  (code :string))


(define-cfun ("csoundPerformanceThreadEvalCode" csound-performance-thread-eval-code) :void
  "Evaluates the given code, calls the `returncb` callback with the value passed to the `return` opcode in global space. "
  (pt :pointer)
  (code :string)
  (returncb :pointer))


(define-cfun ("csoundPerformanceThreadRequestCallback" csound-performance-thread-request-callback) :void
  "Calls the given callback within the context of the callback thread"
  (pt :pointer)
  (func :pointer))


(define-cfun ("csoundPerformanceThreadJoin" csound-performance-thread-join) :int
  "Waits until the performance is finished or fails. Returns a positive value if the end of score was reached or stop() was called, and a negative value if an error occured. Also releases any resources associated with the performance thread object."
  (pt :pointer))


(define-cfun ("csoundPerformanceThreadFlushMessageQueue" csound-performance-thread-flush-message-queue) :void
  "Waits until all pending messages are actually received. (pause, send score event, etc.)"
  (pt :pointer))




