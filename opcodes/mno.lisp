(in-package #:csound)

(defopcode mac ugen (ksig1 asig1 &rest sigs))
(defopcode maca ugen (asig1 aisg2 &rest sigs))
(defopcode madsr ugen (iatt idec islev irel &optional idel ireltim))
(defopcode mandel ugen (ktrig kx ky kmaxlter))
(defopcode mandol ugen (kamp kfreq kpluck kdetune kgain ksize ifn &optional iminfreq))
(defopcode marimba ugen (kamp kfreq ihrd ipos imp kvibf kvamp ivibfn idec &optional idoubles itriples))
(defopcode massign command (ichnl insnum &optional ireset))
(defopcode max~ ugen (ain1 ain2 &rest ains) "max")
(defopcode maxabs ugen (ain ain2 &rest ains))
(defopcode maxabsaccum command (accumulator ainput))
(defopcode maxaccum command (accumulator ainput))
(defopcode maxalloc command (insnum icount))
(defopcode max-k ugen (asig ktrig itype) "max_k")
(defopcode maxtab ugen (tab))
;;; mclock
;;; mdelay
(defopcode median ugen (asig ksize imaxsize &optional iskip))
(defopcode mediank ugen (kin ksize imaxsize &optional iskip))
(defopcode metro ugen (kfreq &optional initphase))
;;; midglobal
;;; midi...
(defopcode min~ ugen (ain1 ain2 &rest ains) "min")
(defopcode minabs ugen (ain1 ain2 &rest ains))
(defopcode minabsaccum command (accumulator ainput))
(defopcode minaccum command (accumulator ainput))
(defopcode mincer ugen (atimpt kamp kpitch ktab klock &optional ifftsize idecim))
(defopcode mintab ugen (tab))
(defopcode mirror ugen (asig klow khigh))
(defopcode mixer-set-level command (isend ibuss kgain) "MixerSetLevel")
(defopcode mixer-set-leveli command (isend ibuss igain) "MixerSetLevel_i")
(defopcode mixer-get-level ugen (isend ibuss) "MixerGetLevel")
(defopcode mixer-send command (asignal isend ibuss ichannel) "MixerSend")
(defopcode mixer-receive ugen (ibuss ichannel) "MixerReceive")
(defopcode mixer-clear command nil "MixerClear")
(defopcode mode ugen (ain kfreq kq &optional iskip))
(defopcode mod-matrix command (iresfn isrcmodfn isrcparmfn imodscale inum_mod inum_parm kupdate) "modmatrix")
(defopcode monitor ugen nil)
(defopcode moog ugen (kamp kfreq kfiltq kfiltrate kvibf kvamp iafn iwfn ivfn))
(defopcode moogladder ugen (ain kcf kres &optional istor))
(defopcode moogvcf ugen (asig xfco xres &optional iscale iskip))
(defopcode moogvcf2 ugen (asig xfco xres &optional iscale iskip))
(defopcode moscil command (kchn knum kvel kdur kpause))
;;; mp3in
;;; mp3len
(defopcode mpulse ugen (kamp kintvl &optional ioffset))
;;; mrtmsg
;;; OSC...
(defopcode multtab ugen (tleft tright))
(defopcode multitap ugen (asig &rest time-n-gain))
(defopcode mute command (insnum &optional iwsitch))
(defopcode mxadsr ugen (iatt idec islev irel &optional idel ireltim))
(defopcode nestedap ugen (asig imode imaxdel idel1 igain1 &optional idel2 igain2 idel3 igain3 istor))
(defopcode nlfilt ugen (ain ka kb kd kc kl))
(defopcode nlfilt2 ugen (ain ka kb kd kc kl))
(defopcode noise ugen (xamp kbeta))
;;; noteoff....
(defopcode nreverb ugen (asig ktime khdif &optional iskip inumcombs ifncombs inumalpas ifnalpas))
(defopcode nsamp func (x))
(defopcode nstrnum func (x))
(defopcode ntrpol ugen (asig1 asig2 kpoint &optional imin imax))
(defopcode octave func (x))
(defopcode octcps func (cps))
(defopcode octmidi ugen nil)
(defopcode octmidib ugen (&optional irange))
(defopcode octmidinn func (midinote))
(defopcode octpch func (pch))
(defopcode oscbnk ugen (kcps kamp kfmd kpmd iovrlap iseed kl1minf kl1maxf kl2minf kl2maxf ilfomode
			keqminf keqmaxf keqminl keqmaxl keqminq keqmaxq ieqmode kfn &optional
			il1fn il2fn ieqffn ieqlfn ieqqfn itabl ioutfn))
(defopcode oscil1 ugen (idel kamp idur ifn))
(defopcode oscil1i ugen (idel kamp idur ifn))
(defopcode oscil3 ugen (xamp xcps ifn &optional iphs))
(defopcode oscil ugen (xamp xcps &optional ifn iphs))
(defopcode oscili ugen (xamp xcps &optional ifn iphs))
(defopcode oscilikt ugen (xamp xcps kfn &optional iphs istor))
(defopcode osciliktp ugen (kcps kfn kphs &optional istor))
(defopcode oscilikts ugen (xamp xcps kfn async kphs &optional istor))
(defopcode osciln ugen (kamp ifrq ifn itimes))
(defopcode oscils ugen (iamp icps iphs &optional iflg))
;;; out32
(defopcode out command (asig &rest asigs))
(defopcode outc command (asig &rest asign))
(defopcode outch command (kchan1 asig1 &rest kchan-sig))
(defopcode outh command (asig1 asig2 asig3 asig4 asig5 asig6))
(defopcode outleta command (sname asignal))
(defopcode outletf command (sname fsignal))
(defopcode outletk command (sname ksignal))
(defopcode outletkid command (sname sintance-id ksignal))
(defopcode outo command (asig1 asig2 asig3 asig4 asig5 asig6 asig7 asig8))
(defopcode outq1 command (asig))
(defopcode outq2 command (asig))
(defopcode outq3 command (asig))
(defopcode outq4 command (asig))
(defopcode outq command (asig1 asig2 asig3 asig4))
(defopcode outrg command (kstart aout1 &rest aoutn))
(defopcode outs1 commamd (asig))
(defopcode outs2 command (asig))
(defopcode outs command (asig1 asig2))
(defopcode outvalue command (channelname kvalue))
;;; outx
(defopcode outz command (ksig1))








