(in-package #:csound)

(defopcode gain ugen (asig krms &optional ihp iskip))
(defopcode gainslider ugen (kindex))
(defopcode gauss ugen (krange))
(defopcode gaussi ugen (krange xamp xcps))
(defopcode gausstring ugen (kamp kcps kdev &optional imode))
(defopcode gbuzz ugen (xamp xcps knh klh kmul ifn &optional iphs))
(defopcode gendy ugen (kamp kampdist kdurdist kadpar kddpar kminfreq kmaxfreq kampscl kdurscl &optional initcps knum))
(defopcode gendy-c ugen (kamp kampdist kdurdist kadpar kddpar kminfreq kmaxfreq kampscl kdurscl &optional initcps knum)
    "gendyc")
(defopcode gendy-x ugen (kamp kampdist kdurdist kadpar kddpar kminfreq kmaxfreq kampscl kdurscl
			      kcurveup kcurvedown &optional initcps knum)
    "gendyx")
;;; getcfg
(defopcode gogobel ugen (kamp kfreq ihrd ipos imp kvibf kvamp ivfn))
(defopcode goto command (label))
(defopcode grain ugen (xamp xpitch xdens kampoff kpitchoff kgdur igfn iwfn imgdur &optional igrnd))
(defopcode grain2 ugen (kcps kfmd kgdur iovrlp kfn iwfn &optional irpow iseed imode))
(defopcode grain3 ugen (kcps kphs kfmod kpmd kgdur kdens imaxovr kfn iwfn kfrpow kprpow &optional iseed imode))
(defopcode granule ugen (xamp ivoice iratio imode ithd ifn ipshift igskip igskip_os ilength kgap igap_os kgsize
			      igsize_os iatt idec &optional iseed ipitch1 ipitch2 ipitch3 ipitch4 ifnenv))
(defopcode guiro ugen (kamp idettack &optional inum idamp imaxshake ifreq ifreq1))
(defopcode harmon ugen (asig kestfrq kamxvar kgenfreq1 kgenfreq2 imode iminfrq iprd))
(defopcode harmon2 ugen (asig koc kfrq1 kfrq2 icpsmode ilowest &optional ipolarity))
(defopcode harmon3 ugen (asig koct kfrq1 kfrq2 kfrq3 icpsmode ilowest &optional ipolarity))
(defopcode harmon4 ugen (asig koct kfrq1 kfrq2 kfrq3 icpsmode ilowest &optional ipolarity))
(defopcode hilbert ugen (asig))
(defopcode hrtfer ugen (asig kaz kelev hrtfcompact))
;;; hrtfearly
(defopcode hrtfmove ugen (asrc kaz kelv ifilel ifiler &optional imode ifade isr))
(defopcode hrtmove2 ugen (asrc kaz kelev ifilel ifiler &optional ioverlap iradius isr))
(defopcode hrtfreverb ugen (asrc ilowrt60 ihighrt60 ifilel ifiler &optional isr imfp iorder))
(defopcode hrtfstat ugen (asrc iaz ielev iflel ifler &optional iradius isr))
(defopcode hsboscil ugen (kamp ktone kbrite ibasfreq iwfn ioctfn &optional ioctcnt iphs))
(defopcode hvs1 command (kx inumparms inumpointsx iouttab ipositionstab isnaptab &optional iconfigtab))
(defopcode hvs2 command (kx ky inumparms inumpointsx inumpointsy iouttab ipositionstab isnaptab &optional iconfigtab))
(defopcode hvs3 command (kx ky kz inumparms inumpointsx inumpointsy inumpointsz iouttab ipositionstab isnaptab &optional iconfigtab))

(defopcode i func (x))
(defopcode igoto command (label))
(defopcode ihold command ())
;;; imagecreate
;;; imagefree
;;; imagegetpixel
;;; ...
(defopcode in ugen ())
(defopcode in32 ugen ())
(defopcode inch ugen (kchan1 &rest rest))
(defopcode inh ugen ())
(defopcode init ugen (iarg))
(defopcode ino ugen ())
(defopcode inq ugen ())
(defopcode inrg command (kstart ain1 &rest ainn))
(defopcode ins ugen ())
;;; insremote
;;; insglobal
(defopcode int func (x))
(defopcode integ ugen (asig &optional iskip))
(defopcode interp ugen (ksig &optional iskip imode))
(defopcode invalue ugen (channelname))
(defopcode inx ugen ())
(defopcode inz command (ksig1))





