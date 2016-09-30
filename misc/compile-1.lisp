;; -*- mode: common-lisp; package: user -*-
;;
;; See the file LICENSE for the full license governing this code.
;;

(in-package :user)

;;; This should not matter
;;; (setq *ignore-package-name-case* t)

;; Forgive them, lord, for they know not what they do.
(eval-when (compile load eval)
  (pushnew :ansi-90 *features*))

(eval-when (compile load eval)
  ;; [bug18430]:
  (setq comp:declared-fixnums-remain-fixnums-switch
	(named-function |(> speed 2)|
			(lambda (safety size speed debug compilation-speed)
			  (declare (ignore safety size debug compilation-speed))
			  (> speed 2)))))

(eval-when (compile load eval)
;;;; Set up translations so we can find stuff.
  (setf (logical-pathname-translations "clim2")
	(list (list ";**;*.*"
		    (format nil "~A**/*.*"
			    (directory-namestring
			     (make-pathname
			      :directory
			      (butlast (pathname-directory
					*load-pathname*)))))))))

;;;; system definitions we need

;;; Basic clim and also all the X stuff
(eval-when (compile load eval)
  (load "clim2:;sys;sysdcl"))

;;; postscript stuff
(eval-when (compile load eval)
  (load "clim2:;postscript;sysdcl"))

;;; HPGL, only for Unix
(eval-when (compile load eval)
  (load "clim2:;hpgl;sysdcl"))

;;; demo stuff
(eval-when (compile load eval)
  (load "clim2:;demo;sysdcl"))

;;; testing stuff (this is really a serious mess)
(eval-when (compile load eval)
  (load "clim2:;test;testdcl"))

;;; climtoys.  I think this is never there, but just to be compatible.
(eval-when (compile load eval)
  (when (probe-file "clim2:;climtoys;sysdcl.lisp")
    (load "clim2:;climtoys;sysdcl")))

;;;; System declarations for compiling, concatenating &c.
;;;
;;; This is all horrible.  There are two entangled problems here which
;;; are both basically artifacts of the fasls that were being built
;;; with the cat-based makefile.  What is built is a large `generic'
;;; fasl -- climg -- which does (or should) not have any port-specific
;;; stuff in it, and then several smaller fasls which have stuff like
;;; the motif port, the postscript port and so on.  The problems are:
;;;
;;; 1. Many of the systems used for building have modules which are
;;; complete other systems.  So if you try to concate them you get
;;; huge fasls.  This is OK for climg, but it doesn't work for the
;;; higher-layer systems.  `cattable' versions of these systems are
;;; defined below which do not have the dependencies.  It's not enough
;;; just to have a concatenate-system which does not walk into
;;; modules, as some of these are two-deep.
;;;
;;; 2. The actual fasls that were built did not correspond to systems
;;; at all.  Typically there was a x.fasl and a debugx.fasl, where
;;; x.fasl had the stuff you needed at runtime was in the x.fasl and
;;; compile-time stuff like def-x macros was in the debugx.fasl.  In
;;; order to get this to work, the systems would need to be split into
;;; two with foo being foo-compiletime and foo-runtime, and then you'd
;;; be able to cat them seperately.  Except that some of the debug
;;; fasls cut across several systems.  What I'm doing about this is to
;;; essentially not have any debugx fasls.  The debugging code goes
;;; into the x fasls, and I build empty fasls for the debug stuff.
;;; This is nasty, but it means that stuff won't break, even if the
;;; basic image is a little bigger.
;;;
;;; PROPOSAL: in a future ACL release we should stop doing all this,
;;; and just ship one large fasl with everything in for each platform.
;;; This makes things a little bit bigger, but it seems just a waste
;;; to spend all this time fiddling.  It's not like CLIM is big any
;;; more, unless you're running on a 386 or something.
;;;
;;; As well as this, the system definitions need looked at more and
;;; cleaned up.

(eval-when (compile load eval)
  (defsystem climg
    ;; climg is generic clim and ends up as climg.fasl.  This is
    ;; clim-standalone + the PS stubs.
    ()
    (:serial
     clim-standalone			;from sys;sysdcl
;;     postscript-clim-stubs		;from postscript;sysdcl
     )))

(eval-when (compile load eval)
  (defsystem climdemo
    ;; climdemo.fasl.  This is a hack becuse files used by the system
    ;; in test;sysdcl have nasties in, other than that we could
    ;; probably make this be just clim-demo + clim-tests.
    ()
    (:serial
;;     clim-demo				;demo;sysdcl
;;     "clim2:;test;test-suite"             ;hack!
     )))

;; (eval-when (compile load eval)
;;   (defsystem hpgl-clim-cat
;;     ;; a cattable hpgl-clim, see clim2:;hpgl;sysdcl
;;     (:default-pathname "clim2:;hpgl;")
;;     (:serial
;;      ("pkg")
;;      ("hpgl-port")
;; ;;     ("hpgl-medium")
;;      )))

(eval-when (compile load eval)
  (defsystem empty-cat
    ;; so we can make empty fasls trivially
    ()
    (:serial)))

(eval-when (compile load eval)
  (defsystem xlib-cat
    ;; a cattable xlib, see clim2:;sys;sysdcl
    (:default-pathname "clim2:;xlib;")
    (:serial
     ;; "pkg"
     ;; "ffi"
     ;; ("load-xlib")
     ;; ("xlib-defs" (:load-before-compile "ffi"))
     ;; ("xlib-funs" (:load-before-compile "ffi"))
     ;; ("x11-keysyms" (:load-before-compile "ffi"))
     ;; ("last" (:load-before-compile "load-xlib" "xlib-funs"))
     )))

(defmacro define-xt-cat-system (name file &rest modules)
  ;; this is like define-xt-system but uses xlib-cat, not xlib.  See
  ;; clim2:;sys;sysdcl.  The `special' file comes before the xlib
  ;; system because it can do various require-type things: I'm not
  ;; sure this is right.
  `(defsystem ,name
       (:default-pathname #p"clim2:;tk;")
     (:serial
      (,file)
      xlib-cat
      ;; ("pkg")
      ;; ("macros")
      ;; ("xt-defs")
      ;; ("xt-funs")
      ;; ("foreign-obj")
      ;; ;; Xlib stuff
      ;; ("xlib")
      ;; ("font")
      ;; ("gcontext")
      ;; ("graphics")
      ;; ;; Toolkit stuff
      ;; ("meta-tk")
      ;; ("make-classes")
      ;; ("foreign")
      ;; ("widget")
      ;; ("resources")
      ;; ("event")
      ;; ("callbacks")
      ;; ("xt-classes")
      ;; ("xt-init")
      ,@modules)))

(eval-when (compile load eval)
  (define-xt-cat-system xm-tk-cat "load-xm"
    ;; cattable xm-tk, see clim2:;sys;sysdcl
    ;; ("xm-defs")
    ;; ("xm-funs")
    ;; ("xm-classes")
    ;; ("xm-callbacks")
    ;; ("xm-init")
    ;; ("xm-widgets")
    ;; ("xm-font-list")
    ;; ("xm-protocols")
    ;; ("convenience")
    ;; ("make-widget")
    ))

(eval-when (compile load eval)
  (defsystem motif-clim-cat
    ;; cattable motif-clim, see clim2:;sys;sysdcl
    (:default-pathname "clim2:;tk-silica;")
    (:serial
;;     xm-tk-cat
     ;; ("pkg")
     ;; ("xt-silica")
     ;; ("xt-stipples")
     ;; ("xm-silica")
     ;; ("xt-graphics")
     ;; ("image")
     ;; ("xt-frames")
     ;; ("xm-frames")
     ;; ("xm-dialogs")
     ;; ("xt-gadgets")
     ;; ("xm-gadgets")
     ;; ("xt-pixmaps")
     ;; ("gc-cursor")
     ;; last
     )))

;; (eval-when (compile load eval)
;;   (defsystem wnn-cat
;;     ;; cattable wnn, see clim2:;sys;sysdcl
;;     (:default-pathname "clim2:;wnn;")
;;     (:serial
;;      "pkg"
;;      "load-wnn"
;;      "jl-defs"
;;      "jl-funs"
;;      "jserver")))

;;; Compiling a system.
;;;
;;; This is just hard-wired -- the makefile says (compile-it
;;; <something>), which determines which top-level system to build,
;;; but all the other systems are wired in here.  And currently there
;;; is only one possible top-level system per platform, unless by some
;;; miracle the openlook stuff still built!
(defun compile-it (sys)
  (flet ((cl (s &key (include-components t)
		     (ignore-if-unknown nil)
		     (load-too nil))
	   (cond ((ignore-errors (excl:find-system s))
		  (excl:compile-system s
				       :include-components include-components)
		  (when load-too
		    (excl:tenuring
		     (excl:load-system s))))
		 ((not ignore-if-unknown)
		  (error "System ~S not known" s))
		 (t nil))))
    (with-compilation-unit ()
      (cl sys)
      ;; OK, now we randomly compile some other systems in a very
      ;; hacky way.  Several of these are just because that's the way
      ;; it was done before.  As well as platform conditionalisation,
      ;; the clim-homegrown and the clim-compatibility (from
      ;; compatibility;sysdcl) systems were not being bult on any
      ;; platform.
      ;; I am not sure if this is the right test...
      ;;;(cl 'wnn)
      ;; (cl 'postscript-clim)
      ;; (cl 'climdemo)
      ;; This currently does not build on windows but I think it
      ;; should do in future
      ;;(cl 'testing)
      (cl 'clim-toys :ignore-if-unknown t)
      ;;(cl 'hpgl-clim)
      )))

;;; Concatenating systems
;;;
;;; This is fairly hacky as well.  This code *knows* about what
;;; pathnames to dump systems under.  Again, SYS is just the top-level
;;; system (it should agree with the one we gave to COMPILE-IT above.
(defun concatenate-it (sys)
  (ecase sys
    ((aclnt-clim)
     (concatenate-system 'aclnt-clim-cat "clim2:;climnt.fasl"))
    ((motif-clim)
     (concatenate-system 'motif-clim-cat "clim2:;climxm.fasl")
     (concatenate-system 'empty-cat "clim2:;clim-debugxm.fasl")))
  ;; these are the basic things that we get
  (concatenate-system 'climg "clim2:;climg.fasl")
  (concatenate-system 'climdemo "clim2:;climdemo.fasl")
  ;; (concatenate-system 'postscript-clim "clim2:;climps.fasl")
  ;; The wnn system depends on ics.  The debug system is just there
  ;; for backwards compatibility
  ;;;(concatenate-system 'wnn-cat "clim2:;climwnn.fasl")
  ;;;(concatenate-system 'empty-cat "clim2:;clim-debugwnn.fasl")
  ;; hpgl only on unix
  ;;;;(concatenate-system 'hpgl-clim-cat "clim2:;climhpgl.fasl")
  ;; formerly the bogusly-named system with X debugging stuff in, now
  ;; exists only for backwards compatibility.
  (concatenate-system 'empty-cat "clim2:;clim-debug.fasl"))
