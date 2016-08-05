;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CL-USER; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.

(in-package :cl-user)

(eval-when (compile load eval)

;;; Tell the world that we're here
;;;--- These need to be in the CLIM.fasl also.
;;;--- Currently they're in EXCL-VERIFICATION but that does not seem the best place.
  (pushnew :clim *features*)
  (pushnew :clim-2 *features*)
  (pushnew :clim-2.1 *features*)
  (pushnew :silica *features*)
  (pushnew :clim-uses-lisp-stream-classes *features*)
  (pushnew :clim-uses-lisp-stream-functions *features*)
  (pushnew :clim-ansi-conditions *features*)
  (pushnew :allegro-v4.0-constructors *features*))

(declaim (declaration non-dynamic-extent))


;;; CLIM is implemented using the "Gray Stream Proposal" (STREAM-DEFINITION-BY-USER)
;;; a proposal to X3J13 in March, 1989 by David Gray of Texas Instruments.  In that
;;; proposal, stream objects are built on certain CLOS classes, and stream functions
;;; (e.g., WRITE-CHAR) are non-generic interfaces to generic functions (e.g.,
;;; STREAM-WRITE-CHAR).  These "trampoline" functions are required because their
;;; STREAM argument is often optional, which means it cannot be used to dispatch to
;;; different methods.

;;; Various Lisp vendors have their own stream implementations, some of which are
;;; identical to the Gray proposal, some of which implement just the trampoline
;;; functions and not the classes, etc.  If the Lisp vendor has not implemented the
;;; classes, we will shadow those class names (and the predicate functions for them)
;;; in the CLIM-LISP package, and define the classes ourselves.  If the vendor has
;;; not implemented the trampoline functions, we will shadow their names, and write
;;; our own trampolines which will call our generic function, and then write default
;;; methods which will invoke the COMMON-LISP package equivalents.

(eval-when (compile load eval)

#+(or allegro
      Minima
      Clozure
      SBCL)
(pushnew :clim-uses-lisp-stream-classes *features*)

#+(or allegro
      Genera				;Except for STREAM-ELEMENT-TYPE
      Minima
      Cloe-Runtime
      (and MCL CCL-2)			;Except for CLOSE (and WITH-OPEN-STREAM)
      Clozure
      SBCL)
(pushnew :clim-uses-lisp-stream-functions *features*)

;;; CLIM-ANSI-Conditions means this lisp truly supports the ANSI CL condition system
;;; CLIM-Conditions      means that it has a macro called DEFINE-CONDITION but that it works
;;;                      like allegro 3.1.13 or Lucid.
(pushnew :clim-ansi-conditions *features*)

#+allegro
(pushnew :allegro-v4.0-constructors *features*)

)	;eval-when

;; We extend defsystem to have a new module class compile-always
;; which always recompiles the module even if not required. This
;; allows us to put defpackage forms within ics-target-case so that at
;; load time only the one case takes effect while at compile time both
;; forms are processed. (cim 2/28/96)

(defclass compile-always (clim-defsystem:lisp-module)
  ())

(defvar *compiled-modules* nil)

(defmethod clim-defsystem:product-newer-than-source ((module compile-always))
  (member module *compiled-modules*))

(defmethod clim-defsystem:compile-module :after ((module compile-always) &key)
  (pushnew module *compiled-modules*))

;; This defsystem module class only compiles the module if it's not
;; ever been compiled - this is used to deal with files that can only
;; be validly compiled with an ics image - see japanese-input-editor
;; (cim 2/28/96)

(defclass compile-once (clim-defsystem:lisp-module)
  ())

(defmethod clim-defsystem:product-newer-than-source ((module compile-once))
  (probe-file (clim-defsystem:product-pathname module)))




(clim-defsystem clim-utils
    (:default-pathname "clim2:;utils;")
  ;; These files establish a uniform Lisp environment
  (:serial
   "excl-verification"
   ("packages" (:module-class compile-always))
   "defun-utilities" ;; extract-declarations and friends
   #+(or Genera (not ansi-90)) "defun"
   "reader"
   "clos-patches"
   "clos"
   #+CLIM-conditions "condpat" ;get the define-condition macro

   ;; General Lisp extensions
   "utilities"
   "lisp-utilities"
   "processes"
   "queue"
   ("timers" (:load-before-compile "queue" "processes"))
   "protocols"

   ;; Establish a uniform stream model
   "clim-streams"
   #-clim-uses-lisp-stream-classes "cl-stream-classes"
   #+Minima "minima-stream-classes"
   #+(and (not clim-uses-lisp-stream-functions) (not Lucid)) "cl-stream-functions"
   #+Lucid "lucid-stream-functions"
   #+Genera "genera-streams"
   #+allegro "excl-streams"
   #+(and MCL CCL-2) "ccl-streams"

   ;; Basic utilities for Silica and CLIM
   "clim-macros"
   ("transformations" #+CLIM-conditions (:load-before-compile "condpat"))
   "regions"
   "region-arithmetic"
   "extended-regions"
   "base-designs"
   "designs"
   ))

(clim-defsystem clim-silica
    (:default-pathname "clim2:;silica;")
  (:serial
   clim-utils

   ;; "Silica"
   "macros"
   "classes"
   "text-style"
   "sheet"
   "mirror"
   "event"
   "port"
   "medium"
   "framem"
   "graphics"
   "pixmaps"
   "std-sheet"

   ;; "Windshield", aka "DashBoard"
   ;; First the layout gadgets
   "layout"
   "db-layout"
   "db-box"
   "db-table"

   ;; Then the "physical" gadgets
   "gadgets"
   "db-border"
   "db-scroll"
   ;; ?? scroll-pane
   #+(and ignore acl86win32) "scroll-pane"
   #+acl86win32 "db-button"
   #+acl86win32 "db-label"
   #+acl86win32 ("db-slider" (:load-before-compile "db-border"))
   ))

(clim-defsystem clim-standalone
    (:default-pathname "clim2:;clim;")
  (:serial
   ;; clim-utils is in clim-silica, should it be?
   ;;clim-utils
   clim-silica

   ;; Basic tools
   "gestures"
   "defprotocol"
   "stream-defprotocols"
   "defresource"
   "temp-strings"
   #+(and MCL CCL-2) "coral-defs"
   "clim-defs"

   ;; Definitions and protocols
   "stream-class-defs"
   "interactive-defs"
   "cursor"
   "view-defs"
   "input-defs"
   "input-protocol"
   "output-protocol"

   ;; Output recording
   ("recording-defs" (:load-before-compile "clim-defs"))
   "formatted-output-defs"
   ("recording-protocol" (:load-before-compile "recording-defs"))
   ("text-recording" (:load-before-compile "recording-protocol"))
   ("graphics-recording" (:load-before-compile "recording-protocol"))
   ("design-recording" (:load-before-compile "graphics-recording"))

   ;; Input editing
   ("interactive-protocol" (:load-before-compile "clim-defs"))
   "input-editor-commands"

   ;; only compile with non-ICS if no fasl file exist
   ;; always compile with ICS in case it was previously compiled by
   ;; non-ICS
   #-acl86win32
   ("japanese-input-editor" (:module-class #-ics compile-once
					   #+ics compile-always))

   ;; Incremental redisplay
   ("incremental-redisplay" (:load-before-compile "clim-defs" "recording-protocol"))

   ;; Windows
   "coordinate-sorted-set"
   "r-tree"
   "window-stream"
   "pixmap-streams"

   ;; Presentation types
   ("ptypes1" (:load-before-compile "clim-defs"))
   ("completer" (:load-before-compile "ptypes1"))
   ("presentations" (:load-before-compile "ptypes1"))
   ("translators" (:load-before-compile "presentations"))
   ("histories" (:load-before-compile "presentations"))
   ("ptypes2" (:load-before-compile "translators"))
   ("standard-types" (:load-before-compile "ptypes2"))
   #+allegro ("excl-presentations" (:load-before-compile "presentations"))

   ;; Formatted output
   ("table-formatting" (:load-before-compile "clim-defs" "incremental-redisplay"))
   ("graph-formatting" (:load-before-compile "clim-defs" "incremental-redisplay"))
   ("surround-output" (:load-before-compile "clim-defs" "incremental-redisplay"))
   ("text-formatting" (:load-before-compile "clim-defs" "incremental-redisplay"))

   ;; Pointer tracking
   "tracking-pointer"
   ("dragging-output" (:load-before-compile "tracking-pointer"))

   ;; Gadgets
   "db-stream"
   "gadget-output"

   ;; Application building substrate
   ("accept" (:load-before-compile "clim-defs" "ptypes2"))
   ("present" (:load-before-compile "clim-defs" "ptypes2"))
   ("command" (:load-before-compile "clim-defs" "ptypes2"))
   ("command-processor" (:load-before-compile "clim-defs" "command"))
   ("basic-translators" (:load-before-compile "ptypes2" "command"))
   ("frames" (:load-before-compile "clim-defs" "command-processor"))
   ("panes" (:load-before-compile "frames"))
   ("default-frame" (:load-before-compile "frames"))
   ("activities" (:load-before-compile "frames"))
   ("db-menu" (:load-before-compile "frames"))
   #+acl86win32 ("db-list" (:load-before-compile "db-menu"))
   #+acl86win32 ("db-text" (:load-before-compile "frames"))
   ("noting-progress" (:load-before-compile "frames"))
   ("menus" (:load-before-compile "defresource" "clim-defs"))
   ("accept-values" (:load-before-compile "clim-defs" "incremental-redisplay" "frames"))
   ("drag-and-drop" (:load-before-compile "frames"))
   "item-list-manager"

   ;; Bootstrap everything
   ("stream-trampolines" (:load-before-compile "defprotocol" "stream-defprotocols"))
   #+lucid "lucid-after"
   #+(or Genera Cloe-Runtime) "prefill"
   ))

(clim-defsystem clim-homegrown
    (:default-pathname "clim2:;homegrown;")
  (:serial
   clim-standalone
   clim-silica
   "scroll-pane"
   "db-button"
   "db-label"
   "db-slider"
   "db-menu"
   ("db-list" (:load-before-compile "db-menu"))
   "db-text"
   "last"))


#+(and allegro (not acl86win32))
(clim-defsystem xlib
    (:default-pathname "clim2:;xlib;")
  (:serial
   clim-standalone
   "pkg"
   "ffi"
   ("load-xlib")
   ("xlib-defs" (:load-before-compile "ffi"))
   ("xlib-funs" (:load-before-compile "ffi"))
   ("x11-keysyms" (:load-before-compile "ffi"))
   ("last" (:load-before-compile "load-xlib" "xlib-funs"))
   ))

#+(and allegro (not acl86win32))
(clim-defsystem wnn
    (:default-pathname "clim2:;wnn;")
  (:serial
   clim-standalone
   "pkg"
   "load-wnn"
   "jl-defs"
   "jl-funs"
   ;; This module gets the same module class as "japanese-input-editor"
   ;; because this module has definitions that the other module uses, and
   ;; without also compiling this we get warnings about undefined functions
   ;; during the compile.  Gross.
   ("jserver" (:module-class #-ics compile-once
			     #+ics compile-always))))

#+(and allegro (not acl86win32))
(macrolet ((define-xt-system (name file &rest modules)
	       `(clim-defsystem ,name
		    (:default-pathname "clim2:;tk;")
		  (:serial
		   xlib
		   (,file)
		   ("pkg")
		   ("macros")
		   ("xt-defs")
		   ("xt-funs")
		   ("foreign-obj")
		   ;; Xlib stuff
		   ("xlib")
		   ("font")
		   ("gcontext")
		   ("graphics")

		   ;; Toolkit stuff
		   ("meta-tk")
		   ("make-classes")
		   ("foreign")
		   ("widget")
		   ("resources")
		   ("event")
		   ("callbacks")
		   ("xt-classes")
		   ("xt-init")
		   ,@modules))))

(define-xt-system xm-tk "load-xm"
  ("xm-defs")
  ("xm-funs")
  ("xm-classes")
  ("xm-callbacks")
  ("xm-init")
  ("xm-widgets")
  ("xm-font-list")
  ("xm-protocols")
  ("convenience")
  ("make-widget"))

(define-xt-system ol-tk "load-ol"
  ("ol-defs")
  ("ol-funs")
  ("ol-classes")
  ("ol-init")
  ("ol-widgets")
  ("ol-callbacks")
  ("make-widget")))

#+allegro
(clim-defsystem last (:default-pathname "clim2:;utils;")
  (:serial ("last")))

#+(and allegro (not acl86win32))
(clim-defsystem motif-clim
    (:default-pathname "clim2:;tk-silica;")
  (:serial
   clim-standalone
   xm-tk
   ("pkg")
   ("xt-silica")
   ("xt-stipples")
   ("xm-silica")
   ("xt-graphics")
   ("image")
   ("xt-frames")
   ("xm-frames")
   ("xm-dialogs")
   ("xt-gadgets")
   ("xm-gadgets")
   ("xt-pixmaps")
   ("gc-cursor")
   last))

#+(and allegro (not acl86win32))
(clim-defsystem openlook-clim
    (:default-pathname "clim2:;tk-silica;")
  (:serial
   clim-standalone
   ol-tk

   ("pkg")
   ("xt-silica")
   ("xt-stipples")
   ("ol-silica")
   ("xt-graphics")
   ("image")
   ("xt-frames")
   ("ol-frames")
   ("xt-gadgets")
   ("ol-gadgets")
   ("xt-pixmaps")
   ("gc-cursor")
   last))

;; (defclass compile-always (defsystem:lisp-module)
;;   ())

;; (defvar *compiled-modules* nil)

;; (defmethod defsystem:product-newer-than-source ((module compile-always))
;;   (member module *compiled-modules*))

;; (defmethod defsystem:compile-module :after ((module compile-always) &key)
;;   (pushnew module *compiled-modules*))

;; (defclass compile-once (defsystem:lisp-module)
;;   ())

;; (defmethod defsystem:product-newer-than-source ((module compile-once))
;;   (probe-file (defsystem:product-pathname module)))

;; (defsystem clim-utils
;;   (:default-pathname "clim2:;utils;")
;;   ;; These files establish a uniform Lisp environment
;;   (:serial
;;    ))

;; (defsystem xlib
;;   (:default-pathname "clim2:;xlib;")
;;   (:serial
;;    clim-standalone
;;    ))

;; (defsystem last
;;   (:default-pathname "clim2/utils")
;;   (:serial ("last")))
