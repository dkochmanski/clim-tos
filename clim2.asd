
;;; This file was written by Earl DuCaine.  It may be used in
;;; accordance with the licence specified in the LICENSE file or the
;;; MIT license.

#+(or ANSI-90 ANSI-CL)
(pushnew :clx-ansi-common-lisp *features*)

(asdf:defsystem :clim2
    :depends-on ()
    :serial t
    :components
    (
     (:file "cat")
     #+Allegro (:file "utils/excl-verification")
     (:file "package")
     (:file "utils/packages")
     (:file "demo/packages")
     (:file "test/test-pkg")
     (:file "load-clim")
     #+Allegro
     (:file "tk/pkg")
     #+Allegro
     (:file "tk-silica/pkg")
     (:file "xlib/package")

     (:file "utils/defun-utilities")
     (:file "utils/reader")
     (:file "utils/processes")
     (:file "utils/lisp-utilities")
     (:file "utils/clos-patches")
     (:file "utils/clos")
     (:file "utils/utilities")
     (:file "utils/queue")
     (:file "utils/timers")
     (:file "utils/protocols")
     (:file "utils/clim-streams")
     #+Allegro (:file "utils/excl-streams")
     (:file "utils/clim-macros")
     (:file "clim/clim-defs")
     (:file "utils/transformations")
     (:file "utils/regions")
     (:file "utils/region-arithmetic")
     (:file "utils/extended-regions")
     (:file "clim/ptypes1")
     (:file "postscript/pkgdcl")
     (:file "utils/base-designs")
     (:file "utils/designs")
     (:file "silica/classes")
     (:file "silica/text-style")
     (:file "silica/macros")
     (:file "silica/sheet")
     (:file "silica/mirror")
     (:file "silica/event")
     (:file "silica/port")
     (:file "silica/medium")
     (:file "silica/framem")
     (:file "silica/graphics")
     (:file "silica/pixmaps")
     (:file "silica/std-sheet")
     (:file "silica/layout")
     (:file "silica/db-layout")
     (:file "silica/db-box")
     (:file "silica/db-table")
     (:file "silica/gadgets")
     (:file "silica/db-border")
     (:file "silica/db-button")
     (:file "silica/db-slider")
     (:file "silica/db-label")
     (:file "silica/db-scroll")
     (:file "clim/recording-defs")
     (:file "clim/gestures")
     (:file "clim/defprotocol")
     (:file "clim/stream-defprotocols")
     (:file "clim/defresource")
     (:file "clim/temp-strings")
     (:file "clim/stream-class-defs")
     (:file "clim/interactive-defs")
     (:file "clim/cursor")
     (:file "clim/view-defs")
     (:file "clim/input-defs")
     (:file "clim/input-protocol")
     (:file "clim/output-protocol")
     (:file "clim/formatted-output-defs")
     (:file "clim/translators")
     (:file "clim/ptypes2")
     (:file "clim/command")
     (:file "clim/command-processor")
     (:file "clim/recording-protocol")
     (:file "clim/presentations")
     (:file "clim/frames")
     (:file "clim/text-recording")
     (:file "clim/graphics-recording")
     (:file "clim/design-recording")
     (:file "clim/interactive-protocol")
     (:file "clim/input-editor-commands")
     ;; (:file "clim/formatted-output-defs")
     (:file "clim/db-menu")
     ;; "clim/db-list.fasl"
     ;; "clim/db-text.fasl"

     (:file "clim/incremental-redisplay")
     (:file "clim/r-tree")
     (:file "clim/window-stream")
     (:file "clim/pixmap-streams")
     ;; (:file "clim/ptypes1")

     (:file "clim/completer")
     ;;(:file "clim/translators")
     (:file "clim/histories")
     ;;(:file "clim/ptypes2")
     #+Allegro (:file "clim/excl-presentations")
     (:file "clim/standard-types")
     (:file "clim/table-formatting")

     (:file "clim/graph-formatting")
     (:file "clim/surround-output")
     (:file "clim/text-formatting")
     (:file "clim/tracking-pointer")
     (:file "clim/dragging-output")
     (:file "clim/db-stream")
     ;; (:file "clim/command")
     ;; (:file "clim/command-processor")
     (:file "clim/gadget-output")
     (:file "clim/accept")
     (:file "clim/present")
     (:file "clim/basic-translators")
     (:file "clim/panes")
     (:file "clim/default-frame")
     (:file "clim/activities")
     ;;     (:file "clim/db-menu")
     (:file "clim/noting-progress")
     (:file "clim/menus")
     (:file "clim/accept-values")

     (:file "clim/drag-and-drop")
     (:file "clim/item-list-manager")
     (:file "clim/stream-trampolines")

     ;; Common Lisp X11 (xlib)
     (:file "xlib/depdefs")
     (:file "xlib/clx")
     #-(or Clozure Allegro) (:file "xlib/dependent")
     #+Clozure (:file "xlib/dep-openmcl")
     #+Allegro (:file "xlib/dep-allegro")
     (:file "xlib/macros")
     (:file "xlib/bufmac")
     (:file "xlib/buffer")
     (:file "xlib/display")
     (:file "xlib/gcontext")
     (:file "xlib/input")
     (:file "xlib/requests")
     (:file "xlib/fonts")
     (:file "xlib/graphics")
     (:file "xlib/text")
     (:file "xlib/attributes")
     (:file "xlib/translate")
     (:file "xlib/keysyms")
     (:file "xlib/manager")
     (:file "xlib/image")
     (:file "xlib/resource")
     #+Allegro (:file "xlib/excldep")
     (:file "xlib/shape")
     (:file "xlib/xvidmode")
     (:file "xlib/xrender")
     (:file "xlib/glx")
     (:file "xlib/gl")
     (:file "xlib/dpms")

     #+Allegro
     (:file "tk/macros")
     #+Allegro
     (:file "tk/xt-defs")
     #+Allegro
     (:file "tk/xt-funs")
     #+Allegro
     (:file "tk/foreign-obj")
      ;; Xlib stuff
     #+Allegro
     (:file "tk/xlib")
     #+Allegro
     (:file "tk/font")
     #+Allegro
     (:file "tk/gcontext")
     #+Allegro
     (:file "tk/graphics")
      ;; Toolkit stuff
     #+Allegro
     (:file "tk/meta-tk")
     #+Allegro
     (:file "tk/make-classes")
     #+Allegro
     (:file "tk/foreign")
     #+Allegro
     (:file "tk/widget")
     #+Allegro
     (:file "tk/resources")
     #+Allegro
     (:file "tk/event")
     #+Allegro
     (:file "tk/callbacks")
     #+Allegro
     (:file "tk/xt-classes")
     #+Allegro
     (:file "tk/xt-init")


     #+Allegro
     (:file "tk/xm-defs")
     #+Allegro
     (:file "tk/xm-funs")
     #+Allegro
     (:file "tk/xm-classes")
     #+Allegro
     (:file "tk/xm-callbacks")
     #+Allegro
     (:file "tk/xm-init")
     #+Allegro
     (:file "tk/xm-widgets")
     #+Allegro
     (:file "tk/xm-font-list")
     #+Allegro
     (:file "tk/xm-protocols")
     #+Allegro
     (:file "tk/convenience")
     #+Allegro
     (:file "tk/make-widget")

     #+Allegro
     (:file "tk-silica/xt-silica")
     #+Allegro
     (:file "tk-silica/xt-stipples")
     #+Allegro
     (:file "tk-silica/xm-silica")
     #+Allegro
     (:file "tk-silica/xt-graphics")
     #+Allegro
     (:file "tk-silica/image")
     #+Allegro
     (:file "tk-silica/xt-frames")
     #+Allegro
     (:file "tk-silica/xm-frames")
     #+Allegro
     (:file "tk-silica/xm-dialogs")
     #+Allegro
     (:file "tk-silica/xt-gadgets")
     #+Allegro
     (:file "tk-silica/xm-gadgets")

     #+Allegro
     (:file "tk-silica/xt-pixmaps")
     #+Allegro
     (:file "tk-silica/gc-cursor")
     (:file "clim/japanese-input-editor")

     ;; system: testing
;     (:file "test/test-driver")
;     (:file "test/test-clim-tests")
;     (:file "test/test-clim")
;     (:file "test/test-demos")

     #+Allegro
     (:file "wnn/pkg")
     #+Allegro
     (:file "wnn/load-wnn")
     #+Allegro
     (:file "wnn/jl-defs")
     #+Allegro
     (:file "wnn/jl-funs")
     #+Allegro
     (:file "wnn/jserver")

     (:file "hpgl/pkg")
     (:file "hpgl/hpgl-port")
     (:file "hpgl/hpgl-medium")

     (:file "postscript/postscript-s")
;;;     (:file "postscript/postscript-clim-stubs")
     (:file "postscript/postscript-port")
     (:file "postscript/postscript-medium")
     (:file "postscript/read-afm")
     (:file "postscript/laserwriter-metrics")
;;;     (:file "postscript/climps")

     (:module demo
	      :depends-on (:package :load-clim)
	      :serial t
	      :components
	      (
;;;	       (:file "packages")
	       (:file "demo-driver")
	       (:file "address-book")
	       (:file "bitmap-editor")
	       (:file "browser")
	       (:file "cad-demo")
	       (:file "cload-demos")
	       (:file "color-editor")
	       (:file "custom-records")
	       (:file "default-frame-top-level")
	       (:file "demo-activity")
	       (:file "demo-last")
	       (:file "graphics-demos")
	       (:file "graphics-editor")
	       (:file "ico")
	       #+Allegro
	       (:file "japanese-graphics-editor")
	       (:file "listener")
	       (:file "navdata")
	       (:file "navfun")
	       (:file "palette")
	       (:file "peek-frame")
	       (:file "plot")
	       #+Allegro
	       (:file "process-browser")
	       (:file "puzzle")
	       (:file "thinkadot")
	       ))
     (:file "clim-example" :depends-on (:demo))
     (:file "test/test-suite")
     ))

(format t "~%to run demo: (clim-demo:start-demo)" nil)
