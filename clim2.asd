
;;; This file was written by Earl DuCaine.  It may be used in
;;; accordance with the licence specified in the LICENSE file or the
;;; MIT license.


(load "misc/compile-1.lisp")

(asdf:defsystem :clim2
    :depends-on (:clim2-pre-compile-1)
    :serial t
    :components
    ((:file "cat")
     (:file "utils/excl-verification")
     (:file "package")
     (:file "utils/packages")
     (:file "demo/packages")
     (:file "test/test-pkg")
     (:file "load-clim")
     (:file "tk/pkg")
     (:file "tk-silica/pkg")
     (:file "xlib/pkg")

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
     (:file "utils/excl-streams")
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
     (:file "clim/excl-presentations")
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

     (:file "xlib/ffi")
     (:file "xlib/load-xlib")
     (:file "xlib/xlib-defs")
     (:file "xlib/xlib-funs")
     (:file "xlib/x11-keysyms")
     (:file "xlib/last")

     (:file "tk/macros")
     (:file "tk/xt-defs")
     (:file "tk/xt-funs")
     (:file "tk/foreign-obj")
      ;; Xlib stuff
     (:file "tk/xlib")
     (:file "tk/font")
     (:file "tk/gcontext")
     (:file "tk/graphics")
      ;; Toolkit stuff
     (:file "tk/meta-tk")
     (:file "tk/make-classes")
     (:file "tk/foreign")
     (:file "tk/widget")
     (:file "tk/resources")
     (:file "tk/event")
     (:file "tk/callbacks")
     (:file "tk/xt-classes")
     (:file "tk/xt-init")


     (:file "tk/xm-defs")
     (:file "tk/xm-funs")
     (:file "tk/xm-classes")
     (:file "tk/xm-callbacks")
     (:file "tk/xm-init")
     (:file "tk/xm-widgets")
     (:file "tk/xm-font-list")
     (:file "tk/xm-protocols")
     (:file "tk/convenience")
     (:file "tk/make-widget")

     (:file "tk-silica/xt-silica")
     (:file "tk-silica/xt-stipples")
     (:file "tk-silica/xm-silica")
     (:file "tk-silica/xt-graphics")
     (:file "tk-silica/image")
     (:file "tk-silica/xt-frames")
     (:file "tk-silica/xm-frames")
     (:file "tk-silica/xm-dialogs")
     (:file "tk-silica/xt-gadgets")
     (:file "tk-silica/xm-gadgets")

     (:file "tk-silica/xt-pixmaps")
     (:file "tk-silica/gc-cursor")
     (:file "clim/japanese-input-editor")
     ;;;(:file "test/test-pkg")
     (:file "test/test-driver")
     (:file "test/test-clim-tests")
     (:file "test/test-clim")
     (:file "test/test-demos")

     (:file "wnn/pkg")
     (:file "wnn/load-wnn")
     (:file "wnn/jl-defs")
     (:file "wnn/jl-funs")
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
	       (:file "japanese-graphics-editor")
	       (:file "listener")
	       (:file "navdata")
	       (:file "navfun")
	       (:file "palette")
	       (:file "peek-frame")
	       (:file "plot")
	       (:file "process-browser")
	       (:file "puzzle")
	       (:file "thinkadot")
	       ))
     (:file "clim-example" :depends-on (:demo))
     (:file "test/test-suite")
     ))

(format t "~%to run demo: (clim-demo:start-demo)" nil)
