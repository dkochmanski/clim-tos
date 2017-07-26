
(in-package #:asdf-user)

(defsystem #:clim-tos
  :description "CLIM 2 implementation released by Franz Inc."
  :license "BSD-4-Clause"
  :depends-on (#:clim-tos/core))

;;; based on `clim-standalone' in sys/sysdcl.lisp
(defsystem #:clim-tos/core
  :description "Core functionality (doesn't contain backends)."
  :depends-on (#:clim-tos/silica)
  :components ((:module "clim"
                        :components
                        (;; Basic tools
                         (:file "gestures")
                         (:file "defprotocol")
                         (:file "stream-defprotocols")
                         (:file "defresource")
                         (:file "temp-strings")
                         (:file "clim-defs")

                         ;; Definitions and protocols
                         (:file "stream-class-defs")
                         (:file "interactive-defs")
                         (:file "cursor")
                         (:file "view-defs")
                         (:file "input-defs")
                         (:file "input-protocol")
                         (:file "output-protocol")

                         ;; Output recording
                         (:file "recording-defs")
                         (:file "formatted-output-defs")
                         (:file "recording-protocol")
                         (:file "text-recording")
                         (:file "graphics-recording")
                         (:file "design-recording")

			 ;; Input editing
                         (:file "interactive-protocol")
                         (:file "input-editor-commands")
                         #+(and allegro (not acl86win32)) (:file "japanese-input-editor")

                         ;; Incremental redisplay
                         (:file "incremental-redisplay")

                         ;; Windows (sheets)
                         (:file "coordinate-sorted-set")
                         (:file "r-tree")
                         (:file "window-stream")
                         (:file "pixmap-streams")

			 ;; Presentation types
                         (:file "ptypes1")
                         (:file "completer")
                         (:file "presentations")
                         (:file "translators")
                         (:file "histories")
                         (:file "ptypes2")
                         (:file "standard-types")
                         #+allegro (:file "excl-presentations")

                         ;; Formatted output
                         (:file "table-formatting")
                         (:file "graph-formatting")
                         (:file "surround-output")
                         (:file "text-formatting")

			 ;; Pointer tracking
                         (:file "tracking-pointer")
                         (:file "dragging-output")

                         ;; Gadgets
                         (:file "db-stream")
                         (:file "gadget-output")

                         ;; Application building substrate
                         (:file "accept")
                         (:file "present")
                         (:file "command")
                         (:file "command-processor")
                         (:file "basic-translators")
                         (:file "frames")
                         (:file "panes")
                         (:file "default-frame")
                         (:file "activities")
                         (:file "db-menu")
                         #+acl86win32 (:file "db-list")
                         #+acl86win32 (:file "db-text")
                         (:file "noting-progress")
                         (:file "menus")
                         (:file "accept-values")
                         (:file "drag-and-drop")
                         (:file "item-list-manager")

                         ;; Bootstrap everything
                         (:file "stream-trampolines")))))

;;; based on `clim-utils'
(defsystem #:clim-tos/utils
  :depends-on (#:closer-mop #:trivial-gray-streams)
  :components
  ((:module "utils"
            :components ((:file "packages")
                         (:file "defun-utilities")
                         (:file "reader")
                         (:file "clos-patches")
                         (:file "clos")

                         ;; General lisp extensions
                         (:file "utilities")
                         (:file "lisp-utilities")
                         (:file "processes")
                         (:file "queue")
                         (:file "timers")
                         (:file "protocols")

                         ;; Establish a uniform stream model
                         (:file "clim-streams")
                         (:file "cl-streams")

                         ;; Basic utilities for Silica and CLIM
                         (:file "clim-macros")
                         (:file "transformations")
                         (:file "regions")
                         (:file "region-arithmetic")
                         (:file "extended-regions")
                         (:file "base-designs")
                         (:file "designs")))))

;;; based on `clim-silica'
(defsystem #:clim-tos/silica
  :depends-on (#:clim-tos/utils
               #:closer-mop)
  :components
  ((:module "silica"
            :components
            (;; Silica
             (:file "macros")
             (:file "classes")
             (:file "text-style")
             (:file "sheet")
             (:file "mirror")
             (:file "event")
             (:file "port")
             (:file "medium")
             (:file "framem")
             (:file "graphics")
             (:file "pixmaps")
             (:file "std-sheet")

             ;; "Windshield", aka "DashBoard"
             ;; Layout gadgets
             (:file "layout")
             (:file "db-layout")
             (:file "db-box")
             (:file "db-table")

             ;; 'Physical' gadgets
             (:file "gadgets")
             (:file "db-border")
             (:file "db-scroll")
             (:file "db-button")
             (:file "db-label")
             (:file "db-slider")
             (:file "scroll-pane")))))

#+ (or)
(defsystem #:clim-tos/homegrown
  :depends-on (#:clim-tos/silica)
  :components
  ((:module "homegrown"
  	    :components
	    ;; not fully compatible with silica (???)
  	    ((:file "scroll-pane")
  	     (:file "db-button")
  	     (:file "db-label")
  	     (:file "db-slider")
  	     (:file "db-menu")
  	     (:file "db-list" :depends-on ("db-menu"))
  	     (:file "db-text")
  	     (:file "last")))))

(defsystem #:clim-tos/postscript
  :description "Draw-only backend as defined in the specification.")

;;; based on `clx-clim' in clx/sysdcl.lisp
(defsystem #:clim-tos/clx-backend
  :description "Reference backend for X-Window system."
  :depends-on (#:clx #:clim-tos/core)
  :components
  ((:module "clx"
	    :components
	    ((:file "pkgdcl")
	     (:file "clx-port")
	     (:file "clx-mirror")
	     (:file "clx-medium")
	     (:file "clx-pixmaps")
	     (:file "clx-frames")))))

;;; based on `clim-test' in test/sysdcl.lisp
(defsystem #:clim-tos/tests
  :description "CLIM tests (framework and definitions)."
  :depends-on (#:clim-tos/core
               #:clim-tos/examples)
  :serial t
  :components
  ((:module "test"
            :components
            ((:file "test-suite")
             (:file "test")
             (:file "test-buttons")
             (:file "test-sliders")
             (:file "simple-test")
             (:file "postscript-tests")))))

;;; based on `clim-demo' in demo/sysdcl.lisp
(defsystem #:clim-tos/examples
  :description "Applications bundled in clim-tos repository."
  :depends-on (#:clim-tos/core
               #:bordeaux-threads)

  :components
  ((:module "demo"
            :components
            ((:file "packages")
             (:file "demo-driver" :depends-on ("packages"))
             (:file "listener" :depends-on ("demo-driver"))
             (:file "graphics-demos" :depends-on ("demo-driver"))
             (:file "palette" :depends-on ("demo-driver"))
             ;(:file "cad-demo" :depends-on ("demo-driver"))
             ;(:file "navdata" :depends-on ("packages"))
             ;(:file "navfun" :depends-on ("navdata"))
             (:file "puzzle" :depends-on ("demo-driver"))
             (:file "address-book" :depends-on ("demo-driver"))
             (:file "thinkadot" :depends-on ("demo-driver"))
             ;(:file "plot" :depends-on ("demo-driver"))
             ;(:file "color-editor" :depends-on ("demo-driver"))
             ;(:file "graphics-editor" :depends-on ("demo-driver"))
             ))))
