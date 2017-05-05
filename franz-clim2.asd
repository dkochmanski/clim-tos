
(in-package #:asdf-user)

(defsystem #:franz-clim2
  :description "CLIM 2 implementation released by Franz Inc."
  :license "BSD-4-Clause"
  :depends-on (#:franz-clim2/core))

;;; based on `clim-standalone' in sys/sysdcl.lisp
(defsystem #:franz-clim2/core
  :description "Core functionality (doesn't contain backends)."
  :depends-on (#:franz-clim2/silica)
  :components ((:module "clim"
                        :components
                        (;#+(or) "Basic tools"
                         (:file "gestures")
                         (:file "defprotocol")
                         (:file "stream-defprotocols")
                         (:file "defresource")
                         (:file "temp-strings")
                         (:file "clim-defs")

                         #+(or) "Definitions and protocols"
                         (:file "stream-class-defs")
                         (:file "interactive-defs")
                         (:file "cursor")
                         (:file "view-defs")
                         (:file "input-defs")
                         (:file "input-protocol")
                         (:file "output-protocol")

                         #+(or) "Output recording"
                         (:file "recording-defs")
                         (:file "formatting-output-defs")
                         (:file "recording-protocol")
                         (:file "text-recording")
                         (:file "graphics-recording")
                         (:file "design-recording")

                         #+(or) "Input editing"
                         (:file "interactive-protocol")
                         (:file "input-editor-commands")
                         #+(and allegro (not acl86win32)) (:file "japanese-input-editor")

                         #+(or) "Incremental redisplay"
                         (:file "incremental-redisplay")

                         #+(or) "Windows"
                         (:file "coordinate-sorted-set")
                         (:file "r-tree")
                         (:file "window-stream")
                         (:file "pixmap-streams")

                         #+(or) "Presentation types"
                         (:file "ptypes1")
                         (:file "completer")
                         (:file "presentations")
                         (:file "translators")
                         (:file "histories")
                         (:file "ptypes2")
                         (:file "standard-types")
                         #+allegro (:file "excl-presentations")

                         #+(or) "Formatted output"
                         (:file "table-formatting")
                         (:file "graph-formatting")
                         (:file "surround-output")
                         (:file "text-formatting")

                         #+(or) "Pointer tracking"
                         (:file "tracking-pointer")
                         (:file "dragging-output")

                         #+(or) "Gadgets"
                         (:file "db-stream")
                         (:file "gadget-output")

                         #+(or) "Application building substrate"
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

                         #+(or) "Bootstrap everything"
                         (:file "stream-trampolines")))))

;;; based on `clim-utils'
(defsystem #:franz-clim2/utils
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
                         #+ (or) (:file "trivial-gray-streams")

                         ;; Basic utilities for Silica and CLIM
                         (:file "clim-macros")
                         (:file "transformations")
                         (:file "regions")
                         (:file "region-arithmetic")
                         (:file "extended-regions")
                         (:file "base-designs")
                         (:file "designs")))))

;;; based on `clim-silica'
(defsystem #:franz-clim2/silica
  :depends-on (#:franz-clim2/utils)
  :components
  ((:module "silica"
            :components
            (;#+(or) "Silica"
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
             #+(or) "Layout gadgets"
             (:file "layout")
             (:file "db-layout")
             (:file "db-box")
             (:file "db-table")

             #+(or) "'Physical' gadgets"
             (:file "gadgets")
             (:file "db-border")
             (:file "db-scroll")))))

(defsystem #:franz-clim2/postscript
  :description "Draw-only backend as defined in the specification.")

(defsystem #:franz-clim2/clx-backend
  :description "Reference backend for X-Window system.")

(defsystem #:franz-clim2/examples
  :description "Applications bundled in franz-clim2 repository.")
