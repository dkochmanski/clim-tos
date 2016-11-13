;; See the file LICENSE for the full license governing this code.
;;

(defpackage :tk
  ;;-- No we really need
  ;; to use the x11 package?
  (:use :common-lisp
	#+allegro :ff
	#+ccl :ccl
	#+ignore :x11)
  (:nicknames :xt)
  #+allegro
  (:import-from :excl #:if*)
  (:import-from :clim-utils #:fintern #:package-fintern #-allegro #:if*)
  (:export
   #:initialize-motif-toolkit
   #:widget-parent
   #:manage-child
   #:get-values
   #:top-level-shell
   #:popup
   #:popdown
   #:manage-child
   #:realize-widget
   #:card32
   #:card29
   #:card24
   #:card16
   #:card8
   #:int32
   #:int16
   #:int8
   #:with-server-grabbed
   #:window-property-list
   ))

#+allegro
(setf (excl:package-definition-lock (find-package :tk)) t)

(in-package :tk)
