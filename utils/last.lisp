;; See the file LICENSE for the full license governing this code.
;;

;;; All this is allegro-sepcific.

(in-package :system)

(provide
 #+mswindows :climnt
 #-mswindows
 (cond ((excl::featurep :clim-motif) :climxm)
       ((excl::featurep :clim-openlook) :climol)
       (t (error "Unknown Xt backend"))))
