;; copyright (c) 1985,1986 Franz Inc, Alameda, Ca.
;; copyright (c) 1986-2002 Franz Inc, Berkeley, CA  - All rights reserved.
;;
;; The software, data and information contained herein are proprietary
;; to, and comprise valuable trade secrets of, Franz, Inc.  They are
;; given in confidence by Franz, Inc. pursuant to a written license
;; agreement, and may be stored and used only in accordance with the terms
;; of such license.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure of the software, data and information
;; contained herein by any agency, department or entity of the U.S.
;; Government are subject to restrictions of Restricted Rights for
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
;;
;; $Id: load.lisp,v 1.9.36.1 2002/02/08 19:11:24 layer Exp $

(in-package :user)

#+(and Allegro (or microsoft-32 mswindows))
(eval-when (compile load eval) (push :acl86win32 *features*))

#+acl86win32
(let ((excl::*enable-package-locked-errors* nil))
  (defun system::rcsnote (&rest args) (warn "RCSNOTE call with ~a~%" args) nil))

#+acl86win32
(setq comp:trust-dynamic-extent-declarations-switch nil)

#+acl86win32
(defvar *clim-root* (make-pathname
                       :device (pathname-device *load-pathname*)
                       :directory (butlast (pathname-directory *load-pathname*))))

#+acl86win32 ;; aclpc gets climpath in do.lisp
(defun climpath (sub) (merge-pathnames sub *clim-root*))

#+aclpc
(let* ((defsys-source-path (climpath "sys\\defsystem.lisp"))
       (defsys-fsl-path (climpath "sys\\defsystem.fsl"))
       (fsl-file-date (if (probe-file defsys-fsl-path)
			  (file-write-date defsys-fsl-path))))
  (when (probe-file defsys-source-path)
    (unless (and fsl-file-date
		 (>= fsl-file-date (file-write-date defsys-source-path)))
      (compile-file defsys-source-path)))
  (load (climpath "sys\\defsystem.fsl")))

;; if you turn on this feature be sure to add appropriate forms below
#+acl86win32-uses-clim-defsystem (compile-file-if-needed (climpath "sys\\defsystem.lisp"))
#+acl86win32-uses-clim-defsystem (load (climpath "sys\\defsystem.fasl"))

#+acl86win32 (pushnew :ansi-90 *features*)
#+acl86win32
(setf (logical-pathname-translations "clim2")
  `((";**;*.*" ,*clim-root*)))

#+aclpc (load (climpath "sys\\sysdcl-pc.lisp"))
#+acl86win32 (load (climpath "sys\\sysdcl.lisp"))

(let (#+acl86win32 (excl::*enable-package-locked-errors* nil))

  (load (climpath "aclpc\\sysdcl.lisp"))

  #+aclpc (clim-defsystem:load-system "aclpc-clim")
  #+acl86win32 (load-system 'aclnt-clim)

  (unless (and (boundp '*no-clim-demos*) *no-clim-demos*)
    ;; note: test-suite.fsl has been in the demo folder in past distributions
    #+aclpc (load (climpath "test\\test-suite.fsl"))
    #+acl86win32 (load (climpath "test\\test-suite.fasl"))
    #+aclpc (load (climpath "demo\\sysdcl-pc.lisp"))
    #+acl86win32 (load (climpath "demo\\sysdcl.lisp"))
    #+aclpc (clim-defsystem:load-system "clim-demo")
    #+acl86win32 (load-system 'clim-demo)))

;;; Remove this feature for the final world.
#+acl86win32-uses-clim-defsystem
(setq *features* (delete :defsystem *features*))

;;;+++ why must we do this?  Without this, for some reason,
;;; create-overlapped-window fails on second invocations!

#+aclpc (load (climpath "aclpc\\acl-class.fsl"))
#+acl86win32 (load (climpath "aclpc\\acl-class.fasl"))

;; postscript backend - has anyone tried this?

#+aclpc (load (climpath "postscript\\sysdcl-pc.lisp"))
#+acl86win32 (load (climpath "postscript\\sysdcl.lisp"))

#+aclpc (clim-defsystem:load-system "postscript-clim")
#+acl86win32 (load-system 'postscript-clim)

(in-package :clim-user)

#+acl86win32x (setq mp::*default-process-quantum* 0.6)

(defun run-tests ()
  (let ((frame (make-application-frame 'clim-tests)))
    (raise-frame frame)
    (run-frame-top-level frame)))
