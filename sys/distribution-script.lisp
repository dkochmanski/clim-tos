;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: USER; Base: 10; Lowercase: Yes -*-
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
;; $Id: distribution-script.lisp,v 1.9.36.1 2002/02/08 19:11:24 layer Exp $

#|
:Distribute Systems CLIM-BINARIES Latest :Distribute Patch Sources No :Include Patches Yes :Query Confirm-Only :Machine Types Imach :File Types Binaries 
|#
(sct:define-distribution-system clim-binaries
    (:default-pathname "SYS:CLIM;REL-2;"
     :distribute-sources nil
     :distribute-binaries t
     :source-category :restricted)
  (:module clim
    ("CLIM" "Genera-CLIM" "CLX-CLIM" "PostScript-CLIM" "CLIM-Demo" "CLIM-Compatibility")
    (:type :system) (:version :latest))
  (:serial 
    ";SYS;LOAD-CLIM2.LISP"
    clim
    ";TEST;TEST-SUITE.*"
    ";TEST;TEST.*"
    ";TEST;TEST-BUTTONS.*"
    ";TEST;TEST-SLIDERS.*"
    ";TEST;SIMPLE-TEST.*"
    ";SPECS;CLIM.PS"))

#|
:Distribute Systems CLIM-SOURCES Newest :Distribute Patch Sources Yes :Include Patches Yes :Query Confirm-Only :Machine Types Imach :File Types Sources
|#
(sct:define-distribution-system clim-sources
    (:default-pathname "SYS:CLIM;REL-2;"
     :distribute-sources t
     :distribute-binaries nil
     :source-category :restricted)
  (:module clim
    ("CLIM" "Genera-CLIM" "CLX-CLIM" "PostScript-CLIM" "CLIM-Demo" "CLIM-Compatibility")
    (:type :system) (:version :newest))
  (:serial 
    ";SYS;LOAD-CLIM2.LISP"
    clim
    ";TEST;TEST-SUITE.LISP"
    ";TEST;TEST.LISP"
    ";TEST;TEST-BUTTONS.LISP"
    ";TEST;TEST-SLIDERS.LISP"
    ";TEST;SIMPLE-TEST.LISP"
    ";SPECS;CLIM.PS"))


;;; Distribution script for Genera 8.3

(sct:define-distribution-system load-clim
    (:default-pathname "SYS:CLIM;REL-2;SYS;"
     :distribute-sources t
     :distribute-binaries t
     :source-category :basic)
  (:serial 
    "LOAD-CLIM2.LISP"
    "SYS:CLIM;REL-2;SPECS;CLIM.PS"))

(sct:define-distribution-system clim-tests
    (:default-pathname "SYS:CLIM;REL-2;TEST;"
     :distribute-sources t
     :distribute-binaries t
     :source-category :basic)
  (:serial 
    "TEST-SUITE.*"
    "TEST.*"
    "TEST-BUTTONS.*"
    "TEST-SLIDERS.*"
    "SIMPLE-TEST.*"))

(defvar *CLIM-tape*
	'((load-clim :newest (:distribute-sources t))
	  (clim 50 (:distribute-sources nil))
	  (genera-clim 50 (:distribute-sources nil))
	  (clx-clim 50 (:distribute-sources t))
	  (postscript-clim 50 (:distribute-sources nil))
	  (clim-demo 50 (:distribute-sources t))
	  (clim-tests :newest (:distribute-sources t))))

(defmacro collect-systems-list (system-list)
  `(loop for (system version args) in ,system-list
	 collecting (list (sct:find-system-named system) version args)))

(cp:define-command (com-write-CLIM-tape :name "Write CLIM tape"
					:command-table "Global")
    ()
  (dis:write-distribution-on-tape (collect-systems-list *CLIM-tape*)
				  :disk
				  :machine-types :all
				  :source-category :basic
				  :distribute-sources t
				  :distribute-binaries t
				  :include-components nil
				  :include-journals t
				  :include-patches t
				  :distribute-patch-sources nil
				  :flatten-files t
				  :compress-files nil
				  :full-length-tapes t
				  :no-reload-system-declaration t
				  :use-cached-checkpoint t
				  :query :confirm
				  :included-files-checkpoint :none))
