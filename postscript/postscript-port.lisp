;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: POSTSCRIPT-CLIM; Base: 10; Lowercase: Yes -*-

;; $fiHeader: postscript-port.lisp,v 1.16 93/03/19 09:44:27 cer Exp $

(in-package :postscript-clim)

"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved."

;;; This does not conform to the conventions described under Appendix C:
;;; Structuring Conventions of the PostScript Language Reference Manual.
;;; Part of the reason for non-conformance is the maybe-send-feature
;;; hack.  Were it possible for this implementation to send more than
;;; one page to the printer, then the stuff downloaded using the
;;; maybe-send-feature hack would be associated with a single page
;;; rather than with the whole document and reordering of the pages by
;;; some other tool could cause features which were first generated by
;;; one page and later referenced by another to become undefined if
;;; those pages were reordered.  The %% comments do conform except for
;;; %%Page and the feature reordering problem.

;;; Line thickness currently uses "1 setlinewidth" for minimum line
;;; thickness.  This can produce a line 2 pixels wide since there can be
;;; 2 pixels that are less than or equal to 1/2 away from the line.  "0
;;; setlinewidth" means to use the minimum for the resolution of the
;;; device.  At least now, for the case of line thickness of 1 :normal, this
;;; will use 0 setlinewidth for output to the Apple Laser Writer.

(defparameter *ps-magic-baseline-factor* 0.2)	;estimation of % baseline to bottom.

(defparameter *1-pixel=points* 1.0)		;1 "average" pixel is about 1.2 points

;; Convert CLIM units ("pixels") to (printers') points
(defmacro pixels-to-points (&rest pixels)
  (let ((forms nil))
    (dolist (pixel pixels)
      (push `(setq ,pixel (* ,pixel *1-pixel=points*)) forms))
    `(progn ,@(nreverse forms))))

;; Convert (printers') points to CLIM units ("pixels")
(defmacro points-to-pixels (&rest points)
  (let ((forms nil))
    (dolist (point points)
      (push `(setq ,point (float (/ ,point *1-pixel=points*) 0f0)) forms))
    `(progn ,@(nreverse forms))))

(defvar *annotate-postscript* nil)

(defmacro annotating-postscript ((medium printer-stream) &body body)
  `(when *annotate-postscript*
     (let* ((,printer-stream (slot-value ,medium 'printer-stream))
	    (text (with-output-to-string (,printer-stream) ,@body))
	    (start 0))
       (loop
	 (let ((newline (position #\newline text :start start)))
	   (write-string "% " ,printer-stream)
	   (write-string text ,printer-stream :start start :end newline)
	   (unless newline (return))
	   (setq start (1+ newline))))
       (fresh-line ,printer-stream))))


;;; How the font hackery works:

;;; For each CLIM text style used in the output, a corresponding scaled
;;; font must exist in the PostScript engine.  (The PostScript books
;;; recommend that for efficiency you cache the results of operations
;;; like scalefont).  There is one PS-FONT-COMPAT-KLUDGE structure
;;; created for each CLIM text style used.  These structures are stored
;;; in the FONT-MAP slot of POSTSCRIPT-MEDIUM streams.
;;; Inside the PostScript engine there is a corresponding array named
;;; fontarray, whose i'th element contains the scaled postscript font
;;; object described by the i'th element of FONT-MAP.

;;; The function GET-FONT-COMPAT-STR is used to find the
;;; PS-FONT-COMPAT-KLUDGE object corresponding to a CLIM text style.
;;; The first time a CLIM text style is encounterred,
;;; GET-FONT-COMPAT-STR creates a PS-FONT-COMPAT-KLUDGE object for it,
;;; stores it in FONT-MAP and generates postscript code to construct the
;;; corresponding scaled postscript font and store it in fontarray
;;; using the postscript estfont procedure defined in the preamble.

;;; CONVERT-CLIM-TO-PS-FONT-DESCRIPTION converts a CLIM text style, eg.
;;; (:serif :bold :large) to a postscript font description, eg. ("Times"
;;; :bold 12).   When estfont is used, it interns a font "Times-Bold" of
;;; size 12 as the appropriate element of fontarray.

;;; This structure is used to represent a PostScript font masquerading as a
;;; CLIM-world entity.  They populate the font-map array of the stream.
(defstruct (ps-font-compat-kludge (:conc-name psfck-))
  index					;index into stream's array.
  style-descriptor			;parse-text-style output.
  true-ps-descriptor			;("Times" :italic 6 [pts])
  points				;point-size
  clim-height				;equivalent size in CLIM "pixels"
  clim-ascent				;top-to-baseline, "pixels"
  clim-descent				;baseline-to-bottom
  width-table				;width array or constant #.
  (established nil))			;T ==> written to the printer

;;; Each element is ((family face) width-or-width-table-array height ascent descent)
(defvar *char-width-tables* nil)

(defun make-new-fcs (text-style index)
  (let ((fcs (make-ps-font-compat-kludge
	       :index index
	       :style-descriptor text-style
	       :true-ps-descriptor (convert-clim-to-ps-font-description text-style))))
    (initialize-fcs fcs)
    fcs))

(defun initialize-fcs (fcs)
  (let* ((points (third (psfck-true-ps-descriptor fcs)))
	 (key `(,(first (psfck-true-ps-descriptor fcs))
		,(second (psfck-true-ps-descriptor fcs))))
	 (metrics (assoc key *char-width-tables* :test #'equal)))
    (unless metrics
      (error "No font metrics for ~S" key))
    (setf (psfck-points fcs) points)
    (let ((width-table (second metrics))
	  (height (third metrics))
	  (ascent (fourth metrics))
	  (descent (fifth metrics)))
      (setf (psfck-clim-height fcs) (* points height))
      (setf (psfck-clim-ascent fcs) (* points ascent))
      (setf (psfck-clim-descent fcs) (* points descent))
      (setf (psfck-width-table fcs) width-table)))
  nil)

;;; Each element is composed of:
;;;      - a CLIM text family keyword,
;;;      - the name of a font to use for that family,
;;;      - a list of point sizes corresponding to the text size keywords
;;;        in the corresponding positions in *psftd-keywords*.
;;; We should be able to phjase this out since we have a
;;; STANDARDIZE-STYLE method for postscript devices.
(defparameter *postscript-font-translate-data*
	     '((:fix "Courier" (4 6 7 9 11 14 18))
	       (:sans-serif "Helvetica" (5 7 8 10 12 16 20))
	       (:serif "Times" (5 7 8 10 12 16 20))))

(defparameter *psftd-keywords* '(:tiny :very-small :small :normal :large :very-large :huge))

;; The second element is a character size, which is either a number or a
;; place to go looking to get the character width table
(defparameter *ps-font-family-data*
	      '(("Times" 
		 (nil "tir" "Times-Roman")
		 (:bold "tib" "Times-Bold")
		 (:italic "tii" "Times-Italic")
		 (:bold-italic "tibi" "Times-BoldItalic"))
		("Helvetica"
		 (nil "he" "Helvetica")
		 (:bold "heb" "Helvetica-Bold")
		 (:italic "heo" "Helvetica-Oblique")
		 (:bold-italic "hebo" "Helvetica-BoldOblique"))
		("Courier"
		 (nil "co" "Courier")
		 (:bold "cob" "Courier-Bold")
		 (:italic "coo" "Courier-Oblique")
		 (:bold-italic "cobo" "Courier-BoldOblique"))))

(defun get-ps-fam-face-name (fcs)
  (let ((fam (first (psfck-true-ps-descriptor fcs)))
	(face (second (psfck-true-ps-descriptor fcs))))
    (let ((famdata (cdr (or (assoc fam *ps-font-family-data* :test #'string-equal)
			    (error "No info for PostScript font family ~A" fam)))))
      (third (or (assoc face famdata)
		 (error "No info for PostScript family ~A face ~A." fam face))))))
	
;; Value is a 3 element list acceptable as a returned value from
;; CONVERT-CLIM-TO-PS-FONT-DESCRIPTION:  a postscript family name as
;; appears in the CARs of the elements of *PS-FONT-FAMILY-DATA*, a face
;; keyword as returned by the function KEYWORDIFY-STYLE-FACE, and a
;; size in points.
(defparameter *ps-font-description-for-undefined-style* '("Courier" nil 4))	      

(defun convert-clim-to-ps-font-description (style)
  (let ((size (text-style-size style))
	(face (keywordify-style-face (text-style-face style)))
	(family (text-style-family style)))
    (if (and *ps-font-description-for-undefined-style*
	     (eq style *undefined-text-style*))
	;;--- Probably pointless to issue any sort of warning here
	*ps-font-description-for-undefined-style*
	(let* ((famdat (or (assoc family *postscript-font-translate-data*)
			   (error "Don't have PostScript support for family ~A." family)))
	       (points (if (numberp size)
			   size
			   (point-size-for-size-keyword size famdat)))
	       (psname (second famdat)))
	  `(,psname ,face ,points)))))

(defun keywordify-style-face (face)
  (cond ((eq face ':roman) nil)
	((atom face) face)
	((or (equal face '(:bold :italic))
	     (equal face '(:italic :bold))) :bold-italic)
	(t (list :numeric-code face))))

(defun point-size-for-size-keyword (size-keyword family-data)
  (nth (or (position size-keyword *psftd-keywords*)
	   (error "Don't have PostScript support for size ~A." size-keyword))
       (third family-data)))

;;; This actually reads metrics for the Apple LaserWriter.  Some day we
;;; should make the font width table stuff device independent.
;;; The metrics are in sys:clim;laserwriter-metrics.lisp.newest.
;;; That file consists of invocations of this function.
;;; Don't bother loading the width information for fixed width fonts like Courrier
;;; which already have their width hard-coded in *ps-font-family-data*?

(defun setup-laserwriter-metrics (font-info)
  (destructuring-bind ((name scale box) . char-widths)
      font-info
  (let ((width-table nil))
    (multiple-value-bind (family face size-kludge)
	(find-family-and-face-for-postscript-font-name name)
      ;; If the kludgy size element of the font's description in
      ;; *ps-font-family-data* is a number, just use it instead of
      ;; building the width table.
      (cond ((numberp size-kludge)
	     (setq width-table size-kludge))
	    ((numberp char-widths)
	     (setq width-table (float (/ char-widths scale) 0f0)))
	    (t
	     (setq width-table (make-array 256))
	     (dolist (char-info char-widths)
	       (let ((char-code (first char-info))
		     (char-width (float (/ (second char-info) scale) 0f0)))
		 (setf (aref width-table char-code) char-width)))))
      (let ((key `(,family ,face)))
	(when (car key)				;check family to make sure we found the name
	  (let* ((temp (assoc key *char-width-tables* :test #'equal))
		 (height (abs (float (/ (- (fourth box) (second box)) scale) 0f0)))
		 (ascent (abs (float (/ (fourth box) scale) 0f0)))
		 (descent (abs (float (/ (second box) scale) 0f0)))
		 (data `(,width-table ,height ,ascent ,descent)))
	    (if temp
		(setf (cdr temp) data)
		(push `(,key ,@data) *char-width-tables*)))))))))

(defun find-family-and-face-for-postscript-font-name (name)
  (dolist (family-data *ps-font-family-data*)
    (let ((family (car family-data)))
      (dolist (face-data (cdr family-data))
	(let ((face (first face-data))
	      (size-kludge (second face-data))
	      (fname (third face-data)))
	  (when (string= name fname)
	    (return-from find-family-and-face-for-postscript-font-name
	      (values family face size-kludge))))))))


(defclass postscript-medium (basic-medium)
     ((printer-stream :initarg :stream)
      (current-color :initform nil)	;for decoding stippled inks
      (features-sent :initform nil)
      (curfont :initform nil)			;a psfck structure
      (ch1buf :initform (make-array 1 :element-type #+ANSI-90 'character
						    #-ANSI-90 'string-char))))

(defmethod implementation-pixels-per-point ((medium postscript-medium))
  (float (/ *1-pixel=points*) 0f0))

(defmethod medium-force-output ((medium postscript-medium))
  (force-output (slot-value medium 'printer-stream)))

(defmethod medium-finish-output ((medium postscript-medium))
  (finish-output (slot-value medium 'printer-stream)))

(defmethod medium-clear-output ((medium postscript-medium))
  (clear-output (slot-value medium 'printer-stream)))


;;; Support routines

(defmacro making-ps-array ((printer-stream) &body body)
  `(progn (write-string " [ " ,printer-stream)
	  ,@body
	  (write-string " ] " ,printer-stream)))

(defmacro making-ps-hex-string ((printer-stream) &body body)
  `(progn (write-char #\< ,printer-stream)
	  ,@body
	  (write-char #\> ,printer-stream)))

(defmacro with-postscript-gsave (medium &body body)
  `(flet ((with-gsave-body () ,@body))
     (declare (dynamic-extent #'with-gsave-body))
     (invoke-with-postscript-gsave ,medium #'with-gsave-body)))

(defmethod invoke-with-postscript-gsave ((medium postscript-medium) continuation)
  (let ((printer-stream (slot-value medium 'printer-stream)))
    (format printer-stream " gsave~%")
    (funcall continuation)
    (format printer-stream " grestore~%")))

(defmethod maybe-send-feature ((medium postscript-medium) feature-name code)
  (with-slots (features-sent printer-stream) medium
    (unless (member feature-name features-sent)
      (annotating-postscript (medium printer-stream)
	(format printer-stream "---------------- Feature ~A ----------------"
	  feature-name))
      (write-string code printer-stream)
      (annotating-postscript (medium printer-stream)
	(format printer-stream "---------------- End Feature ~A ----------------"
	  feature-name))
      (push feature-name features-sent))))

(defmethod ps-pos-op ((medium postscript-medium) op x y &rest args)
  (declare (dynamic-extent args))
  (let ((printer-stream (slot-value medium 'printer-stream))
	(port (port medium)))
    (pixels-to-points x y)
    (write-char #\space printer-stream)
    (ps-optimal-flonize
      (+ (* (slot-value port 'page-indent)
	    (slot-value port 'device-units-per-inch)) x)
      printer-stream)
    (write-char #\space printer-stream)
    (ps-optimal-flonize
      (- (* (slot-value port 'page-height)
	    (slot-value port 'device-units-per-inch)) y)
      printer-stream)
    (dolist (arg args)
      (write-char #\space printer-stream)
      (if (numberp arg)
	  (ps-optimal-flonize arg printer-stream)
	  (write arg :stream printer-stream :escape nil)))
    (format printer-stream " ~A" op)
    (if (string-equal op "m")
	(write-char #\space printer-stream)
	(terpri printer-stream))))

(defmethod ps-rel-pos-op ((medium postscript-medium) op x y &rest args)
  (declare (dynamic-extent args))
  (pixels-to-points x y)
  (let ((printer-stream (slot-value medium 'printer-stream)))
    (write-char #\space printer-stream)
    (ps-optimal-flonize x printer-stream)
    (write-char #\space printer-stream)
    (ps-optimal-flonize (- y) printer-stream)
    (dolist (arg args)
      (write-char #\space printer-stream)
      (if (numberp arg)
	  (ps-optimal-flonize arg printer-stream)
	  (write arg :stream printer-stream :escape nil)))
    (format printer-stream " ~A~%" op)))

;;; the software that drives the LGP2 (sys:hardcopy;postscript.lisp)
;;; uses LGP:FAST-PRINT-NUM except for writing transformation matrices,
;;; for which it uses 
;;;       (prin1 (if (fixp elem) elem (float elem)) output-stream)
;;; and also some cases of Format ~D.
(defun ps-optimal-flonize (n stream)
  ;; Lifted from definition of LGP:FAST-PRINT-NUM in SYS:HARDCOPY;POSTSCRIPT.LISP
  (if (and (not (zerop n))
	   (< -1 n 1))
      (format stream "~F" (float n))
      (multiple-value-bind (integer frac)
	  (etypecase n
	    (integer (values (abs n) 0))
	    (float (truncate (round (* (abs n) 100.)) 100))
	    (rational (truncate (round (* (abs n) 100.)) 100)))
	(if (>= integer 10000.)
	    (if (integerp n)
		(write n :stream stream :escape nil :base 10 :radix nil)
		(format stream "~$" n))
	    (let ((from 8)
		  (negative-p nil))
	      (when (minusp n)
		(setq negative-p t))
	      (with-stack-array (string from :element-type #+ANSI-90 'character 
							   #-ANSI-90 'string-char)
		(macrolet ((add-char (char)
			     `(setf (aref string (decf from)) ,char)))
		  (when (/= frac 0)
		    (multiple-value-bind (frac1 frac2)
			(truncate frac 10.)
		      (when (/= frac2 0)
			(add-char (code-char (+ (char-code #\0) frac2))))
		      (add-char (code-char (+ (char-code #\0) frac1))))
		    (add-char #\.))
		  (let ((digit 0))
		    (loop
		      (multiple-value-setq (integer digit)
			(truncate integer 10.))
		      (add-char (code-char (+ (char-code #\0) digit)))
		      (when (zerop integer) (return))))
		  (when negative-p (add-char #\-))
		  (write-string string stream :start from)
		  n)))))))

;; "Not suitable for ritual use."
(defparameter *ps-ellipse-code*
"
/emtrx matrix def
/elpd 8 dict def
/ellipse {{arc} ellipsei} def
/ellipsen {{arcn} ellipsei} def
/ellipsei {elpd begin /arcp exch def /ea exch def /sa exch def /yra exch def /xra exch def
      /y exch def /x exch def
emtrx currentmatrix
x y translate xra yra scale 0 0 1 sa ea arcp setmatrix end} def
")

(defparameter *pattern-code*
        ;; for drawing filled patterns
        "/imgdict 12 dict def
%draw image.  One source pixel to one user space unit.
%width width-rounded-up height
/img { imgdict begin
        [/height /bitwidth /width ] {exch def} forall
        /nbits bitwidth height mul def
        /str 100 string def
        nbits 0 ne {
          gsave width height scale
          bitwidth height true [bitwidth 0 0 height neg 0 height] 
            {   nbits 800 ge {/nbits nbits 800 sub def str} 
                               {nbits 8 idiv string /nbits 0 def}
                            ifelse 
                  currentfile exch readhexstring pop}
          imagemask grestore
        } if end
        } def
/fmod { 2 copy div floor mul sub } bind def
%draw patterned rectangle.  One source pixel to scale device units (ignoring user scale).
%width height pattern scale
/pat { imgdict begin gsave
        [/scal /patseq ] {exch def} forall
        /patheight patseq length def
% This makes the assumption that the zero th element of the array
% describes the first row of the pattern
        /patwidth patseq 0 get length 8 mul def
%back up to an even phase boundary
        /pswidth patwidth scal mul def
        /psheight patheight scal mul def
% untransform the width,height by the CTW
        pswidth psheight idtransform
        0 0 transform psheight fmod neg exch pswidth fmod neg exch idtransform
        3 -1 roll exch dup 0 gt {add} {exch pop} ifelse
        3 1 roll dup 0 gt {add} {exch pop} ifelse exch 2 copy translate
        3 -1 roll exch abs add 3 1 roll abs add exch dtransform
        psheight div abs ceiling cvi patheight mul /height exch def
        pswidth div abs ceiling cvi patwidth mul /width exch def
        width 0 ne { height 0 ne {
        /scanline -1 def /linebits 0 def
        width height idtransform abs scale scal dup scale
        width height true [width 0 0 height neg 0 height] 
% This procedure returns the same row the right number of times
% I suppose if the pattern does not fit exactly it does not matter
% since there is a clipping region.
% This certainly makes the assumption that one element = one line
        { linebits 0 le { /linebits width def
                          /scanline scanline 1 add patheight mod def
                          /linepat patseq scanline get def
                        } if
        /linebits linebits patwidth sub def linepat }
        imagemask } if } if grestore end
      } def
%draw pattern in all of visible area.
%pattern scale opaque-p
/patfill1 { initmatrix clippath
%condition-case for nocurrentpoint, returning empty rectangle
             errordict begin
               /nocurrentpoint dup dup load exch { pop 0 0 0 0 } def 
                 pathbbox
               6 -2 roll def end
% pattern scale opaque-p bx1 by1 bx2 by2
             4 2 roll 2 copy translate 4 -2 roll
% pattern scale opaque-p  bx1 by1 bx2 by2 
             3 -1 roll sub 3 1 roll exch sub exch
% pattern scale opaque-p dx dy
             3 -1 roll { 2 copy gsave 1 setgray newpath
                         0 0 moveto 0 exch lineto 0 rlineto currentpoint pop 0 lineto
                         closepath fill grestore } if
% The above appears to draw a rectangle clipped by the path
% pattern scale dx dy
             4 -2 roll pat } def
%like fill, etc. but with pattern, scale and opaque-p options.
/patfill { gsave clip patfill1 grestore newpath } def
/pateofill { gsave eoclip patfill1 grestore newpath } def
/patstroke { gsave strokepath clip patfill1 grestore newpath } def
/cerfill { imgdict begin
 [/depth /height /width] {exch def} forall
 gsave
   clip
% Get the bounding box of the clip path on the stack   
   initmatrix clippath
   %condition-case for nocurrentpoint, returning empty rectangle
             errordict begin
               /nocurrentpoint dup dup load exch { pop 0 0 0 0 } def 
                 pathbbox
               6 -2 roll def end
% Stack is bx1 by1 bx2 by2
% So set the origin
  pop pop translate
  /str 2 string def
  width height scale
  width height depth [width 0 0 height neg 0 height]
  { currentfile str readhexstring pop}
  image
 grestore
end } def
")

(defvar *postscript-prologue*
        "statusdict /waittimeout 30 put
/fontarray ~D array def
/f {fontarray exch get setfont} def
/estfont {findfont exch scalefont fontarray 3 1 roll put} def
/m {moveto} def
")

(defmethod postscript-prologue ((medium postscript-medium)
				&key scale-factor (orientation :portrait)
				     header-comments)
  (let ((printer-stream (slot-value medium 'printer-stream))
	(port (port medium)))
    (format printer-stream "%!PS-Adobe-2.0 EPSF-2.0~%")
    (multiple-value-bind (left top right bottom)
	(postscript-bounding-box-edges (medium-sheet medium))
      (format printer-stream "%%BoundingBox: ~D ~D ~D ~D~%"
	(float left) (float top) (float right) (float bottom)))
    (format printer-stream "%%Creator: CLIM 2.0~%")
    (let ((title (getf header-comments :title)))
      (when title
        (format printer-stream "%%Title: ~A~%" title)))
    (let ((for (or (getf header-comments :for) #+Genera zl:user-id)))
      (when for
        (format printer-stream "%%For: ~A~%" for)))
    
    (multiple-value-bind (second minute hour date month year)
	(decode-universal-time (get-universal-time))
      (format printer-stream "%%CreationDate: ~D-~A-~D ~2,'0D:~2,'0D:~2,'0D~%"
	date (svref #("Jan" "Feb" "Mar" "Apr" "May" "Jun"
		      "Jul" "Aug" "Sep" "Oct" "Nov" "Dec") (1- month)) year
	hour minute second))
    (format printer-stream "%%DocumentFonts: (atend)~%")
    (format printer-stream "%%EndComments~%")
    (format printer-stream *postscript-prologue* (length (slot-value (port medium) 'font-map)))
    (ecase orientation
      (:portrait
        (format printer-stream
            "/format-rotation 0 def ~%/format-y-translation 0 def~%"))
      (:landscape
        (format printer-stream
            "/format-rotation -90 def ~%/format-y-translation ~D def~%" 
          (float (* (slot-value port 'page-height)
		    (slot-value port 'device-units-per-inch))))))
    (format printer-stream "/format-scale ~D def~%" (float (or scale-factor 1)))
    (format printer-stream
        "/new-matrix {0 format-y-translation translate
		      format-rotation rotate
		      format-scale format-scale scale} def
	 /new-page {showpage new-matrix} def~%")
    (postscript-device-prologue port printer-stream)
    (format printer-stream "%%EndProlog~%")
    (format printer-stream "~%new-matrix~%")))

(defmethod postscript-epilogue ((medium postscript-medium))
  (let* ((printer-stream (slot-value medium 'printer-stream))
	 (port (port medium))
	 (font-map (slot-value port 'font-map)))
    (format printer-stream "showpage~%")
    (postscript-device-epilogue port printer-stream)
    (format printer-stream "%%Trailer~%")
    (let ((font-names-used nil))
      (dotimes (index (length font-map))
        (let ((fcs (aref font-map index)))
          (when fcs
            (pushnew (get-ps-fam-face-name fcs) font-names-used :test #'string-equal))))
      (format printer-stream "%%DocumentFonts:~{~^ ~A~}~%" (nreverse font-names-used)))))

(defun send-pattern (medium printer-stream pattern)
  (maybe-send-feature medium 'pattern-program *pattern-code*)
  (making-ps-array (printer-stream)
    (let ((height (array-dimension pattern 0))
	  (width (array-dimension pattern 1)))
      (dotimes (j height)
	(making-ps-hex-string (printer-stream)
	  (send-raster printer-stream pattern 0 j width (1+ j) nil)))))
  (terpri printer-stream)
  (write-string " 4 true " printer-stream))

#+ignore
(defun send-raster (printer-stream raster left top right bottom &optional (terpri t))
  (assert (= bottom (1+ top)))
  (unless (zerop (rem right 8))
    (error "Sorry, can't hack right /= 0 (mod 8); you have ~D" right))
  (with-stack-array (arr (array-total-size raster) 
			 :element-type #+Genera '(unsigned-byte 8)
				       #-Genera (array-element-type raster)
			 :displaced-to raster)
    (with-temporary-string (buf :length 100)
      (let ((bytes-per-row (truncate (array-dimension raster 1) 8))
	    (bytes-per-raster (ceiling (- right left) 8)))
	(let ((toprow (* top bytes-per-row))
	      (botrow (* bottom bytes-per-row))
	      (bigend-digit-char "084c2a6e195d3b7f")
	      (j 0)
	      #+(or Genera Minima) (buf buf)
	      #+(or Genera Minima) (arr arr))
	  (declare (type vector bigend-digit-char buf arr))
	  (flet ((force-buf ()
		   (setf (fill-pointer buf) j)
		   (with-temporary-substring (subbuf buf 0 j)
		     (write-string subbuf printer-stream)
		     (when terpri
		       (terpri printer-stream)))))
	    (do* ((index (- botrow bytes-per-row) (- index bytes-per-row))
		  (i index index))
		 ((< index toprow))
	      (repeat bytes-per-raster
		(let ((byte (aref arr i)))
		  (setf (aref buf j) 
			(aref bigend-digit-char (ldb (byte 4 0) byte)))
		  (setf (aref buf (1+ j))
			(aref bigend-digit-char (ldb (byte 4 4) byte)))
		  (incf i)
		  (incf j 2)
		  (when (> j 80) (force-buf)))))
	    (when (> j 0) (force-buf))))))))

(defun send-raster (printer-stream raster left top right bottom &optional (terpri t))
  terpri
  (assert (= bottom (1+ top)))
  (let ((bits-per-cell 1))
    (assert (<= bits-per-cell 8))
    ;;-- really left-right * bits-per-cell has to be mod 8
    (unless (zerop (rem (- right left) 8))
      (error "Sorry, can't hack right - left /= 0 (mod 8); you have ; ~D,~D" right left))
    (let ((i left)
	  (n-cells (/ 8 bits-per-cell)))
      (loop 
	(when (= i right) (return))
	(let ((value 0))
	  (dotimes (j n-cells)
	    (setq value (+ (ash value bits-per-cell) (aref raster top j))))
	  (incf i n-cells)
	  (write (truncate (ash value -4)) :stream printer-stream :base 16)
	  (write (logand value 15) :stream printer-stream :base 16))))))

(defmethod ps-fill (medium printer-stream (ink rectangular-tile))
  ;;--- Kludgy way to determine whether to use patterned drawing
  (multiple-value-bind (array width height)
      (decode-tile-as-stipple ink)
    (declare (ignore width height))
      (unless array
      (error "Rectangular tiles other than stipples are not supported yet."))
    (send-pattern medium printer-stream array)
    (format printer-stream " patfill~%")))


(defmethod ps-fill (medium printer-stream (ink pattern))
  (multiple-value-bind (array designs)
      (decode-pattern ink)
    (destructuring-bind (height width) (array-dimensions array)
      (maybe-send-feature medium 'pattern-program *pattern-code*)
      (let ((depth 8))
	(flet ((index-fn (index)
		 (round (* 255 (multiple-value-call #'color-luminosity
				 (color-rgb
				 (let ((ink (elt designs index)))
				   (cond ((eq ink +foreground-ink+)
					  (medium-foreground medium))
					 ((eq ink +background-ink+)
					  (medium-background medium))
					 (t ink)))))))))
	  (format printer-stream " ~D ~D ~D cerfill~%" width height depth)
	  (send-hexstring-array printer-stream array width height
				:function #'index-fn
				:bits-per-cell depth))))))

(defun send-hexstring-array (stream array width height &key
						       (bits-per-cell 1)
						       (function #'identity))
  (let* ((n-cells (/ 8 bits-per-cell))
	 (buffer (make-string 80))
	 (pointer 0)
	 (chars "0123456789abcdef"))
    (declare (optimize (speed 3)
		       (safety 0))
	     (type simple-string chars buffer)
	     (fixnum pointer n-cells bits-per-cell))
    (assert (<= bits-per-cell 8))
    (dotimes (row height)
      (let ((column 0))
	(loop
	  (let ((value 0))
	    (unless (< column width) (return))
	    (dotimes (i n-cells)
	      (if (< column width)
		  (progn
		    (setq value (+ (ash value bits-per-cell) (funcall function (aref array row column))))
		    (incf column))
		(setq value (ash value bits-per-cell))))
	    (setf (aref buffer pointer) (aref chars (truncate (ash value -4))))
	    (incf pointer)
	    (setf (aref buffer pointer) (aref chars (logand value 15)))
	    (incf pointer)
	    (when (= pointer 80)
	      (write-line buffer stream)
	      (setf pointer 0))))))
    (write-line (subseq buffer 0 pointer) stream)))

(defmethod ps-fill (medium printer-stream ink)
  (declare (ignore medium ink))
  (format printer-stream " fill~%"))

(defun ps-stroke (medium printer-stream ink)
  (cond ((typep ink 'rectangular-tile)
	 ;;--- Kludgy way to determine whether to use patterned drawing
	 (multiple-value-bind (array width height)
	     (decode-tile-as-stipple ink)
	   (declare (ignore width height))
	   (unless array
	     (error "Rectangular tiles other than stipples are not supported yet."))
	   (send-pattern medium printer-stream array)
	   (format printer-stream " patstroke~%")))
	(t
	 (format printer-stream " stroke~%"))))


(defclass postscript-port (basic-port)
    ;; 72 points per inch on PostScript devices
    ((font-map :initform (make-array 30 :initial-element nil))
     (device-units-per-inch :initform 72)))

(defmethod port-type ((port postscript-port))
  ':postscript)

(defmethod make-medium ((port postscript-port) sheet)
  (make-instance 'postscript-medium
    :port port
    :sheet sheet))

(defmethod restart-port ((port postscript-port))
  ;; We don't need no stinking events...
  )

;;--- Eventually do better than this
(defclass postscript-palette (basic-palette) ())

(defmethod make-palette ((port postscript-port) &key)
  (make-instance 'postscript-palette
    :port port 
    :color-p t
    :dynamic-p nil))

(defmethod initialize-instance :after ((port postscript-port) &key)
  (with-slots (silica::default-palette) port
    (setf silica::default-palette (make-instance 'postscript-palette))))

(defmethod realize-graft ((port postscript-port) (graft standard-graft))
  (with-slots (page-width page-height) port
    (with-slots (silica::mm-height silica::mm-width
				   silica::pixel-height silica::pixel-width
				   silica::pixels-per-point) 
	graft
      (setf silica::pixel-width    (* page-width 72)
	    silica::pixel-height   (* page-height 72)
	    silica::mm-width	   (* page-width 25.4)
	    silica::mm-height      (* page-height 25.4)
	    silica::pixels-per-point 1)
      
      (setf (sheet-region graft)
	(ecase (graft-units graft)
	  (:device (make-rectangle* 0 0 silica::pixel-width silica::pixel-height))
	  (:mm    (make-rectangle* 0 0 silica::mm-width silica::mm-height))
	  (:homogenous (make-rectangle* 0.0 0.0 1.0 1.0))))

      (setf (sheet-native-transformation graft) +identity-transformation+)
      ;;  (setf (sheet-mirror graft) (realize-mirror port graft)) ;;Is the mirror already realized?
      ;;(update-native-transformation port graft)
      )))


(defmethod standardize-text-style ((port postscript-port) style &optional character-set)
  (declare (ignore character-set))
  (let ((size (text-style-size style)))
    (if (numberp size)
	style
      (let* ((family (text-style-family style))
	     (sizes (third (assoc family *postscript-font-translate-data*)))
	     (position (position size *psftd-keywords*))
	     (new-size (and position (nth position sizes))))
	(cond (new-size
	       (make-text-style family (text-style-face style) new-size))
	      ((eql family :stand-in-for-undefined-style)
	       (make-text-style family (text-style-face style) 10))
	      (t
	       (cerror "Use the undefined text style stand-in instead"
		       "This display device does not know how to map the logical text ~
                        style size ~S in ~S"
		       size style)
	       *undefined-text-style*))))))

;; Some people need to be able to specialize this
(defmethod postscript-device-prologue ((port postscript-port) printer-stream)
  (declare (ignore printer-stream)))

;; Some people need to be able to specialize this, too
(defmethod postscript-device-epilogue ((port postscript-port) printer-stream)
  (declare (ignore printer-stream)))


(defclass apple-laser-writer (postscript-port)
    ((x-resolution :initform 300)	;pixels per inch
     (y-resolution :initform 300)
     (page-indent :initform  0.5)	;in inches
     (page-width  :initform  7.5)
     (page-height :initform 10.5)))

(defmethod normal-line-thickness ((port postscript-port) thickness)
  thickness)

(defmethod normal-line-thickness ((port apple-laser-writer) thickness)
  (if (= thickness 1)
      0
      (* 0.5f0 thickness (/ (slot-value port 'device-units-per-inch)
			    (slot-value port 'x-resolution)))))

(defmethod initialize-instance :after ((port apple-laser-writer) &key server-path)
  (declare (ignore server-path))
  ;;--- Set the "undefined" text style mapping to "Courier"
  )


(defclass postscript-stream
	  (sheet-permanently-enabled-mixin
	   permanent-medium-sheet-output-mixin
	   sheet-transformation-mixin
	   clim-internals::graphics-output-recording
	   clim-internals::output-recording-mixin
	   clim-internals::output-protocol-mixin
	   basic-sheet)
	  ((multi-page :initform nil :initarg :multi-page)
	   ;; Need this for hacking "scrolling" of multi-page output
	   (device-transformation :accessor sheet-device-transformation
				  :initform +identity-transformation+)
	   (generating-postscript :initform t :accessor stream-generating-postscript))
  (:default-initargs :default-text-margin 1000))

(defmethod close ((stream postscript-stream) &key abort)
  (unless abort
    (postscript-epilogue (sheet-medium stream))))

(defmethod postscript-bounding-box-edges ((stream postscript-stream))
  (with-bounding-rectangle* (left top right bottom)
      (stream-output-history stream)
    (pixels-to-points left top right bottom)
    (values (floor left) (floor top)
	    (ceiling right) (ceiling bottom))))

(defmethod pane-viewport-region ((stream postscript-stream))
  +everywhere+)

(defmethod window-inside-width ((stream postscript-stream))
  (let ((port (port stream)))
    (float (/ (* (slot-value port 'page-width)
		 (slot-value port 'device-units-per-inch))
	      *1-pixel=points*) 0f0)))

(defmethod window-inside-height ((stream postscript-stream))
  (let ((port (port stream)))
    (float (/ (* (slot-value port 'page-height)
		 (slot-value port 'device-units-per-inch))
	      *1-pixel=points*) 0f0)))

(defmethod stream-ensure-cursor-visible ((stream postscript-stream)
					 &optional cx cy)
  (declare (ignore cx cy))
  nil)

(defmethod stream-move-for-line-height-change ((stream postscript-stream)
					       movement old-height cursor-x cursor-y)
  (declare (ignore movement old-height cursor-x cursor-y)))

;; Replay some PostScript output, breaking it into multiple pages

(defmethod stream-replay ((stream postscript-stream) &optional region)
  (let* ((port (port stream))
	 (medium (sheet-medium stream))
	 (printer-stream (slot-value medium 'printer-stream))
	 (multi-page (slot-value stream 'multi-page))
	 (output-record (stream-output-history stream))
	 (viewport-x 0)
	 (viewport-y 0))
    (when (stream-drawing-p stream)
      (when output-record
	(letf-globally (((stream-recording-p stream) nil))
	  (if (or (and region
		       (not (eq region +everywhere+)))
		  (not multi-page))
	      (replay output-record stream (or region +everywhere+))
	      (with-bounding-rectangle* (left top right bottom) output-record
		(let* ((page-width
			 (floor (* (slot-value port 'page-width)
				   (slot-value port 'device-units-per-inch))
				*1-pixel=points*))
		       (page-height
			 (floor (* (slot-value port 'page-height)
				   (slot-value port 'device-units-per-inch))
				*1-pixel=points*))
		       (first-page t))
		  (setq viewport-x 0 viewport-y 0)
		  ;; Draw each chunk of output on its own page
		  (unwind-protect
		      (do ((y top (+ y page-height)))
			  ((> y bottom))
			(do ((x left (+ x page-width)))
			    ((> x right))
			  (if first-page
			      (setq first-page nil)
			      (format printer-stream "gsave new-page grestore~%"))
			  (let ((region (make-bounding-rectangle
					  x y (+ x page-width) (+ y page-height))))
			    (setf (sheet-device-transformation stream)
				  (make-translation-transformation
				    (- viewport-x) (- viewport-y)))
			    (replay output-record stream region))
			  (incf viewport-x page-width))
			(setf viewport-x 0)
			(incf viewport-y page-height))
		    (setq viewport-x 0 viewport-y 0))))))))))

(defmethod invoke-with-output-recording-options :before 
	   ((stream postscript-stream) continuation record draw)
  (declare (ignore record continuation))
  ;; See [clim2bug459]
  (when (and draw 
	     (not (stream-drawing-p stream))
	     (not (stream-generating-postscript stream)))
    (error "Cannot turning drawing on for postscript stream ~S" stream)))


(defmacro with-output-to-postscript-stream ((stream-var file-stream &rest args) &body body)
  (declare (arglist (stream-var file-stream
		     &key (device-type 'apple-laser-writer)
			  multi-page scale-to-fit
			  header-comments (orientation :portrait))
		    &body body))
  `(flet ((postscript-output-body (,stream-var) ,@body))
     (declare (dynamic-extent #'postscript-output-body))
     (invoke-with-output-to-postscript-stream ,file-stream #'postscript-output-body ,@args)))

;; This could really be WITH-OPEN-STREAM, but that isn't going to call CLIM:CLOSE.
;; Fixed in the CLOS stream system.
(defun invoke-with-output-to-postscript-stream (file-stream continuation
						&key (device-type 'apple-laser-writer)
						     multi-page scale-to-fit
						     header-comments
						     (orientation :portrait))
  (assert (not (and multi-page scale-to-fit)) (multi-page scale-to-fit)
    "You may not use both ~S and ~S" ':multi-page ':scale-to-fit)
  (let* ((port (make-instance device-type))
	 (stream (make-instance 'postscript-stream
				:multi-page multi-page))
	 (abort-p t))
    (setf (port stream :graft (find-graft :port port)) port)
    (let ((medium (sheet-medium stream)))
      (setf (slot-value medium 'printer-stream) file-stream)
      (unwind-protect
	  (multiple-value-prog1
	      (with-output-recording-options (stream :record t :draw nil)
		(letf-globally (((stream-generating-postscript stream) nil))
		  (funcall continuation stream)))
	    (force-output stream)
	    (multiple-value-bind (width height) 
		(bounding-rectangle-size (stream-output-history stream))
	      (let* ((page-width
		      (floor (* (slot-value port 'page-width)
				(slot-value port 'device-units-per-inch))
			     *1-pixel=points*))
		     (page-height
		      (floor (* (slot-value port 'page-height)
				(slot-value port 'device-units-per-inch))
			     *1-pixel=points*))
		     (scale-factor
		      (progn
			(when (eq orientation :landscape)
			  (rotatef page-width page-height))
			(if (or (not scale-to-fit)
				(and (< width page-width)
				     (< height page-height)))
			    1
			  (min (/ page-width width)
			       (/ page-height height))))))
		;; Now do the output to the printer, breaking up the output into
		;; multiple pages if that was requested
		(let ((string
		       (with-output-to-string (string-stream)
			 (letf-globally (((slot-value medium 'printer-stream) string-stream))
			   (with-output-recording-options (stream :record nil :draw t)
			     (stream-replay stream nil))))))
		  (postscript-prologue medium
				       :scale-factor scale-factor
				       :orientation orientation
				       :header-comments header-comments)
		  (write-string string (slot-value medium 'printer-stream)))))
	    (setq abort-p nil))
	(close stream :abort abort-p)
	(destroy-port port)))))
