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
;; $Id: xm-funs.lisp,v 1.22.34.4 2002/02/08 19:11:25 layer Exp $

;;
;; This file contains compile time only code -- put in clim-debug.fasl.
;;

(in-package :tk)

(def-foreign-call (xm_string_create_l_to_r "XmStringCreateLtoR")
    ((x :foreign-address) (y :foreign-address))
  :returning :foreign-address
  :call-direct t)

(def-foreign-call (xm_string_concat "XmStringConcat")
    ((x :foreign-address) (y :foreign-address))
  :returning :foreign-address
  :call-direct t)

(def-foreign-call (xm_string_copy "XmStringCopy")
    ((x :foreign-address))
  :returning :foreign-address
  :call-direct t)

(def-foreign-call (xm_string_get_l_to_r "XmStringGetLtoR")
    ((x :foreign-address) (y :foreign-address) (z :foreign-address))
  :returning (:int fixnum)
  :call-direct t
  :arg-checking nil)

;;; New method to support Motif2.1
(def-foreign-call (xm_string_free "XmStringFree")
    ((x :foreign-address))
  :returning :void
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xm_get_pixmap "XmGetPixmap")
    ((w :foreign-address) (x :foreign-address) y z)
  :returning :foreign-address
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xm_font_list_init_font_context "XmFontListInitFontContext")
    ((x :foreign-address) (y :foreign-address))
  :returning (:int fixnum)
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xm_font_list_free_font_context "XmFontListFreeFontContext")
    ((x :foreign-address))
  :returning :void
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xm_font_list_get_next_font "XmFontListGetNextFont")
    ((x :foreign-address) (y :foreign-address) (z :foreign-address))
  :returning (:int fixnum)
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xm_font_list_create "XmFontListCreate")
    ((x :foreign-address) (y :foreign-address))
  :returning :foreign-address
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xm_font_list_free "XmFontListFree")
    ((x :foreign-address))
  :returning :void
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xm_font_list_entry_free "XmFontListEntryFree")
    ((x :foreign-address))
  :returning :void
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xm_add_protocol_callback "XmAddProtocolCallback")
    ((v :foreign-address) w x (y :foreign-address) (z :foreign-address))
  :returning :void
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xm_intern_atom "XmInternAtom")
    ((x :foreign-address) (y :foreign-address) (z :int fixnum))
  :returning :foreign-address
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xm_main_window_set_area "XmMainWindowSetAreas")
    ((a :foreign-address) (b :foreign-address) (c :foreign-address)
     (d :foreign-address) (e :foreign-address) (f :foreign-address))
 :returning :void
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xm_process_traversal "XmProcessTraversal")
    ((x :foreign-address) y)
  :returning (:int fixnum)
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xm-message-box-get-child "XmMessageBoxGetChild")
    ((x :foreign-address) (y :int fixnum))
  :returning :foreign-address
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xm-selection-box-get-child "XmSelectionBoxGetChild")
    ((x :foreign-address) (y :int fixnum))
  :returning :foreign-address
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xm_file_selection_do_search "XmFileSelectionDoSearch")
    ((x :foreign-address) (y :foreign-address))
  :returning :void
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xm_option_label_gadget "XmOptionLabelGadget")
    ((x :foreign-address))
  :returning :foreign-address
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xm_option_button_gadget "XmOptionButtonGadget")
    ((x :foreign-address))
  :returning :foreign-address
  :call-direct t
  :arg-checking nil)

(def-foreign-call (initializemydrawingareaquerygeometry "InitializeMyDrawingAreaQueryGeometry")
    (x)
  :returning :void
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xm_get_focus_widget "XmGetFocusWidget")
    ((x :foreign-address))
  :returning :foreign-address
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xm_is_traversable "XmIsTraversable")
    ((x :foreign-address))
  :returning :foreign-address
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xm_font_list_append_entry "XmFontListAppendEntry")
    ((x :foreign-address) (y :foreign-address))
  :returning :foreign-address
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xm_font_list_entry_create "XmFontListEntryCreate")
    ((x :foreign-address) (y :foreign-address) (z :foreign-address))
  :returning :foreign-address
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xm_font_list_entry_get_font "XmFontListEntryGetFont")
    ((x :foreign-address) (y :foreign-address))
  :returning :foreign-address
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xm_font_list_next_entry "XmFontListNextEntry")
    ((x :foreign-address))
  :returning :foreign-address
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xm_toggle_button_set_state "XmToggleButtonSetState")
    ((x :foreign-address) (y :int fixnum) (z :int fixnum))
  :returning :void
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xm_toggle_button_get_state "XmToggleButtonGetState")
    ((x :foreign-address))
  :returning (:int fixnum)
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xm_text_field_get_selection "XmTextFieldGetSelection")
    ((x :foreign-address))
  :returning :int
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xm_text_get_selection "XmTextGetSelection")
    ((x :foreign-address))
  :returning :int
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xm_scale_set_value "XmScaleSetValue")
    ((x :foreign-address) (y :int fixnum))
  :returning :void
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xm_get_display "XmGetXmDisplay")
    ((x :foreign-address))
  :returning :int
  :call-direct t
  :arg-checking nil)

(def-foreign-call (xm_change_color "XmChangeColor")
    ((x :foreign-address) (y :int fixnum))
  :returning :void
  :call-direct t
  :arg-checking nil)

(provide :clim-debugxm)
