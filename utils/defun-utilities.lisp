;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-UTILS; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :clim-utils)

;;;"Copyright (c) 1991 International Lisp Associates.  All rights reserved."

;;; Moved here from DEFUN.  DEFUN now only contains the portable implementation
;;; of the DYNAMIC-EXTENT declaration, and so is not loaded into Lisps which 
;;; implement that declaration.

;;; This file has to be loaded BEFORE DEFUN.

(defparameter *declarations-may-be-exposed-by-macro-expansion* nil)

(cl:defun extract-declarations (body &optional environment)
  (let ((declarations nil)
	(documentation nil))
    (block process-declarations
      (loop
	(when (null body) (return-from process-declarations))
	(let ((form (first body)))
	  (cond ((stringp form)
		 (setf documentation (or documentation form)
		       body (cdr body)))
		((atom form) (return-from process-declarations))
		;; X3J13 says this nonsense is not required any more:
		(*declarations-may-be-exposed-by-macro-expansion*
		 (block expand-macros
		   (loop
		     (when (eq (first form) 'declare)
		       (setf declarations (append declarations (cdr form))
			     body (cdr body))
		       (return-from expand-macros))
		     (multiple-value-bind (new-form macro-expanded-p)
			 (macroexpand-1 form environment)
		       (unless macro-expanded-p (return-from process-declarations))
		       (setf form new-form)))))
		(t (if (eq (first form) 'declare)
		       (setf declarations (append declarations (cdr form))
			     body (cdr body))
		     (return-from process-declarations)))))))
    (values documentation `((declare ,@declarations)) body)))


;;; DEFINE-GROUP: defines a "group" of definitions which are related
;;; somehow.  In Genera, this causes the function-parents to be set
;;; correctly, for example, and also if you attempt to abort out of the
;;; middle you get told that something might be left inconsistent.
#+Genera 
(defmacro define-group (name type &body body)
  `(sys:multiple-definition ,name ,type ,@body))

#+(and allegro (not acl86win32) (version>= 4 1))
(defmacro define-group (name type &body body)
  `(progn
     (excl::record-source-file ',name :type ',type)
     ,@body))

#-(or Genera (and (not acl86win32) (and allegro (version>= 4 1))))
(defmacro define-group (name type &body body)
  (declare (ignore name type))
  `(progn ,@body))

(defmacro with-warnings-for-definition (name type &body body)
  #-Genera (declare (ignore name type)) ;-- Why?
  #+Genera `(let ((compiler:default-warning-function ,name)
		  (compiler:default-warning-definition-type ',type))
	      ,@body)
  #-Genera `(let () ,@body))

(defmacro defun-inline (name lambda-list &body body)
  `(progn ;; define-group ,name defun-inline
;;;; don't use define-group, because it does a excl::record-source-file,
;;;; which will be also done by defun!  This causes duplicate definition in
;;;; file warnings.
     (eval-when (:compile-toplevel :load-toplevel :execute) (proclaim '(inline ,name)))
     (defun ,name ,lambda-list
       ,@body)))

#+Genera
(progn
  (setf (get 'defun-inline 'zwei:definition-function-spec-parser)
	(zl:::scl:function (:property zl:::scl:defun zwei:definition-function-spec-parser)))
  (setf (get 'defun-inline 'zwei:definition-function-spec-type) 'zl:::scl:defun)
  (setf (get 'defun-inline 'gprint::formatter) 
	(zl:::scl:function (:property zl:::scl:defun gprint::formatter)))
  (pushnew 'defun-inline zwei:*irrelevant-functions*)
  (pushnew 'defun-inline zwei:*irrelevant-defining-forms*))

;;
;; Backwards compatibility for new ics functions during beta2 development
;;
#+allegro
(in-package :excl)
#+(and allegro (not (version>= 5 (0 1) :pre-beta2 7)))
(progn
  (defmacro with-native-string ((native-string-var string-exp)
				&body body)
    `(let ((,native-string-var ,string-exp))
       ,@body))

  (eval-when (:compile-toplevel :load-toplevel :execute) (export 'with-native-string))

  (defun mb-to-string (mb-vector)
    (let* ((lgth (length mb-vector))
	   (string (make-string lgth)))
      (dotimes (i lgth string)
	(setf (schar string i) (code-char (aref mb-vector i))))))

  (eval-when (:compile-toplevel :load-toplevel :execute) (export 'mb-to-string)))
