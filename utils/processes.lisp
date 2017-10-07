;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-UTILS; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :clim-utils)

;;;"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
;;; Portions copyright (c) 1988, 1989, 1990 International Lisp Associates.
;;; Portions copyright (c) 1992, 1993 Franz, Inc."
;;; Copyright (c) 2017 Daniel Kochmański

(defvar *multiprocessing-p* bt:*supports-threads-p*)

;;; Locks

(defmacro with-lock-held ((place &optional state) &body body)
  (declare (ignore state))
  `(bt:with-lock-held (,place) ,@body))

(defun make-lock (&optional (lock-name "a CLIM lock"))
  (bt:make-lock lock-name))

(defmacro with-recursive-lock-held ((place &optional state) &body body)
  (declare (ignore state))
  `(bt:with-recursive-lock-held (,place) ,@body))

(defun make-recursive-lock (&optional (lock-name "a recursive CLIM lock"))
  (bt:make-recursive-lock lock-name))


;;; Atomic operations

(defmacro without-scheduling (&body forms)
  "Evaluate the forms w/o letting any other process run."
  #+Allegro    `(excl:without-interrupts ,@forms) 
  #+LispWorks  `(sys::without-scheduling ,@forms)
  #+Lucid      `(lcl:with-scheduling-inhibited ,@forms)
  #+Xerox      `(progn ,@forms)
  #+Cloe-Runtime `(progn ,@forms)
  #+aclpc      `(progn ,@forms)
  ;; should be process:with-no-other-processes if this is used as
  ;; a global locking mechanism
  #+Genera     `(scl:without-interrupts ,@forms)
  #+Minima     `(minima:with-no-other-processes ,@forms)
  #+(and MCL CCL-2)  `(ccl:without-interrupts ,@forms)
  #+Clozure    `(ccl:without-interrupts ,@forms)
  #+SBCL       `(sb-sys:without-interrupts ,@forms))

;; Atomically increments a fixnum value
#+Genera
(defmacro atomic-incf (reference &optional (delta 1))
  (let ((location '#:location)
        (old-value '#:old)
        (new-value '#:new))
    `(loop with ,location = (scl:locf ,reference)
           for ,old-value = (scl:location-contents ,location)
           for ,new-value = (sys:%32-bit-plus ,old-value ,delta)
           do (when (scl:store-conditional ,location ,old-value ,new-value)
                (return ,new-value)))))

#+Clozure
(defmacro atomic-incf (reference &optional (delta 1))
  (declare (ignore delta))
  `(ccl::atomic-incf ,reference))

#+SBCL
(defmacro atomic-incf (reference &optional (delta 1))
  `(sb-ext:atomic-incf ,reference ,delta))

#-(or Genera Clozure SBCL)
(defmacro atomic-incf (reference &optional (delta 1))
  (let ((value '#:value))
    (if (= delta 1)
        `(without-scheduling 
           (let ((,value ,reference))
             (if (eq ,value most-positive-fixnum)
                 (setf ,reference most-negative-fixnum)
	       (setf ,reference (the fixnum (1+ (the fixnum ,value)))))))
      #+ignore (warn "Implement ~S for the case when delta is not 1" 'atomic-incf)
      #-ignore ;; maybe?
      (if (< delta 0)
	  `(without-scheduling
	     (let ((,value ,reference))
	       (if (< ,delta (- ,value most-negative-fixnum))
		   (setf ,reference most-positive-fixnum)
		 (setf ,reference (the fixnum (+ (the fixnum ,delta) (the fixnum ,value)))))))
	`(without-scheduling
	   (let ((,value ,reference))
	     (if (> ,delta (- most-positive-fixnum ,value))
		 (setf ,reference most-negative-fixnum)
	       (setf ,reference (the fixnum (+ (the fixnum ,delta) (the fixnum ,value))))))))
      )))

;; Atomically decrements a fixnum value
#+Genera
(defmacro atomic-decf (reference &optional (delta 1))
  (let ((location '#:location)
        (old-value '#:old)
        (new-value '#:new))
    `(loop with ,location = (scl:locf ,reference)
           for ,old-value = (scl:location-contents ,location)
           for ,new-value = (sys:%32-bit-difference ,old-value ,delta)
           do (when (scl:store-conditional ,location ,old-value ,new-value)
                (return ,new-value)))))

#+Clozure
(defmacro atomic-decf (reference &optional (delta 1))
  (declare (ignore delta))
  `(ccl::atomic-decf ,reference))

#+SBCL
(defmacro atomic-decf (reference &optional (delta 1))
  `(sb-ext:atomic-decf ,reference ,delta))

#-(or Genera Clozure SBCL)
(defmacro atomic-decf (reference &optional (delta 1))
  (let ((value '#:value))
    (if (= delta 1)
        `(without-scheduling 
           (let ((,value ,reference))
             (if (eq ,value most-negative-fixnum)
                 (setf ,reference most-positive-fixnum)
                 (setf ,reference (the fixnum (1- (the fixnum ,value)))))))
        (warn "Implement ~S for the case when delta is not 1" 'atomic-decf))))


;;; Processes

(defun make-process (function &key name)
  #+(or (and MCL CCL-2)) (declare (ignore function name))
  (when *multiprocessing-p*
    #+LispWorks  (mp:process-run-function name nil function)
    #+Lucid      (lcl:make-process :function function :name name)
    #+Allegro    (mp:process-run-function name function)
    #+Xerox      (il:add.process (funcall function) 'il:name name)
    #+Genera     (scl:process-run-function name function)
    #+Minima     (minima:make-process name :initial-function function)
    #+Clozure    (ccl:process-run-function name function)
    #+SBCL       (sb-thread:make-thread function :name name)
    #-(or LispWorks Lucid Allegro Xerox Genera Minima Clozure SBCL)
    (warn "No implementation of MAKE-PROCESS for this system.")
    ))

(eval-when (:compile-toplevel :load-toplevel :execute) (proclaim '(inline processp)))
(defun processp (object)
  #+(and MCL CCL-2)  (member object '(:user :event :interrupt))
  #+Lucid      (lcl:processp object)
  #+Allegro    (mp::process-p object)
  #+LispWorks  (mp::process-p object)
  ;; In 7.3 and after it is `(process:process-p ,object)
  #+Genera     (process:process-p object)
  #+Minima     (typep object 'minima-internals::basic-process)
  #+Clozure    (ccl::processp object)
  #+SBCL       (sb-thread::thread-p object)
  #-(or (and MCL CCL-2) Lucid Allegro LispWorks Genera Minima Clozure SBCL)
  (progn (warn "No implementation of PROCESSP for this system.")
         nil)
  )

(defun destroy-process (process)
  #+(or (and MCL CCL-2)) (declare (ignore process))
  #+Lucid      (lcl:kill-process process)
  #+Allegro    (mp:process-kill process)
  #+LispWorks  (mp:process-kill process)
  #+Xerox      (il:del.process process)
  #+Genera     (scl:process-kill process)
  #+Minima     (minima:process-kill process)
  #+(and MCL CCL-2)  nil
  #+Clozure    (ccl:process-kill process)
  #+SBCL       (sb-thread:terminate-thread process)
  #-(or Lucid Allegro LispWorks Xerox Genera Minima (and MCL CCL-2) Clozure SBCL)
  (warn "No implementation of DESTROY-PROCESS for this system.")
  )

#+(and MCL CCL-2)
(defvar *current-process* :user)

(eval-when (:compile-toplevel :load-toplevel :execute) (proclaim '(inline current-process)))
(defun current-process ()
  #+Lucid        lcl:*current-process*
  #+Allegro      mp:*current-process*
  #+LispWorks    mp:*current-process*
  #+Xerox        (il:this.process)
  #+Genera       scl:*current-process*
  #+Minima       (minima:current-process)
  #+(and MCL CCL-2)  *current-process*
  #+Cloe-Runtime nil
  #+aclpc        nil
  #+Clozure      ccl:*current-process*
  #+SBCL         sb-thread:*current-thread*
  )

(eval-when (:compile-toplevel :load-toplevel :execute) (proclaim '(inline all-processes)))
(defun all-processes ()
  #+Lucid        lcl:*all-processes*
  #+Allegro      mp:*all-processes*
  #+LispWorks    (mp::list-all-processes)
  #+Genera       sys:all-processes
  #+(and MCL CCL-2)  (adjoin *current-process* '(:user))
  #+Cloe-Runtime nil
  #+aclpc        nil
  #+Clozure      (ccl:all-processes)
  #+SBCL         sb-thread::*all-threads*
  )

(defun show-processes ()
  #+Lucid             (lcl::show-processes)
  #+Genera            (si:com-show-processes)
  #-(or Lucid Genera) (all-processes)
  )
  
(eval-when (:compile-toplevel :load-toplevel :execute) (proclaim '(inline process-yield)))
(defun process-yield ()
  (bt:thread-yield))

(defun process-wait (reason predicate)
  (declare (ignore reason))
  (loop (when (funcall predicate) (return))
     (process-yield)))

(defun process-wait-with-timeout (reason timeout predicate)
  (if (numberp timeout)
      (bt:with-timeout (timeout) (process-wait reason predicate))
      (process-wait reason predicate)))

(defun process-interrupt (process function)
  (declare #+CCL-2 (ignore process))
  #+Lucid     (lcl:interrupt-process process function)
  #+Allegro   (mp:process-interrupt process function)
  #+LispWorks (mp:process-interrupt process function)
  #+Genera    (scl:process-interrupt process function)
  #+(and MCL CCL-2) (let ((*current-process* :interrupt))
                      (funcall function))
  #+Minima    (minima:process-interrupt process function)
  #+Clozure   (ccl:process-interrupt process function)
  #+SBCL      (sb-thread:interrupt-thread process function)
  #-(or Lucid Allegro LispWorks Genera (and MCL CCL-2) Minima Clozure SBCL)
  (warn "No implementation of PROCESS-INTERRUPT for this system.")
  )

(defun restart-process (process)
  #+Lucid (lcl::restart-process process)
  #+Allegro (mp:process-reset process)
  #+LispWorks (mp:process-reset process)
  #+Genera (process:process-reset process)
  #+Minima (minima:process-reset process)
  #+Clozure (ccl:process-reset process)
  #+SBCL    (sb-thread::wake-thread process)
  #-(or Lucid Allegro LispWorks Genera Minima Clozure SBCL)
  (warn "No implementation of RESTART-PROCESS for this system.")
  )

(defun enable-process (process)
  #+Lucid (lcl::activate-process process)
  #+Allegro (mp:process-enable process)
  #+LispWorks (mp:process-enable process)
  #+Genera (process:process-enable process)
  #+Minima (minima:process-enable process)
  #+Clozure (ccl:process-enable process)
  #+SBCL (sb-thread::wake-thread process)
  #-(or Lucid Allegro LispWorks Genera Minima Clozure SBCL)
  (warn "No implementation of ENABLE-PROCESS for this system.")
  )

(defun disable-process (process)
  #+Lucid (lcl::deactivate-process process)
  #+Allegro (mp:process-disable process)
  #+LispWorks (mp:process-disable process)
  #+Genera (process:process-disable process)
  #+Minima (minima:process-disable process)
  #+Clozure (ccl:process-reset process)
  #-(or Lucid Allegro LispWorks Genera Minima Clozure)
  (warn "No implementation of DISABLE-PROCESS for this system.")
  )

(defun process-name (process)
  #+Lucid (lcl::process-name process)
  #+Allegro (mp:process-name process)
  #+LispWorks (mp:process-name process)
  #+Genera (process:process-name process)
  #+Minima (minima:process-name process)
  #+Clozure (ccl:process-name process)
  #+SBCL    (sb-thread:thread-name process)
  #-(or Lucid Allegro LispWorks Genera Minima Clozure SBCL)
  (warn "No implementation of PROCESS-NAME for this system.")
  )

(defun process-state (process)
  #+Lucid (lcl::process-state process)
  #+Allegro (cond ((mp:process-active-p process) "active")
                  ((mp:process-runnable-p process) "runnable")
                  (t "deactivated"))
  #+LispWorks (cond ((mp:process-active-p process) "active")
                    ((mp:process-runnable-p process) "runnable")
                    (t "deactivated"))
  #+Genera (process:process-state process)
  #+Minima (minima:process-state process)
  #+Clozure (ccl:process-whostate process)
  #-(or Lucid Allegro LispWorks Genera Minima Clozure)
  (warn "No implementation of PROCESS-STATE for this system.")
  )

(defun process-whostate (process)
  #+Lucid (lcl::process-whostate process)
  #+Allegro (mp:process-whostate process)
  #+LispWorks (mp:process-whostate process)
  #+Genera (process:process-whostate process)
  #+Minima (minima:process-whostate process)
  #+Clozure (ccl:process-whostate process)
  #-(or Lucid Allegro LispWorks Genera Minima Clozure)
  (warn "No implementation of PROCESS-WHOSTATE for this system.")
  )
