;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-UTILS; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :clim-utils)

;;;"Copyright (c) 1990, 1991, 1992 Symbolics, Inc.  All rights reserved.
;;; Portions copyright (c) 1988, 1989, 1990 International Lisp Associates.
;;; Portions copyright (c) 1992, 1993 Franz, Inc."


;;; Locks 
#+Allegro
(eval-when (compile load eval)
;;;  (require :mdproc)
  (require :process))

(defvar *multiprocessing-p* 
  #+(or Allegro Genera Lucid Lispworks Minima Clozure SBCL) t
  #-(or Allegro Genera Lucid LispWorks Minima Clozure SBCL) nil
    )

;;; This is to keep it quiet: On ACL it's safe to declare the
;;; predicate & args dynamic-extent on platforms with native threads
;;; *only*, which at present (6.0beta) is Windows platforms.
;;;
;;; the real definition of process-wait is in
;;; clim2:;aclpc;acl-clim.lisp.  That definition is almost certainly
;;; bogus because it misunderstands the whole way multithreading
;;; works: the definition above should be used instead.  But the
;;; Windows event-loop depends on this misunderstanding, and I don't
;;; want to change that.
;;;
#+(and allegro mswindows)
(excl:defun-proto process-wait (wait-reason predicate &rest args)
  (declare (dynamic-extent predicate args)))
  
;;-- I dont think we need this
;#+Allegro
;(unless (excl::scheduler-running-p)
;  (mp:start-scheduler))

(defmacro with-lock-held ((place &optional state) &body forms)
  #+(or allegro Xerox Genera (and MCL CCL-2) Minima Clozure SBCL)
  (declare (ignore state #+(and MCL CCL-2) place))
  #+Allegro      `(mp:with-process-lock (,place) ,@forms)
  #+Lucid        `(lcl:with-process-lock (,place ,@(if state (cons state nil)))
                   ,@forms)
  #+LispWorks    `(mp::with-lock (,place) ,@forms)
  #+Xerox        `(il:with.monitor ,place ,@forms)
  #+Cloe-Runtime `(progn ,@forms)
  #+aclpc        `(progn ,@forms)
  #+Genera       `(process:with-lock (,place) ,@forms)
  #+Minima       `(minima:with-lock (,place) ,@forms)
  #+(and MCL CCL-2) `(progn ,@forms)
  #+Clozure      `(ccl:with-lock-grabbed (,place) ,@forms)
  #+SBCL         `(sb-thread:with-mutex (,place :wait-p t) ,@forms)
  )

(defun make-lock (&optional (lock-name "a CLIM lock"))
  #-(or Allegro Genera Minima Clozure SBCL) (declare (ignore lock-name))
  #+Allegro          (mp::make-process-lock :name lock-name)
  #+LispWorks        (mp::make-lock)
  #+Lucid            nil
  #+(and MCL CCL-2)  nil
  #+Xerox          (il:create.monitorlock)
  #+Cloe-Runtime   nil
  #+aclpc          nil
  #+Genera         (process:make-lock lock-name)
  #+Minima         (minima:make-lock lock-name)
  #+Clozure        (ccl:make-lock lock-name)
  #+SBCL           (sb-thread:make-mutex :name lock-name)
  )

;;; A lock that CAN be relocked by the same process.
#-(or Genera Minima)
(defmacro with-simple-recursive-lock ((lock &optional (state "Unlock")) &body forms)
  `(flet ((foo () ,@forms))
     (declare (dynamic-extent #'foo))
     (invoke-with-simple-recursive-lock ,lock ,state #'foo)))

#-(or Genera Minima)
(defun invoke-with-simple-recursive-lock (place state continuation)
  (let ((store-value (current-process))
        (place-value (first place)))
    (if (and place-value (eql place-value store-value))
        (funcall continuation)
        (progn
          (unless (null place-value)
            (flet ((waiter ()
                     (null (first place))))
              #-allegro (declare (dynamic-extent #'waiter))
              (process-wait state #'waiter)))
          (unwind-protect
              (progn (rplaca place store-value)
                     (funcall continuation))
            (rplaca place nil))))))

(defmacro with-recursive-lock-held ((place &optional state) &body forms)
  #+(or Xerox Genera (and MCL CCL-2) Minima Clozure)
  (declare (ignore state #+(and MCL CCL-2) place))
  #+Genera  `(process:with-lock (,place) ,@forms)
  #+Minima  `(minima:with-lock (,place) ,@forms)
  #+(and MCL CCL-2)  `(progn ,@forms)
  #+Clozure `(ccl:with-lock-grabbed (,place) ,@forms)
  #-(or Genera Minima (and MCL CCL-2) Clozure)
  `(with-simple-recursive-lock (,place ,state) ,@forms)
  )

(defun make-recursive-lock (&optional (lock-name "a recursive CLIM lock"))
  #-(or Genera Minima) (declare (ignore lock-name))
  #+(and MCL CCL-2) nil
  #+Genera (process:make-lock lock-name :recursive T)
  #+Minima (minima:make-lock lock-name :recursive T)
  #+Clozure (ccl::make-recursive-lock)
  #-(or Genera Minima Clozure) (cons nil nil)
  )


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
  #+SBCL       `(sb-sys:without-interrupts ,@forms)
   )

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

(eval-when (compile load eval) (proclaim '(inline processp)))
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

(eval-when (compile load eval) (proclaim '(inline current-process)))
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

(eval-when (compile load eval) (proclaim '(inline all-processes)))
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
  
(eval-when (compile load eval) (proclaim '(inline process-yield)))
(defun process-yield ()
  #+Lucid        (lcl:process-allow-schedule)
  #+Allegro      (mp:process-allow-schedule)
  #+LispWorks    (mp::process-allow-scheduling)
  #+Xerox        (il:block)
  #+Genera       (scl:process-allow-schedule)
  #+Minima       (sleep 1/10)
  #+(and MCL CCL-2)  (ccl:event-dispatch)
  #+Cloe-Runtime nil
  #+aclpc        nil
  #+Clozure      (ccl::yield)
  #+SBCL         (sb-thread:thread-yield)
  )

#-mswindows
(defun process-wait (wait-reason predicate)
  (declare #+SBCL (ignore wait-reason)
           #+(or Genera Minima) (dynamic-extent predicate)
           )
  "Cause the current process to go to sleep until the predicate returns TRUE."
  #+Lucid      (lcl:process-wait wait-reason predicate)
  #+Allegro    (mp:process-wait wait-reason predicate)
  #+LispWorks  (mp:process-wait wait-reason predicate)
  #+Xerox             (let ((il:*who-line-state* wait-reason))
                        (loop
                          (il:block)
                          (when (and (funcall predicate))
                            (return))))
  #+(and MCL CCL-2)  (ccl::process-wait wait-reason predicate)
  #+Cloe-Runtime nil
  #+aclpc        nil
  #+Genera     (scl:process-wait wait-reason predicate)
  #+Minima     (minima:process-wait wait-reason predicate)
  #+Clozure    (ccl:process-wait wait-reason predicate)
  #+SBCL       (sb-ext:wait-for predicate)
  #-(or Lucid Allegro LispWorks Xerox (and MCL CCL-2) Cloe-Runtime aclpc Genera Minima Clozure SBCL)
  (warn "No implementation of PROCESS-WAIT for this system.")
  )

(defun process-wait-with-timeout (wait-reason timeout predicate)
  (declare #+SBCL (ignore wait-reason)
           #+(or Genera Minima) (dynamic-extent predicate)
           )
  "Cause the current process to go to sleep until the predicate returns TRUE or
   timeout seconds have gone by." 
  (when (null timeout)
    ;; ensure genera semantics, timeout = NIL means indefinite timeout
    (return-from process-wait-with-timeout
      (process-wait wait-reason predicate)))
  #+Allegro    (mp:process-wait-with-timeout wait-reason timeout predicate)
  #+LispWorks  (mp:process-wait-with-timeout wait-reason timeout predicate)
  #+Lucid             (lcl:process-wait-with-timeout wait-reason timeout predicate)
  #+Genera     (sys:process-wait-with-timeout wait-reason (* timeout 60.) predicate)
  #+(and MCL CCL-2)  (ccl::process-wait-with-timeout wait-reason timeout predicate)
  #+Clozure    (ccl:process-wait-with-timeout wait-reason timeout predicate)
  #+SBCL       (sb-ext:wait-for predicate :timeout timeout)
  #-(or Allegro LispWorks Lucid Genera (and MCL CCL-2) Clozure SBCL)
  (warn "No implementation of PROCESS-WAIT-WITH-TIMEOUT for this system.")
  )

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
