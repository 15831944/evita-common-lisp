;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - runtime - ffi - definitions
;;; /lisp/ffi/ffi-defs2.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1992-2002 by Project Vogue. All rights reserved.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/ffi/ffi-defs2.lisp#3 $
;;;
;;; Description:
;;;  This file contains variable declarations used in runtime.
;;;
;
(in-package :si)

;;;; dll-entry
(defclass dll-entry (foreign-object)
  ((function  :type (ptr void))
   (proc-info :type dll-proc-info) )
  (:metaclass foreign-class) )

(macrolet (
  (define (bits)
    (let* ((int   (intern (format nil "INT~D"  bits)))
           (int*  (intern (format nil "~A*" int)))
           (uint  (intern (format nil "UINT~D" bits)))
           (uint* (intern (format nil "~A*" uint))) )
     `(progn
        (defclass ,int  (foreign-object) () (:metaclass foreign-class))

        (setf (getf (slot-value (find-class ',int) 'plist) 'type)
              '(signed-byte ,bits) )

        (defclass ,uint (foreign-object) () (:metaclass foreign-class))

        (setf (getf (slot-value (find-class ',int) 'plist) 'type)
              '(unsigned-byte ,bits) )

        (defclass ,int* (foreign-object)
          ((value :type ,int))
          (:metaclass foreign-class) )

        (defclass ,uint* (foreign-object)
          ((value :type ,uint))
          (:metaclass foreign-class) )) ) )
  )
  (define 8)
  (define 16)
  (define 32)
  #+64bit (define 64) )

(defclass int (foreign-object) () (:metaclass foreign-class))

(defclass int*  (foreign-object)
  ((value :type int))
  (:metaclass foreign-class))

(defclass uint  (foreign-object) () (:metaclass foreign-class))

(defclass uint*  (foreign-object)
  ((value :type uint))
  (:metaclass foreign-class))


(defclass void (foreign-object)
  ()
  (:metaclass foreign-class) )

(defclass void* (foreign-object)
  ()
  (:metaclass foreign-class) )

(setf (getf (slot-value (find-class 'int) 'plist) 'type)
      '(signed-byte #+32bit 32 #+64bit 64) )

(setf (getf (slot-value (find-class 'uint) 'plist) 'type)
      '(unsigned-byte #+32bit 32 #+64bit 64) )

(dolist (ctype '(int*  int16*  int32*  int8*
                 uint* uint16* uint32* uint8*
                 void* ))
  (setf (getf (slot-value (find-class ctype) 'plist) 'type)
        '(signed-byte #+32bit 32 #+64bit 64) ) )
