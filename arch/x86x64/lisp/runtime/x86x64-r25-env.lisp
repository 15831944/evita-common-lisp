;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Runtime - 25 Environment
;;; arch/x86/lisp/runtime/x86-r25-env.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/x86x64/lisp/runtime/x86x64-r25-env.lisp#2 $
;;;
;
(in-package :si)

;;;; Higest CPUID Source Operand for Intel 64 and IA-32 Processor
;;; From Intel 64 and IA-32 Architecutes Software Developer's Manual Vol 2a
;;;
;;;                     Basic       Extended
;;; Eariler Intel486    No CPUID    No CPUID
;;; Later Intel486      01          None
;;; Pentium Pro/ II     02          None
;;; Pentium III         03          None
;;; Pentium 4           02          8000 0004
;;; Intel Xeon          02          8000 0004
;;; Pentium M           02          8000 0004
;;; Pentium 4 w/ HT     05          8000 0008
;;; Pentium D (8xx)     05          8000 0008
;;; Pentium D (9xx)     06          8000 0008
;;; Intel Core Duo      0A          8000 0008
;;; Intel Xeon 5100     0A          8000 0008

;;;; machine-type
;;; Description:
;;;  Returns macine type information from CPUID/EAX=1's family, model and
;;;  stepping.
;;; Note:
;;;  We don't cache result string since saved image can be used in machines
;;;  other than machine used for saving image.
(defun machine-type ()
  (let ((eax (cpuid 1)))
    (format nil "i~D86.~D.~D"
        (ldb (byte 4 8) eax)
        (ldb (byte 4 4) eax)
        (ldb (byte 4 0) eax) ) ) )


;;;; machine-version
;;; Description:
;;;  Returns process brand string (CPUID/EAX=80000002...80000004).
;;; Note:
;;;  We don't cache result string since saved image can be used in machines
;;;  other than machine used for saving image.
(defun machine-version ()
  (let ((space-p t))
    (labels (
      ;; decode1
      (decode1 (s code)
        (cond
          ((zerop code))
          ((not (eql code #x20))
            (write-char (code-char code) s)
            (setq space-p nil) )
          ((not space-p)
            (write-char #\Space s)
            (setq space-p t) )) )

      ;; decode4
      (decode4 (s n)
        (decode1 s (ldb (byte 8  0) n))
        (decode1 s (ldb (byte 8  8) n))
        (decode1 s (ldb (byte 8 16) n))
        (decode1 s (ldb (byte 8 24) n)) )

      ;; get-brand-string
      (get-brand-string ()
        (with-output-to-string (s)
          (loop for eax from 2 to 4 do
            (multiple-value-bind (s0 s1 s2 s3) (cpuid (logior #x80000000 eax))
              (decode4 s s0)
              (decode4 s s1)
              (decode4 s s2)
              (decode4 s s3) )) ) )
    )
    ;;
    (if (>= (cpuid #x80000000) #x80000004)
        (get-brand-string)
      ;; We don't support old machine.
      "unknown" ) ) ) )
