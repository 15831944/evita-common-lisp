;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - arch - x86 - runtime - 25 Environment
;;; platform/win/lisp/runtime/pl-win-r25-command-line.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1992-2002 by Project Vogue. All rights reserved.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/platform/win/lisp/runtime/pl-win-r00-ffi.lisp#1 $
;;;
;;; Description:
;;;  This file contains variable declarations used in runtime.
;;;
;
(in-package :si)

#|
(x86:defasm win32-get-command-line ()
   (call ((:dll "kernel32.dll" "GetCommandLineW")))
   (jmp #'.box-int) )

(x86:defasm win32-get-environment-strings ()
   (call ((:dll "kernel32.dll" "GetEnvironmentStringsW")))
   (jmp #'.box-int) )

(x86:defasm win32-free-environment-strings (eax)
   (call #'.unbox-int)
   (push eax)
   (jmp ((:dll "kernel32.dll" "FreeEnvironmentStringsW"))) )

(x64:defasm win32-get-command-line ()
   (sub $rsp 40)
   (call ((:dll "kernel32.dll" "GetCommandLineW")))
   (add $rsp 40)
   (jmp #'.box-int) )

(x86x64:defasm win32-free-environment-strings (rax)
   (sub $rsp 40)
   (call #'.unbox-int)
   (mov rcx rax)
   (call ((:dll "kernel32.dll" "FreeEnvironmentStringsW")))
   (add $rsp 40)
   (mov $r0 $rnil)
   (ret) )

|#

(labels (
  (install (fname initargs)
    (let ((fn (apply #'allocate-funobj
                     (class-description 'native-code-function)
                     initargs ) ))
      (apply #'initialize-funobj fn
        :name fname
        :frame #+x86 #x40 #+x64 #x80
        initargs )
      (setf (fdefinition fname) fn) ) )

  #+x86
  (install-0-1 (name lib proc)
    (let ((initargs
      `(:codevec ,(make-array 11
                      :element-type '(unsigned-byte 8)
                      :initial-contents '(255 21 0 0 0 0 233 0 0 0 0) )
        :annotations
          ((:dllproc 2 (,proc . ,lib)) (:ncallee 7 .box-int)) ) ))
      (install name initargs) ) )

  #+x64
  (install-0-1 (name lib proc)
    (let ((initargs
      `(:codevec ,(make-array 19
                      :element-type '(unsigned-byte 8)
                      :initial-contents '(
                           #x48 #x83 #xEC #x28  ; 0000 SUB RSP, 40
                           #xFF #x15 0 0 0 0    ; 0004 CALL dll
                           #x48 #x83 #xC4 #x28  ; 000A ADD RSP, 40
                           #xE9 0 0 0 0 ))      ; 000E JMP .box-int
        :annotations
          ((:dllproc 6 (,proc . ,lib)) (:ncallee 15 .box-int)) ) ))
      (install name initargs) ) )

  #+x86
  (install-1-0 (name lib proc)
    (let ((initargs
      `(:codevec ,(make-array 13
                      :element-type '(unsigned-byte 8)
                      :initial-contents '(232 0 0 0 0 255 240 255 37 0 0 0 0) )
        :annotations
          ( (:ncallee 1 .box-int) (:dllproc 9 (,proc . ,lib))) ) ))
      (install name initargs) ) )

  #+x64
  (install-1-0 (name lib proc)
    (let ((initargs
      `(:codevec ,(make-array 26
                      :element-type '(unsigned-byte 8)
                      :initial-contents '(
                           #x48 #x83 #xEC #x28  ; 0000 SUB RSP, 40
                           #xE8 0 0 0 0         ; 0004 CALL .unbox-int
                           #x48 #x8B #xC8       ; 0009 MOV RCX, RAX
                           #xFF #x15 0 0 0 0    ; 000C CALL dll
                           #x48 #x83 #xC4 #x28  ; 0012 ADD RSP, 40
                           #x49 #x8B #xC5       ; 0016 MOV RAX, R13
                           #xC3 ))              ; 0019 RET
        :annotations
          ( (:ncallee 5 .box-int) (:dllproc 14 (,proc . ,lib))) ) ))
      (install name initargs) ) )
    )
    ;;
    (install-0-1 'win32-get-command-line
        "kernel32.dll" "GetCommandLineW" )

    (install-0-1 'win32-get-environment-strings
        "kernel32.dll" "GetEnvironmentStringsW" )

    (install-1-0 'win32-free-environment-strings
        "kernel32.dll" "FreeEnvironmentStringsW" )
 ) ; labels


(declaim (ftype (function () (unsigned-byte #+x86 32 #+x64 48))
  win32-get-command-line
  win32-get-environment-strings ) )

(declaim (ftype (function ((unsigned-byte #+x86 32 #+x64 48)) t)
  win32-free-environment-strings ) )
