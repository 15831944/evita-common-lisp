;;;;    TAKL benchmark test (3.4, p.105)
;;;    takl.lisp
;;;    Written by Yoshimi "VOGUE" FUNAI.
;;;    @(#)$Id: /proj/evcl/lisp/bench/takl.lisp 6 2005-12-18 21:58:00 yosi $
;;;
;
#+acl
(in-package :cl-user)

#+acl
(eval-when (:compile-toplevel)
  (declaim (optimize (speed 3) (safety 1) (space 0) (debug 0))) )

#|
(defun zerop (n)
  (= 0 n) )
|#

(defun listn (n)
  (unless (zerop n)
    (cons n (listn (1- n))) ) )

(defvar l18 (listn 18))
(defvar l12 (listn 12))
(defvar l6  (listn 6))

; Returns true when x is shorter than y.
(defun shorter-p (x y)
  (and y (or (null x) (shorter-p (cdr x) (cdr y)))) )

(defun takl (x y z)
  (if (not (shorter-p y x))
      z
    (takl (takl (cdr x) y z)
          (takl (cdr y) z x)
          (takl (cdr z) x y) )) )

#|
(defun takl (x y z)
  (labels ((shorter-p (x y)
         (and y (or (null x) 
            (shorter-p (cdr x) (cdr y)) )) ))
    (if (not (shorter-p y x))
      z
      (takl (takl (cdr x) y z)
    (takl (cdr y) z x)
    (takl (cdr z) x y) )) ) )
|#

(defun takl30 ()
  (takl l18 l12 l6)     ; 1
  (takl l18 l12 l6)     ; 2
  (takl l18 l12 l6)     ; 3
  (takl l18 l12 l6)     ; 4
  (takl l18 l12 l6)     ; 5
  (takl l18 l12 l6)     ; 6
  (takl l18 l12 l6)     ; 7
  (takl l18 l12 l6)     ; 8
  (takl l18 l12 l6)     ; 9
  (takl l18 l12 l6)     ; 10
  (takl l18 l12 l6)     ; 11
  (takl l18 l12 l6)     ; 12
  (takl l18 l12 l6)     ; 13
  (takl l18 l12 l6)     ; 14
  (takl l18 l12 l6)     ; 15
  (takl l18 l12 l6)     ; 16
  (takl l18 l12 l6)     ; 17
  (takl l18 l12 l6)     ; 18
  (takl l18 l12 l6)     ; 19
  (takl l18 l12 l6)     ; 20
  (takl l18 l12 l6)     ; 20
  (takl l18 l12 l6)     ; 21
  (takl l18 l12 l6)     ; 22
  (takl l18 l12 l6)     ; 23
  (takl l18 l12 l6)     ; 24
  (takl l18 l12 l6)     ; 25
  (takl l18 l12 l6)     ; 26
  (takl l18 l12 l6)     ; 27
  (takl l18 l12 l6)     ; 28
  (takl l18 l12 l6)     ; 29
  (takl l18 l12 l6)     ; 30
  ) ; takl30

(defun run-takl ()
  #-xyzzy
  (format t "; ~A-~A-~A/~A ~A~%"
      (or #+evita "evcl"
          #+clisp "clisp"
          #+allegro "acl"
          (lisp-implementation-type) )
      (lisp-implementation-version)
      (machine-type)
      (machine-version)
      (machine-instance) )
  (time (takl30)) )


; evcl-3.0.2425-i686.15.6/Intel(R) Core(TM)2 CPU 6600 @ 2.40GH   0.078
; acl-6.2-i686.8.6/730MHz           0.22 x30
; evcl-2.0.1730.1-i686.8.3/730MHz   0.62 x30 - x86-comp3
; clisp-2.33-i1586.2.9/2.7GHz       0.94 x30
; evcl-2.0.4418.2-i1586.2.9/2.7GHz  1.53 x30 - Visual Studio 2005
; evcl-2.0.2731.1-i1586.2.9/2.7GHz  1.56 x30 - write watch
; evcl-2.0.2306.1-i1586.2.9/2.7GHz  1.65 x30
; evcl-2.0.3103.1-i1586.2.9/2.7GHz  1.75 x30 - stack downard growth
; evcl-2.0.1104.1-i686.8.10/996MHz  2.23 x30 - evm-comp2?
; clisp-2.30-i686.8.6/863MHz        2.67 x30
; evcl-2.0.1730.1-i686.8.3/730MHz   9.81 x30 - evm-comp1

; clisp-2.28-i686.8.6/863MHz        2.59 x10
; clisp-2.28-i686.8.6/730MHz        2.89 x10
; clisp-2.27-i686.8.6/730MHz        0.22 x1

; STRCE = Self Tail-Recursive Call

; evcl-2.0.0705.1-i686.8.1/497MHz     3.65  x10
; evcl-1.0.7710.1-I686.8.6/497MHz     0.88  x1

; evcl-2.0.1708.2-i1586.2.7/2.7GHz    0.72  x10
; evcl-2.0.1007.1-i686.8.6/863MHz     1.59  compiler-v2
; evcl-2.0.1106.1-i686.8.6/863MHz     2.51  setjmp-opt
; evcl-2.0.0827.1-i686.8.6/863MHz     2.71  pxLocalFrame
; evcl-2.0.0827.1-i686.8.6/863MHz     2.90  pxEvalStack
; evcl-2.0.0827.1-i686.8.6/863MHz     3.13  jmp tbl[ecx*4]
; evcl-2.0.0818.1-i686.8.6/863MHz     3.69
; evcl-2.0.0631.1-i686.8.6/863MHz     2.06  vm1
; evcl-2.0.0524.1-i686.8.6/863MHz     5.06  (x10, branch optimization, STRC)

; evcl-2.0.1308.1-i686.8.3/730MHz     0.13
; evcl-2.0.1220.1-i686.8.3/730MHz     0.19  check RA, NARGS
; evcl-2.0.1212.2-i686.8.3/730MHz     0.16  native takl and shorter-p
; evcl-2.0.1212.2-i686.8.3/730MHz     0.58  native shorter-p
; evcl-2.0.1028.2-i686.8.3/730MHz     1.62  compiler-v2
; evcl-2.0.1001.1-i686.8.6/730MHz     2.46  reorder
; evcl-2.0.0908.1-i686.8.6/730MHz     2.78  short branch
; evcl-2.0.0827.1-i686.8.6/730MHz     3.18  pxLocalFrame
; evcl-2.0.0817.1-i686.8.3/730MHz     4.09 (bytecode)
; evcl-2.0.0815.1-i686.8.3/730MHz     2.77  m_nIP
; evcl-2.0.0627.1-i686.8.3/730MHz     2.41  primitive car, cdr
; evcl-2.0.0627.1-i686.8.3/730MHz     2.87  push-val-loc
; evcl-2.0.0624.1-i686.8.3/730MHz     2.92  make switch table with EAX index
; evcl-2.0.0624.1-i686.8.3/730MHz     3.25  remove branch from getOperand1
; evcl-2.0.0611.1-i686.8.3/730MHz     3.74  (intrinsic cdr)
; evcl-2.0.0524.1-i686.8.6/730MHz     5.99  (x10, branch optimization, STRC)
; evcl-1.0.7710.1-I686.8.6/730MHz     0.60  x1
; evcl-1.0.7518.1-I686.8.6/730MHz     0.66  x1 (no intrinsic, no tail recursive, m_nIP)
; evcl-1.0.7518.1-I686.8.6/730MHz     0.62  x1 (no intrinsic, no tail recursive)

; evlisp-1.0.7308.1-I686.8.6/730MHz   0.37  (remove __vret)
; evlisp-1.0.7307.1-I686.8.6/730MHz   0.72

; MacScheme 38.56



; Wed Dec 22 21:03:31 JST 1993 version 0.0.2 on sofina.kisi.kao.co.jp (2.5.7)
; CPU time (non-gc)   2.17 sec user   0.00 sec system
; CPU time (gc)       0.00 sec user   0.00 sec system
; CPU time (total)    2.17 sec user   0.00 sec system
; real time 2.17 sec

; Wed Dec 22 18:55:18 JST 1993 version 0.0.1 on sofina.kisi.kao.co.jp (2.5.4)
; CPU time (non-gc)   2.23 sec user   0.00 sec system
; CPU time (gc)       0.00 sec user   0.00 sec system
; CPU time (total)    2.23 sec user   0.00 sec system
; real time 2.40 sec


; Thu Nov 18 21:27:03 JST 1993 version 0.0.50 on smash (SunOS4.1.3)
; CPU time (non-gc)   4.38 sec user   0.00 sec system
; CPU time (gc)       0.00 sec user   0.00 sec system
; CPU time (total)    4.38 sec user   0.00 sec system
; real time 4.39 sec

; Thu Nov 18 21:27:03 JST 1993 version 0.0.5 on cena
; CPU time (non-gc)   4.74 sec user   0.00 sec system
; CPU time (gc)       0.00 sec user   0.00 sec system
; CPU time (total)    4.74 sec user   0.00 sec system
; real time 4.75 sec


; Tue Nov 16 15:10:09 JST 1993 version 0.0 on rouage (DEC Alpha)
; CPU time (non-gc)   2.96 sec user   0.00 sec system
; CPU time (gc)       0.00 sec user   0.00 sec system
; CPU time (total)    2.96 sec user   0.00 sec system
; real time 2.98 sec


; Mon Nov  8 12:21:40 JST 1993  version 0.0.45 on smash (NIL_AT_ZERO_P)
; CPU time (non-gc)   4.44 sec user   0.01 sec system
; CPU time (gc)       0.00 sec user   0.00 sec system
; CPU time (total)    4.44 sec user   0.01 sec system
; real time 4.57 sec


; Fri Nov  5 20:57:03 JST 1993 version 0.0 on cena (RS6000/580)
; CPU time (non-gc)   5.08 sec user   0.00 sec system
; CPU time (gc)       0.00 sec user   0.00 sec system
; CPU time (total)    5.08 sec user   0.00 sec system
; real time 5.10 sec


; Tue Nov  2 22:27:44 JST 1993 version 0.0.42 on smash
; CPU time (non-gc)   4.45 sec user   0.00 sec system
; CPU time (gc)       0.00 sec user   0.00 sec system
; CPU time (total)    4.45 sec user   0.00 sec system
; real time 4.45 sec

; Thr Aug 19 07:13:19 JST 1993, version 3.0 on kiss
; CPU time (non-gc)   9.47 sec user   0.02 sec system
; CPU time (gc)       0.00 sec user   0.00 sec system
; CPU time (total)    9.47 sec user   0.02 sec system
; real time 9.48 sec


; Mon Aug 16 12:45:49 JST 1993, version 3.2.11 on smash
; CPU time (non-gc)   5.42 sec user   0.00 sec system
; CPU time (gc)       0.00 sec user   0.00 sec system
; CPU time (total)    5.42 sec user   0.00 sec system
; real time 5.42 sec


; Wed Aug 4 16:59:38 JST 1993, version 3.2.8 on smash
; CPU time (non-gc)   9.89 sec user   0.07 sec system
; CPU time (gc)       0.00 sec user   0.00 sec system
; CPU time (total)    9.89 sec user   0.07 sec system
; real time 10.50 sec


; Wed Aug 4 13:58:22 JST 1993, version 3.2.8 on smash
; CPU time (non-gc)  10.04 sec user   0.00 sec system
; CPU time (gc)       0.00 sec user   0.00 sec system
; CPU time (total)   10.04 sec user   0.00 sec system
; real time 10.12 sec

; SparcLT
; CPU time (non-gc)  21.80 sec user   0.00 sec system
; CPU time (gc)       0.00 sec user   0.00 sec system
; CPU time (total)   21.80 sec user   0.00 sec system
; real time 22.23 sec

;Mon Jul 12 11:21:14 JST 1993 (Sun4/75 gcc 2.3.2)
; CPU time (non-gc)  15.82 sec user   0.02 sec system
; CPU time (gc)       0.00 sec user   0.00 sec system
; CPU time (total)   15.82 sec user   0.02 sec system
; real time 16.35 sec

;Wed Mar 31 12:39:51 JST 1993
; CPU time (non-gc)   0.01 sec user  12.34 sec system
; CPU time (gc)       0.00 sec user   0.00 sec system
; CPU time (total)    0.01 sec user  12.34 sec system
; real time 12.21 sec


;Tue Aug 25 14:52:43 1992 (Sun4/75 gcc 2.2.2)
; CPU time (non-gc) 9.95 sec user 0 sec system
; CPU time (gc)     0 sec user 0 sec system
; CPU time (total)  9.95 sec user 0 sec system
; real time 10.0743 sec

;Mon Apr 13 19:35:48 JST 1992 (Sun4/75 gcc_2.1)
; CPU time (non-gc) 20.95 sec user 0.01 sec system
; CPU time (gc)     0 sec user 0 sec system
; CPU time (total)  20.95 sec user 0.01 sec system
; real time 21.0321 sec

;Fri Feb  7 20:46:46 JST 1992 (Sun4/75 gcc_1.40)
; CPU time (non-gc) 27.39 sec user 0.12 sec system
; CPU time (gc)     0 sec user 0 sec system
; CPU time (total)  27.39 sec user 0.12 sec system
; real time 27.8966 sec

;Wed Jul 10 04:54:24 JST 1991(LUNA88K no -g)
; CPU time (non-gc)    32.71 sec user,     0.00 sec system
; CPU time (gc)         0.00 sec user,     0.00 sec system
; CPU time (total)     32.71 sec user,     0.00 sec system
; real time 32.70 sec

;Wed Jul 10 04:11:53 JST 1991(SUN4/330)
; CPU time (non-gc)    40.99 sec user,     0.02 sec system
; CPU time (gc)         0.00 sec user,     0.00 sec system
; CPU time (total)     40.99 sec user,     0.02 sec system
; real time 41.07 sec

;Wed Jul 10 04:13:47 JST 1991(LUNA88K)
; CPU time (non-gc)    34.02 sec user,     0.02 sec system
; CPU time (gc)         0.00 sec user,     0.00 sec system
; CPU time (total)     34.02 sec user,     0.02 sec system
; real time 34.03 sec

;Thu Jul  4 16:58:15 JST 1991
; CPU time (non-gc)    50.67 sec user,     0.27 sec system
; CPU time (gc)         0.00 sec user,     0.00 sec system
; CPU time (total)     50.67 sec user,     0.27 sec system
; real time 55.09 sec

;Wed May 29 10:15:47 JST 1991
; CPU time (non-gc)    30.93 sec user,     0.16 sec system
; CPU time (gc)         0.00 sec user,     0.00 sec system
; CPU time (total)     30.93 sec user,     0.16 sec system
; real time 32.10 sec

;Sun Apr  7 17:31:06 JST 1991
; CPU time (non-gc)    33.85 sec user,     0.16 sec system
; CPU time (gc)         0.00 sec user,     0.00 sec system
; CPU time (total)     33.85 sec user,     0.16 sec system
; real time 34.52 sec

;Fri Mar  1 02:54:31 JST 1991 Make cdr and null? mu-instruction.
; CPU time (non-gc)    34.18 sec user,     0.06 sec system
; CPU time (gc)         0.00 sec user,     0.00 sec system
; CPU time (total)     34.18 sec user,     0.06 sec system
; real time 34.50 sec

;Fri Mar  1 02:14:40 JST 1991 mu_call() => call_procedure
; CPU time (non-gc)    49.10 sec user,     0.17 sec system
; CPU time (gc)         0.00 sec user,     0.00 sec system
; CPU time (total)     49.10 sec user,     0.17 sec system
; real time 50.40 sec

;Fri Mar  1 01:23:27 JST 1991 (integrable-procedure)
; CPU time (non-gc)    50.70 sec user,     0.23 sec system
; CPU time (gc)         0.00 sec user,     0.00 sec system
; CPU time (total)     50.70 sec user,     0.23 sec system
; real time 51.45 sec

;Fri Mar  1 00:56:31 JST 1991
; CPU time (non-gc)    56.51 sec user,     0.33 sec system
; CPU time (gc)         0.00 sec user,     0.00 sec system
; CPU time (total)     56.51 sec user,     0.33 sec system
; real time 57.20 sec

;Tue Jan 22 05:29:10 JST 1991 (local-define shorter?)
; CPU time (non-gc)    58.81 sec user,     0.09 sec system
; CPU time (gc)         0.00 sec user,     0.00 sec system
; CPU time (total)     58.81 sec user,     0.09 sec system
; real time 59.96 sec

; Tue Jan 22 05:26:51 JST 1991
; CPU time (non-gc)    54.74 sec user,     0.02 sec system
; CPU time (gc)         0.00 sec user,     0.00 sec system
; CPU time (total)     54.74 sec user,     0.02 sec system
; real time 55.34 sec

; Fri Nov 30 14:45:02 JST 1990 (integrate usual procedure)
; CPU time (non-gc)    23.62 sec user,     2.13 sec system
; CPU time (gc)         0.00 sec user,     0.00 sec system
; CPU time (total)     23.62 sec user,     2.13 sec system
; real time 31.78 sec

; Fri Nov 30 02:27:18 JST 1990
; CPU time (non-gc)    53.58 sec user,     0.36 sec system
; CPU time (gc)         0.00 sec user,     0.00 sec system
; CPU time (total)     53.58 sec user,     0.36 sec system
; real time 54.48 sec


; Fri Dec 24 19:16:48 JST 1993 tl-1.0 on smash
; CPU time (non-gc)  33.24 sec user   0.01 sec system
; CPU time (gc)       0.00 sec user   0.00 sec system
; CPU time (total)   33.24 sec user   0.01 sec system
; Real time 34.26


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; For simple compiler
;;;
#|
; no unless
(defun listn (n)
  (if (not (zerop n))
    (cons n (listn (1- n))) ) )

; no and or
(defun shorter-p (x y)
  (if y
      (if (null x)
          t
         (shorter-p (cdr x) (cdr y)) )) )

; no labels
(defun takl (x y z)
    (if (not (shorter-p y x))
      z
      (takl (takl (cdr x) y z)
            (takl (cdr y) z x)
            (takl (cdr z) x y) )) )

(defun run-takl () (takl (listn 18) (listn 12) (listn 6)))
|#

