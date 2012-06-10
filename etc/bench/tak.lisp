#+acl
(in-package :cl-user)

#+acl
(eval-when (compile)
  (declaim (optimize (speed 3) (safety 1) (space 0) (debug 0))) )

;(format t "(tak ~D ~D ~D)~%" x y z)
(defun tak (x y z)
   (if (not (< y x))
       z
    (tak (tak (- x 1) y z)
         (tak (- y 1) z x)
         (tak (- z 1) x y) )) )

 ;(format t "(tak ~D ~D ~D)~%" x y z)

(defun tak (x y z)
   (if (not (< y x))
        z
        (tak (tak (1- x) y z)
            (tak (1- y) z x)
            (tak (1- z) x y) )) )

(defun tak (x y z)
  (tagbody loop
    (if (>= y x)
        (return-from tak z)
        (let ((x1 (tak (1- x) y z))
              (y1 (tak (1- y) z x))
              (z1 (tak (1- z) x y)) )
          (setq x x1)
          (setq y y1)
          (setq z z1)
          (go loop) ))) )

(defun tak (x y z)
    (if (>= y x)
        z
        (tak (tak (1- x) y z)
             (tak (1- y) z x)
             (tak (1- z) x y) )) )

(defmacro my-time (form)
  `(let (start end)
     (setq start (get-internal-real-time))
     ,form
     (setq end (get-internal-real-time))
     (format t "Elapsed time: ~S sec.~%"
        (float (/ (- end start) internal-time-units-per-second)) ) ) )

(defun tak30 ()
  (tak 18 12 6) ; 1
  (tak 18 12 6) ; 2
  (tak 18 12 6) ; 3
  (tak 18 12 6) ; 4
  (tak 18 12 6) ; 5
  (tak 18 12 6) ; 6
  (tak 18 12 6) ; 7
  (tak 18 12 6) ; 8
  (tak 18 12 6) ; 9
  (tak 18 12 6) ; 10
  (tak 18 12 6) ; 11
  (tak 18 12 6) ; 12
  (tak 18 12 6) ; 13
  (tak 18 12 6) ; 14
  (tak 18 12 6) ; 15
  (tak 18 12 6) ; 16
  (tak 18 12 6) ; 17
  (tak 18 12 6) ; 18
  (tak 18 12 6) ; 19
  (tak 18 12 6) ; 20
  (tak 18 12 6) ; 21
  (tak 18 12 6) ; 22
  (tak 18 12 6) ; 23
  (tak 18 12 6) ; 24
  (tak 18 12 6) ; 25
  (tak 18 12 6) ; 26
  (tak 18 12 6) ; 27
  (tak 18 12 6) ; 28
  (tak 18 12 6) ; 29
  (tak 18 12 6) ; 30
 ) ; tak30


(defun run-tak ()
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
  (time (tak30)) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; (time (tak 36 12 6)) => 7
;;;

; acl-6.2-i686.8.3/730MHz            0.40
; evcl-2.0.1220.1-i686.8.3/730MHz    0.40    x86
; evcl-2.0.1220.1-i686.8.3/730MHz    0.43    x86, validate NARGS
; evcl-2.0.1728.1.i686.8.3/730MHz    0.45    x86, inline 1-, inline >=
; lisp-work-4.2-i686.8.3/730MHz      0.52
; evcl-2.0.1220.1-i686.8.3/730MHz    0.81    x86, validate RA and NARGS

; evcl-2.0.2823.1-i1586.3.4/3.1GHz   1.44
; clisp-2.33--i1586.2.9/2.7GHz       1.63
; evcl-2.0.2306.1-i1586.2.9/2.7GHz   1.82
; evcl-2.0.1728.1.i686.8.3/730MHz    5.27    evm, inline 1-, inline >=
; evcl-2.0.2109.1.i686.8.3/730MHz    6.39    evm entry
; evcl-2.0.2103.1.i686.8.3/730MHz    6.53    evm
; evcl-2.0.1728.1.i686.8.3/730MHz    7.04    evm
; evcl-2.0.1728.1.i686.8.3/730MHz    8.40    evm, inline 1-, generic >=
; evcl-2.0.1728.1.i686.8.3/730MHz   10.93    evm, generic 1- >=
; evcl-2.0.2204.1.i686.8.6/863MHz   10.02    x86 no intrinsic

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; (time (tak 18 12 6)) ; => 7 x 30
;;;;

; evcl-3.0.2425-i686.15.6/Intel(R) Core(TM)2 CPU 6600 @ 2.40GH 0.249
; acl-6.2-i686.8.3/730MHz           0.06    x30
; acl-6.2-i686.8.3/730MHz           0.05    x10
; lisp-work-4.2-i686.8.3/730MHz     0.09    x30
; evcl-2.0.2823.1-i1586.3.4/3.1GHz  0.23    x30
; evcl-2.0.4418.2-i1586.2.9/2.7GHz  0.28    x30
; evcl-2.0.1708.2-i1586.2.7/2.7GHz  0.30    x30
; evcl-2.0.2306.1-i1586.2.9/2.7GHz  0.30    x30
; evcl-2.0.1104.1-i686.8.10/996MHz  0.86    x30
; clicp-2.30-i686.8.3/730MHz        1.79    x30
; clisp-2.28-i686.8.6/863MHz        1.54    x30
; evcl-2.0.0705.1-i686.8.1/497MHz   1.06    x10

; evcl-2.0.1106.1-i686.8.6/863MHz   0.97    x30 setjmp-opt
; evcl-2.0.0827.1-i686.8.6/863MHz   1.04    x30
; evcl-2.0.0827.1-i686.8.6/863MHz   0.72    x20
; evcl-2.0.0818.1-i686.8.6/863MHz   0.89    x20
; evcl-2.0.0628.1-i686.8.6/863MHz   0.59    x20 vm1
; evcl-2.0.0628.1-i686.8.6/863MHz   0.40    x10

; evcl-2.0.2109.1-i686.8.3/730MHz   1.05    c-4, evm, entry
; evcl-2.0.2103.1-i686.8.3/730MHz   1.05    c-4, evm, call_lit8
; evcl-2.0.2102.1-i686.8.3/730MHz   1.14    c-4, evm
; evcl-2.0.1227.1-i686.8.3/730MHz   1.29    evm+profile
; evcl-2.0.1220.1-i686.8.3/730MHz   0.14    validate RA, NARGS
; evcl-2.0.1212.2-i686.8.3/730MHz   0.10    native >=, 1-
; evcl-2.0.1212.2-i686.8.3/730MHz   0.98    native >=
; evcl-2.0.1028.2-i686.8.3/730MHz   1.17    x30
; evcl-2.0.0827.1-i686.8.6/730MHz   1.22    x30
; evcl-2.0.0817.1-i686.8.3/730MHz   0.97    x20
; evcl-2.0.0815.1-i686.8.3/730MHz   0.79    x20
; evcl-2.0.0630.1-i686.8.3/730MHz   0.36    x10 >=_2op
; evcl-2.0.0627.1-i686.8.3/730MHz   0.50    x10
; evcl-2.0.0624.1-i686.8.3/730MHz   0.73    x10
; evcl-2.0.0624.1-i686.8.3/730MHz   0.82    x10
; evcl-2.0.0524.1-i686.8.6/863MHz   0.74    x10
; evcl-1.0.7710.1-i686.8.3/500MHz   0.12    x1
; evcl-1.0.7518.1-i686.8.3/730MHz   0.07    x1(no intrinsic)

; EvLisp  P3/730 NT5
; 2002-01-04    0.03    Intrinsic 1+, >=
; 2001-12-28    0.05

; clisp-2.27 P3/730 NT5
; interpreter   0.20
; compile       0.05

; ACL.6.1-P3/500-NT5    0.01

; xyzzy P3/500 NT5
; interpreter   0.87 (P3/500, XP)
; compiled      0.40 (P3/500, XP)

; xyzzy P3/730 NT5
; interpreter   0.68 (P3/730, NT5)
; compiled      0.32 (P3/730, NT5)


; EvLisp Pentium/133 NT4 build 1234
; 05/12/96 0.26     fixnum comparison
; 05/16/96 0.34     generic-number comparison
; 05/20/96 0.28     vm_exec checks primitive function call.
; 05/20/96 0.34     vm_exec includes try-catch
; 05/21/96 0.29     vm_exec supports catch

;;; TAKL

#|
(defun listn (n)
  (if (not (= 0 n))
      (cons n (listn (1- n))) ) )


(setq a (make-array 10 :element-type 'character :fill-pointer 0))


(defun foo ()
  (let ((i 0))
    (tagbody
      loop
       (if (>= i 1000000) (return-from foo 0))
       (setq i (1+ i))
       (go loop) ) ) )
|#
; clisp 0.5
; xyzzy 0.8
; evcl  0.2
