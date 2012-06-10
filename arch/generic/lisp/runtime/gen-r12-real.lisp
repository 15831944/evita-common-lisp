;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SI; Base: 10 -*-
;;;;
;;;; evcl - runtime - 12 Numbers - Real
;;; arch/generic/lisp/runtime/gen-r12-real.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/runtime/gen-r12-real.lisp#10 $
;
(in-package :si)

;;;; ceiling/2 - when sign(x) == sign(y) => q+1, r-y
;;;  truncating to positive infinity.
(defun ceiling/2 (x y)
    (declare (type integer x y))
    (declare (values integer integer))
  (multiple-value-bind (q r) (truncate/2 x y)
    (cond
      ((eql r 0) (values q r))
      ((or (and (plusp x)  (plusp y)) (and (minusp x) (minusp y)))
        (values (+ q 1) (- r y)) )
      (t
        (values q r) )) ) )

;;;; floor/2 - when sign(x) != sign(y) => q-1, r+y
;;;  truncating to negative infinity.
(defun floor/2 (x y)
    (declare (type integer x y))
    (declare (values integer integer))
  (multiple-value-bind (q r) (truncate/2 x y)
    (cond
      ((eql r 0) (values q r))
      ((or (and (plusp x)  (plusp y)) (and (minusp x) (minusp y)))
        (values q r) )
      (t (values (- q 1) (+ r y))) ) ) )

;;;; round/2
;;;  truncating to nearest integer
(defun round/2 (x y)
    (declare (type integer x y))
    (declare (values integer integer))
  (multiple-value-bind (q r) (truncate/2 x y)
    (if (eql r 0)
        (values q r)
      (let ((y/2 (ash y -1)))
        (cond
         ((and (plusp x)  (plusp y))
           (if (or (< r y/2) (and (= r y/2) (evenp q)))
               (values q r)
             (values (1+ q) (- r y)) ) )
          ((and (minusp x) (minusp y))
           (if (or (< y/2 r) (and (= r y/2) (evenp q)))
               (values q r)
             (values (1+ q) (- r y)) ) )
          ((plusp x) ; (minusp y)   -> N,P
            (let ((y/2 (- y/2)))
              (if (or (< r y/2) (and (= r y/2) (evenp q)))
                  (values q r)
                (values (1- q) (+ r y)) ) ) )
          (t ; (minusp x) (plusp y)
            (let ((y/2 (- y/2)))
              (if (or (< y/2 r) (and (= r y/2) (evenp q)))
                  (values q r)
                (values (1- q) (+ r y)) ) ) )) )) ) )

;;;; truncate/2
;;; See built-in function.


(macrolet (
  ;; Shortcut
  (den (x) `(ref ratio denominator ,x))
  (num (x) `(ref ratio numerator   ,x))

  ;; defidiv
  (defidiv (gdiv idiv fdiv32 fdiv64)
   `(defun ,gdiv (x &optional (y 1 y-p))
        (declare (values integer real))
      (if (not y-p)
          (typecase x
            (integer
              (values x 0) )
            (ratio
              (multiple-value-bind (q r) (,idiv (num x) (den x))
                (values q r) ) )
            (single-float
              (let ((q (float32-truncate (,fdiv32 x))))
                (values q (- x q)) ) )
            (double-float
              (let ((q (float64-truncate (,fdiv64 x))))
                (values q (- x q)) ) )
            (otherwise (expect-real x)) )
        (typecase x
          (integer
            (typecase y
              (integer
                (,idiv x y) )
              (ratio
                (,idiv (* x (den y)) (num y)) )
              (single-float
                (let ((q (float32-truncate (,fdiv32 (/ (float x) y)))))
                  (values q (- x (* q y))) ) )
              (double-float
                (let ((q (float64-truncate (,fdiv64 (/ (float x 0d0) y)))))
                  (values q (- x (* q y))) ) )
              (otherwise
                (expect-real y) )) )
          (ratio
            (typecase y
              (integer
                (,idiv (* (num x) y) (den x)) )
              (ratio
                (,idiv (* (num x) (den y)) (* (den x) (num y))) )
              (single-float
                (let ((q (float32-truncate (,fdiv32 (/ (float x) y)))))
                  (values q (- x (* q y))) ) )
              (double-float
                (let ((q (float64-truncate (,fdiv64 (/ (float x 0d0) y)))))
                  (values q (- x (* q y))) ) )
              (otherwise
                (expect-real y) )) )
          (single-float
            (typecase y
              (rational
                (let ((q (float32-truncate (,fdiv32 (/ x (float y))))))
                  (values q (- x (* q y))) ) )
              (single-float
                (let ((q (float32-truncate (,fdiv32 (/ x y)))))
                  (values q (- x (* q y))) ) )
              (double-float
                (let ((q (float64-truncate (,fdiv64 (/ (float x 0d0) y)))))
                  (values q (- x (* q y))) ) )
              (otherwise
                (expect-real y) )) )
          (double-float
            (typecase y
              (rational
                (let ((q (float64-truncate (,fdiv64 (/ x (float y 0d0))))))
                  (values q (- x (* q y))) ) )
              (single-float
                (let ((q (float64-truncate (,fdiv64 (/ x (float y 0d0))))))
                  (values q (- x (* q y))) ) )
              (double-float
                (let ((q (float64-truncate (,fdiv64 (/ x y)))))
                  (values q (- x (* q y))) ) )
              (otherwise
                (expect-real y) )) )
          (otherwise
            (expect-real x) ))) ) )

  ;; deffdiv
  (deffdiv (fdiv idiv fdiv32 fdiv64)
   `(defun ,fdiv (x &optional (y 1 y-p))
        (declare (values float real))
      (if (not y-p)
          (typecase x
            (integer
              (values (float x) 0f0) )
            (ratio
              (multiple-value-bind (q r) (,idiv (num x) (den x))
                (values (float q) r) ) )
            (single-float
              (let ((q (,fdiv32 x)))
                (values q (- x q)) ) )
            (double-float
              (let* ((x (float x 0d0))
                     (q (,fdiv64 x)) )
                (values q (- x q)) ) )
            (otherwise (expect-real x)) )
        (typecase x
          (rational
            (typecase y
              (rational
                (multiple-value-bind (q r) (,idiv x y)
                  (values (float q) r) ) )
              (single-float
                (let* ((x (float x))
                       (q (,fdiv32 (/ x y))) )
                  (values q (- x (* q y))) ) )
              (double-float
                (let* ((x (float x 0d0))
                       (q (,fdiv64 (/ x y))) )
                  (values q (- x (* q y))) ) )
              (otherwise
                (expect-real y) )) )

          ;; single-float: all opeation on float
          (single-float
            (typecase y
              (rational
                (let* ((y (float y))
                       (q (,fdiv32 (/ x y))) )
                  (values q (- x (* q y))) ) )
              (single-float
                (let* ((q (,fdiv32 (/ x y))))
                  (values q (- x (* q y))) ) )
              (double-float
                (let* ((x (float x 0d0))
                       (q (,fdiv64 (/ x y))) )
                  (values q (- x (* q y))) ) )
              (otherwise
                (expect-real y) )) )

          ;; double-float: all opeation on float
          (double-float
            (typecase y
              (rational
                (let* ((y (float y 0d0))
                       (q (,fdiv64 (/ x y))) )
                  (values q (- x (* q y))) ) )
              (single-float
                (let* ((y (float y 0d0))
                       (q (,fdiv64 (/ x y))) )
                  (values q (- x (* q y))) ) )
              (double-float
                (let* ((q (,fdiv64 (/ x y))) )
                  (values q (- x (* q y))) ) )
              (otherwise
                (expect-real y) )) )
          (otherwise
            (expect-real x)) )) ) )

  ;; defmod
  (defmod (gmod idiv rdiv fmod32 fmod64)
   `(defun ,gmod (x y)
      (typecase x
        (integer
          (typecase y
            (integer (nth-value 1 (,idiv x y)))
            (ratio   (nth-value 1 (,rdiv x y)))
            (single-float (,fmod32 (float x) y))
            (double-float (,fmod64 (float x 0d0) y))
            (otherwise (expect-real y)) ) )
        (ratio
          (typecase y
            (integer (nth-value 1 (,rdiv x y)))
            (ratio   (nth-value 1 (,rdiv x y)))
            (single-float (,fmod32 (float x) y))
            (double-float (,fmod64 (float x 0d0) y))
            (otherwise (expect-real y)) ) )
        (single-float
          (typecase y
            (integer      (,fmod32 x (float y)))
            (ratio        (,fmod32 x (float y)))
            (single-float (,fmod32 x y))
            (double-float (,fmod64 (float x 0d0) y))
            (otherwise (expect-real y)) ) )
        (double-float
          (typecase y
            (integer      (,fmod64 x (float y 0d0)))
            (ratio        (,fmod64 x (float y 0d0)))
            (single-float (,fmod64 x (float y 0d0)))
            (double-float (,fmod64 x y))
            (otherwise (expect-real y)) ) )
        (otherwise (expect-real x)) ) ) )
    )
    (labels (
        (expect-real (x)
          (error 'type-error :datum x :expected-type 'real) )
    )
    ;; 12.2.19 ceiling
    ;; 12.2.19 floor
    ;; 12.2.19 round
    ;; 12.2.19 truncate
    (defidiv cl:ceiling  ceiling/2      float32-fceiling  float64-fceiling)
    (defidiv cl:floor    floor/2        float32-ffloor    float64-ffloor)
    (defidiv cl:round    round/2        float32-fround    float64-fround)
    (defidiv cl:truncate truncate/2     identity         identity)

    ;; 12.2.19 fceiling
    ;; 12.2.19 ffloor
    ;; 12.2.19 fround
    ;; 12.2.19 ftruncate
    (deffdiv cl:fceiling  ceiling/2     float32-fceiling  float64-fceiling)
    (deffdiv cl:ffloor    floor/2       float32-ffloor    float64-ffloor)
    (deffdiv cl:fround    round/2       float32-fround    float64-fround)
    (deffdiv cl:ftruncate truncate/2    float32-ftruncate float64-ftruncate)

    ;; 12.2.36 mod
    ;; 12.2.36 rem
    (defmod cl:mod floor/2    floor       float32-mod float64-mod)
    (defmod cl:rem truncate/2 truncate    float32-rem float64-rem)

    ;; 12.2. 17 minusp
    (defun cl:minusp (x)
      (typecase x
        (fixnum (< x 0))
        (bignum (< x 0))
        (ratio  (< x 0))
        (single-float (< (decode-float32 x) 0))
        (double-float (< (decode-float64 x) 0))
        (otherwise (expect-real x)) ) )

    ;; 12.2.17 plusp
    (defun cl:plusp (x)
      (typecase x
        (fixnum (> x 0))
        (bignum (> x 0))
        (ratio  (> x 0))
        (single-float (> (decode-float32 x) 0))
        (double-float (> (decode-float64 x) 0))
        (otherwise (expect-real x)) ) )

    ;; 12.2.17 zerop
    (defun cl:zerop (x)
      (typecase x
        (fixnum (eql x 0))
        (bignum (eql x 0))
        (ratio  nil)
        (single-float (eql (logand (decode-float32 x) #x7fffffff) 0))
        (double-float
          (multiple-value-bind (hx lx) (decode-float64 x)
            (eql (logior (logand hx #x7fffffff) lx) 0) ) )
        (otherwise (expect-real x)) ) )
  ) ) ; macrolet
