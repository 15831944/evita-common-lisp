;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SI; Base: 10 -*-
;;;;
;;;; evcl - runtime - 12 Numbers - Float
;;; arch/generic/lisp/runtime/gen-r12-float.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/gen-float-complex.lisp#4 $
;
(in-package :si)

;;;; iresult
;;; Returns an integer if specified single-lfoat value is an integer,
;;; otherwise this function returns single-float value.
;;;
;;; Note: (iresult -0.0) returns single-float -0.0 instead of integer 0.
;;;
(defun iresult (fy)
    (declare (values (or integer single-float)))
  (let* ((iy (decode-float32 fy))
         (ey (- (ash (logand iy #x7f800000) -23) 127)) )
    (cond
      ((eql iy 0)
        iy )
      ((< ey 0)
        fy )
      ((< ey 23)
        (if (eql (logand iy (ash #x007fffff (- ey))) 0)
            (logior (ash 1 ey) (ash (logand iy #x007FFFFF) (- ey 23)))
          fy ) )
      ((eql ey 128) fy)
      (t (ash (logior #x00800000 (logand iy #x007FFFFF))
              (- ey 23) ) )) ) )


(macrolet (
  (define (fn (&rest arg*) &body decl*-form*)
    (let ((cfn32
            (if (eq fn 'atan/1)
                'complex32-atan/1
              (intern (format nil "COMPLEX32-~A" fn)) ) )
          (cfn64
            (if (eq fn 'atan/1)
                'complex64-atan/1/1
              (intern (format nil "COMPLEX64-~A" fn)) ) )
          (ffn32
            (if (eq fn 'atan/1)
                'float32-atan/1
              (intern (format nil "FLOAT32-~A" fn)) ) )
          (ffn64
            (if (eq fn 'atan/1)
                'float64-atan/1
              (intern (format nil "FLOAT64-~A" fn)) ) ))

     `(defun ,fn ,arg*
          (declare (ftype (function (single-float) single-float) ,ffn32))
          (declare (ftype (function (double-float) double-float) ,ffn64))
        (labels (
          (,cfn32 (z)
            (declare (values (complex single-float)))
            (declare (type (complex single-float)))
            ,@decl*-form* )
          (,cfn64 (z)
            (declare (values (complex double-float)))
            (declare (type (complex double-float)))
            ,@decl*-form* )
          )
          ;;
          (typecase z
            (fixnum (iresult (,ffn32 (float z))))
            (bignum (iresult (,ffn32 (float z))))
            (ratio  (iresult (,ffn32 (float z))))
            (single-float (,ffn32 z))
            (double-float (,ffn64 z))
            ((complex rational)
              (,cfn32
                (complex (float (realpart z)) (float (imagpart z))) ) )
            ((complex single-float) (,cfn32 z))
            ((complex double-float) (,cfn64 z))
            (otherwise
              (error 'type-error :datum z :expected-type 'number) )) ) ) ) )
  )
  ;; acos z = -i log (z + i sqrt{1 - z^2})
  (define cl:acos (z)
    (let* ((z2  (sqrt (- 1 (* z z))))
           (iz2 (complex (- (imagpart z2)) (realpart z2)))
           (z3  (log (+ iz2 z))) )
      (complex (imagpart z3) (- (realpart z3))) ) )

  ;; acosh z = 2 log(sqrt((z+1)/2) + sqrt((z-1)/2))
  (define cl:acosh (z)
    (* 2 (log (+ (sqrt (/ (+ z 1) 2)) (sqrt (/ (- z 1) 2))))) )

  ;; asin z = -i log (iz + sqrt{1 - z^2})
  (define cl:asin (z)
    (let* ((iz (complex (- (imagpart z)) (realpart z)))
           (z2 (sqrt (- 1 (* z z))))
           (z3 (log (+ iz z2))) )
      (complex (imagpart z3) (- (realpart z3))) ) )

  ;;  asinh z = log(z + sqrt((1+x^2)))
  (define cl:asinh (z)
    (log (+ z (sqrt (+ 1 (* z z))))) )

  ;; atan z = -i log((1 + iz) sqrt{1/(1+z^2)})
  (define atan/1 (z)
    (let* ((z2 (complex (1+ (- (imagpart z))) (realpart z)))
           (z3 (* z2 z2))
           (z4 (1+ (* z z)))
           (z5 (/ z3 z4))
           (z6 (/ (log z5) 2)) )
      (complex (imagpart z6) (- (realpart z6))) ) )

  ;; atanh z = (log(1+x) - log(1-x))/2
  (define cl:atanh (z)
    (/ (- (log (+ 1 z) (log (- 1 z)))) 2) )

  ;; cos z = (e^{iz} + e^{-iz}) / 2
  (define cl:cos (z)
    (let* ((e^iz (exp (complex (imagpart z) (realpart z))))
           (e^-iz (/ 1 e^iz)) )
      (/ (+ e^iz e^-iz) 2) ) )

  ;; cosh
  (define cl:cosh (z)
    (/ (+ (exp z) (exp (- z))) 2) )

  ;; exp
  ;;  FIXME 2007-03-04: We should use expm1(x) + 1.0 for x < 1.0
  (define cl:exp (z)
    (let* ((x (realpart z))
           (y (imagpart z))
           (siny (sin y))
           (cosy (cos y))
           (expx (funcall #'exp x)) )
      (complex (* expx cosy) (* expx siny)) ) )

  ;;  sin z = (e^{iz} - e^{-iz}) / 2i
  (define cl:sin (z)
    (let* ((e^iz (exp (complex (imagpart z) (realpart z))))
           (e^-iz (/ 1 e^iz)) )
      (/ (- e^iz e^-iz) 2) ) )

  ;; sinh
  (defun cl:sinh (z)
    (/ (- (exp z) (exp (- z))) 2) )

  ;;  tan z = (e^{iz} - e^{-iz}) / i (e^{iz} + e^{-iz})
  (define cl:tan (z)
    (let* ((e^iz  (exp (complex (imagpart z) (realpart z))))
           (e^-iz (/ 1 e^iz))
           (w  (/ (- e^iz e^-iz) (+ e^iz e^-iz))) )
      (complex (imagpart w) (realpart w)) ) )

  ;; tanh
  (define cl:tanh (z)
    (let ((e^z  (exp z))
          (e^-z (exp (- z))) )
      (/ (- e^z e^-z) (+ e^z e^-z)) ) )
  )

;;;; 12.2.29 abs z
;;;     (abs z) = (sqrt (+ (expt (realpart z) 2) (expt (imagpart z) z)))
(defun cl:abs (z)
  (typecase z
    (fixnum (if (minusp z) (- z) z))
    (bignum (if (minusp z) (- z) z))
    (ratio  (if (minusp z) (- z) z))
    (single-float (float32-abs z))
    (double-float (float64-abs z))
    ((complex rational)
      (float32-hypot (float (realpart z)) (float (imagpart z))) )
    ((complex single-float)
      (float32-hypot (realpart z) (imagpart z)) )
    ((complex double-float)
      (float64-hypot (realpart z) (imagpart z)) )
    (otherwise
      (error 'type-error :datum z :expected-type 'number) )) )


;;;; atan z or atan x y
(defun cl:atan (x &optional (y 0 y-p))
  (macrolet (
    (dispatch ()
     `(typecase y
        (fixnum (iresult (float32-atan/2 (float x) (float y))))
        (bignum (iresult (float32-atan/2 (float x) (float y))))
        (ratio  (iresult (float32-atan/2 (float x) (float y))))
        (single-float (float32-atan/2 (float x) y))
        (double-float (float64-atan/2 (float x 0d0) y))
        (otherwise (error :datum y :expected-type 'real)) ) )
    )
    ;;
    (if (not y-p)
        (atan/1 y)
      (typecase x
        (fixnum (dispatch))
        (bignum (dispatch))
        (ratio  (dispatch))
        (single-float (dispatch))
        (double-float
          (typecase y
            (fixnum (float64-atan/2 (float x) (float y)))
            (bignum (float64-atan/2 (float x) (float y)))
            (ratio  (float64-atan/2 (float x) (float y)))
            (single-float (float64-atan/2 (float x) y))
            (double-float (float64-atan/2 (float x 0d0) y))
            (otherwise (error :datum y :expected-type 'real)) ) ))) ) )


;;;; cis x => (complex (cos x) (sin x))
(defun cl:cis (x)
  (macrolet (
    (implement (&rest name*)
     `(typecase x
        ,@(mapcar (lambda (ty) `(,ty (complex (cos x) (sin x)))) name*)
        (otherwise (error 'type-error :datum x :expected-type 'real)) ) )
    )
    (implement fixnum bignum ratio single-float double-float) ) )


;;;; 12.2.48 conjugate
(defun cl:conjugate (z)
  (typecase z
    (real z)
    ((complex rational)
      (complex (realpart z) (- (imagpart z))) )
    ((complex single-float)
      (complex (realpart z) (- (imagpart z))) )
    ((complex double-float)
      (complex (realpart z) (- (imagpart z))) )
    (otherwise
      (error 'type-error :datum z :expected-type 'number) )) )


;;;; 12.2.31 expt
(defun cl:expt (base power)
    (declare (values number))
    (declare (type number base power))
  (typecase base
    (integer
      (typecase power
        (integer (iexpt base power))
        (ratio   (iresult (float32-expt (float base) (float power))))
        (single-float (float32-expt (float base) power))
        (double-float (float64-expt (float base 0d0) power))
        ((complex rational) (exp (* power (log base))))
        ((complex single-float) (exp (* power (log base))))
        ((complex double-float) (exp (* power (log base))))
        (otherwise
          (error 'type-error :datum power :expected-type 'number) )) )
    (ratio
      (typecase power
        (integer (iresult (float32-expt (float base) (float power))))
        (ratio   (iresult (float32-expt (float base) (float power))))
        (single-float (float32-expt (float base) power))
        (double-float (float64-expt (float base 0d0) power))
        ((complex rational) (exp (* power (log base))))
        ((complex single-float) (exp (* power (log base))))
        ((complex double-float) (exp (* power (log base))))
        (otherwise
          (error 'type-error :datum power :expected-type 'number) )) )
    (single-float
      (typecase power
        (integer (float32-expt base (float power)))
        (ratio   (float32-expt base (float power)))
        (single-float (float32-expt base power))
        (double-float (float64-expt (float base 0d0) power))
        ((complex rational) (exp (* power (log base))))
        ((complex single-float) (exp (* power (log base))))
        ((complex double-float) (exp (* power (log base))))
        (otherwise
          (error 'type-error :datum power :expected-type 'number) )) )
    (double-float
      (typecase power
        (integer (float64-expt base (float power 0d0)))
        (ratio   (float64-expt base (float power 0d0)))
        (single-float (float64-expt (float base 0d0) power))
        (double-float (float64-expt base power))
        ((complex rational) (exp (* power (log base))))
        ((complex single-float) (exp (* power (log base))))
        ((complex double-float) (exp (* power (log base))))
        (otherwise
          (error 'type-error :datum power :expected-type 'number) )) )
    (otherwise
      (error 'type-error :datum base :expected-type 'number) )) )


;;; log/1
;;;  (log z) = (complex (log (abs z) (phase z)))
(defun log/1 (z)
  (labels (
    (flog32 (x)
      (if (not (minusp x))
          (float32-log x)
        (complex (float32-log (- x)) #.(float pi))) )

    (flog64 (x)
      (if (not (minusp x))
          (float64-log x)
        (complex (float64-log (- x)) #.(float pi 0d0))) )

    (rlog (x)
      (if (not (minusp x))
          (iresult (float32-log x))
        (complex (float32-log (- x)) #.(float pi))) )
    )
    (typecase z
      (fixnum (rlog (float z)))
      (bignum (rlog (float z)))
      (ratio  (rlog (float z)))
      (single-float (flog32 z))
      (double-float (flog64 z))
      ((complex rational)     (complex (flog32 (abs z)) (phase z)))
      ((complex single-float) (complex (flog32 (abs z)) (phase z)))
      ((complex double-float) (complex (flog64 (abs z)) (phase z)))
      (otherwise
        (error 'type-error :datum z :expected-type 'number) )) ) )


;;; log_b z = log z / log b
(defun cl:log (z &optional (b 0 b-p))
  (labels (
    (log/2 (z b)
      (let ((w (/ (log/1 z) (log/1 b))))
        (if (and (rationalp z) (rationalp b) (typep w 'single-float))
            (iresult w)
          w ) ) )
    )
  (if b-p (log/2 z b) (log/1 z)) ) )


;;; phase x+yi = atan y x
;;; phase x = 0 where x >= 0
;;; phase x = pi where x < 0
(defun cl:phase (z)
  (typecase z
    (fixnum (if (minusp z) (float pi) (float 0)))
    (bignum (if (minusp z) (float pi) (float 0)))
    (ratio  (if (minusp z) (float pi) (float 0)))
    ((complex rational)
      (atan (float (imagpart z)) (float (realpart z))) )
    ((complex single-float)
      (atan (imagpart z) (realpart z)) )
    ((complex double-float)
      (atan (imagpart z) (realpart z)) )
    (otherwise
      (error 'type-error :datum z :expected-type 'number) )) )


;;;; 12.2.37 signum
(defun cl:signum (z)
  (typecase z
    (fixnum
      (cond ((zerop z) z) ((plusp z) 1) (t -1)) )
    (bignum
      (if (plusp z) 1 -1) )
    (ratio
      (if (plusp z) 1 -1) )
    (single-float
      (cond ((zerop z) z) ((plusp z) 1) (t -1)) )
    (double-float
      (cond ((zerop z) z) ((plusp z) 1) (t -1)) )
    ((complex rational)
      (/ z (abs z)) )
    ((complex single-float)
      (/ z (abs z)) )
    ((complex double-float)
      (/ z (abs z)) )
    (otherwise
      (error 'type-error :datum z :expected-type 'number) )) )


;;;; 12.2.38 sqrt
;;; sqrt z = e^{(log z) / 2}
(defun cl:sqrt (z)
  (macrolet (
    ;; csqrt
    (csqrt (sqrt z.r z.i)
     `(let* ((z.r ,z.r)
             (z.i ,z.i)
             (x (abs z.r))
             (y (abs z.i))
             (w
                (if (>= x y)
                    (let ((r (/ y x)))
                      (* (,sqrt x)
                         (,sqrt (* 0.5 (+ 1 (,sqrt (+ 1 (* r r))))))) )
                  (let ((r (/ x y)))
                    (* (,sqrt y)
                       (,sqrt (* 0.5 (+ r (,sqrt (+ 1 (* r r))))))) )) ))
        (multiple-value-bind (c.r c.i)
            (cond
              ((>= z.r 0) (values w (/ z.i (* w 2))))
              ((>= z.i 0) (values w (/ z.i (* w 2))))
              (t          (values (- w) (/ z.i (* w -2)))) )
          (complex c.r c.i) ) ) )

    ;; ffsqrt
    (ffsqrt (sqrt zero arg)
      `(let ((x ,arg))
         (if (minusp x) (complex ,zero (,sqrt (- x))) (,sqrt x)) ) )
    )
  (labels (
    ;; fisqrt
    (fisqrt (ix)
      (if (>= ix 0)
          (iresult (float32-sqrt (float ix)))
        (complex 0 (fisqrt (- ix))) ) )
    )
  (typecase z
    (fixnum
      (fisqrt z) )
    (bignum
      (fisqrt z) )
    (ratio
      ;; FIXME 2007-03-31: We should signal underflow instead of overflow
      ;; when (float (denominator z)) signals overflow.
      ;; FIXME 2007-03-31: If |z| is too small, we should signal inexact.
      (/ (fisqrt (numerator z)) (fisqrt (denominator z))) )
    (single-float
      (ffsqrt float32-sqrt 0f0 z) )
    (double-float
      (ffsqrt float64-sqrt 0d0 z) )
    ((complex rational)
      (csqrt float32-sqrt (float (realpart z)) (float (imagpart z))) )
    ((complex single-float)
      (csqrt float32-sqrt (realpart z) (imagpart z)) )
    ((complex double-float)
      (csqrt float64-sqrt (realpart z) (imagpart z)) )
    (otherwise
      (error 'type-error :datum z :expected-type 'number) )) ) ) )
