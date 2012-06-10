;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - float64 - expt
;;; arch/generic/lisp/macth/gen-float64-expt.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float64/gen-float64-expt.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     float64-expt
;
(in-package :si)

#|
 See fdlibm (http://www.netlib.org/fdlibm/)
 See http://sources.redhat.com/newlib/

 /* @(#)e_pow.c 5.1 93/09/24
 *
 * ====================================================
 * Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
 *
 * Developed at SunPro, a Sun Microsystems, Inc. business.
 * Permission to use, copy, modify, and distribute this
 * software is freely granted, provided that this notice
 * is preserved.
 * ====================================================


 * __ieee754_pow(x,y) return x**y
 *
 *                    n
 * Method:  Let x =  2   * (1+f)
 *      1. Compute and return log2(x) in two pieces:
 *              log2(x) = w1 + w2,
 *         where w1 has 53-24 = 29 bit trailing zeros.
 *      2. Perform y*log2(x) = n+y' by simulating multi-precision
 *         arithmetic, where |y'|<=0.5.
 *      3. Return x**y = 2**n*exp(y'*log2)
 *
 * Special cases:
 *      1.  (anything) ** 0  is 1
 *      2.  (anything) ** 1  is itself
 *      3.  (anything) ** NAN is NAN
 *      4.  NAN ** (anything except 0) is NAN
 *      5.  +-(|x| > 1) **  +INF is +INF
 *      6.  +-(|x| > 1) **  -INF is +0
 *      7.  +-(|x| < 1) **  +INF is +0
 *      8.  +-(|x| < 1) **  -INF is +INF
 *      9.  +-1         ** +-INF is NAN
 *      10. +0 ** (+anything except 0, NAN)               is +0
 *      11. -0 ** (+anything except 0, NAN, odd integer)  is +0
 *      12. +0 ** (-anything except 0, NAN)               is +INF
 *      13. -0 ** (-anything except 0, NAN, odd integer)  is +INF
 *      14. -0 ** (odd integer) = -( +0 ** (odd integer) )
 *      15. +INF ** (+anything except 0,NAN) is +INF
 *      16. +INF ** (-anything except 0,NAN) is +0
 *      17. -INF ** (anything)  = -0 ** (-anything)
 *      18. (-anything) ** (integer) is (-1)**(integer)*(+anything**integer)
 *      19. (-anything except 0 and inf) ** (non-integer) is NAN
 *
 * Accuracy:
 *      pow(x,y) returns x**y nearly rounded. In particular
 *                      pow(integer,integer)
 *      always returns the correct integer provided it is
 *      representable.
 *
 * Constants :
 * The hexadecimal values are the intended ones for the following
 * constants. The decimal values may be used, provided that the
 * compiler will convert from decimal to binary accurately enough
 * to produce the hexadecimal values shown.
|#
(defun float64-expt (x y)
    (declare (values double-float))
    (declare (type double-float x y))
  (prog* (
    (bp_0 1.0d0)
    (bp_1 1.5d0)
    (dp_h_0 0d0)
    (dp_h_1 #+nil 5.84962487220764160156e-01
            #.(encode-float64 #x3FE2B803 #x40000000) )
    (dp_l_0 0d0)
    (dp_l_1 #+nil 1.35003920212974897128e-08
            #.(encode-float64 #x3E4CFDEB #x43CFD006) )
    (zero 0d0)
    (one  1d0)
    (two  2d0)
    (two53 #+nil 9007199254740992.0
            #.(encode-float64 #x43400000 #x00000000) )
    (huge  1.0d300)
    (tiny  1.0d-300)
     ;; poly coefs for (3/2)*(log(x)-2s-2)/3*s**3
    (L1     #+nil 5.99999999999994648725e-01
            #.(encode-float64 #x3FE33333 #x33333303) )
    (L2     #+nil 4.28571428578550184252e-01
            #.(encode-float64 #x3FDB6DB6 #xDB6FABFF) )
    (L3     #+nil 3.33333329818377432918e-01
            #.(encode-float64 #x3FD55555 #x518F264D) )
    (L4     #+nil 2.72728123808534006489e-01
            #.(encode-float64 #x3FD17460 #xA91D4101) )
    (L5     #+nil 2.30660745775561754067e-01
            #.(encode-float64 #x3FCD864A #x93C9DB65) )
    (L6     #+nil 2.06975017800338417784e-01
            #.(encode-float64 #x3FCA7E28 #x4A454EEF) )
    (P1     #+nil 1.66666666666666019037e-01
            #.(encode-float64 #x3FC55555 #x5555553E) )
    (P2     #+nil -2.77777777770155933842e-03
            #.(encode-float64 #xBF66C16C #x16BEBD93) )
    (P3     #+nil 6.61375632143793436117e-05
            #.(encode-float64 #x3F11566A #xAF25DE2C) )
    (P4     #+nil -1.65339022054652515390e-06
            #.(encode-float64 #xBEBBBD41 #xC5D26BF1) )
    (P5     #+nil  4.13813679705723846039e-08
            #.(encode-float64 #x3E663769 #x72BEA4D0) )
    (lg2    #+nil  6.93147180559945286227e-01
            #.(encode-float64 #x3FE62E42 #xFEFA39EF) )
    (lg2_h  #+nil  6.93147182464599609375e-01
            #.(encode-float64 #x3FE62E43 #x00000000) )
    (lg2_l  #+nil -1.90465429995776804525e-09
            #.(encode-float64 #xBE205C61 #x0CA86C39) )
    (ovt    8.0085662595372944372e-0017) ;  -(1024-log2(ovfl+.5ulp))
    (cp     #+nil  9.61796693925975554329e-01
            #.(encode-float64 #x3FEEC709 #xDC3A03FD) ) ; 2/(3ln2)
    (cp_h   #+nil  9.61796700954437255859e-01
            #.(encode-float64 #x3FEEC709 #xE0000000) ) ; (float)cp
    (cp_l   #+nil -7.02846165095275826516e-09
            #.(encode-float64 #xBE3E2FE0 #x145B01F5) ) ; tail of cp_h
    (ivln2  #+nil  1.44269504088896338700e+00
            #.(encode-float64 #x3FF71547 #x652B82FE) ) ; 1/ln2
    (ivln2_h #+nil  1.44269502162933349609e+00
            #.(encode-float64 #x3FF71547 #x60000000) ) ; 24b 1/ln2
    (ivln2_l  #+nil  1.92596299112661746887e-08
            #.(encode-float64 #x3E54AE0B #xF85DDF44) ) ); 1/ln2 tail
    (multiple-value-bind (hx lx) (decode-float64 x)
        (declare (type (signed-byte 32) hx lx))
    (let ((ix (logand hx #x7fffffff)))
        (declare (type (signed-byte 32) ix))
    (multiple-value-bind (hy ly) (decode-float64 y)
        (declare (type (signed-byte 32) hy ly))
    (let ((iy (logand hy #x7fffffff)))
        (declare (type (signed-byte 32) hy))
    (let ((t1 0d0)
          (t2 0d0) )
        (declare (type double-float t1 t2))
      ;; y==zero: x**0 = 1
      (when (eql (logior iy ly) 0)
        (return one) )

      ;; +-NaN return x+y
      (when (or (> ix #x7ff00000)
                (and (eql ix #x7ff00000) (not (eql lx 0)))
                (> iy #x7ff00000)
                (and (eql iy #x7ff00000) (not (eql ly 0))) )
        (return (+ x y)) )

      ;; determine if y is an odd int when x < 0
      ;; yisint = 0       ... y is not an integer
      ;; yisint = 1       ... y is an odd int
      ;; yisint = 2       ... y is an even int
      (let ((yisint
              (cond
                ((>= hx 0)
                  0 )
                ((>= iy #x43400000)
                  2 )   ;  even integer y
                ((< iy #x3ff00000)
                  0 )
                (t
                  (let ((k (- (ash iy -20) #x3ff))) ; exponent
                    (cond
                     ((> k 20)
                       (let ((j  (ash ly (- k 52))))
                         (if (eql (ash j (- 52 k)) ly)
                             (- 2 (logand j 1))
                           0 ) ) )
                     ((eql ly 0)
                       (let ((j  (ash iy (- k 20))))
                         (if (eql (ash j (- 20 k)) iy)
                             (- 2 (logand j 1))
                           0 ) ) )
                     (t 0) ) ) )) ))

        ;; special value of y
        (when (eql ly 0)
          (cond
            ((eql iy #x7ff00000)                ; y is +-inf
              (cond
                ((eql (logior (- ix #x3ff00000) lx) 0)
                  (return (- y y)) )            ; inf**+-1 is NaN
                ((>= ix #x3ff00000)             ;  (|x|>1)**+-inf = inf,0
                  (return (if (>= hy 0) y  zero)) )
                (t                              ; (|x|<1)**-,+inf = inf,0
                  (return (if (< hy 0) (- y) zero)) )) )
            ((eql iy #x3ff00000)                ; y is  +-1
              (return (if (< hy 0) (/ one x) x)) )
            ((eql hy #x40000000)
              (return (* x x)) )                ; y is  2
            ((eql hy #x3fe00000)                ; y is  0.5
              (when (>= hx 0)                   ; x >= +0
                (return (float64-sqrt x)) ) )))

        (let ((ax (float64-abs x)))
          ;; special value of x
          (when (and (eql lx 0)
                     (or (eql ix #x7ff00000)    ; x is +-inf
                         (eql ix 0)             ; x is +-0
                         (eql ix #x3ff00000) )) ; x is +-1
            (let ((z
                    (if (< hy 0)
                        (/ one ax)  ; z = (1/|x|)
                      ax ) ))
              (when (< hx 0)
                (cond
                  ((eql (logior (- ix #x3ff00000) yisint) 0)
                    (setq z (/ (- z z) (- z z))) )  ; (-1)**non-int is NaN
                  ((eql yisint 1)
                    (setq z (- z)) )))   ; (x<0)**odd = -(|x|**odd)
              (return z) ))

          ;; (x<0)**(non-int) is NaN
          (when (eql (logior (1+ (ash hx -31)) yisint) 0)
            (return (/ (- x x) (- x x))) )

        ;; |y| is huge
        (if (> iy #x41e00000)     ; if |y| > 2**31
          (progn
            (when (> iy #x43f00000)   ; if |y| > 2**64, must o/uflow
              (when (<= ix #x3fefffff)
                (return
                  (if (< hy 0)(* huge huge) (* tiny tiny)) ))

              (when (>= ix #x3ff00000)
                (return
                  (if (> hy 0)(* huge huge) (* tiny tiny)) )))

              ;; over/underflow if x is not close to one
              (when (< ix #x3fefffff)
                (return
                  (if (< hy 0) (* huge huge) (* tiny tiny)) ))

              (when (> ix #x3ff00000)
                (return
                  (if (> hy 0)  (* huge huge) (* tiny tiny)) ))

              ;; now |1-x| is tiny <= 2**-20, suffice to compute
              ;; log(x) by x-x^2/2+x^3/3-x^4/4
              (let* (
                (tt (- ax 1))    ;; tt has 20 trailing zeros
                (w (* (* tt tt)
                         (- 0.5d0
                            (* tt (- 0.3333333333333333d0 (* tt 0.25d0))))) )
                (u (* ivln2_h tt))   ; ivln2_h has 21 sig. bits
                (v (- (* tt ivln2_l) (* w ivln2))) )
              (setq t1 (let* ((t1 (+ u v))
                              (th (decode-float64 t1)) )
                          (encode-float64 th 0) ) )
              (setq t2 (- v (- t1 u))) ))

          (let ((n 0)
                (j 0)
                (k 0)
                (bp_k 0d0)
                (dp_l_k 0d0)
                (dp_h_k 0d0) )
            ;; take care subnormal number
            (when (< ix #x00100000)
              (setq ax (* ax two53))
              (decf n 53)
              (setq ix (decode-float64 ax)) )
            (incf n (- (ash ix -20) #x3ff))
            (setq j (logand ix #x000fffff))

            ;; determine interval
            (setq ix (logior j #x3ff00000)) ; normalize ix

            (cond
              ((<= j #x3988E)
                (setq k 0)
                (setq dp_l_k dp_l_0)
                (setq dp_h_k dp_h_0)
                (setq bp_k   bp_0) )  ; |x|<sqrt(3/2)
              ((< j #xBB67A)
                (setq k 1)
                (setq dp_l_k dp_l_1)
                (setq dp_h_k dp_h_1)
                (setq bp_k   bp_1) )    ; |x|<sqrt(3)
              (t
                (setq k 0)
                (setq dp_l_k dp_l_0)
                (setq dp_h_k dp_h_0)
                (setq bp_k   bp_0)
                (incf n)
                (decf ix #x00100000) ))

            (setq ax (encode-float64 ix lx))

            ;; compute s = s_h+s_l = (x-1)/(x+1) or (x-1.5)/(x+1.5)
            (let* (
              (u (- ax bp_k))    ; bp[0]=1.0, bp[1]=1.5
              (v (/ one (+ ax bp_k)))
              (s (* u v))
              (s_h (encode-float64 (decode-float64 s) 0))
         ;; t_h=ax+bp[k] High
              (t_h (encode-float64
                        (+ (logior (ash ix -1) #x20000000)
                           #x00080000
                           (ash k 18) )
                        0 ) )
              (t_l (- ax (- t_h bp_k)))
              (s_l (* v (- (- u (* s_h t_h)) (* s_h t_l))))
         ;; compute log(ax)
              (s2 (* s s))
              (r  (+ (* (* S2 S2) 
                        (+ L1 (* S2
                        (+ L2 (* S2
                        (+ L3 (* S2
                        (+ L4 (* S2
                        (+ L5 (* S2 L6)))))))))))
                    (* s_l (+ s_h s)) ) )
              (s2 (* s_h s_h))
              (t_h (encode-float64 (decode-float64 (+ 3 s2 r)) 0))
              (t_l (- r (- (- t_h 3.0d0) s2)))
         ;; u+v = s*(1+...)
              (u (* s_h t_h))
              (v (+ (* s_l t_h) (* t_l s)))
         ;; 2/(3log2)*(s+...)
              (p_h (encode-float64 (decode-float64 (+ u v)) 0))
              (p_l (- v (- p_h u)))
              (z_h (* cp_h p_h)) ;   cp_h+cp_l = 2/(3*log2)
              (z_l (+ (+ (* cp_l p_h) (* p_l cp)) dp_l_k))
         ;; log2(ax) = (s+..)*2/(3*log2) = n + dp_h + z_h + z_l
              (tt (float n 0d0)) )
            (setq t1  (encode-float64
                        (decode-float64 (+ z_h z_l dp_h_k tt))
                        0 ))
            (setq t2  (- z_l (- (- (- t1 tt) dp_h_k) z_h))) ) ))

        (let* ((s    ; s (sign of result -ve**odd) = -1 else = 1
                (if (eql (logior (1+ (ash hx -31)) (1- yisint)) 0)
                    (- one) ; (-ve)**(odd int)
                  one ) )
            ;; split up y into y1+y2 and compute (y1+y2)*(t1+t2)
            (y1 (encode-float64 (decode-float64 y) 0))
            (p_l (+ (* (- y y1) t1) (* y t2)))
            (p_h (* y1 t1))
            (z (+ p_l p_h)) )

        (multiple-value-bind (j i) (decode-float64 z)

        (cond
          ((>= j #x40900000)    ; z >= 1024
            (cond
              ((not (eql (logior (- j #x40900000) i) 0))    ; if z > 1024
                (return (* s huge huge)) )      ; overflow
              ((> (+ p_l ovt) (- z p_h))
                (return  (* s huge huge)) )) )  ; overflow
          ((>= (logand j #x7fffffff) #x4090cc00)            ; z <= -1075
            (cond
              ((not (eql (logior (- j #xc090cc00) i) 0))    ; z < -1075
                (return (* s tiny tiny)) )      ; underflow
              ((<= p_l (- z p_h))
                (return (* s tiny tiny)) )) ))  ; underflow

     ;;
     ;; compute 2**(p_h+p_l)
     ;;
        (let* ((i (logand j #x7fffffff))
               (k (- (ash i -20) #x3ff))
               (n 0) )

          (when (> i #x3fe00000)    ; if |z| > 0.5, set n = [z+0.5]
            (setq n  (+ j (ash #x00100000 (- (1+ k)))))
            (setq k  (- (ash (logand n #x7fffffff) -20) #x3ff)) ; new k for n
            (let ((tt (encode-float64
                        (logand n (lognot (ash #x000fffff (- k)))) 0 ) ))
              (setq n  (ash (logior (logand n #x000fffff) #x00100000)
                            (- k 20)))
              (when (< j 0) (setq n (- n)))
              (decf p_h tt) ))

          (let* ((tt (encode-float64 (decode-float64 (+ p_l p_h)) 0))
                 (u (* tt lg2_h))
                 (v (+ (* (- p_l (- tt p_h)) lg2) (* tt lg2_l)))
                 (z (+ u v))
                 (w (- v (- z u)))
                 (t2 (* z z))
                 (t1 (- z (* t2 (+ p1
                          (* t2 (+ p2
                          (* t2 (+ p3
                          (* t2 (+ p4
                          (* t2 p5)))))))))) )
                 (r  (- (/ (* z t1) (- t1 two)) (+ w (* z w))))
                 (z  (- one (- r z)))
                 (j  (+ (decode-float64 z) (ash n 20)))
                 (z2
                   (if (<= (ash j -20) 0)
                       (scale-float64 z n)  ; subnormal output
                     (encode-float64 j (nth-value 1 (decode-float64 z))) ) ))
            (return (* s z2)) ) ) ) ) ) ) ) ) ) ) ) ) )
