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
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float32/gen-float32-expt.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     float64-expt
;
(in-package :si)

#|
 See fdlibm (http://www.netlib.org/fdlibm/)
 See http://sources.redhat.com/newlib/

/* ef_pow.c -- float version of e_pow.c.
 * Conversion to float by Ian Lance Taylor, Cygnus Support, ian@cygnus.com.
 */

/*
 * ====================================================
 * Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
 *
 * Developed at SunPro, a Sun Microsystems, Inc. business.
 * Permission to use, copy, modify, and distribute this
 * software is freely granted, provided that this notice
 * is preserved.
 * ====================================================
 */
|#
(defun float32-expt (x y)
    (declare (values single-float))
    (declare (type single-float x y))
  (prog* (
    (bp_0       1.0f0)
    (bp_1       1.5f0)
    (dp_h_0     0.0f0)
    (dp_h_1     #+nil 5.84960938e-01 #.(encode-float32 #x3f15c000))
    (dp_l_0     0.0f0)
    (dp_l_1     #+nil 1.56322085e-06 #.(encode-float32 #x35d1cfdc))
    (zero       0.0f0)
    (one        1.0f0)
    (two        2.0f0)
    (two24      #+nil 16777216.0    #.(encode-float32 #x4b800000))
    (huge       1.0f30)
    (tiny       1.0f-30)
    ;; poly coefs for (3/2)*(log(x)-2s-2/3*s**3)
    (L1     #+nil 6.0000002384e-01  #.(encode-float32 #x3f19999a))
    (L2     #+nil 4.2857143283e-01  #.(encode-float32 #x3edb6db7))
    (L3     #+nil 3.3333334327e-01  #.(encode-float32 #x3eaaaaab))
    (L4     #+nil 2.7272811532e-01  #.(encode-float32 #x3e8ba305))
    (L5     #+nil 2.3066075146e-01  #.(encode-float32 #x3e6c3255))
    (L6     #+nil 2.0697501302e-01  #.(encode-float32 #x3e53f142))
    (P1     #+nil 1.6666667163e-01  #.(encode-float32 #x3e2aaaab))
    (P2     #+nil -2.7777778450e-03 #.(encode-float32 #xbb360b61))
    (P3     #+nil 6.6137559770e-05  #.(encode-float32 #x388ab355))
    (P4     #+nil -1.6533901999e-06 #.(encode-float32 #xb5ddea0e))
    (P5     #+nil 4.1381369442e-08  #.(encode-float32 #x3331bb4c))
    (lg2    #+nil 6.9314718246e-01  #.(encode-float32 #x3f317218))
    (lg2_h  #+nil 6.93145752e-01    #.(encode-float32 #x3f317200))
    (lg2_l  #+nil 1.42860654e-06    #.(encode-float32 #x35bfbe8c))
    ;; -(128-log2(ovfl+.5ulp))
    (ovt    #+nil 4.2995665694e-08  #.(encode-float32 #x03338AA3B))
    ;;  =2/(3ln2)
    (cp     #+nil 9.6179670095e-01  #.(encode-float32 #x3f76384f))
     ;; head of cp
    (cp_h   #+nil 9.6179199219e-01  #.(encode-float32 #x3f763800))
    ;; tail of cp_h
    (cp_l    #+nil 4.7017383622e-06  #.(encode-float32 #x369dc3a0))
    ;;  1/ln2
    (ivln2   #+nil 1.4426950216e+00 #.(encode-float32 #x3fb8aa3b))
    ;; 16b 1/ln2
    (ivln2_h #+nil 1.4426879883e+00 #.(encode-float32 #x3fb8aa00))
    ;;  1/ln2 tail
    (ivln2_l #+nil 7.0526075433e-06 #.(encode-float32 #x36eca570))
    ;;
    (hx (decode-float32 x))
    (ix (logand hx #x7fffffff))
    (hy (decode-float32 y))
    (iy (logand hy #x7fffffff))
    (t1 0f0)
    (t2 0f0)
    )
    ;; y==zero: x**0 = 1
    (when (eql iy 0) (return one))

    ;; +-NaN return x+y
    (when (or (> ix #x7F800000) (> iy #x7F800000))
      (return (+ x y)) )

    ;; special value of y
    (cond
      ((eql iy #x7F800000)
        ;; y is +-inf
        (cond
          ((eql ix #x3f800000)
           (return (- y y)) )   ; inf**+-1 is NaN
          ((> ix #x3f800000)    ; (|x|>1)**+-inf = inf,0
            (return (if (>= hy 0) y zero)) )
          (t                    ; (|x|<1)**-,+inf = inf,0
            (return (if (< hy 0) (- y) zero)) )))

      ((eql iy #x3f800000)
        ;; y is  +-1
        (return (if (< hy 0) (/ one x) x)) )
      ((eql hy #x40000000)
        (return (* x x)) )      ; y is  2
      ((and (eql hy #x3f000000)) ; y is  0.5
        (when (>= hx 0) (return (float32-sqrt x))) ))

    ;; determine if y is an odd int when x < 0
    ;; yisint = 0       ... y is not an integer
    ;; yisint = 1       ... y is an odd int
    ;; yisint = 2       ... y is an even int
    (let ((yisint
            (cond
              ((>= hx 0) 0)
              ((>= iy #x4b800000) 2)    ; even integer y
              ((>= iy #x3f800000)
                (let* ((k (- (ash iy 23) #x7f)) ; exponent
                       (j (ash iy (- 23 k))) )
                  (if (eql (ash j (- 23 k)) iy)
                      (- 2 (logand j 1))
                    0 ) ) )
              (t 0) ) )
          (ax (encode-float32 ix)) )

      ;; special value of x
      (when (or (eql ix #x7f800000)     ; +-inf?
                (eql ix 0)              ; +-zero?
                (eql ix #x3f800000) )   ; +-one?
        (let ((z (if (< hy 0) (/ one ax) ax)))
          (cond
            ((>= hx 0)
              (return z) )
            ((eql (logior (- ix #x3f800000) yisint) 0)
                ;; (-1)**non-int is NaN
                (return (/ (- z z) (- z z))) )
            ((eql yisint 1)
              ;; (x<0)**odd = -(|x|**odd)
              (return (- z)) )
            (t
              (return z) )) ))

      ;; (x<0)**(non-int) is NaN
      (when (and (< hx 0) (not (eql yisint 0)))
        (return (/ (- x x) (- x x))) ) 

      ;; |y| is huge
      (if (> iy #x4d000000)
        (progn ; if |y| > 2**27
          ;; over/underflow if x is not close to one
          (when (< ix #x3f7ffff8)
            (return (if (< hy 0) (* huge huge) (* tiny tiny))) )
          (when (> ix #x3f800007)
            (return (if (> hy 0)(* huge huge) (* tiny tiny))) )

          ;; now |1-x| is tiny <= 2**-20, suffice to compute
          ;; log(x) by x-x^2/2+x^3/3-x^4/4
          (let* (
            (tt (- ax 1))   ; t has 20 trailing zeros
            (w  (* (* tt tt)
                (- 0.5f0 (* tt (- 0.333333333333f0 (* tt 0.25f0))))) )
            (u  (* ivln2_h tt)) ; ivln2_h has 16 sig. bits
            (v  (- (* tt ivln2_l) (* w ivln2)))
            )
            (setq t1 (encode-float32
                    (logand (decode-float32 (+ u v)) #xfffff000) ))
            (setq t2 (- v (- t1 u))) ))
        (let (
          (bp_k 0f0)
          (dp_h_k 0f0)
          (dp_l_k 0f0)
          (j 0)
          (k 0)
          (n 0)
          )
        ;; take care subnormal number
        (when (<= ix #x007FFFFF)
          (setq ax (* ax two24))
          (decf n 24)
          (setq ix (decode-float32 ax)) )

        (incf n  (- (ash ix -23) #x7f))
        (setq j (logand ix ix #x007fffff))

        ;; determine interval
        (setq ix (logior j #x3f800000)) ;; normalize ix

        (cond
          ((<= j #x1cc471)
            (setq bp_k   bp_0)
            (setq dp_h_k dp_h_0)
            (setq dp_l_k dp_l_0)
            (setq k 0) )    ; |x|<sqrt(3/2)
          ((< j #x5db3d7)
            (setq bp_k   bp_1)
            (setq dp_h_k dp_h_1)
            (setq dp_l_k dp_l_1)
            (setq k 1) )    ; |x|<sqrt(3)
          (t
            (setq bp_k   bp_0)
            (setq dp_h_k dp_h_0)
            (setq dp_l_k dp_l_0)
            (setq k 0)
            (incf n)
            (decf ix #x00800000) ))

        (setq ax (encode-float32 ix))

    ;; compute s = s_h+s_l = (x-1)/(x+1) or (x-1.5)/(x+1.5)
        (let* (
          (u   (- ax bp_k))   ; bp[0]=1.0, bp[1]=1.5
          (v   (/ one (+ ax bp_k)))
          (s   (* u v))
          (s_h (encode-float32 (logand (decode-float32 s) #xfffff000)))

    ;; t_h=ax+bp[k] High
          (t_h (encode-float32
                    (+ (logior (ash ix -1) #x20000000)
                       #x0040000
                       (ash k 21) )) )
          (t_l (- ax (- t_h bp_k)))
          (s_l (* v (- (- u (* s_h t_h)) (* s_h t_l))))

    ;; compute log(ax)
          (s2 (* s s))
          (r  (+ (* (* s2 s2) 
                    (+ l1 (* s2
                    (+ l2 (* s2
                    (+ l3 (* s2
                    (+ l4 (* s2
                    (+ l5 (* s2 l6)))))))))))
                (* s_l (+ s_h s)) ) )
          (s2 (* s_h s_h))
          (t_h (encode-float32
                    (logand (decode-float32 (+ 3 s2 r)) #xfffff000) ) )
          (t_l (- r (- (- t_h 3.0f0) s2)))

    ;; u+v = s*(1+...)
          (u (* s_h t_h))
          (v (+ (* s_l t_h) (* t_l s)))
    ;; 2/(3log2)*(s+...)
          (p_h (encode-float32
                    (logand (decode-float32 (+ u v)) #xfffff000)) )
          (p_l (- v (- p_h u)))
          (z_h (* cp_h p_h))    ; cp_h+cp_l = 2/(3*log2)
          (z_l (+ (+ (* cp_l p_h) (* p_l cp)) dp_l_k))

    ;; log2(ax) = (s+..)*2/(3*log2) = n + dp_h + z_h + z_l
          (tt  (float n))
          )
       (setq t1 (encode-float32
                    (logand (decode-float32 (+ z_h z_l dp_h_k tt))
                            #xfffff000 )))
       (setq t2 (- z_l (- (- (- t1 tt) dp_h_k) z_h))) ) ))

    (let* (
      (s    ; s (sign of result -ve**odd) = -1 else = 1
        (if (eql (logior (1+ (ash hx -31)) (1- yisint)) 0)
            (- one) ; (-ve)**(odd int)
          one ) )
    ;; split up y into y1+y2 and compute (y1+y2)*(t1+t2)
      (y1 (encode-float32 (logand (decode-float32 y) #xfffff000)))
    ;; split up y into y1+y2 and compute (y1+y2)*(t1+t2)
      (p_l (+ (* (- y y1) t1) (* y t2)))
      (p_h (* y1 t1))
      (z (+ p_l p_h))
      (j (decode-float32 z))
      (i (logand j #x7fffffff))
      )
      (if (> j 0)
          (cond
            ((> i #x43000000)
              ;; overflow
              (return (* s huge huge)) )
            ((eql i #x43000000)
              (when (> (+ p_l ovt) (- z p_h))
                ;; overflow
                (return (* s huge huge)) ) ))
          (cond
            ((> i #x43160000)
              ;; underflow
              (return (* s tiny tiny)) )
            ((eql i #x43160000)
              (when (<= p_l (- z p_h))
                ;; underflow
                (return (* s tiny tiny)) ) )))

    ;; compute 2**(p_h+p_l)
      (let* (
        (k  (- (ash i -23) #x7f))
        (n  0)
        )
        (when (> i #x3f000000)
          ;; if |z| > 0.5, set n = [z+0.5]
          (setq n (+ j (ash #x00800000 (- (+ k 1)))))
          (setq k (- (ash (logand n #x7fffffff) -23) #x7f))    ; new k for n
          (let ((tt (encode-float32
                        (logand n (lognot (ash #x007fffff (- k)))) ) ))
            (setq n (ash (logior (logand n #x007fffff) #x00800000)
                         (- (- 23 k)) ))
            (when (< j 0) (setq n (- n)))
            (decf p_h tt) ))
        (let* (
            (tt (encode-float32
                    (logand (decode-float32 (+ p_l p_h)) #xfffff000) ))
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
            (j  (+ (decode-float32 z) (ash n 23)))
            (z2
              (if (<= (ash j -23) 0)
                  (scale-float32 z n)  ; subnormal output
                (encode-float32 j) ) )
            )
          (return (* s z2)) ) ) ) ) ) )
