;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - Kernel rem pi/2
;;; arch/generic/lisp/math/gen-math-f64-krem-pio2.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float64/gen-float64-krem-pio2.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     float64-krem-pio2
;
(in-package :si)

#|
/* @(#)k_rem_pio2.c 5.1 93/09/24
/*
 * ====================================================
 * Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
 *
 * Developed at SunPro, a Sun Microsystems, Inc. business.
 * Permission to use, copy, modify, and distribute this
 * software is freely granted, provided that this notice
 * is preserved.
 * ====================================================


/*
 * __kernel_rem_pio2(x,y,e0,nx,prec,ipio2)
 * double x[],y[]; int e0,nx,prec; int ipio2[];
 *
 * __kernel_rem_pio2 return the last three digits of N with
 *              y = x - N*pi/2
 * so that |y| < pi/2.
 *
 * The method is to compute the integer (mod 8) and fraction parts of
 * (2/pi)*x without doing the full multiplication. In general we
 * skip the part of the product that are known to be a huge integer (
 * more accurately, = 0 mod 8 ). Thus the number of operations are
 * independent of the exponent of the input.
 *
 * (2/pi) is represented by an array of 24-bit integers in ipio2[].
 *
 * Input parameters:
 *      x[]        The input value (must be positive) is broken into nx
 *              pieces of 24-bit integers in double precision format.
 *              x[i] will be the i-th 24 bit of x. The scaled exponent
 *              of x[0] is given in input parameter e0 (i.e., x[0]*2^e0
 *              match x's up to 24 bits.
 *
 *              Example of breaking a double positive z into x[0]+x[1]+x[2]:
 *                      e0 = ilogb(z)-23
 *                      z  = scalbn(z,-e0)
 *              for i = 0,1,2
 *                      x[i] = floor(z)
 *                      z    = (z-x[i])*2**24
 *
 *
 *      y[]        ouput result in an array of double precision numbers.
 *              The dimension of y[] is:
 *                      24-bit  precision        1
 *                      53-bit  precision        2
 *                      64-bit  precision        2
 *                      113-bit precision        3
 *              The actual value is the sum of them. Thus for 113-bit
 *              precison, one may have to do something like:
 *
 *              long double t,w,r_head, r_tail;
 *              t = (long double)y[2] + (long double)y[1];
 *              w = (long double)y[0];
 *              r_head = t+w;
 *              r_tail = w - (r_head - t);
 *
 *      e0        The exponent of x[0]
 *
 *      nx        dimension of x[]
 *
 *      prec        an integer indicating the precision:
 *                      0        24  bits (single)
 *                      1        53  bits (double)
 *                      2        64  bits (extended)
 *                      3        113 bits (quad)
 *
 *      ipio2[]
 *              integer array, contains the (24*i)-th to (24*i+23)-th
 *              bit of 2/pi after binary point. The corresponding
 *              floating value is
 *
 *                      ipio2[i] * 2^(-24(i+1)).
 *
 * External function:
 *      double scalbn(), floor();
 *
 *
 * Here is the description of some local variables:
 *
 *      jk        jk+1 is the initial number of terms of ipio2[] needed
 *              in the computation. The recommended value is 2,3,4,
 *              6 for single, double, extended,and quad.
 *
 *      jz        local integer variable indicating the number of
 *              terms of ipio2[] used.
 *
 *      jx        nx - 1
 *
 *      jv        index for pointing to the suitable ipio2[] for the
 *              computation. In general, we want
 *                      ( 2^e0*x[0] * ipio2[jv-1]*2^(-24jv) )/8
 *              is an integer. Thus
 *                      e0-3-24*jv >= 0 or (e0-3)/24 >= jv
 *              Hence jv = max(0,(e0-3)/24).
 *
 *      jp        jp+1 is the number of terms in PIo2[] needed, jp = jk.
 *
 *      q[]        double array with integral value, representing the
 *              24-bits chunk of the product of x and 2/pi.
 *
 *      q0        the corresponding exponent of q[0]. Note that the
 *              exponent for q[i] would be q0-24*i.
 *
 *      PIo2[]        double precision array, obtained by cutting pi/2
 *              into 24 bits chunks.
 *
 *      f[]        ipio2[] in floating point
 *
 *      iq[]        integer array by breaking up q[] in 24-bits chunk.
 *
 *      fq[]        final product of x*(2/pi) in fq[0],..,fq[jk]
 *
 *      ih        integer. If >0 it indicates q[] is >= 0.5, hence
 *              it also indicates the *sign* of the result.
 *
|#


;;; Constants:
;;; The hexadecimal values are the intended ones for the following
;;; constants. The decimal values may be used, provided that the
;;; compiler will convert from decimal to binary accurately enough
;;; to produce the hexadecimal values shown.

#ifdef __STDC__
static const double PIo2[] = {
#else
static double PIo2[] = {
#endif
  1.57079625129699707031e+00, /* #x3FF921FB, #x40000000
  7.54978941586159635335e-08, /* #x3E74442D, #x00000000
  5.39030252995776476554e-15, /* #x3CF84698, #x80000000
  3.28200341580791294123e-22, /* #x3B78CC51, #x60000000
  1.27065575308067607349e-29, /* #x39F01B83, #x80000000
  1.22933308981111328932e-36, /* #x387A2520, #x40000000
  2.73370053816464559624e-44, /* #x36E38222, #x80000000
  2.16741683877804819444e-51, /* #x3569F31D, #x00000000
};

(defun float64-kernel-rem-pio2 (x e0 nx prec ipio2)
    (declare (values (member -1 0 1) double-float double-float))
    (declare (type (simple-array double-float (*)) x))
    (declare (type fixnum e0 nx))
    (declare (type (integer 0 3) prec))
  (prog* (
    (zero   0.0d0)
    (one    1.0d0)
    (two24  #+nil 1.67772160000000000000e+07
            #.(encode-float64 #x41700000 #x00000000) )
    (twon24 #+nil 5.96046447753906250000e-08
            #.(encode-float64 #x3E700000, #x00000000) )
    ;; initialize jk
    (jk (svref #(2 3 4 6) prec))
    (jp jk)
    ;; determine jx,jv,q0, note that 3>q0
    (jx (- nx 1))
    (jv (truncate (- e0 3) 24)) ; if(jv<0) jv=0;
    (q0 (- e0 (* 24 (+ jv 1))))
    ;; set up f[0] to f[jx+jk] where f[jx+jk] = ipio2[jv+jk]
    (f
      (loop
        with f = (make-array 20 :element-type 'double-float)
        for i to (+ jx jk)
        for j = (- jv jx) then (1+ j) do
        (setq (elt f i)
              (if (minusp j) zero (float (elt ipio2 j) 0d0)) )
        finally (return f) ) )
    ;;compute q[0],q[1],...q[jk]
    (q
      (loop
        with q = (make-array 20 :element-type 'double-float)
        for i to jk do
          (loop
            with fw of-type double-float = 0d0
            for j to jx do
              (incf fw (* (elt x j) (elt f (- (+ jx i) j))))
              (setf (elt q i) fw) )
        finally (return q) ) )
    (jz jk)
    (iq (make-array 20 :element-type '(signed-byte 32)))
    (z (elt q jz))
    (n 0)
    )
  recompute
    ;; distill q[] into iq[] reversingly
    (loop
      for i = 0 then (1+ i)
      for j = jz then (1- j)
      while (> j 0) do
        (let ((fw  (float (truncate (* twon24 z)) 0d0)))
          (setf (elt iq i) (truncate (- z (* two24 fw))))
          (setq z (+ (elt q (1- j)) fw)) ))

    ;; compute n
    (setq z  (float64-scale z q0)           ; actual value of z
    (decf z  (* 8.0d0 (floor (* z 0.125d0)  ; trim off integer >= 8
    (setq n (truncate z))
    (decf n (float n 0d0))
    (let ((ih
            (cond
              ((> q0 0)    ; need iq[jz-1] to determine n
                (let ((i  (ash (elt iq (- jz-1)) (- (- 24 q0)))))
                  (incf n i)
                  (decf (elt iq (- jz 1)) (ash i (- 24 q0)))
                  (ash (elt iq (- jz 1)) (- (- 23 q0))) ) )
              ((eql q0 0) (ash (elt iq (- jz 1)) -23) )
              ((>= z 0.5d0) 2)
              (t 0) ) ))
      (when (> ih 0)    ; q > 0.5
        (incf n)
        (let ((carry 0))
          ;; compute 1-q
          (loop for i below jz do
            (let ((j (elt iq i)))
              (cond
                ((not (eql carry 0))
                  (setf (elt iq i) (- #xffffff j)) )
                ((not (eql j 0))
                  (setq carry 1)
                  (setf (elt iq i) (- #x1000000 j)) )) ))

            ;; rare case: chance is 1 in 12
            (when (> q0 0)
              (case q0
                (1 (setf (elt iq (- jz 1))
                        (logand (elt iq (- jz 1)) #x7fffff) ) )
                (2 (setf (elt iq (- jz 1))
                        (logand (elt iq (- jz 1)) #x3fffff) ) )))
                    case 2:
                       iq[jz-1] &= #x3fffff; break;

            (when (eql ih 2)
              (setq z (- one z))
              (unless (eql carry 0)
                (decf z (float64-scale one (truncate q0))) )) ))

    /* check if recomputation is needed
        if(z==zero) {
            j = 0;
            for (i=jz-1;i>=jk;i--) j |= iq[i];
            if(j==0) { /* need recomputation
                for(k=1;iq[jk-k]==0;k++);   /* k = no. of terms needed

                for(i=jz+1;i<=jz+k;i++) {   /* add q[jz+1] to q[jz+k]
                    f[jx+i] = (double) ipio2[jv+i];
                    for(j=0,fw=0.0;j<=jx;j++) fw += x[j]*f[jx+i-j];
                    q[i] = fw;
                }
                jz += k;
                goto recompute;
            }
        }

    /* chop off zero terms
        if(z==0.0) {
            jz -= 1; q0 -= 24;
            while(iq[jz]==0) { jz--; q0-=24;}
        } else { /* break z into 24-bit if necessary
            z = scalbn(z,-(int)q0);
            if(z>=two24) {
                fw = (double)((__int32_t)(twon24*z));
                iq[jz] = (__int32_t)(z-two24*fw);
                jz += 1; q0 += 24;
                iq[jz] = (__int32_t) fw;
            } else iq[jz] = (__int32_t) z ;
        }

    /* convert integer "bit" chunk to floating-point value
        fw = scalbn(one,(int)q0);
        for(i=jz;i>=0;i--) {
            q[i] = fw*(double)iq[i]; fw*=twon24;
        }

    /* compute PIo2[0,...,jp]*q[jz,...,0]
        for(i=jz;i>=0;i--) {
            for(fw=0.0,k=0;k<=jp&&k<=jz-i;k++) fw += PIo2[k]*q[i+k];
            fq[jz-i] = fw;
        }

    /* compress fq[] into y[]
        switch(prec) {
            case 0:
                fw = 0.0;
                for (i=jz;i>=0;i--) fw += fq[i];
                y[0] = (ih==0)? fw: -fw;
                break;
            case 1:
            case 2:
                fw = 0.0;
                for (i=jz;i>=0;i--) fw += fq[i];
                y[0] = (ih==0)? fw: -fw;
                fw = fq[0]-fw;
                for (i=1;i<=jz;i++) fw += fq[i];
                y[1] = (ih==0)? fw: -fw;
                break;
            case 3:        /* painful
                for (i=jz;i>0;i--) {
                    fw      = fq[i-1]+fq[i];
                    fq[i]  += fq[i-1]-fw;
                    fq[i-1] = fw;
                }
                for (i=jz;i>1;i--) {
                    fw      = fq[i-1]+fq[i];
                    fq[i]  += fq[i-1]-fw;
                    fq[i-1] = fw;
                }
                for (fw=0.0,i=jz;i>=2;i--) fw += fq[i];
                if(ih==0) {
                    y[0] =  fq[0]; y[1] =  fq[1]; y[2] =  fw;
                } else {
                    y[0] = -fq[0]; y[1] = -fq[1]; y[2] = -fw;
                }
        }
        return n&7;
}

