;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 - Number - float32- rem pi/2
;;; arch/generic/lisp/float32/gen-floa32-rem-pio2.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/generic/lisp/libm/float32/gen-float32-krem-pio2.lisp#1 $
;;;
;;; Description:
;;;  This file contains implementation of following macros:
;;;     float32-rem-pio2
;
(in-package :si)

#|
 *
 From fdlibm (http://www.netlib.org/fdlibm/)
 See http://sources.redhat.com/newlib/

/* kf_rem_pio2.c -- float version of k_rem_pio2.c
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

/* In the float version, the input parameter x contains 8 bit
   integers, not 24 bit integers.  113 bit precision is not supported.  */

#ifdef __STDC__
static const int init_jk[] = {4,7,9}; /* initial value for jk */
#else
static int init_jk[] = {4,7,9};
#endif

#ifdef __STDC__
static const float PIo2[] = {
#else
static float PIo2[] = {
#endif
  1.5703125000e+00, /* #x3fc90000 */
  4.5776367188e-04, /* #x39f00000 */
  2.5987625122e-05, /* #x37da0000 */
  7.5437128544e-08, /* #x33a20000 */
  6.0026650317e-11, /* #x2e840000 */
  7.3896444519e-13, /* #x2b500000 */
  5.3845816694e-15, /* #x27c20000 */
  5.6378512969e-18, /* #x22d00000 */
  8.3009228831e-20, /* #x1fc40000 */
  3.2756352257e-22, /* #x1bc60000 */
  6.3331015649e-25, /* #x17440000 */
};
|#
;; nx=3
;; prec=2
(defun float32-krem-pio2 (tx0 tx1 tx2 e0)
    (declare (values (integer 0 7) single-float single-float))
    (declare (type single-float tx0 tx1 tx2))
    (declare (type (signed-byte 11) e0))
  (prog* (
    (one    1f0)
    (two8   #+nil 2.5600000000e+02 #.(encode-float32 #x43800000))
    (twon8  #+nil 3.9062500000e-03 #.(encode-float32 #x3b800000))
    ;; initialize jk
    (jk  9) ;; init_jk[prec]={4,7,9} prec=2
    (jp jk)
    ;; determine jx,jv,q0, note that 3>q0
    (nx (cond ((not (eql tx2 0f0)) 3) ((not (eql tx1 0f0)) 2) (t 1)))
    (jx (- nx 1))
    (jv (max (truncate (- e0 3) 8) 0))
    (q0 (- e0 (* 8 (+ jv 1))))
    ;; set up f[0] to f[jx+jk] where f[jx+jk] = ipio2[jv+jk]
    (j  (- jv jx))
    (m  (+ jx jk))
    (f
      (let ((f (make-array 20 :element-type 'single-float)))
          (declare (dynamic-extent f))
        (if (< j 0)
            (dotimes (i m) (setf (elt f i) 0f0))
          (let ((xx (elt +pio2-table+ j)))
            (dotimes (i m) (setf (elt f i) xx)) ))
        f ) )
    ;; compute q[0],q[1],...q[jk]
    (q
      (let ((q (make-array 20 :element-type 'single-float)))
          (declare (dynamic-extent q))
        (dotimes (i jk)
          (let ((fw 0f0))
            (dotimes (j jx)
            for(j=0,fw=0.0 j<=jx j++)
            {
                fw += x[j]*f[jx+i-j]
                q[i] = fw 
            }
        }

        jz = jk;
recompute:
    ;; distill q[] into iq[] reversingly
        for(i=0,j=jz,z=q[jz];j>0;i++,j--) {
            fw    =  (float)((__int32_t)(twon8* z));
            iq[i] =  (__int32_t)(z-two8*fw);
            z     =  q[j-1]+fw;
        }

    ;; compute n
        z  = scalbnf(z,(int)q0);        ;; actual value of z
        z -= (float)8.0*floorf(z*(float)0.125);        ;; trim off integer >= 8
        n  = (__int32_t) z;
        z -= (float)n;
        ih = 0;
        if(q0>0) {        ;; need iq[jz-1] to determine n
            i  = (iq[jz-1]>>(8-q0)); n += i;
            iq[jz-1] -= i<<(8-q0);
            ih = iq[jz-1]>>(7-q0);
        }
        else if(q0==0) ih = iq[jz-1]>>8;
        else if(z>=(float)0.5) ih=2;

        if(ih>0) {        ;; q > 0.5
            n += 1; carry = 0;
            for(i=0;i<jz ;i++) {        ;; compute 1-q
                j = iq[i];
                if(carry==0) {
                    if(j!=0) {
                        carry = 1; iq[i] = #x100- j;
                    }
                } else  iq[i] = #xff - j;
            }
            if(q0>0) {                ;; rare case: chance is 1 in 12
                switch(q0) {
                case 1:
                       iq[jz-1] &= #x7f; break;
                    case 2:
                       iq[jz-1] &= #x3f; break;
                }
            }
            if(ih==2) {
                z = one - z;
                if(carry!=0) z -= scalbnf(one,(int)q0);
            }
        }

    ;; check if recomputation is needed
        if(z==zero) {
            j = 0;
            for (i=jz-1;i>=jk;i--) j |= iq[i];
            if(j==0) { ;; need recomputation
                for(k=1;iq[jk-k]==0;k++);   ;; k = no. of terms needed

                for(i=jz+1;i<=jz+k;i++) {   ;; add q[jz+1] to q[jz+k]
                    f[jx+i] = (float) ipio2[jv+i];
                    for(j=0,fw=0.0;j<=jx;j++) fw += x[j]*f[jx+i-j];
                    q[i] = fw;
                }
                jz += k;
                goto recompute;
            }
        }

    ;; chop off zero terms
        if(z==(float)0.0) {
            jz -= 1; q0 -= 8;
            while(iq[jz]==0) { jz--; q0-=8;}
        } else { ;; break z into 8-bit if necessary
            z = scalbnf(z,-(int)q0);
            if(z>=two8) {
                fw = (float)((__int32_t)(twon8*z));
                iq[jz] = (__int32_t)(z-two8*fw);
                jz += 1; q0 += 8;
                iq[jz] = (__int32_t) fw;
            } else iq[jz] = (__int32_t) z ;
        }

    ;; convert integer "bit" chunk to floating-point value
        fw = scalbnf(one,(int)q0);
        for(i=jz;i>=0;i--) {
            q[i] = fw*(float)iq[i]; fw*=twon8;
        }

    ;; compute PIo2[0,...,jp]*q[jz,...,0]
        for(i=jz;i>=0;i--) {
            for(fw=0.0,k=0;k<=jp&&k<=jz-i;k++) fw += PIo2[k]*q[i+k];
            fq[jz-i] = fw;
        }

    ;; compress fq[] into y[]
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

            case 3:        ;; painful
                for (i=jz;i>0;i--)
                {
                    fw      = fq[i-1]+fq[i];
                    fq[i]  += fq[i-1]-fw;
                    fq[i-1] = fw;
                }

                for (i=jz;i>1;i--)
                {
                    fw      = fq[i-1]+fq[i];
                    fq[i]  += fq[i-1]-fw;
                    fq[i-1] = fw;
                }

                for (fw=0.0,i=jz;i>=2;i--)
                {
                    fw += fq[i];
                }

                if(ih==0)
                {
                    y[0] =  fq[0];
                    y[1] =  fq[1];
                    y[2] =  fw;
                }
                else
                {
                    y[0] = -fq[0];
                    y[1] = -fq[1];
                    y[2] = -fw;
                }
        }
        return n&7;
}

