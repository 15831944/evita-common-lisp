;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - 12 Numbers
;;; lisp/runtime/r12-number.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime3/r12-integer.lisp#3 $
;;;
;
(in-package :si)

;;;; 12.2.60 boole
(defun cl:boole (op integer-1 integer-2)
  (ecase op
    (boole-clr          0)
    (boole-set          1)
    (boole-1            integer-1)
    (boole-2            integer-2)
    (boole-c1           (lognot   integer-1))
    (boole-c2           (lognot   integer-2))
    (boole-and          (logand   integer-1 integer-2))
    (boole-ior          (logior   integer-1 integer-2))
    (boole-xor          (logxor   integer-1 integer-2))
    (boole-eqv          (logeqv   integer-1 integer-2))
    (boole-nand         (lognand  integer-1 integer-2))
    (boole-nor          (lognor   integer-1 integer-2))
    (boole-andc1        (logandc1 integer-1 integer-2))
    (boole-andc2        (logandc2 integer-1 integer-2))
    (boole-orc1         (logorc1  integer-1 integer-2))
    (boole-orc2         (logorc2  integer-1 integer-2)) ) )


;;;; iexpt
(defun iexpt (base-number power-number)
    (declare (type number base-number))
    (declare (type (integer 0 *) power-number))
  (let ((result 1))
    (loop
      (cond
        ((= 0 power-number)
          (return) )
        ((logbitp 0 power-number)
          (setq result (* result base-number))
          (decf power-number) )
        (t
          (setq base-number (* base-number base-number))
          (setq power-number (ash power-number -1)) )) )
     result ) )


;;;; 12.2.38 isqrt
;;;
;;; Syntax:
;;;     isqrt natual => natual-root
;;;
;;; Description:
;;;  Using newton method by boyland@aspen.Berkeley.EDU.
;;;
;;; See: http://www-2.cs.cmu.edu/afs/cs/project/ai-repository/ai/lang/lisp/code/math/isqrt/0.html
;;;
;
(defun cl:isqrt (n &aux n-len-quarter n-half n-half-isqrt
                       init-value q r m iterated-value)
  (cond
   ((> n 24)            ; theoretically (> n 15) ,i.e., n-len-quarter > 0
    (setq n-len-quarter (ash (- (integer-length n) 1) -2))
    (setq n-half (ash n (- (ash n-len-quarter 1))))
    (setq n-half-isqrt (isqrt n-half))
    (setq init-value (ash n-half-isqrt n-len-quarter))
    (multiple-value-setq (q r) (floor n init-value))
    (setq iterated-value (ash (+ init-value q) -1))
    (cond ((oddp q)
           iterated-value)
          (t
           (setq m (- iterated-value init-value))
           (if (> (* m m) r)
             (1- iterated-value)
             iterated-value))))
   ((> n 15) 4)
   ((> n  8) 3)
   ((> n  3) 2)
   ((> n  0) 1)
   ((> n -1) 0)
   (t (error 'type-error :datum n :expected-type 'unsigned-byte)) ) )


;;;; 12.2.34 lcm 
(defun cl:lcm (&rest integers)
    (declare (dynamic-extent integers))
  (let ((lcm 1))
        (declare (type integer lcm))
    (dolist (integer integers lcm)
      (let ((integer integer))
         (declare (type integer integer))
        (when (minusp integer) (setq integer (- integer)))
        (setq lcm (if (or (zerop lcm) (zerop integer))
                      0
                    (truncate (* lcm integer) (gcd lcm integer)) )) ) ) ) )


;;;; 12.2.63 logcount
;;; Note:
;;;  (logcount x) = (logcount (- (+ x 1))) = (logcount (lognot x)))
;
(defun cl:logcount (integer)
  (let ((count 0))
    (if (minusp integer)
        (loop
          (when (= -1 integer) (return count))
          (unless (logbitp 0 integer)
            (incf count) )
          (setq integer (ash integer -1)) )
    (loop
      (when (zerop integer) (return count))
      (when (logbitp 0 integer)
        (incf count) )
      (setq integer (ash integer -1)) )) ) )


;;;; 12.2.65 logtest
(defun cl:logtest (integer-1 integer-2)
  (not (zerop (logand integer-1 integer-2))) )


;;;; 12.2.59 parse-integer
(defun cl:parse-integer (string &key (start 0) end (radix 10) junk-allowed)
    (declare (type string strng))
    (declare (type ext:sequence-index start))
    (declare (type ext:sequence-end end))
    (declare (type (integer 2 36) radix))
    (declare (values (or integer null) ext:sequence-index))
  (multiple-value-bind (bstring offset end) (string-data string start end)
      (declare (type simple-string bstring))
      (declare (type ext:sequence-index offset end))
    (let ((sign    1)
          (ndigits 0)
          (index   offset)
          (result  0)
          (char    #\?) )

      ;; Skip leading whitespaces
      (loop
        (when (= index end) (return))
        (setq char (schar bstring index))
        (unless (whitespace-char-p char) (return))
        (incf index) )

      ;; Sign
      (case char
        (#\+ (incf index))
        (#\- (incf index) (setq sign -1)) )

      ;; Digits
      (loop
        (when (= index end) (return))
        (setq char (schar bstring index))
        (let ((digit (digit-char-p char radix)))
          (unless digit (return))
          (incf ndigits)
          (setq result (+ (* result radix) digit))
          (incf index) ))

      ;; Skip trailing whitespaces
      (loop
        (when (= index end) (return))
        (setq char (schar bstring index))
        (unless (whitespace-char-p char) (return))
        (incf index) )

      (if (zerop ndigits)
          (setq result nil)
        (setq result (* sign result)) )

      (decf index (- offset start))
      (decf end   (- offset start))

      (cond
        (junk-allowed
          (values result index) )
        ((and result (= index end))
          (values result end) )
        (t
          (error 'integer-syntax-error
                 :string string
                 :radix radix
                 :start start
                 :end   (- end offset) ))) ) ) )
