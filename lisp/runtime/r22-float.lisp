;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; evcl - Runtime - 22 Printer - Float
;;; runtime/r22-float.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r22-float.lisp#6 $
;;;
;;; Description:
;;;  This file implements following float printer functions:
;;;     format-float-$                  ~$
;;;     format-float-E                  ~E
;;;     format-float-E-digits
;;;     format-float-F                  ~F
;;;     format-float-F-aux
;;;     format-float-F-digits
;;;     format-float-G                  ~G
;;;     format-float-G-aux
;
(in-package :si)


;;;; Float Marker
;;;
;;; Syntax:
;;;     float-marker x => marker
(defun float-marker (x)
  (let ((type (type-of x)))
    (cond
     ((eq type *read-default-float-format*) #\e)
     ((eq type 'short-float)                #\s)
     ((eq type 'single-float)               #\f)
     ((eq type 'double-float)               #\d)
     ((eq type 'long-float)                 #\l)
     (t (error "Float type ~S is undefined." type)) ) ) )


;;;; float-to-digits-old
;;; This is Dragon4(Formmatter-Feeding Process for Floating-Point Printout).
;;; Preforming Free-Format Perfect Psoitive Floating-Ponint Printout.
;;;
;;; See. Guy L. Steele Jr and Jon L. White, ``Hot to Print Floating-Point
;;; Numbers Accurately'', p.112-126, Proc. of ACM SIGPLAN '90, June 1992.
;;;
;;; b = 2, B = 10
;;;
;;; cutoff-mode == :relative => cutoff-place < 0
;;; f < b^p
;;; x = f * b^(e-p)
;;;
;;; Returns:
;;;     list-of-digits
;;;     expt
;;;
(defun float-to-digits-old (f e p cutoff-mode cutoff-place)
  (when (zerop f)
    (return-from float-to-digits-old (values '(0) 0)) )

  (let* ((round-up-p nil)
         (R (ash f (max (- e p) 0)))
         (S (ash 1 (max 0 (- p e))))
         (M- (ash 1 (max 0 (- e p))))
         (M+ M-)
         (k 0) )

    ;; Fixup
    ;;
    (when (= f (ash 1 (1- p)))
      (setq M+ (ash M+ 1))
      (setq R (ash R 1))
      (setq S (ash S 1)) )

    (let ((S/B (ceiling S 10)))
      (loop
       (unless (< R S/B) (return))
       (decf k)
       (setq R (* R 10))
       (setq M- (* M- 10))
       (setq M+ (* M+ 10)) ) )

    (loop
     (loop
      (unless (>= (+ (* 2 R) M+) (* 2 S)) (return))
      (setq S (* S 10))
      (incf k) )

     ;; Performe any necessary adjustment of M- and M+ to take into account
     ;; the formmating requirements.
     (ecase cutoff-mode
       ((:absolute))
       ((:normal)   (setq cutoff-place k))
       ((:relative) (incf cutoff-place k))
       ((:variable) (setq cutoff-place (if (minusp k)
                                       (- 1 cutoff-place)
                                       (1+ (- k cutoff-place)) ))) )

     ;; CutoffAdjust
     (when (or (eq cutoff-mode :relative) (eq cutoff-mode :absolute))
       (let ((a (- cutoff-place k))
             (y S) )
         (if (>= a 0)
           (dotimes (j a) (setq y (* y 10)))
           (dotimes (j (- a)) (setq y (ceiling y 10))) )
         ;; (asset (= y (ceiling (* S (expt 10 a)))))
         (setq M- (max y M-))
         (setq M+ (max y M+))
         (if (= M+ y) (setq round-up-p t)) ))

     (unless (>= (+ (* 2 R) M+) (* 2 S)) (return)) )

    ;; Generating.
    (let ((result '())
          (expt (1- k))
          low-p high-p U )
      (loop
       (decf k)
       (multiple-value-setq (U R) (floor (* R 10) S))
       (setq M- (* M- 10))
       (setq M+ (* M+ 10))
       (setq low-p (< (* 2 R) M-))
       (setq high-p (if round-up-p
                     (>= (* 2 R) (- (* 2 S) M+))
                     (> (* 2 R) (- (* 2 S) M+)) ))
       (unless (and (not low-p) (not high-p) (/= k cutoff-place))
         (return) )
       (push U result) )

      (push (cond
              ((and low-p (not high-p)) U)
              ((and high-p (not low-p)) (1+ U))
              ((<= (* 2 R) S) U)
              (t (1+ U)) )
            result )
      (values (nreverse result) expt) ) ) )


;;;; Float To Digits
;;;
;;; Returns:
;;;     list-of-digits
;;;
;;; Called by:
;;;     print-float
;;;
;;; Description:
;;;  Convert floating-point number into list of digits based 10.
;;;
;;; Reference:
;;;  Printing Floating-Point Numbers Quickly and Accurately, Robert G Burger,
;;;  and R. Kent Dybvig, Proceedings of the SIGPLAN '96 Conference on
;;;  Programming Language Design and Implementation.
;;;
;;; Note:
;;;  This algorithm is faster than Guy L. Steel's algorithm.
;;;
;
(defun float-to-digits (f e min-e p)
  (labels (
    (generate (r s m+ m- low-ok? high-ok?)
      (multiple-value-bind (d r)
          (truncate (* r 10) s)
        (let* ((m+ (* m+ 10))
               (m- (* m- 10))
               (tc1 (if low-ok?  (<= r m-) (< r m-)))
               (tc2 (if high-ok? (>= (+ r m+) s) (> (+ r m+) s))) )
          (if (not tc1)
              (if (not tc2)
                  (cons d (generate r s m+ m- low-ok? high-ok?))
                (list (+ d 1)) )
            (if (not tc2)
                (list d)
              (if (< (* r 2) s)
                  (list d)
                (list (+ d 1)) ))) ) ) )

    (scale (r s m+ m- k low-ok? high-ok?)
      (cond
        ((if high-ok?
             (>= (+ r m+) s)
           (> (+ r m+) s) )
          (scale r (* s 10) m+ m- (+ k 1) low-ok? high-ok?) )

        ((if high-ok?
             (< (* (+ r m+) 10) s)
           (<= (* (+ r m+) 10) s) )
          (scale (* r 10) s (* m+ 10) (* m- 10) (- k 1) low-ok? high-ok?) )

        (t
          (values (generate r s m+ m- low-ok? high-ok?) k) )) )
    )
    (let ((round? (zerop (mod f 2))))
      (if (>= e 0)
          (if (not (= f (expt 2 (- p 1))))
              (let ((be (expt 2 e)))
                (scale (* f be 2) 2 be be 0 round? round?) )
            (let* ((be (expt 2 e))
                   (be1 (* be 2)) )
              (scale (* f be1 2) (* 2 2) be1 be 0 round? round?) ))
        (if (or (= e min-e) (not (= f (expt 2 (- p 1)))))
            (scale (* f 2) (* (expt 2 (- e)) 2) 1 1 0 round? round?)
        (scale (* f 4) (* (expt 2 (- 1 e)) 2) 2 1 0 round? round?) )) ) ) )


;;;; ieee-integer-decode-float
;;;
;;; Syntax:
;;;     ieee-integer-decode-float float => significand, exponent, sign, type
;;;
;;; Arguments and Values:
;;;     float   a float
;;;     type    one of :number, :nan, :snan, :inifinity, or :subnormal
;;;
;;; Description:
;;;  Decodes float as integer-decode-float and returns type of float.
;
(defun ieee-integer-decode-float (x)
  (etypecase x
    (single-float
      (multiple-value-bind (sg e s) (integer-decode-float32 x)
          (values sg e s
            (cond
             ((not (eql e 255)) :number)
             ((eql sg 0) :infinity)
             ((logbitp 22 sg) :nan)
             (t :snan) )) ) )
    (double-float
      (multiple-value-bind (sg e s) (integer-decode-float64 x)
          (values sg e s
            (cond
             ((not (eql e 2047)) :number)
             ((eql sg 0) :infinity)
             ((logbitp 51 sg) :nan)
             (t :snan) )) ) )) )


;;;; format-float-$
(defun format-float-$ (stream x modifier d n w padchar)
  (labels (
    ;; float-digits-length
    (float-digits-length (digits expt dcount)
      (let ((n (length digits)))
       (cond
        ;; No integer part
        ;;
        ((minusp expt)
          (1+ dcount) )     ; +1 for "."

        ;; No fraction part
        ;;
       ((<= n (1+ expt))
         (+ n expt dcount 1) )

       (t
        (+ (- n expt) dcount) )) ) )

    ;; format-float-$-aux
    (format-float-$-aux (stream x modifier d n w padchar)
      (let ((digits '())
            (expt 0)
            (sign nil) )

        (multiple-value-bind (f e s type)
            (ieee-integer-decode-float x)

          (case type
            ((:number))
            (otherwise
              (format stream "#<~A ~C~A>"
                (type-of x)
                (if (minusp sign) #\- #\+)
                type )
              (return-from format-float-$-aux t) ))

          (let ((p (float-precision x)))
            (multiple-value-setq (digits expt)
                  (float-to-digits-old f (+ e p) p :normal 0) ) )
            (setq sign (if (minusp s) #\- (if (logbitp 1 modifier) #\+ nil))) )

        (let ((k (abs expt))
              (ww (if w w 0)) )
          (if (and (>= k ww) (>= k 100))
              (format-float-E-aux stream x (logbitp 1 modifier)
                            w (+ d n -1) nil 1 nil padchar nil )
            (let ((nn (float-digits-length digits expt d))
                 (leading-zero-p nil) )
              (when sign (incf nn))

              (when (and (minusp expt) (>= ww (1+ nn)))
                (incf nn)
                (setq leading-zero-p t) )

              ;; output pad & sign.
              ;;
              (when (and sign (logbitp 0 modifier))
                (stream-write-char stream sign) )

              (write-chars padchar (- ww nn) stream)

              (when (and sign (not (logbitp 0 modifier)))
                (stream-write-char stream sign) )

              ;; output digits.
              ;;
              (format-float-F-digits stream nil digits expt d
                                     leading-zero-p ) )) ) ) )
    )
    ;; format-float-$
    (typecase x
      (float
        (format-float-$-aux stream x modifier d n w padchar) )
      (rational
        (format-float-$-aux stream (float x) modifier d n w padchar) )
      (otherwise
        (format-integer stream x 0 10 w padchar padchar 0) )) ) )


;;;; format-float-E-aux
;;; Called by:
;;;     format-float-D
;;;     format-float-F-aux
;;;     format-float-G-aux
(defun format-float-E-aux (stream x at-sign-p
                           wspec dspec espec kspec ovfchar padchar exptchar )

    (declare (type (or fixnum    null) espec))
    (declare (type (or character null) ovfchar))
    (declare (type (or character null) exptchar))
  (labels (
    ;; exponent-length
    ;;
    (exponent-length (expt)
      (do ((e (abs expt))
           (ewidth 1)
           (j 10) )
          ((< e j) ewidth)
            (setq j (* j 10))
            (incf ewidth) ) )
    )
    ;;
    ;; format-float-E-aux
    ;;
    (unless exptchar (setq exptchar (float-marker x)) )

    (multiple-value-bind (f e sign type)
        (ieee-integer-decode-float x)

      (case type
        ((:number))
        (otherwise
          (format stream "#<~A ~C~A>"
            (type-of x)
            (if (minusp sign) #\- #\+)
            type )
          (return-from format-float-E-aux t) ))

      (let ((p (float-precision x))
            (sign (if (minusp sign) #\- (and at-sign-p #\+)))
            (digits '())
            (expt 0) )

        (cond
         (wspec
          (if dspec
            (progn
              (multiple-value-setq (digits expt)
                (float-to-digits-old f (+ e p) p
                                          :relative (- 0 dspec kspec) ))
              (setq expt (- expt kspec -1))
              (unless espec
                (setq espec (exponent-length expt)) )
              (when (plusp kspec)
                (incf dspec) ))

            (let ((ww (- wspec (if sign 4 3))))   ; 3 for ".E+"
              (unless espec
                (multiple-value-setq (digits expt)
                  (float-to-digits-old f (+ e p) p :variable (1+ ww)) )
                (setq expt (- expt kspec -1))
                (setq espec (exponent-length expt)) )

              (multiple-value-setq (digits expt)
                (float-to-digits-old f (+ e p) p
                                          :variable (- ww espec -1) ))

              (setq expt (- expt kspec -1))
              (setq dspec (length digits)) ))

          (let ((nn (+ dspec espec (if sign 4 3))))
            (if (and ovfchar (or (< wspec nn)
                                 (< espec (exponent-length expt)) ))
              (write-chars ovfchar wspec stream)
              (let ((leading-zero-p nil)
                    (trailing-zero-p nil))
                (when (and (<= kspec 0) (>= wspec (1+ nn)))
                  (setq leading-zero-p t)
                  (incf nn) )
                (write-chars padchar (- wspec nn) stream)
                (format-float-E-digits stream
                                       sign digits dspec kspec
                                       exptchar expt espec
                                       leading-zero-p
                                       trailing-zero-p ) )) ) )

         (dspec
          (multiple-value-bind (digits expt)
              (float-to-digits-old f (+ e p) p :relative (- 0 dspec kspec))
            (setq expt (- expt kspec 1))
            (format-float-E-digits stream
                                   sign digits dspec kspec
                                   exptchar expt espec
                                   t t ) ) )
         (t
          (multiple-value-bind (digits expt)
              (float-to-digits-old f (+ e p) p :normal 0)
            (setq expt (- expt kspec -1))
            (format-float-E-digits stream
                                   sign digits nil kspec
                                   exptchar expt espec
                                   t t ) ) )) ) ) ) )


;;;; format-float-E-digits
;;;     digits          list of digits
;;;     d               number of digits to print after the decimal point
;;;     k               scale factor, default is one.
;;;     marker          float-marker
;;;     exp             exponent
;;;     ewidth
;;;     leading-zero-p
;;;     trailing-zero-p
(defun format-float-E-digits (stream sign digits d k marker expt ewidth
                                     leading-zero-p trailing-zero-p )
    (declare (values unspecified))
    (declare (type list digits))
    (declare (type (or character null) sign))
    (declare (type (or fixnum null) d))
    (declare (type fixnum k))
    (declare (type character marker))
    (declare (type fixnum expt))
    (declare (type (or fixnum null) ewidth))
  (let ((d (or d (max k (length digits)))))
    (decf k)

    (when sign (stream-write-char stream sign))

    (when (minusp k)
      (when leading-zero-p
        (stream-write-char stream #\0) )
      (stream-write-char stream #\.)
      (write-chars #\0 (1- (- k)) stream)
      (decf d (1- (- k))) )

    (loop
     (when (< (decf d) 0) (return))
     (stream-write-char
        stream
        (if (null digits) #\0 (digit-char (pop digits))) )
     (when (zerop k) (stream-write-char stream #\.))
     (decf k) )

    (when (and (= k -1) trailing-zero-p)
      (stream-write-char stream #\0) )

    ;; print exponent.
    (stream-write-char stream marker)
    (stream-write-char stream (if (minusp expt) #\- #\+))
    (format-integer stream (abs expt) 0 10 (or ewidth 0) #\0 #\, 0) ) )


;;;; format-float-F-aux
;;;
;;; Syntax:
;;;   format-float-F-aux ...
;;;     => overflow-p
;;;
(defun format-float-F-aux (stream x at-sign-p w d k ovfchar padchar)
  (multiple-value-bind (f e sign type)
      (ieee-integer-decode-float x)

      (case type
        ((:number))
        (otherwise
          (format stream "#<~A-~A>" (type-of x) type)
          (return-from format-float-F-aux t) ))

    (let ((p    (float-precision x))
          (sign (if (minusp sign) #\- (and at-sign-p #\+))) )

      (cond
       (w
        (let ((ww (if sign (1- w) w))
              digits expt )
          (multiple-value-setq (digits expt)
            (if d
                (float-to-digits-old f (+ e p) p :absolute (- 0 d k))
              (float-to-digits-old f (+ e p) p :variable ww) ) )
          (incf k expt)

          (let ((n             (length digits))
                (leading-zero-p nil) )

            (cond
             ;; No integer part.
             ;;
             ((minusp k)
              (if d
                (setq n (1+ d))             ; +1 for "."
                (progn
                  (incf n (- k))
                  (setq d (1- n)) ))
              (when (>= ww (1+ n))
                (setq leading-zero-p t)
                (incf n) ) )

             ;; No fraction part.
             ;;
             ((<= n (1+ k))
              (incf n (1+ k))
              (unless d
                (setq d 0)
                (when (>= ww (1+ n))
                  (setq d 1) ))
              (incf n d) )

             ;; Both integer and fraction
             ;;
             (t
              (if d
                (setq n (+ k d 2))  ; +1 for ".", +1 for integer part.
                (progn
                  (incf n)              ; +1 for "."
                  (setq d (- n k 2))
                  (when (and (zerop d) (>= ww (1+ n)))
                    (setq d 1) ))) ))

            (if (and ovfchar (< ww n))
              (write-chars ovfchar w stream)
              (progn
                (write-chars padchar (- ww n) stream)
                (format-float-F-digits stream sign digits k d
                                       leading-zero-p ))) ) ) )

       (d
        (multiple-value-bind (digits expt)
            (float-to-digits-old f (+ e p) p :absolute (- 0 d k))
          (format-float-F-digits stream sign digits (+ k expt) d t) ) )

       (t
        (multiple-value-bind (digits expt)
            (float-to-digits-old f (+ e p) p :normal 0)

          (if (>= (abs expt) 100)
              (format-float-E-aux stream x nil 0 nil nil 1 nil #\Space nil)
            (progn
               (when sign (stream-write-char stream sign))
               (print-float-digits-F
                    stream digits (+ k expt) ))) ) ))
    nil ) ) )


;;;; format-float-F-digits
(defun format-float-F-digits (stream sign digits k d leagind-zero-p)
  (when sign
    (stream-write-char stream sign) )

  (when (minusp k)
    (when leagind-zero-p
      (stream-write-char stream #\0) )
    (stream-write-char stream #\.)
    (write-chars #\0 (1- (- k)) stream) )

  (setq d (- d))
  (loop
     (unless (>= k d) (return))
     (stream-write-char
        stream (if (null digits) #\0 (digit-char (pop digits))) )
     (when (zerop k) (stream-write-char stream #\.))
     (decf k) ) )


;;;; format-float-G-aux
(defun format-float-G-aux (stream x at-sign-p w d e k ovfchar padchar exptchar)
  (let ((n (if (= 0.0 x) 0 (ceiling (log (abs x) 10)))))
    (unless d
      (multiple-value-bind (f e sign type)
          (ieee-integer-decode-float x)
          (declare (ignore sign))

      (case type
        (:normal)
        (otherwise
          (format stream "#<~A-~A>" (type-of x) type)
          (return-from format-float-G-aux t) ))

        (multiple-value-bind (digits expt)
            (let ((p (float-precision x)))
               (float-to-digits-old f (+ e p) p :normal 0) )
          (let ((q (length digits)))
            (cond
              ((minusp expt) (decf q expt))
              ((> expt q)    (setq q expt))
              (t             (incf q expt)) )

            (setq d (max q (min n 7))) ) ) ))

    (let* ((ee (if e (+ e 2) 4))
           (ww (if w (- w ee) nil))
           (dd (- d n)) )
      (cond
        ((not (<= 0 dd d))
          (format-float-E-aux stream x at-sign-p w d e (if k k 1)
                              ovfchar padchar exptchar ) )
        ((format-float-F-aux stream x at-sign-p ww dd 0 ovfchar padchar)
          (when ww (write-chars ovfchar ee stream)) )
        (t
          (when ww (write-chars padchar ee stream)) )) ) ) )


;;;; Print Floating-Pointer Number in "E" format.
;;; Called by:
;;;     print-object double-float
;;;     print-object single-float
(defun print-float-digits-E (stream digits expt marker)
  (write-char (digit-char (pop digits)) stream)
  (write-char #\. stream)
  (do ((k 0))
      ((endp digits) (when (= k 0) (write-char #\0 stream)))
    (write-char (digit-char (pop digits)) stream)
    (incf k) )
  (write-char marker stream)
  (when (minusp expt)
    (write-char #\- stream)
    (setq expt (- expt)) )
  (print-fixnum-aux expt stream 10) )


;;;; Print Floating-Pointer Number in "F" format.
;;;
;;; Called by:
;;;     print-float
(defun print-float-digits-F (stream digits k)
  (when (minusp k)
    (write-string "0." stream)
    (write-chars #\0 (1- (- k)) stream) )
  (loop
   (write-char (if (null digits) #\0 (digit-char (pop digits))) stream)
   (when (zerop k) (write-char #\. stream))
   (decf k)
   (unless (or digits (>= k -1)) (return)) ) )


;;;; Print Float Object
;;;
;;; Description:
;;;  Print floating-pointer number in following steps:
;;;   1. Get sign and print minus when x is negative.
;;;   2. Print absolute value:
;;;   3.  When x in [10^-3, 10^7] or 0
;;;   3-1  Prints integer "." fraction. There
;;;        is at least one character both side of ".".
;;;   3-2  Print exponent marker and "0" when x isn't match
;;;        *read-default-float-number*. Ex: "2.71828S0"
;;;   4.  When x is not [10^-3, 10^7] or 0
;;;   4-1  Print x after scaling to [1, 10)
;;;   4-2  Print at least one digit before and after ".".
;;;   4-3  Print exponent marker "E" when x is type of
;;;        *read-default-float-format*, otherwise print exponent marker "s",
;;;        "d" or "l".
;;;   4-4  Print exponent.
;
(defmethod cl:print-object ((x double-float) stream)
  (labels (
    ;; float-marker
    (float-marker ()
      (if (eq *read-default-float-format* 'double-float) #\e #\d) )

    ;; print-infinity
    (print-infinity (sign)
      (cond
        ((not *print-readably*)
          (format stream "#<Double-Float ~Cinf>"
            (if (< sign 0) #\- #\+) ) )
        (*read-eval*
          (if (< sign 0)
              (format stream "#.(/ ~S ~S)" 1d0 0d0)
            (format stream "#.(/ ~S ~S)" -1d0 0d0) ) )
        (t (error 'print-not-readable :object x) )) )

    ;; print-nan
    (print-nan (sign)
      (cond
        ((not *print-readably*)
          (format stream "#<Double-Float ~CNaN>"
            (if (< sign 0) #\- #\+) ) )
        (*read-eval*
          (if (< sign 0)
              (format stream "#.(/ ~S ~S)" 0d0 0d0)
            (format stream "#.(- (/ ~S ~S))" 0d0 0d0) ) )
        (t (error 'print-not-readable :object x) )) )

    ;; print-number
    (print-number (f e sign)
      (when (< sign 0) (write-char #\- stream))
      (multiple-value-bind (digits expt)
          (if (and (eql f 0) (eql e 0))
              (values '(0) 0)
            (float-to-digits f e -1021 (float-precision x)) )
        (decf expt)
        (if (<= -3 expt 6)
            ;; 10^-3 <= |x| < 10^7 => Free-Format.
            (progn
              (print-float-digits-F stream digits expt)
              (unless (eql (float-marker) #\e)
                (write-char #\d stream)
                (write-char #\0 stream) ))

           ;; |x| < 10^-3 or |x| >= 10^7 => Free-Format Exponential.
           (print-float-digits-E stream digits expt (float-marker)) ) ))

    ;; print-snan
    (print-snan (sign)
      (cond
        ((not *print-readably*)
          (format stream "#<Double-Float ~CSNaN>"
            (if (< sign 0) #\- #\+) ) )
        (*read-eval*
          (write-string "#." stream)
          (prin1 '(encode-float64 1 255 (if (< sign 0) 1 0)) stream) )
        (t (error 'print-not-readable :object x) )) )
    )
    ;;
    (multiple-value-bind (f e sign type)
        (ieee-integer-decode-float x)
      (ecase type
        ((:infinity) (print-infinity sign))
        ((:nan)      (print-nan sign))
        ((:number)   (print-number f e sign))
        ((:snan)     (print-snan sign)) ) ) ) )


(defmethod cl:print-object ((x single-float) stream)
  (labels (
    ;; float-marker
    (float-marker ()
      (if (eq *read-default-float-format* 'single-float) #\e #\f) )

    ;; print-infinity
    (print-infinity (sign)
      (cond
        ((not *print-readably*)
          (format stream "#<Single-Float ~Cinf>"
            (if (< sign 0) #\- #\+) ) )
        (*read-eval*
          (if (< sign 0)
              (write-string "#.(/ 1e0 0d0)" stream)
            (write-string "#.(/ -1e0 0d0)" stream) ) )
        (t (error 'print-not-readable :object x) )) )

    ;; print-nan
    (print-nan (sign)
      (cond
        ((not *print-readably*)
          (format stream "#<Single-Float ~CNaN>"
            (if (< sign 0) #\- #\+) ) )
        (*read-eval*
          (if (< sign 0)
              (write-string "#.(/ 0d0 0d0)" stream)
            (write-string "#.(/ -0d0 0d0)" stream) ) )
        (t (error 'print-not-readable :object x) )) )

    ;; print-number
    (print-number (f e sign)
      (when (< sign 0) (write-char #\- stream))
      (multiple-value-bind (digits expt)
          (if (and (eql f 0) (eql e 0))
              (values '(0) 0)
            (float-to-digits f e -125 (float-precision x)) )
        (decf expt)
        (if (<= -3 expt 6)
            ;; 10^-3 <= |x| < 10^7 => Free-Format.
            (progn
              (print-float-digits-F stream digits expt)
              (unless (eql (float-marker) #\e)
                (write-char #\f stream)
                (write-char #\0 stream) ))

           ;; |x| < 10^-3 or |x| >= 10^7 => Free-Format Exponential.
           (print-float-digits-E stream digits expt (float-marker)) ) ))

    ;; print-snan
    (print-snan (sign)
      (cond
        ((not *print-readably*)
          (format stream "#<Single-Float ~CSNaN>"
            (if (< sign 0) #\- #\+) ) )
        (*read-eval*
          (write-string "#." stream)
          (prin1 '(encode-float64 (ash 1 52) 0 (if (< sign 0) 1 0)) stream) )
        (t (error 'print-not-readable :object x) )) )
    )
    ;;
    (multiple-value-bind (f e sign type)
        (ieee-integer-decode-float x)
      (ecase type
        ((:infinity) (print-infinity sign))
        ((:nan)      (print-nan sign))
        ((:number)   (print-number f e sign) )
        ((:snan)     (print-snan sign)) ) ) ) )


;;;; format-float-E
;;;; format-float-F
;;;; format-float-G
;
(macrolet (
  (define (fmtchar)
    (let ((fn-entry (intern (format nil "FORMAT-FLOAT-~C" fmtchar)))
          (fn-aux   (intern (format nil "FORMAT-FLOAT-~C-AUX" fmtchar)))
          (args    '(w d e k ovfchar padchar exptchar)) )

      (when (char= #\F fmtchar)
        (setq args (copy-list args))
        (setq args (delete 'e args))
        (setq args (delete 'exptchar args)) )

    `(defun ,fn-entry (stream x at-sign-p ,@args)
       (typecase x
         (float
           (,fn-aux stream x at-sign-p ,@args) )

         (rational
           (,fn-aux stream (float x) at-sign-p ,@args) )

         (otherwise
           (format-integer stream x 0 10 (or w 0) padchar padchar 0) )) ) ) )
  )
  ;;
  (define #\E)
  (define #\F)
  (define #\G)
 ) ;macrolet


;;;; 22.3.3.4  Tilde Dollarsign: Monetary Floating-Point
(define-format #\$ ((d 2) (n 1) (w 0) (padchar #\Space))
  (format-float-$ stream (next-arg) (modifier) d n w padchar) )


;;;; 22.3.3.2  Tilde E: Exponential Floating-Point
;;;; 22.3.3.1  Tilde F: Fixed-Format Floating-Point
;;;; 22.3.3.3  Tilde G: General Floating-Point
(macrolet (
  (define-format-float (fmtchar)
    (let* ((fn (intern (format nil "FORMAT-FLOAT-~C" fmtchar)))
           (params
             (ecase fmtchar
               ((#\E) '((w nil) (d nil) (e nil) (k 1)
                        (ovfchar nil) (padchar #\Space)
                        (exptchar nil) ))
               ((#\F) '((w nil) (d nil) (k 0)
                        (ovfchar nil) (padchar #\Space) ))
               ((#\G)
                      '((w nil) (d nil) (e nil) (k 1)
                        (ovfchar nil) (padchar #\Space)
                        (exptchar nil) ))) )
            (args (mapcar #'first params)) )
      `(define-format ,fmtchar ,params
         (,fn stream (next-arg) (logbitp 2 (modifier :at-sign)) ,@args) ) ) )
  )
  ;;
  (define-format-float #\E)
  (define-format-float #\F)
  (define-format-float #\G)
 ) ;macrolet
