;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - arch - x64 - devel - 24 System Construction
;;; arch/x64/lisp/devel/x64-d00-fns.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1992-2002 by Project Vogue. All rights reserved.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/x86x64/lisp/devel/x86x64-d00-fns.lisp#6 $
;;;
;;; Description:
;;;  This file contains variable declarations used in runtime.
;;;
;
(in-package :si)

;;;; decode-code-annotation
;;; Used by with-code-annotation-iterator.
(defun decode-code-annotation (fn typ ofs)
    (declare (values symbol fixnum t))
    (declare (type native-code-object fn))
    (declare (type (integer 0 15) typ))
    (declare (type (unsigned-byte 24) ofs))
  (labels (
    ;; decode-callee
    (decode-callee ()
        (declare (values function))
      ;; 4 = size of operand(Jv) of CALL instruction
      (.unbox-int (+ (.box-int fn) ofs (!elt 'int32 fn ofs) 4)) )

    ;; decode-clit
    (decode-clit ()
      (!elt 't fn ofs) )

    ;; decode-cvar
    (decode-cvar ()
      (if (<= (!elt 'uint fn ofs) #xFFFF)
          (!elt 't fn ofs)
        (.unbox-int
            (+ (!elt 'uint fn ofs)
                #.(- (offsetof 'closed-cell 'value))
                #.(tagof 'closed-cell) ))) )

    ;; decode-dll-proc
    (decode-dll-proc ()
        (declare (values (cons string string)))
      (let* ((pinfo (ref dll-entry proc-info (fetch-dll-entry)))
             (finfo (ref dll-proc-info file-info pinfo)) )
        (cons (ref dll-file-info filename  finfo)
              (ref dll-proc-info proc-name pinfo) ) ) )

    ;; decode-xpoint
    (decode-xpoint ()
        (declare (values fixnum))
      (!elt 'fixnum fn ofs) )

    ;; decode-label
    (decode-label ()
        (declare (values fixnum))
      (- (!elt 'uint fn ofs)
         (+ (.box-int fn)
            #.(- (sizeof 'native-code-function)
                 (tagof 'native-code-function) ))) )

    ;; decode-symfun
    (decode-symfun ()
        (declare (values symbol))
      (.unbox-int
        (+ (!elt 'uint fn ofs)
           #.(- (offsetof 'symbol 'function))
           #.(tagof 'symbol) ) ) )

    ;; decode-symsetf
    (decode-symsetf ()
        (declare (values setf-cell))
      (.unbox-int
        (+ (!elt 'uint fn ofs)
           #.(- (offsetof 'setf-cell 'function))
           #.(tagof 'setf-cell) ) ) )

    ;; decode-symval
    (decode-symval ()
        (declare (values value-cell))
      (.unbox-int
        (+ (!elt 'uint fn ofs)
            #.(- (offsetof 'value-cell 'value))
            #.(tagof 'value-cell) )) )

    ;; decode-tlvofs
    (decode-tlvofs ()
        (declare (values tlv-record))
      ;; ofs_to_idx(ofs) = (ofs - offsetof(Thread, m_tlv)) / sizeof(Val)
      ;; X86: 279 = #x45C / 4
      ;; X64: #x8B8 = offsetof(Thread, m_tlv) - sizeof(Val)
      (let ((index (ash (!elt 'uint32 fn ofs) #+x86 -2 #+x64 -3)))
        (svref *tlv-vector* (- index 279)) ) )

    ;; fetch-dll-entry
    ;;  dll-link (!elt 'uint32 fn ofs) = DllEntry
    #+x86
    (fetch-dll-entry ()
      (!elt 'dll-entry fn ofs) )

    ;; fetch-dll-entry
    ;;  dll-link (!elt 'int32 fn ofs) = relative DllEntry
    #+x64
    (fetch-dll-entry ()
      (.unbox-int
         (+ (!elt 'int32 fn ofs)
            (+ (.box-int fn)
                #.(- (sizeof 'native-code-function)
                     (tagof 'native-code-function)
                     -4 ))
            ofs )) )
    )
    ;;
    (ecase typ
      (( 0) (values :lispval ofs (!elt 't fn ofs)))
      (( 1) (values :ncallee ofs (function-name (decode-callee))))
      (( 2) (values :lcallee ofs (decode-callee)))
      (( 3) (if (zerop (!elt 'int32 fn ofs))
                (values :lcallee ofs 0)
              (values :lcallee  ofs (decode-callee)) ) )
      (( 4) (values :symfun     ofs (decode-symfun)))
      (( 5) (values :symval     ofs (ref value-cell name (decode-symval))))
      (( 6) (values :symsetf    ofs (ref setf-cell name (decode-symsetf))))
      (( 7) (values :not-used-7 ofs (!elt 'int fn ofs)))
      (( 8) (values :tlvofs     ofs (ref tlv-record name (decode-tlvofs))))
      (( 9) (values :xpoint     ofs (decode-xpoint)))
      ((10) (values :clit       ofs (decode-clit)))
      ((11) (values :cvar       ofs (decode-cvar)))
      ((12) (values :dllproc    ofs (decode-dll-proc)))
      ((13) (values :not-used-13  ofs (!elt 'int fn ofs)))
      ((14) (values :label      ofs (decode-label)))
      ((15) (values :not-used-15    ofs (!elt 'int fn ofs))) ) ) )


;;;; find-code-annotation
;;; FIXME 2007-04-01: We must use native-code-function for ref macro until
;;; we support native-code-object in compiler.
(defun find-code-annotation (fn ra)
    (declare (type native-code-object fn))
    (declare (type (unsigned-byte 24) ra))
  (loop
    with desc = (the fundesc
                    (.unbox-int
                        (+ (.box-int fn)
                           (ref native-code-function length fn)
                           (- (tagof 'native-code-function))
                           (- (sizeof 'fundesc)) )))
    with ovh  = #.(sizeof 'native-code-function)
    with low  = (- (ref fundesc annot-offset desc) ovh)
    with high = (- (ref fundesc gcmap-offset desc) 4 ovh)
    while (<= low high) do
      (let* ((guess (logand (ash (+ high low) -1) (lognot 3)))
             (datum (!elt 'uint32 fn guess))
             (ofs   (ldb (byte 24 4) datum)) )
        (cond
          ((eql ofs ra)
            (let ((typ (ldb (byte 4 0) datum)))
              (return (decode-code-annotation fn typ ra)) ) )
          ((< ra ofs) (setq high (- guess 4)))
          (t (setq low  (+ guess 4))) ) )
    finally (return (values nil nil nil)) ) )
