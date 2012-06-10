;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - arch - x86x64 - devel - 24 System Construction - FASD
;;; arch/x86x64/lisp/devel/x86x64-d24-fasd.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1992-2002 by Project Vogue. All rights reserved.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/x86x64/lisp/devel/x86x64-d24-fasd.lisp#5 $
;;;
;;; Description:
;;;  This file contains variable declarations used in runtime.
;;;
;
(in-package :si)

;;;; fasd-prepare-funobj
(defun devel::fasd-prepare-funobj (fasd fn)
    (declare (type devel::fasd fasd))
    (declare (type native-code-object fn))
  (devel::fasd-prepare fasd (class-of fn))
  (devel::fasd-prepare fasd (function-name fn))
  (with-code-annotation-iterator (next fn)
    (loop
      (multiple-value-bind (typ ofs datum) (next)
          (declare (ignore ofs))
        (unless typ (return))
        (devel::fasd-prepare fasd datum) )) ) )


;;;; fasd-serialize-funobj
(defun devel::fasd-serialize-funobj (fasd fn label)
    (declare (type devel::fasd fasd))
    (declare (type native-code-object fn))
  (multiple-value-bind (frame code-size ofsGcMap ofsGcMapZ)
      (let* ((desc-offset
                (- (ref native-code-function length fn)
                   #.(sizeof 'fundesc) ) )
             (desc (the fundesc
                        (.unbox-int
                            (+ (- (.box-int fn)
                                  #.(tagof 'native-code-function) )
                               desc-offset ))) ))
        (values (ref fundesc frame        desc)
                (ref fundesc code-size    desc)
                (ref fundesc gcmap-offset desc)
                desc-offset ) )
  (labels (
    ;; compute-initargs
    (compute-initargs ()
      (with-collector (collect)
        (let* ((codevec (extract-code))
               (annons  (extract-annotations codevec)) )
          (collect :name)
          (collect (ref native-code-function name fn))
          (collect :frame)
          (collect frame)
          (collect :annotations)
          (collect annons)
          (collect :codevec)
          (collect codevec)
          (collect :gcmap)
          (collect (extract-gcmap)) ) ) )

    ;; extract-annotations
    ;;  Returns list of code annotations and set zero to annotated site in
    ;;  codevec. Zeroing isn't mandatory but it helps finding unitialized
    ;;  annotation and reducing size of FASL when it is compressed.
    (extract-annotations (codevec)
        (declare (type (simple-array (unsigned-byte 8) (*)) codevec))
      (with-collector (collect)
        (with-code-annotation-iterator (next fn)
          (loop
            (multiple-value-bind (typ idx datum) (next)
              (unless typ (return))
              (setf (elt codevec (+ idx 0)) 0)
              (setf (elt codevec (+ idx 1)) 0)
              (setf (elt codevec (+ idx 2)) 0)
              (setf (elt codevec (+ idx 3)) 0)
              (collect (list typ idx datum)) )) ) ) )

    ;; extract-code
    (extract-code ()
        (declare (values (simple-array (unsigned-byte 8) (*))))
      (loop
        with blob = (make-array code-size :element-type '(unsigned-byte 8))
        for idx below code-size do
          (setf (row-major-aref blob idx) (!elt 'uint8 fn idx))
        finally (return blob) ) )

    ;; extract-gcmap
    (extract-gcmap ()
        (declare (values (simple-array (unsigned-byte 8) (*))))
      (loop
        with idxGcMap  = (- ofsGcMap  #.(sizeof 'native-code-function))
        with idxGcMapZ = (- ofsGcMapZ #.(sizeof 'native-code-function))
        with blob = (make-array (- idxGcMapZ idxGcMap)
                            :element-type '(unsigned-byte 8) )
            for ofs from idxGcMap below idxGcMapZ
            for idx = 0 then (1+ idx) do
              (setf (elt blob idx) (!elt 'uint8 fn ofs))
            finally (return blob) ) )
    )
    ;;
    (devel::fasd-write-op-uint fasd devel::FASL-OP-FUNOBJ
        (ref native-code-function length fn)
        label )
    (devel::fasd-serialize fasd (class-of fn))
    (devel::fasd-serialize fasd (compute-initargs)) ) ) )
