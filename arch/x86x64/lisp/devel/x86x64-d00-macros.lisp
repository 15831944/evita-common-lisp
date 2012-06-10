;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - arch - x64 - devel - 24 System Construction
;;; arch/x86x64/lisp/devel/x86x64-d00-macros.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1992-2002 by Project Vogue. All rights reserved.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/arch/x86x64/lisp/devel/x86x64-d00-macros.lisp#4 $
;;;
;;; Description:
;;;  This file contains variable declarations used in runtime.
;;;
;
(in-package :si)

;;;; with-code-annotation-iterator
(defmacro with-code-annotation-iterator ((next fn-form) &body decl*-form*)
  (let ((fn '#:fn)
        (ofsStart       '#:ofsAnnon)
        (ofsEnd         '#:ofsGcMap)
        (ofsRunner      '#:ofsRunner)
        (sizeof-funobj  #.(sizeof 'native-code-function)) )
   `(let ((,fn ,fn-form))
        (declare (type native-code-object ,fn))
      (multiple-value-bind (,ofsStart ,ofsEnd)
          (let ((desc (the fundesc
                           (.unbox-int
                              (+ (- (.box-int ,fn)
                                 #.(sizeof 'fundesc)
                                 #.(tagof 'native-code-function) )
                                 (ref native-code-function length ,fn) ))) ))
             (values (- (ref fundesc annot-offset desc) ,sizeof-funobj)
                     (- (ref fundesc gcmap-offset desc) ,sizeof-funobj) ) )
        (let ((,ofsRunner ,ofsStart))
          (macrolet (
            (,next ()
              `(if (eql ,',ofsRunner ,',ofsEnd)
                   (values nil 0 nil)
                  (let ((datum (!elt 'uint32 ,',fn ,',ofsRunner)))
                    (incf ,',ofsRunner 4)
                    (let ((typ (ldb (byte  4 0) datum))
                          (ofs (ldb (byte 24 4) datum)) )
                      (decode-code-annotation ,',fn typ ofs) ) )) )
            ) ,@decl*-form* ) ) ) ) ) )
