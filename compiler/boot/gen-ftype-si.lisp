;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - bootstrap - generate ftype
;;; compiler/boot/gen-ftype-si.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/compiler/boot/gen-ftype-si.lisp#8 $
;;;
(in-package :cl-user)


(defun transcode (s x)
  (labels (
    ;; classify-cons
    (classify-cons (x)
      (cond
        ((not (listp (cdr x))) 'cons)
        ((not (null (last x 0))) 'list*)
        (t 'list) ) )

    ;; pretty-package-name
    (pretty-package-name (x)
      (or (first (package-nicknames x)) (package-name x)) )

    ;; xcode-symbol
    (xcode-symbol (x)
      (write-char #\Q s)
      (loop for ch across (symbol-name x) do
        (if (alphanumericp ch)
            (write-char (char-downcase ch) s)
          (write-char #\_ s) )) )
    )
    ;;
    (etypecase x
      (null (format s "nil"))
      (fixnum (format s "Fixnum::Encode(~D)" x))
      (symbol
        (cond
          ((eq x t)
            (format s "t") )
          #+nil ((find-class x nil nil)
            (xcode-symbol x) )
          ((or (eq (symbol-package x) #.(find-package :cl))
               (eq (symbol-package x) #.(find-package :si)) )
            (format s "Q(\"~A\")" x) )
          ((eq (symbol-package x) #.(find-package :keyword))
            (format s "Q(\":~A\")" x) )
          ((eq (symbol-package x) #.(find-package :cl-user))
            (format s "Q(\"~A\")" x) )
          (t
            (format s "Q(\"~S\")" x) )) )
       (si:setf-cell
         (format s "intern_setf_cell(")
         (transcode s (ext:ref si:setf-cell si::name x))
         (format s ")") )
       (cons
        (ecase (classify-cons x)
          ((cons)
            (format s "cons(")
            (transcode s (car x))
            (format s ", ")
            (transcode s (cdr x))
            (format s ");") )
          ((list)
            (format s "list")
            (let ((comma "("))
              (dolist (elt x)
                (format s "~A" comma)
                (transcode s elt)
                (setq comma ", ") ) )
            (format s ")") )
          ((list*)
            (format s "listA")
            (let ((comma "(")
                  (runner x) )
              (loop
                (format s "~A" comma)
                (unless (consp runner)
                  (transcode s runner)
                  (return) )
                (transcode s (car runner))
                (setq runner (cdr runner))
                (setq comma ", ") ) )
            (format s ")") )) )) ) )

;;;; generate
(defun generate (input &optional
        (output
            (make-pathname
                :name (format nil "cm_bt_~A" (pathname-name input))
                :type "inc"
                :defaults input )))
  (labels (
    ;; emit-footer
    (emit-footer (out)
      (format out "~%// EOF~%") )

    ;; emit-header
    (emit-header (out)
      (format out "// This is a program generated file. DO NOT EDIT!~%")
      (format out "// @(#)$Id: //proj/evcl3/mainline/compiler/boot/gen-ftype-si.lisp#8 $~2%") )

    ;; gen-1
    (gen-1 (s sig)
      (let ((fname (first sig)))
        (if (not (and (consp fname) (eq (first fname) '*)))
            (gen-1-aux s sig)
          (let ((rest (rest sig)))
            (dolist (fname (rest fname))
              (gen-1-aux s (cons fname rest)) ) )) ) )

    ;; gen-1-aux
    (gen-1-aux (s sig)
      (unless (and (eql (length sig) 3)
                   (listp (second sig)) )
        (error "Invalid signature: ~:W" sig) )
      (format s "  // ~(~S ~S)~)~%" (first sig) (cons 'function (rest sig)))
      (format s "  add_ftype(")
      (let ((fname (first sig)))
        (when (consp fname) (setq fname (si::intern-setf-cell (second fname))))
        (transcode s fname) )
      (format s ",~%    ")
      (transcode s (second sig))
      (format s ",~%    ")
      (transcode s (third sig))
      (format s " );~2%") )
    )
    ;;
    (let ((*package* (find-package :si)))
      (with-open-file (in input)
        (with-open-file (out output :direction :output :if-exists :supersede)
          (emit-header out)
          (loop
            for signature = (read in nil)
            until (null signature) do
              (gen-1 out signature) )
          (emit-footer out) ) ) ) ) )

(format t "~2%Type~%~4T~(~S~)~2%"
    '(generate "/proj/evcl3/compiler/boot/ftypes.lisp") )
