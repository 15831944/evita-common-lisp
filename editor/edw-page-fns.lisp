;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Editor Windowing
;;; editor/edw-defs.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r00-fns.lisp#2 $
;;;
;;; Description:
;;;  Text View Formatter
;
(in-package :editor)

(declaim
  (ftype (function (format-context tv-line tv-marker interval)
                   (or tv-box null) )
         tv/add-marker )

  (ftype (function (format-context tv-line tv-box) tv-box)
         tv/add-box )

  (ftype (function (format-context tv-line character interval)
                   (or tv-box null) )
         tv/add-char )

  (ftype (function (format-context) (values (or character null)
                                            interval ))
         tv/fetch-char )

  (ftype (function (format-context tv-line) tv-line)
         tv/finish-line )

  (ftype (function (format-context tv-line) tv-line)
         tv/format-line )

  (ftype (function (format-context character interval) w:font)
         tv/get-font )

  (ftype (function (format-context tv-line tv-box w:font) tv-box)
         tv/init-box )

  (ftype (function (format-context fixnum fixnum) tv-line)
         tv/make-line )

  (ftype (function (format-context tv-line w:font tv-marker) tv-text-box)
         tv/make-marker-box )

  (ftype (function (format-context tv-line w:font) tv-text-box)
         tv/make-text-box )

  (ftype (function (format-context tv-line w:font) tv-xtext-box)
         tv/make-xtext-box )

  (ftype (function (format-context) t)
         tv/next-char )
 ) ; declaim


;;;; format-page
(defun format-page (context)
    (declare (type format-context context))
  (let* ((page (slot-value context 'page))
         (page-rect (slot-value page 'rect))
         (line-left (w:rect-left page-rect))
         (line-top  (w:rect-top  page-rect)) )
    (loop
      (let ((line (tv/make-line context line-left line-top)))
        (tv/format-line context line)
        (append-tv-line (slot-value page 'lines) line)
        (when (>= (w:rect-bottom (slot-value line 'rect))
                  (w:rect-bottom (slot-value page 'rect)) )
          (return) )
        (setq line-top (w:rect-bottom (slot-value line 'rect))) )) ) )


;;;; tv/format-line
(defun tv/format-line (context line)
    (declare (values tv-line))
    (declare (type format-context context))
    (declare (type tv-line line))
  (loop
    (multiple-value-bind (char intv) (tv/fetch-char context)
      (cond
        ((null char)
          (tv/add-marker context line :end-of-buffer intv)
          (return) )
        ((char= char #\Newline)
          (tv/add-marker context line :end-of-line intv)
          (return) )
        ((char= char #\Tab)
          (unless (tv/add-marker context line :wrap intv)
            (return) ) )
        ((not (tv/add-char context line char intv))
          (tv/add-marker context line :wrap intv)
          (return) ))
      (tv/next-char context) ))
    (tv/finish-line context line) )


;;;; tv/add-box
(defun tv/add-box (context line box)
    (declare (values tv-box))
    (declare (type format-context context))
    (declare (type tv-line line))
    (declare (type tv-box box))
    (declare (ignore context))
  (setf (w:rect-right (slot-value line 'rect))
        (w:rect-right (slot-value box  'rect)) )

  (setf (slot-value line 'ascent)
        (max (slot-value line 'ascent)  (slot-value box 'ascent)) )

  (setf (slot-value line 'descent)
        (max (slot-value line 'descent) (slot-value box 'descent)) )

  (append-tv-box (slot-value line 'boxes) box)
  box )


;;;; tv/add-char
(defun tv/add-char (context line char intv)
    (declare (values t))
    (declare (type format-context context))
    (declare (type tv-line line))
    (declare (type character char))
    (declare (type interval intv))
  (labels (
    ;; add
    (add (box char)
        (declare (type tv-text-box box))
        (declare (type w:font font))
        (declare (type character char))
     (vector-push-extend char (slot-value box 'string))

      (let* ((font (slot-value box 'font))
             (cx (w:get-text-extent font char)) )
        (incf (w:rect-right (slot-value box  'rect)) cx)
        (incf (w:rect-right (slot-value line 'rect)) cx) )

      box )

    ;; add-text
    (add-text ()
      (let ((font (tv/get-font context char intv)))
        (if font
            (when (fit font char) (add (get-text-box font) char))
          (add-xtext) ) ) )

    ;; add-xtext
    (add-xtext ()
      (let ((font (tv/get-font context #\9 intv)))
        (when (fit-aux (* (w:get-text-extent font "9") 5))
          (let ((box (get-xtext-box font))
                (code (char-code char)) )
            (add box #\u)
            (add box (digit-char (logand (ash code -12) 15) 16))
            (add box (digit-char (logand (ash code  -8) 15) 16))
            (add box (digit-char (logand (ash code  -4) 15) 16))
            (add box (digit-char (logand (ash code  -0) 15) 16))
            box )) ) )

    ;; fit
    (fit (font char)
      (fit-aux (w:get-text-extent font char)) )

    ;; fit-aux
    (fit-aux (cx)
      (<= (+ (w:rect-right (slot-value line 'rect)) cx)
          (w:rect-right (slot-value (slot-value context 'page) 'rect)) ) )

    ;; get-text-box
    (get-text-box (font)
      (let ((box (slot-value (slot-value line 'lines) 'tail)))
        (if (and (typep box 'tv-text-box)
                 (eq (slot-value box 'font) font) )
            box
          (let ((box (tv/make-text-box context line font)))
            (tv/add-box context line box) )) ) )

    ;; get-xtext-box
    (get-xtext-box (font)
      (let ((box (slot-value (slot-value line 'lines) 'tail)))
        (if (and (typep box 'tv-xtext-box)
                 (eq (slot-value box 'font) font) )
            box
          (let ((box (tv/make-xtext-box context line font)))
            (tv/add-box context line box) )) ) )
    )
    ;;
    (or (add-text) (add-xtext)) ) )


;;;; tv/add-marker
(defun tv/add-marker (context line marker intv)
    (declare (values tv-marker-box))
    (declare (type format-context context))
    (declare (type tv-marker marker))
    (declare (type interval intv))
  (let* ((font (tv/get-font context #\x intv))
         (box  (tv/make-marker-box context line font marker))
         (rect (slot-value box 'rect)) )
      (incf (w:rect-right rect) (w:get-text-extent font #\x))
      (tv/add-box context line box) ) )


;;;; tv/finish-line
(defun tv/finish-line (context line)
    (declare (type tv-line line))
    (declare (ignore context))
  (let ((boxes (slot-value line 'boxes)))

    ;; Set buffer start and end positions that this line covers.
    (setf (slot-value line 'buffer-end)
          (slot-value (slot-value boxes 'tail) 'buffer-end) )

    (setf (slot-value line 'buffer-start)
          (slot-value (slot-value boxes 'head) 'buffer-start) )

    ;; Set bottom corrdinate of line and boxes
    (let* ((line-rect (slot-value line 'rect))
           (bottom (+ (w:rect-top line-rect)
                      (slot-value line 'ascent)
                      (slot-value line 'descent) )) )
      (setf (w:rect-bottom line-rect) bottom)

      (for-each (box boxes)
        (setf (w:rect-bottom (slot-value box 'rect)) bottom) ) )

    line ) )


;;;; tv/init-box
(defun tv/init-box (context line box font)
    (declare (values tv-box))
    (declare (type format-context context))
    (declare (type tv-line line))
    (declare (type tv-box box))
  (let ((box-rect  (slot-value box 'rect))
        (line-rect (slot-value line 'rect)) )
    (setf (slot-value box 'buffer-end)   (slot-value context 'posn))
    (setf (slot-value box 'buffer-start) (slot-value context 'posn))

    (setf (slot-value box 'ascent)  (w:font-ascent  font))
    (setf (slot-value box 'descent) (w:font-descent font))

    (setf (w:rect-bottom box-rect) (w:rect-top  line-rect))
    (setf (w:rect-left   box-rect) (+ (w:rect-left line-rect)
                                      (w:font-height font) ))
    (setf (w:rect-right  box-rect) (w:rect-left line-rect))
    (setf (w:rect-top    box-rect) (w:rect-top  line-rect))

    box ) )


;;;; tv/make-marker-box
(defun tv/make-text-box (context line font marker)
    (declare (values tv-marker-box))
    (declare (type format-context context))
    (declare (type tv-line line))
  (let ((box (make-instance 'tv-marker-box :marker marker)))
    (tv/init-box context line box font) ) )


;;;; tv/make-text-box
(defun tv/make-text-box (context line font)
    (declare (values tv-text-box))
    (declare (type format-context context))
    (declare (type tv-line line))
  (let ((box (make-instance 'tv-text-box
                            :font   font
                            :string (make-array 20 :element-type 'character
                                                   :fill-pointer 0 )) ))
    (tv/init-box context line box font) ) )


;;;; tv/make-xtext-box
(defun tv/make-xtext-box (context line font)
    (declare (values tv-xtext-box))
    (declare (type format-context context))
    (declare (type tv-line line))
  (let ((box (make-instance 'tv-xtext-box
                            :font   font
                            :string (make-array 20 :element-type 'character
                                                   :fill-pointer 0 )) ))
    (tv/init-box context line box font) ) )
