;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Editor - Text Buffer Core
;;; editor/ed-text-buffer-defs.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r00-fns.lisp#2 $
;;;
;;;
;;; Description:
;;;  Class definition of Text Buffer Core.
;
(in-package :editor)

;;;; buffer-char
(defun buffer-char (buffer posn)
    (declare (values character))
    (declare (type text-buffer buffer))
    (declare (type position posn))
  (validate-position buffer posn)
  (let ((gap-start (slot-value buffer 'gap-start)))
    (if (< start gap-start)
        (schar (slot-value buffer 'chars) posn)
     (let ((gap-end (slot-value buffer 'gap-end)))
       (schar (slot-value buffer 'chars) (+ (- posn gap-start) gap-end)) )) ) )


;;;; buffer-length
(defun buffer-length (buffer)
    (declare (values position))
    (declare (type text-buffer buffer))
 (slot-value buffer 'length) )


;;;; buffer-substr
(defun buffer-substr (buffer start end &optional (string "" string-p))
    (declare (values simple-string))
    (declare (type text-buffer))
    (declare (type position start end))
  (validate-positions buffer start end)
  (let ((chars     (slot-value buffer 'chars))
        (gap-end   (slot-value buffer 'gap-end))
        (gap-start (slot-value buffer 'gap-start))
        (string    (if string-p string (make-string (- end start)))) )
    (if (>= start gap-start)
        ;; We extract text after gap.
        ;;  ggggg<....>
        (replace string chars
            :end2   (+ gap-end (- end   gap-start))
            :start2 (+ gap-end (- start gap-start)) )
      ;; We extract text before gap. There are three caes:
      ;;   1: <....>ggggg
      ;;   2: <...ggg>ggg
      ;;   3: <...gggg...>
      (let ((middle (min gap-start end)))
        (replace string chars
            :start2 start
            :end2   middle )
        (replace string chars
            :start1 (- middle start)
            :start2 gap-end
            :end2   (+ gap-end (- end middle)) ) ))
    string ) )


;;;; core-delete
(defun core-delete (buffer start end)
    (declare (values count))
    (declare (type text-buffer buffer))
    (declare (type position start end))
  (let ((n (validate-positions buffer start end)))
    (core-move-gap buffer start)
    (incf (slot-value buffer 'gap-end) n)
    (decf (slot-value buffer 'length) n)
    n ) )


;;;; core-extend
(defun core-extend (buffer posn extend)
    (declare (values text-buffer))
    (declare (type text-buffer buffer))
    (declare (type position posn))
    (declare (type count extend))
  (when (plusp extend)
    (core-move-gap buffer posn)
    (let ((gap-start (slot-value buffer 'gap-start))
          (gap-end   (slot-value buffer 'gap-end)) )
      (unless (>= (- gap-end gap-start) text-buffer-minimum-gap-length)
        (let* ((cur-chars (slot-value buffer 'chars))
               (length (* (1+ (truncate (+ (length cur-chars) extend)
                                        text-buffer-extension ))
                           text-buffer-extension ))
               (new-chars (make-string length)) )
          (replace new-chars cur-chars :end2 gap-start)
          (replace new-chars cur-chars
                   :start1 gap-end
                   :start2 gap-end ) )) ))
  buffer )


;;;; core-insert-string
(defun core-insert-string (buffer posn string start end)
    (declare (values count))
    (declare (type text-buffer buffer))
    (declare (type position posn))
    (declare (type string string))
    (declare (type sequence-index start))
    (declare (type sequence-end end))
  (validate-position buffer posn)
  (multiple-value-bind (string start end) (string-data string start end)
    (let ((n (- end start)))
      (core-extend buffer posn n)
      (loop
        with chars = (slot-value buffer 'chars)
        for idx from start below end do
          (setf (schar chars posn) (schar string idx))
          (incf posn) )
      (incf (slot-value buffer 'gap-start) n)
      (incf (slot-value buffer 'length) n)
      n ) ) )


;;;; core-move-gap
;;;
;;; Note:
;;;  Length of gap isn't changed.
(defun core-move-gap (buffer new-start)
    (declare (values text-buffer))
    (declare (type text-buffer buffer))
    (declare (type position new-start))
  (let* ((chars     (slot-value buffer 'chars))
         (cur-end   (slot-value buffer 'gap-end))
         (cur-start (slot-value buffer 'gap-start))
         (diff      (- cur-start new-start))
         (new-end   (- cur-end diff)) )
    (setf (slot-value buffer 'gap-end)   new-end)
    (setf (slot-value buffer 'gap-start) new-start)
    (cond
      ((plusp diff)
        ;; Move GAP backward =
        ;;      Move string between new-start and cur-start before cur-end.
        ;;  abcdef...ghijk
        ;;     ^  s  e
        ;;  abc...defghijk
        ;;     s  e
        (replace chars chars
            :start1 new-end
            :end2   cur-start
            :start2 new-start ) )
      ((minusp diff)
        ;; Move GAP forward =
        ;;      Move string between cur-end and new-end after cur-start.
        ;;  abcdef...ghijk
        ;;        s  e  ^
        ;;  abcdefghi...jk
        ;;           s  e
        (replace chars chars
            :start1 cur-start
            :end2   new-end
            :start2 cur-end ) ))
    buffer ) )


;;;; validate-position
(defun validate-position (buffer posn)
    (declare (values position))
    (declare (type text-buffer buffer))
    (declare (type position posn))
  (unless (<= 0 posn (slot-value buffer 'length))
    (error 'editor-invalid-position
           :buffer   buffer
           :position posn ))
  posn )


;;;; validate-positions
(defun validate-positions (buffer start end)
    (declare (values length))
    (declare (type text-buffer buffer))
    (declare (type position start end))
  (unless (<= 0 start end (slot-value buffer 'length))
    (error 'editor-invalid-positions
           :buffer buffer
           :end    end
           :start  start ))
  (- end start) )
