;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Editor - Text Buffer Core
;;; editor/ed-buffer-core-defs.lisp
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
;;;  Class definition of Text Buffer Basic.
;
(in-package :editor)

;;;; cl:initialize-instance
(defmethod cl:initialize-instance :after ((buffer edit-buffer) &rest args)
    (declare (ignore args))
  (let ((intv  (make-instance 'interval :end 1))
        (intvs (slot-value buffer 'intervals)) )
    (setf (slot-value intvs 'head) intv)
    (setf (slot-value intvs 'tail) intv)
    (setf (slot-value buffer 'root-interval) intv) ) )


;;;; buffer-add-range
(defun buffer-add-range (buffer range)
    (declare (values range))
    (declare (type edit-buffer buffer))
    (declare (type range range))
  (append-range-anchor
    (slot-value buffer 'ranges)
    (make-instance 'range-anchor :anchor (make-weak-pointer range)) )
  range )


;;;; buffer-change
(defun buffer-change (buffer posn delta)
    (declare (values edit-buffer))
    (declare (type edit-buffer buffer))
    (declare (type position posn))
    (declare (type count delta))
  (let ((bufend (slot-value buffer 'length)))
  (labels (
    ;; relocate-intervals
    (relocate-intervals ()
      (let* ((intvs (slot-value buffer 'intervals))
             (head  (slot-value intvs 'head)) )
        (with-interval-iterator (next intvs)
          (loop
            (let ((intv (next)))
              (unless intv (return))

              (let ((start (slot-value intv 'start)))
                (when (> start posn)
                  (setf (slot-value intv 'start)
                        (min (max (+ start delta) posn) bufend) )) )

              (let ((end (slot-value intv 'end)))
                (when (> end posn)
                  (setf (slot-value intv 'end)
                        (min (max (+ end delta) posn) bufend) )) )

              ;; Delete interval if it is empty.
              (when (and (not (eq head intv))
                         (eql (slot-value intv 'end)
                              (slot-value intv 'start) ))

                (setf (slot-value buffer 'root-interval)
                    (btree-delete (slot-value buffer 'root-interval) intv) )

                (delete-interval intvs intv) ) )) ) ) )

    ;; relocate-ranges
    (relocate-ranges ()
      (let ((ranges (slot-value buffer 'ranges)))
        (with-range-anchor-iterator (next ranges)
          (loop
            (let ((anchor (next)))
              (unless anchor (return))

              (let ((range (weak-pointer-value anchor)))
                (if (null range)
                    (delete-range-anchor ranges anchor)
                  (progn
                    (let ((start (slot-value range 'start)))
                      (when (> start posn)
                        (setf (slot-value range 'start)
                            (min (max (+ start delta) posn) bufend) )) )

                    (let ((end (slot-value range 'end)))
                      (when (> end posn)
                        (setf (slot-value range 'end)
                          (min (max (+ end delta) posn)
                               bufend ))) ))) ) )) ) ) )
    )
    ;;
    (unless (zerop delta)
      (relocate-intervals)
      (relocate-ranges)
      (setf (slot-value buffer 'change-tick)
            (logand (1+ (slot-value buffer 'change-tick))
                    (1- (ash 1 20)) )) )
    buffer ) ) )


;;;; buffer-changed-p
(defun buffer-changed-p (buffer)
    (declare (values t))
    (declare (type edit-buffer buffer))
  (/= (slot-value buffer 'change-tick) (slot-value buffer 'save-tick)) )


;;;; (setf buffer-changed-p)
(defun (setf buffer-changed-p) (changed-p buffer)
    (declare (values t))
    (declare (type edit-buffer buffer))
  (setf (slot-value buffer 'save-tick)
        (if changed-p 0 (slot-value buffer 'change-tick)) )
  changed-p )


;;;; buffer-delete
(defun buffer-delete (buffer start end)
    (declare (values length))
    (declare (type edit-buffer buffer))
    (declare (type position start end))
  (validate-buffer-not-read-only buffer)
  (validate-buffer-ready buffer)
  (edit-journal-check-point buffer)
  (edit-journal-record-delete buffer start end)
  (core-delete buffer start end)
  (buffer-change buffer start (- start end))
  (- end start) )


;;;; buffer-insert-string
(defun buffer-insert-string (buffer posn string &optional (start 0) end)
    (declare (values length))
    (declare (type edit-buffer buffer))
    (declare (type position posn))
    (declare (type sequence-index start))
    (declare (type sequence-end   end))
  (validate-buffer-not-read-only buffer)
  (validate-buffer-ready buffer)
  (let* ((intv (get-interval buffer posn))
         (len  (core-insert-string buffer posn string start end))
         (eposn (+ posn len)) )
    (buffer-change buffer posn len)
    (when (< (slot-value intv 'end) eposn)
      (setf (slot-value intv 'end) eposn) )
    (edit-journal-check-point buffer)
    (edit-journal-record-insert buffer posn eposn)
    len ) )


;;;; buffer-read-only-p
(defun buffer-read-only-p (buffer)
    (declare (values t))
    (declare (type edit-buffer buffer))
  (not (zerop (logand (slot-value buffer 'state) buffer-read-only))) )


;;;; buffer-read-only-p
(defun (setf buffer-read-only-p) (read-only-p buffer)
    (declare (values t))
    (declare (type edit-buffer buffer))
  (setf (slot-value buffer 'state)
        (if read-only-p
            (logior (slot-value buffer 'state) buffer-read-only)
            (logand (slot-value buffer 'state) (lognot buffer-read-only)) ))
  read-only-p )


;;;; buffer-not-ready-p
(defun buffer-not-ready-p (buffer)
    (declare (values t))
    (declare (type edit-buffer buffer))
  (not (zerop (logand (slot-value buffer 'state) buffer-not-ready))) )


;;;; buffer-not-ready-p
(defun (setf buffer-not-ready-p) (not-ready-p buffer)
    (declare (values t))
    (declare (type edit-buffer buffer))
  (setf (slot-value buffer 'state)
        (if not-ready-p
            (logior (slot-value buffer 'state) buffer-not-ready)
            (logand (slot-value buffer 'state) (lognot buffer-not-ready)) ))
  not-ready-p )


;;;; make-range
(defun make-range (buffer &optional (start 0) (end 0))
    (declare (values range))
    (declare (type edit-buffer buffer))
    (declare (type position start end))
  (validate-positions buffer start end)
  (let ((range (make-instance 'range
                              :buffer buffer
                              :end    end
                              :start  start ) ))
    (buffer-add-range buffer range) ) )


;;;;; validate-buffer-not-read-only
(defun validate-buffer-not-read-only (buffer)
  (when (buffer-read-only-p buffer)
    (error 'buffer-read-only :buffer buffer) ) )


;;;;; validate-buffer-ready
(defun validate-buffer-ready (buffer)
  (when (buffer-not-ready-p buffer)
    (error 'buffer-not-ready :buffer buffer) ) )
