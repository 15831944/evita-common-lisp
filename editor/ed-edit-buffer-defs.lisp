;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Editor - Text Buffer Core
;;; editor/ed-text-buffer-defs.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2007 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/r00-fns.lisp#2 $
;;;
;;;
;;; Description:
;;;  Class definition of Edit Buffer.
;
(in-package :editor)

(deftype font-style () '(member :italic :normal))
(deftype font-weight () '(member :bold :normal))
(deftype rgb () '(integer 0 #xffffff))

;;;; style
(defclass style (editor-object)
  ((background
        :initform   0
        :type       rgb )
   (color
        :initform   0
        :type       rgb )
   (font-family
        :initform   "Consolas"
        :type       string )
   (font-size
        :initform   13
        :type       (integer 6 100) )
   (font-style
        :initform   :normal
        :type       font-style )
   (font-weight
        :initform   :normal
        :type       font-weight )
   (line-height
        :initform   16
        :type       (integer 6 200) ))
  (:metaclass structure-class) )


(let ((seed 0))
  (defun treap-random ()
    (setq seed (logand (+ (* seed 1664525) 1013904223) most-positive-fixnum)) )
  ) ; let


;;;; interval
(defclass interval (editor-object)
  ((end
      :initarg  :end
      :initform 0
      :type     position )
   (next
      :initform nil
      :type     (or interval null) )
   (prev
      :initform nil
      :type     (or interval null) )
   (left
      :initform nil
      :type     (or interval null) )
   (priority
      :initform (treap-random)
      :type     (integer 0 #.most-positive-fixnum) )
   (right
      :initform nil
      :type     (or interval null) )
   (start
      :initarg  :start
      :initform 0
      :type     position )
   (style
      :initform (make-instance 'style)
      :type     style ))
  (:metaclass structure-class) )


;;;; range
(defclass range (editor-object)
  ((buffer
      :initarg :buffer
      :type    edit-buffer )
   (end
      :initarg  :end
      :initform 0
      :type     position )
   (next
      :initform nil
      :type     (or range null) )
   (prev
      :initform nil
      :type     (or range null) )
   (start
      :initarg  :start
      :initform 0
      :type     position ))
  (:metaclass structure-class) )


;;;; range-anchor
(defclass range-anchor (editor-object)
  ((anchor
      :initarg  :anchor
      :initform (required)
      :type     weak-pointer )
   (next
      :initform nil
      :type     (or range-anchor null) )
   (prev
      :initform nil
      :type     (or range-anchor null) ))
  (:metaclass structure-class) )


;;;; define-dlinked-anchor
(defmacro define-dlinked-anchor (items item)
  (let ((append (intern (format nil "APPEND-~A" item)))
        (delete (intern (format nil "DELETE-~A" item)))
        (with-iterator (intern (format nil "WITH-~A-ITERATOR" item))) )
   `(progn
      (defclass ,items (editor-object)
        ((head :initform nil :type (or ,item null))
         (tail :initform nil :type (or ,item null)) )
        (:metaclass structure-class) )

      (defun ,append (items item)
        (unless (slot-value items 'head)
          (setf (slot-value items 'head) item) )
        (let ((last (slot-value items 'tail)))
          (if last
              (setf (slot-value last 'next) item)
            (setf (slot-value items 'last) item) ) ) )

      (defun ,delete (items item)
        (let ((next (shiftf (slot-value item 'next) nil))
              (prev (shiftf (slot-value item 'prev) nil)) )
          (if next
              (setf (slot-value next 'prev) prev)
            (setf (slot-value items 'prev) prev) )
          (if prev
              (setf (slot-value prev 'next) next)
            (setf (slot-value items 'next) next) ) ) )

      (defmacro ,with-iterator ((next items-form) &body body)
       `(let ((.runner (slot-value ,items-form 'head)))
          (macrolet (
            (,next ()
             `(let ((item .runner))
                (when item
                  (setq .runner (slot-value .runner 'next))
                  item ) ) ) )
            ,@body ) ) )
      ',items ) ) )

(define-dlinked-anchor intervals interval)
(define-dlinked-anchor range-anchors range-anchor)

(defmacro for-each ((var items-form) &body body)
`(let ((.runner (slot-value ,items-form 'head)))
  (loop
    (unless .runner (return))
    (let ((,var .runner)) ,@body)
    (setq .runner (slot-value .runner 'next)) ) ) )



(deftype undo-state ()
  '(member :disabled :logging :redo :undo) )

(defconstant edit-journal-flag-no-merge (ash 1 0))
(defconstant edit-journal-flag-truncate (ash 1 1))


;;;; edit-journal-record
(defclass edit-journal-record (editor-object)
  ((next
      :initform nil
      :type     (or edit-journal-record null) )
   (prev
      :initform nil
      :type     (or edit-journal-record null) ))
  (:metaclass structure-class) )

(define-dlinked-anchor edit-journal-records edit-journal-record)


;;;; edit-jorunal
(defclass edit-journal (editor-object)
  ((flags
      :initform 0
      :type     fixnum )
   (records
      :initform (make-instance 'edit-journal-records)
      :type     edit-journal-records )
   (redo
      :initform nil
      :type     (or edit-journal-record null) )
   (state
      :initform :logging
      :type     undo-state )
   (undo
      :initform nil
      :type     (or edit-journal-record null) ))
  (:metaclass structure-class) )


(defconstant buffer-read-only (ash 1 0))
(defconstant buffer-not-ready (ash 1 1))


;;;; edit-buffer
(defclass edit-buffer (text-buffer)
  ((change-tick
        :initform 0
        :type     length )
   (intervals
        :initform (make-instance 'intervals)
        :type     intervals )
   (journal
        :initform (make-instance 'edit-journal)
        :type     edit-journal )
   (ranges
        :initform (make-instance 'range-anchors)
        :type     range-anchors )
   (root-interval
        :type     interval )
   (save-tick
        :initform 0
        :type     length )
   (state
        :initform 0
        :type     fixnum ))
  (:metaclass structure-class) )


;;;; edit-buffer
(declaim
  ;; [B]
  (ftype (function (edit-buffer range) range)
    buffer-add-range )

  (ftype (function (edit-buffer position count) edit-buffer)
    buffer-change )

  (ftype (function (edit-buffer) t)
    buffer-changed-p )

  (ftype (function (t edit-buffer) t)
    (setf buffer-changed-p) )

  (ftype (function (edit-buffer position character length) length)
    buffer-insert-chars )

  (ftype (function (edit-buffer position string
                    &optional sequence-index sequence-end )
                   length )
    buffer-insert-string )

  (ftype (function (edit-buffer) t)
    buffer-read-only-p )

  (ftype (function (t edit-buffer) t)
    (setf buffer-read-only-p) )

  (ftype (function (edit-buffer) t)
    buffer-not-ready-p )

  (ftype (function (t edit-buffer) t)
    (setf buffer-not-ready-p) )

  ;; [G]
  (ftype (function (edit-buffer position) interval)
    get-interval )

  ;; [I]
  (ftype (function (edit-buffer interval) interval)
    insert-interval )

  ;; [M]
  (ftype (function (edit-buffer &optional position position) range)
    make-range )

  ;; [S]
  (ftype (function (edit-buffer position position style) edit-buffer)
    set-text-style )

  ;; [V]
  (ftype (function (edit-buffer) null)
    validate-buffer-not-read-only )

  (ftype (function (edit-buffer) null)
    validate-buffer-ready )
 )


;;;; edit-journal
(declaim
  (ftype (function (edit-buffer) edit-journal)
         edit-journal-check-point )
  (ftype (function (edit-buffer position position) edit-journal)
         edit-journal-record-delete )
  (ftype (function (edit-buffer position position) edit-journal)
         edit-journal-record-insert )
 )


;;;; btree
(declaim
  (ftype (function (interval interval) interval)
    btree-delete )

  (ftype (function (interval interval) interval)
    btree-insert )

  (ftype (function (interval) interval)
    btree-rotate-left )

  (ftype (function (interval) interval)
    btree-rotate-right )
  )

;;;; interval
(declaim
  ;; [A]
  (ftype (function (intervals interval) interval)
    append-interval )

  ;; [C]
  (ftype (function (interval &key (:end position)
                                  (:start position)
                                  (:style style) )
                   interval )
    copy-interval )

  ;; [D]
  (ftype (function (intervals interval) interval)
    delete-interval )
 )


;;;; range-anchor
(declaim
  ;; [A]
  (ftype (function (range-anchors range-anchor) range-anchor)
    append-range-anchor )

  ;; [D]
  (ftype (function (range-anchors range-anchor) range-anchor)
    delete-range-anchor )
 )


;;;; range
(declaim
  ;; [R]
  (ftype (function (range) position)
    range-end )

  (ftype (function (range) position)
    range-start )
 )
