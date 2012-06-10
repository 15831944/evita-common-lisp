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

;;;; copy-interval
(defun copy-interval (intv &key (end   (slot-value intv 'end))
                                (start (slot-value intv 'start))
                                (style (slot-value intv 'style)) )
    (declare (values interval))
    (declare (type interval intv))
  (make-instance 'interval
        :end   end
        :start start
        :style style ) )


;;;; get-interval
(defun get-interval (buffer posn)
    (declare (values interval))
    (declare (type edit-buffer buffer))
    (declare (type position posn))
  (labels (
    (find (tree)
 (format t "; find ~S~%" tree)
      (cond
        ((<= (slot-value tree 'start) posn (1- (slot-value tree 'end)))
          tree )
        ((< posn (slot-value tree 'start))
          (find (slot-value tree 'left)) )
        (t
          (find (slot-value tree 'right)) )) )
    )
    ;;
    (find (slot-value buffer 'root-interval)) ) )


;;;; insert-interval
(defun insert-interval (buffer intv)
    (declare (values interval))
    (declare (type edit-buffer buffer))
    (declare (type interval intv))
  (let* ((prev (get-interval buffer (slot-value intv 'start)))
         (next (slot-value prev 'next)) )
    (setf (slot-value prev 'next) intv)
    (when next (setf (slot-value next 'prev) intv))
    (setf (slot-value intv 'prev) prev)
    (setf (slot-value intv 'next) next)
    (setf (slot-value buffer 'root-interval)
      (btree-insert (slot-value buffer 'root-interval) intv) )
    intv ) )


;;;; print-object interval
(defmethod cl:print-object ((o interval) s)
  (print-unreadable-object (o s :type t :identity t)
    (format s "[~D ~D]" (slot-value o 'start) (slot-value o 'end)) ) )


;;;; set-text-style
(defun set-text-style (buffer start end style)
    (declare (values edit-buffer))
    (declare (type edit-buffer buffer))
    (declare (type position start end))
    (declare (type style style))
  (validate-positions buffer start end)
  (let* ((sintv (get-interval buffer start))
         (eintv (get-interval buffer (1- end)))
         (intv sintv) )
    (loop
      (unless (eq (slot-value intv 'style) style)
        (let ((iend (shiftf (slot-value intv 'end) start)))
          (insert-interval buffer
                          (copy-interval intv
                                         :end    end
                                         :start  start
                                         :style  style ))
          (unless (eql iend end)
            (insert-interval buffer
                             (copy-interval intv
                                            :end   iend
                                            :start end
                                            :style style ))) ))
      (when (eq intv eintv) (return buffer))
      (setq intv (slot-value intv 'next)) ) ) )
