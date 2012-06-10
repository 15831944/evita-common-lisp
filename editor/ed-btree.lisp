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

;; btree-delete
(defun btree-delete (tree intv)
    (declare (values interval))
    (declare (type interval tree intv))
  (labels (
    ;; btree-delete-aux
    (btree-delete-aux (node)
        (declare (values interval))
        (declare (type interval node))
      (let ((left  (slot-value node 'left))
            (right (slot-value node 'right)) )
        (cond
          ((null left)
            right )
          ((null right)
            left )
          ((< (slot-value left 'priority) (slot-value right 'priority))
            (let ((node (btree-rotate-right node)))
              (setf (slot-value node 'right) (btree-delete-aux node))
              node ) )
          (t
            (let ((node (btree-rotate-left node)))
              (setf (slot-value node 'left) (btree-delete-aux node))
              node ) )) ) )
    )
  (cond
    ((eq tree intv)
      (btree-delete-aux tree) )
    ((< (slot-value intv 'start) (slot-value tree 'start))
      (setf (slot-value tree 'left)
        (btree-delete (slot-value tree 'left) intv) )
      tree )
    (t
      (setf (slot-value tree 'right)
        (btree-delete (slot-value tree 'right) intv) )
      tree )) ) )


;;;; btree-insert -- insert intv to binary-tree.
(defun btree-insert (tree intv)
    (declare (values interval))
    (declare (type interval tree intv))
  (cond
    ((null tree)
      intv )
    ((< (slot-value intv 'start) (slot-value tree 'start))
      (let ((left (btree-insert (slot-value tree 'left) intv)))
        (setf (slot-value tree 'left) left)
        (if (> (slot-value left 'priority) (slot-value tree 'priority))
            (btree-rotate-right tree)
          tree ) ) )
    (t
      (let ((right (btree-insert (slot-value tree 'right) intv)))
        (setf (slot-value tree 'right) right)
        (if (> (slot-value right 'priority) (slot-value tree 'priority))
            (btree-rotate-left tree)
          tree ) ) )) )


;;;; btree-rotate-left
(defun btree-roate-left (node)
    (declare (interval node))
  (let ((right (slot-value node 'right)))
    (shiftf (slot-value node 'right) (slot-value right 'left) node) ) )


;;;; btree-rotate-right
(defun btree-roate-right (node)
    (declare (interval node))
  (let ((left (slot-value node 'left)))
    (shiftf (slot-value node 'left) (slot-value left 'right) node) ) )
