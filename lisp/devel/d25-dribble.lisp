;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;;; evcl - Dev - 25 Environment - Dribble
;; dev/d25-dribble.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/devel/d25-dribble.lisp#2 $
;;;
;;; Description:
;;;  This file contains implementation of redirection style dribble function.
;;;
;;; BUGBUG: REVIEW: We should compare redirection and recursive style dribble.
;
(in-package :devel)

;;;; *dribble-original-input*
;;;
;;; Description:
;;;  Original value of *standard-input*.
;
(ext:deftlv *dribble-original-input* nil)


;;;; *dribble-original-output*
;;;
;;; Description:
;;;  Original value of *standard-output*.
;
(ext:deftlv *dribble-original-output* nil)


;;;; *dribble-output*
;;;
;;; Description:
;;;  Output stream of dribble file.
;
(ext:deftlv *dribble-output* nil)


;;;; 25.2.19 dribble
;
(defun cl:dribble (&optional pathname)
  (labels (
    ;; end-dribble
    ;;
    (end-dribble ()
      (fresh-line)
      (format t "; Dribble file ~S closed.~%" (pathname *dribble-output*))

       (close *standard-input*)
       (close *standard-output*)

      (shiftf *standard-output* *dribble-original-output* nil)
      (shiftf *standard-input*  *dribble-original-input* nil)

      (multiple-value-bind (s n h d m y) (get-decoded-time)
        (format *dribble-output*
                ";;;; Dribble end at ~D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D~%"
                y m d
                h n s ) )

      (close *dribble-output*)
      (setq *dribble-output* nil) )

    ;; start-dribble
    ;;
    (start-dribble ()
      (fresh-line *dribble-output*)

      (multiple-value-bind (s n h d m y) (get-decoded-time)
        (format *dribble-output*
                ";;;; Dribble start at ~D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D~%"
                y m d
                h n s ) )

      (format *dribble-output*
              "; Running on ~A (~A ~A)~%"
              (machine-instance)
              (machine-type)
              (machine-version) )

      (format *dribble-output*
              "; ~A ~A on ~A ~A~%"
              (lisp-implementation-type)
              (lisp-implementation-version)
              (software-type)
              (software-version) ) )
    )
    ;;
    ;; dribble
    ;;
    (if (null pathname)
        (if (null *dribble-output*)
            (format t "; No dribble was in progress~%")
          (end-dribble) )
      (let ((out (open pathname
                       :direction         :output
                       :if-exists         :supersede
                       :if-does-not-exist :create )))
        (format t "; Dribbling to file ~S~%" pathname)

        (if *dribble-output*
            (end-dribble)
          (progn
            (setq *dribble-original-output* *standard-output*)
            (setq *dribble-original-input*  *standard-input*) ))

        (setq *dribble-output* out)

        (setq *standard-output*
          (make-broadcast-stream *dribble-original-output* out))

        (setq *standard-input*
          (make-echo-stream *dribble-original-input* out) )

        (start-dribble) ))
    (values) ) )
