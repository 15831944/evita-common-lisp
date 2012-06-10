;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Runtime - 51 Networking
;;; runtime/net/net-socket-stream.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/net/net-socket-stream.lisp#2 $
;;;
;;;
;;; Description:
;;;  TBD
;
(in-package :net)

(xc::define-unsafe-accessors socket-stream)


(defmethod ext:stream-line-column ((stream socket-stream))
    (declare (values ext:sequence-index))
  (.socket-stream-send-column stream) )


(defmethod ext:stream-start-line-p ((stream socket-stream))
    (declare (values ext:sequence-index))
  (zerop (.socket-stream-send-column stream)) )


(defclass ip-end-point (end-point)
  ((blob :type blob))
  (:metaclass structure-class) )


#|
struct sockaddr_in
{
    sint16  sin_family;         // +0
    uint16  sin_port;           // +2 (network byte order)
    in_addr sin_addr;           // +4
    sint8   siz_zero[8];
};
|#

;;;; print-object ip-end-point
;
(defmethod print-object ((o ip-end-point) s)
    (declare (type stream stream))
  (labels (
    ;; print-address
    (print-address (o s)
      (let* ((a (slot-value o 'blob))
             (port (logior (ash (aref a 2) 8) (aref a 3))) )
          (declare (type blob a))
        (format s "~D.~D.~D.~D~@[:~D~]"
            (aref a 4) (aref a 5) (aref a 6) (aref a 7)
            (and (not (zerop port)) port) ) ) )
    )
    ;;
    (if (not *print-escape*)
        (print-address o s)
      (print-unreadable-object (o s)
        (write-string "IP-End-Point " s)
        (print-address o s) ))
    o ) )


;;;; print-object socket-stream
;
(defmethod cl:print-object ((o net:socket-stream) s)
    (declare (type stream s))
  (cond
    ((slot-value o 'net::remote)
      (format s "#<Socket active ~A>" (slot-value o 'net::remote)) )
    ((slot-value o 'net::local)
      (format s "#<Socket passive ~A>" (slot-value o 'net::local)) )
    (t
      (print-unreadable-object (o s :type t :identity t)) )) )


;;;; make-socket
;
(defun make-socket (&key local remote (external-format :default))
    (declare (type (or null ip-end-point) local remote))
    (declare (values socket-stream))
  (setq external-format (si::ensure-external-format external-format))
  (let ((s (make-instance 'socket-stream
                          :local local
                          :remote remote
                          :external-format external-format ) ))
    (ext:realize-instance s)
    (when remote
      (setf (si::.stream-flags s) si::STREAM-FLAG-BOTH)

      (let ((bufsiz 1024))
        (setf (slot-value s 'recv-max) bufsiz)
        (setf (slot-value s 'recv-buffer) (si::.allocate-foreign bufsiz))

        (setf (slot-value s 'send-max) bufsiz)
        (setf (slot-value s 'send-buffer) (si::.allocate-foreign bufsiz)) ))
    s ) )


#|

;;; Active socket

(setq h (net:ip-end-point "localhost" 1234))


(setq h (net:ip-end-point "localhost" 80))
(setq s (net:make-socket :remote h))
(format s "GET / HTTP/1.0~%")
(format s "Host: localhost~%")
(terpri s)
(force-output s)
(loop for line = (read-line s nil) while line do (format t "~A~%" line))


;;; Passive socket
(setq h (net:ip-end-point :any 1234))
(setq s (net:make-socket :local h))
(setq c (net:accept s))
(loop for line = (read-line c nil) while line do (format t "~A~%" line))


(setq h (net:ip-end-point "localhost" 1234))
(setq s (net:make-socket :remote h))
(format s "foo")
(force-output s)
|#
