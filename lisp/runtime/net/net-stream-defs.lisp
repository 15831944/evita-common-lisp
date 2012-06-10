;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: System; Base: 10 -*-
;;;;
;;;; evcl - Runtime - 51 Networking
;;; runtime/net/net-stream-defs.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/runtime/net/net-stream-defs.lisp#2 $
;;;
;;;
;;; Description:
;;;  TBD
;
(in-package :net)

(deftype address-family () '(member :unspec :unix :inet))
    ;; AF_UNSPEC    0
    ;; AF_UNIX      1
    ;; AF_INET      2

(deftype socket-type () '(member :dgram :raw :stream))
    ;; SOCK_STREAM      1
    ;; SOCK_DGRAM       2
    ;; SOCK_RAW         3
    ;; SOCK_RDM         4
    ;; SOCK_SEQPACKET   5

(deftype protocol-type () '(member :tcp :udp :ip :default))
    ;; IPPROTO_IP   0
    ;; IPPROTO_ICMP 1
    ;; IPPROTO_IGMP 2
    ;; IPPROTO_GGP  3
    ;; IPPROTO_TCP  6
    ;; IPPROTO_UDP  17
    ;; IPPROTO_RAW  255

(deftype buffer () '(simple-array (unsigned-byte 8) *))
(deftype blob () '(simple-array (unsigned-byte 8) *))


;;; Note: I/O direction isn't set until make-socket with remote, accept
;;; or connect.
;
(defclass socket-stream (si:platform-stream)
  ((si::external-format :initform 0)
   (local
     :initarg :local
     :initform nil
     :type (or ip-end-point null) )
   (remote
     :initarg :remote
     :initform nil
     :type (or ip-end-point null) )

   (recv-length
     ;; Receive buffer index
     :initform  0
     :type      ext:sequence-index )
   (recv-index
     ;; Receive buffer index
     :initform  0
     :type      ext:sequence-index )
   (recv-max
      :initform 0
      :type     ext:sequence-index )
   (recv-buffer
     ;; Receive buffer allocated in foreign area.
     :initform  0
     :type      fixnum )
   (recv-unread-1
      ;; unread-char
      :initform nil
      :type     (or null character) )

   (send-column
     ;; Send buffer index
     :initform  0
     :type      ext:sequence-index )
   (send-index
     ;; Send buffer index
     :initform  0
     :type      ext:sequence-index )
   (send-max
      :initform 0
      :type     ext:sequence-index )
   (send-buffer
     ;; Send buffer allocated in foreign area.
     :initform 0
     :type     fixnum ) ) )
