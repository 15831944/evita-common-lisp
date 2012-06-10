;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; evcl - clos - built-in-class
;;; arch/evm/lisp/clos/evm-o09-built-in-class.lisp
;;;
;;; This file is part of Evita Common Lisp.
;;;
;;; Copyright (C) 1996-2006 by Project Vogue.
;;; Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
;;;
;;; @(#)$Id: //proj/evcl3/mainline/lisp/clos/o09-built-in-class.lisp#2 $
;;;
;;; Description:
;;; This file contains built-in-class definitions.
;
(in-package :si)


;;;; array
;
#+nil
(defclass cl:array (t)
  ((classd          :type class-description)
   (rank            :type array-rank)
   (flags           :type fixnum)
   (displaced-to    :type (or array null))
   (offset          :type ext:sequence-index)
   (dimension       :type sequence-index :allocation :rest) )
  (:metaclass built-in-class) )


;;;; class-description
;;;
;;; Slots:
;;;   slots     -- A list of effective-slot-definition objects.
;
(defclass class-description (t)
  ((classd          :type class-description)
   (class           :type class)
   (hash-code       :type (unsigned-byte 17))
   (slots           :type list)
   (detail          :type fixnum)
   (type-code       :type (unsigned-byte 8))
   (format-hint     :type ext:sequence-index) )
  (:metaclass built-in-class) )


;;;; complex
;
(defclass cl:complex (number)
  ((classd      :type class-description)
   (zero        :type fixnum)
   (realpart    :type real)
   (imagpart    :type real) )
  (:metaclass built-in-class) )


;;;; cons
;
(defclass cl:cons (list)
  ((car)
   (cdr) )
  (:metaclass built-in-class) )


;;;; hash-table
;
(defclass cl:hash-table (t)
  ((classd              :type class-description)
   (vector              :type simple-vector)
   (test                :type (member eq eql equal equalp))
   (rehash-size         :type (or single-float (unsigned-byte 20))) )
  (:metaclass built-in-class) )


;;;; instance
(defclass instance (t)
  ((classd  :type class-description)
   (storage) )
  (:metaclass built-in-class) )


;;;; ext:latch
;
(defclass ext:latch (t)
  ((classd      :type class-description)
   (name        :type t)
   (thread      :type fixnum)
   (state       :type t)
   (lock-count  :type fixnum)
   (spin-lock   :type fixnum) )
  (:metaclass built-in-class) )


;;;; ext:mutex
;
(defclass ext:mutex (t)
  ((classd      :type class-description)
   (name        :type t)
   (thread      :type fixnum)
   (state       :type t) )
  (:metaclass built-in-class) )


;;;; value-cell
;
(defclass value-cell (t)
  ((classd      :type class-description)
   (value       :type t)
   (name        :type symbol)
   (type        :type (member defvar setf symbol-plist)) )
  (:metaclass built-in-class) )


;;;; package
;
(defclass cl:package (t)
  ((classd                  :type class-description)
   (internal-table          :type simple-vector)
   (external-table          :type simple-vector)
   (names                   :type list)
   (use-list                :type list)
   (used-by-list            :type list)
   (shadowing-symbols       :type list)
   (protect                 :type symbol) )
  (:metaclass built-in-class) )


;;;; pointer
;
(defclass ext:pointer (t)
  ((classd      :type class-description)
   (address     :type fixnum)
   (type        :type fixnum)
   (base        :type t) )
  (:metaclass built-in-class) )


;;;; ratio
;
(defclass cl:ratio (rational)
  ((classd      :type class-description)
   (zero        :type fixnum)   ; for alignment
   (numerator   :type integer)
   (denominator :type integer) )
  (:metaclass built-in-class) )


;;;; readtable
;
(defclass cl:readtable (t)
  ((classd  :type class-description)
   (case    :type (member :downcase :upcase :preserve :invert))
   (vector  :type simple-vector)
   (table   :type hash-table) )
  (:metaclass built-in-class) )


;;;; symbol
;
(defclass cl:symbol (t)
  ((hash-code :type fixnum)
   (function  :type function)
   (name      :type simple-string)
   (package   :type (or package null)) )
  (:metaclass built-in-class) )


;;;; plist-cell
;
(defclass plist-cell (t)
  ((classd      :type class-description)
   (plist)
   (name) )
  (:metaclass built-in-class) )


;;;; setf-cell
;
(defclass setf-cell (t)
  ((classd      :type class-description)
   (function)
   (name) )
  (:metaclass built-in-class) )


;;;; value-cell
;
(defclass value-cell (t)
  ((classd      :type class-description)
   (value)
   (name)
   (type) )
  (:metaclass built-in-class) )


;;;; vector
;;;
;;; string, bit-vector
;;;
;;; Since subclasses of vector such as simple-vector don't inherit
;;; slots.
;
#+nil
(defclass cl:vector (array sequence)
  ((classd              :type class-description)
   (fill-pointer        :type sequence-index)
   (flags               :type fixnum)
   (displaced-to        :type array)
   (displaced-offset    :type sequence-index)
   (total-size          :type array-total-size) )
  (:metaclass built-in-class) )


;;;; ext:weak-cons
;
(defclass ext:weak-cons (t)
  ((classd      :type class-description)
   (next    :type (or ext:weak-cons null))
   (car     :type t)
   (cdr     :type t) )
  (:metaclass built-in-class) )


;;;; ext:weak-pointer
;
(defclass ext:weak-pointer (t)
  ((classd      :type class-description)
   (value       :type t) )
  (:metaclass built-in-class) )



;;;; xc::environment
;;;
;;; Description:
;;;  Represents global environment.
;
(defclass xc::environment (structure-object)
  ((outer        :type (or xc::environment null))   ; [1]
   (variables    :type hash-table)                  ; [2]
   (functions    :type hash-table)                  ; [3]
   (writers      :type hash-table)                  ; [4]
   (types        :type hash-table)                  ; [5]
   (classes      :type hash-table)                  ; [6]
   (others       :type hash-table)                  ; [7]
   (local-p      :type boolean) )                   ; [8]
  (:metaclass structure-class) )


;;;; frame-iterator
;;;
;;; Description:
;;;  Represents function frame iterator.
;
(defclass frame-iterator (structure-object)
  ((function :type (or function null))
   (ip       :type fixnum)
   (fp       :type fixnum)
   (sp       :type fixnum) )
  (:metaclass structure-class) )
