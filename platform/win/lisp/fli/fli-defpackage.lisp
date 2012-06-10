(in-package :cl-user)

(defpackage :fli
  (:use :common-lisp :extension)
  (:export
    ;; [A]
        #:allocate-foreign-object
    ;; [C]
        #:call
        #:copy-byte-16
        #:copy-byte-32
        #:copy-byte-8
        #:cstring
    ;; [D]
        #:defctype
        #:define
    ;; [F]
        #:fill-byte-16
        #:fill-byte-32
        #:fill-byte-8
        #:free-foreign-object
    ;; [I]
        #:int
        #:int*
        #:int16
        #:int16*
        #:int32
        #:int32*
        #+64bit #:int64
        #+64bit #:int64*
        #:int8
        #:int8*
    ;; [M]
        #:move-byte-16
        #:move-byte-32
        #:move-byte-8
    ;; [N]
        #:null-pointer-p
    ;; [P]
        #:pointer-eq
    ;; [U]
        #:uint
        #:uint*
        #:uint16
        #:uint16*
        #:uint32
        #:uint32*
        #+64bit #:uint64
        #+64bit #:uint64*
        #:uint8
        #:uint8*
    ;; [V]
        #:value
        #:void
        #:void*
   ) ; export
   (:import-from :system
    ;; [I]
        #:int
        #:int*
        #:int16
        #:int16*
        #:int32
        #:int32*
        #+64bit #:int64
        #+64bit #:int64*
        #:int8
        #:int8*
    ;; [O]
        #:offsetof
    ;; [S]
        #:sizeof
    ;; [T]
        #:tagof
    ;; [U]
        #:uint
        #:uint*
        #:uint16
        #:uint16*
        #:uint32
        #:uint32*
        #+64bit #:uint64
        #+64bit #:uint64*
        #:uint8
        #:uint8*
    ;; [V]
        #:value
        #:void
        #:void*
   ) ; import-from
 ) ; defpackage
