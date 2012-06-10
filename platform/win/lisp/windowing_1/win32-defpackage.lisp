(in-package :cl-user)

(defpackage :win32
  (:use :common-lisp :extension)
  (:export
    ;; [!]
    #:!draw-text
    #:!get-text-extent
    ;; [+]
    #:+bottom-window+
    #:+null-window+
    #:+no-top-most-window+
    #:+top-most-window+
    #:+top-window+
    ;; [C]
    #:create-window
    ;; [F]
    #:font
    #:font-ascent
    #:font-descent
    #:font-height
    #:font-width
    ;; [I]
    #:initialize
    ;; [M]
    #:make-font
    ;; [R]
    #:realize-font
    #:rect
    #:rect-bottom
    #:rect-height
    #:rect-left
    #:rect-right
    #:rect-top
    #:rect-width
    ;; [S]
    #:status-bar-parts
    #:status-bar-text
    #:status-bar-simple-p
    ;; [W]
    #:window
    #:window-height
    #:window-procedure
    #:window-rect
    #:window-width
    ;; [X]
    #:x-point
    ;; [Y]
    #:y-point )
  (:import-from :system
    ;; [+]
    +null-pointer+
    ;; [A]
    #:allocate-foreign-object
    ;; [F]
    #:foreign-class
    #:foreign-object
    #:foreign-pointer
    #:free-foreign-object
    ;; [I]
    #:int
    #:int16
    #:int32
    #:int8
    ;; [L]
    #:lambda-name
    ;; [N]
    #:null-pointer-p
    ;; [O]
    #:offsetof
    ;; [P]
    #:pointer
    #:pointer-eq
    #:ptr
    ;; [S]
    #:sizeof
    ;; [T]
    #:tagof
    ;; [U]
    #:uint
    #:uint16
    #:uint32
    #:uint8
    #:vec
    ;; [V]
    #:value ) )
