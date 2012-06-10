(in-package :cl-user)

(defpackage :widget
  (:nicknames #:w)
  (:use :common-lisp :extension)
  (:export
    ;; [A]
    #:add-child
    #:application
    #:application-widget
    ;; [C]
    #:composite-widget
    ;; [D]
    #:defapplication
    #:defwidget
    #:draw-text
    ;; [G]
    #:get-text-extent
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
    #:make-rect
    ;; [O]
    #:on-widget-draw
    #:on-widget-resize
    ;; [P]
    #:pane-widget
    ;; [R]
    #:realize-font
    #:realize-widget
    #:rect
    #:rect-bottom
    #:rect-height
    #:rect-left
    #:rect-right
    #:rect-top
    #:rect-width
    ;; [S]
    #:set-position
    #:status-bar-parts
    #:status-bar-simple-p
    #:status-bar-text
    ;; [T]
    #:toplevel-widget
    ;; [W]
    #:widget
    #:window-height
    #:window-rect
    #:window-width
    ;; [X]
    #:x-point
    ;; [Y]
    #:y-point )
  (:import-from :win32
    ;; [F]
    #:font
    #:font-ascent
    #:font-descent
    #:font-height
    #:font-width
    ;; [I]
    #:idle-procedure
    ;; [M]
    #:make-font
    #:make-rect
    ;; [R]
    #:realize-font
    #:rect
    #:rect-bottom
    #:rect-height
    #:rect-left
    #:rect-right
    #:rect-top
    #:rect-width
    ;; [W]
    #:window-height
    #:window-rect
    #:window-width
    ;; [X]
    #:x-point
    ;; [Y]
    #:y-point )
  ) ; defpackage
