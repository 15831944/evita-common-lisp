(in-package :win32)

(deftype x-point () '(signed-byte 16))
(deftype y-point () '(signed-byte 16))


;;;; windowing-object
(defclass windowing-object (structure-object)
  ()
  (:metaclass structure-class) )

;;;; graphic-object
(defclass graphic-object (windowing-object)
  ((handle
    :initarg  :handle
    :initform (make-handle)
    :type     handle ))
  (:metaclass structure-class) )

;;;; brush
(defclass brush (graphic-object)
  ()
  (:metaclass structure-class) )


;;;; font
(defclass font (graphic-object)
  ((handle
      :initform nil
      :type     (or si::handle null) )
   (logfont
      :initform +null-pointer+
      ;; FIXME 2007-07-07 yosi@msn.com We should have (pointer class).
      :type     (pointer LOGFONT) )
   (text-metric
      :initform +null-pointer+
      :type     (pointer TEXTMETRIC) ))
  (:metaclass structure-class) )


;;;; drawable
(defclass drawable (windowing-object)
  ((hdc
        :initform (make-handle)
        :type     si::handle ))
  (:metaclass structure-class) )


;;;; window
(defclass window (drawable)
  ((hwnd
        :initarg  :handle
        :initform 0
        :type     fixnum )
   (rect
        :initform (make-rect)
        :type     rect ))
  (:metaclass structure-class) )


;;;; context
(defclass context (windowing-object)
  ((applications
      :initform '()
      :type     list )
   (creation
      :type window )
   (event-function
      :initarg :event-function
      :type    function )
   (idle-function
      :initarg :idle-function
      :type    function )
   (instance
      :initform 0
      :type     fixnum )
   (msgbuf
      ;; FIXME 2007-07-01 We should allocate MSG in foreign heap instead of
      ;; lisp heap.
      :initform (make-blob (sizeof 'win32-MSG))
      :type (blob #.(sizeof 'win32-MSG)) )
   (plist
      :initform '()
      :type     list ))
  (:metaclass structure-class) )


(deftlv *context* nil)

(defgeneric window-procedure (window uMsg wParam lParam))

(defgeneric idle-procedure (application fixnum)
  (:method (a c) (declare (ignore a c))) )

(declaim
  (ftype (function (window &key (:class   string)
                                (:ctrl-id fixnum)
                                (:exstyle fixnum)
                                (:height  (or fixnum null))
                                (:parent  window)
                                (:style   fixnum)
                                (:title   string)
                                (:width   (or fixnum null))
                                (:x       (or fixnum null))
                                (:y       (or fixnum null)) )
                    window )
    create-window )

  ;; [I]
  (ftype (function () t)
    initialize )

  (ftype (function (context) t)
    initialize-context )

  ;; [W]
  (ftype (function (window) fixnum)
    window-height )

  (ftype (function (window) rect)
    window-rect )

  (ftype (function (window) fixnum)
    window-width )
 ) ; declaim

;;; For SetWindowPos
(defvar +null-window+        (make-instance 'window))
(defvar +top-window+         (make-instance 'window))
(defvar +bottom-window+      (make-instance 'window :handle 1))
(defvar +top-most-window+    (make-instance 'window :handle -1))
(defvar +no-top-most-window+ (make-instance 'window :handle -2))

(macrolet (
  (define (name value)
    `(defvar ,(intern (format nil "+BRUSH.~A+" name))
             (make-instance 'brush :handle (make-handle ,value)) ) )
  )
    (define SCROLLBAR         0)
    (define BACKGROUND        1)
    (define ACTIVECAPTION     2)
    (define INACTIVECAPTION   3)
    (define MENU              4)
    (define WINDOW            5)
    (define WINDOWFRAME       6)
    (define MENUTEXT          7)
    (define WINDOWTEXT        8)
    (define CAPTIONTEXT       9)
    (define ACTIVEBORDER      10)
    (define INACTIVEBORDER    11)
    (define APPWORKSPACE      12)
    (define HIGHLIGHT         13)
    (define HIGHLIGHTTEXT     14)
    (define BTNFACE           15)
    (define BTNSHADOW         16)
    (define GRAYTEXT          17)
    (define BTNTEXT           18)
    (define INACTIVECAPTIONTEXT 19)
    (define BTNHIGHLIGHT      20)
    (define 3DDKSHADOW        21)
    (define 3DLIGHT           22)
    (define INFOTEXT          23)
    (define INFOBK            24)
    (define HOTLIGHT          26)
    (define GRADIENTACTIVECAPTION 27)
    (define GRADIENTINACTIVECAPTION 28)
    (define MENUHILIGHT       29)
    (define MENUBAR           30)
 ) ; macrolet

(defvar +brush.DESKTOP+ +brush.BACKGROUND+)
(defvar +brush.3DFACE+ +brush.BTNFACE+)
(defvar +brush.3DSHADOW+ +brush.BTNSHADOW+)
(defvar +brush.3DHIGHLIGHT+ +brush.BTNHIGHLIGHT+)
(defvar +brush.3DHILIGHT+ +brush.BTNHIGHLIGHT+)
(defvar +brush.BTNHILIGHT+ +brush.BTNHIGHLIGHT+)
