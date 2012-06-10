(in-package :win32)

(deftype intptr () '(signed-byte #+32bit 32 #+64bit 64))

(deftype hbrush () 'intptr)
(deftype hdc    () 'intptr)
(deftype hpen   () 'intptr)
(deftype hwnd   () 'intptr)


;;;; point
(defclass point (foreign-object)
  ((x :type int32)
   (y :type int32) )
  (:metaclass foreign-class) )


;;;; rect
(defclass rect (foreign-object)
  ((left   :type int32)
   (top    :type int32)
   (right  :type int32)
   (bottom :type int32) )
  (:metaclass foreign-class) )

;;;; size
(defclass point (foreign-object)
  ((x :type int32)
   (y :type int32) )
  (:metaclass foreign-class) )


;;;; windowing-object
(defclass windowing-object (structure-object)
  ()
  (:metaclass structure-class) )

;;;; window
(defclass window (windowing-object)
  ((hwnd :type hwnd))
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
      :initform (fli:allocate-object (find-class 'MSG))
      :type     (pointer MSG) )
   (plist
      :initform '()
      :type     list ))
  (:metaclass structure-class) )
