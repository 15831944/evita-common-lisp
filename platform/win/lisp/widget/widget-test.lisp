(in-package :cl-user)

(w:defapplication test-application w:application)

(w:defwidget test-frame w:application-widget
  (counter :initform 0 :type fixnum) )

(defmethod w:on-widget-draw ((w test-frame))
  (let ((rc (w:window-rect w)))
    (win32::|FillRect| w rc win32::+brush.btnface+)
    (win32::|ExtTextOut| w 100 100 0 rc "This is a test widget.") ) )

(defun test-it ()
  (win32::initialize)
  (let ((w (make-instance 'test-frame)))
    (w:realize-widget w) ) )

#|

cd "/proj/evcl3/platform/win/lisp/widget"
ld "widget-test"

|#
