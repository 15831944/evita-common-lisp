(in-package :win32)

(defun initialize ()
  (unless *context*
    (setq *context*
     (make-instance 'context
        :event-function
            (lambda (window umsg wparam lparam)
                (declare (lambda-name :event-function))
              (let ((result (window-procedure window umsg wparam lparam)))
                (when (eql |WM_NCDESTROY| uMsg)
                  (setf (slot-value window 'hwnd) 0) )
                result ) )
        :idle-function
            (lambda (context counter)
                (declare (lambda-name :idle-function))
              (let ((morep nil))
                (dolist (application (slot-value context 'applications) morep)
                  (when (idle-procedure application counter)
                    (setq morep t) ) ) ) )))
    (initialize-windowing *context*) ) )



(defmethod cl:print-object ((o window) s)
  (print-unreadable-object (o s :type t)
    (let ((rc (window-rect o)))
      (format s "hwnd=~X ~D+~D+~Dx~D"
          (slot-value o 'hwnd)
          (rect-left   rc)
          (rect-top    rc)
          (rect-right  rc)
          (rect-bottom rc) ) ) )
  o )


(defmethod cl:print-object ((o windowing-object) s)
  (print-unreadable-object (o s :type t :identity t))
  o )


;;;; window-procedure window
;;; Returns nil to call DefWindowProc.
(defmethod window-procedure ((window window) uMsg wParam lParam)
    (declare (ignore uMsg wParam lParam))
  nil )

;;;; window-height
(defun window-height (window)
    (declare (values fixnum))
    (declare (type window))
  (rect-height (slot-value window 'rect)) )


;;;; window-rect
(defun window-rect (window)
    (declare (values rect))
    (declare (type window window))
  (slot-value window 'rect) )

;;;; window-width
(defun window-width (window)
    (declare (values fixnum))
    (declare (type window))
  (rect-width (slot-value window 'rect)) )

