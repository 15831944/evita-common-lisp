(in-package :win32)

(defclass hello-window (window)
  ((font :type font)
   (status-bar :type window) )
  (:metaclass structure-class) )

(defclass status-bar (window) () (:metaclass structure-class))

(declaim
  (ftype (function (list status-bar) list)
    (setf status-bar-parts) )

  (ftype (function (t status-bar) list)
    (setf status-bar-simple-p) )

  (ftype (function (simple-string status-bar &optional fixnum) list)
    (setf status-bar-text) )

 ) ;declaim

;;;; status-bar-parts
(defun status-bar-parts (status-bar)
  (let* ((nparts (|SendMessage/ii| status-bar SB_GETPARTS 0 -1))
         (partv  (make-array nparts :element-type '(signed-byte 32))) )
    (|SendMessage/is| status-bar SB_GETPARTS nparts partv)
    (coerce partv 'list) ) )


;;;; (setf status-bar-parts)
(defun (setf status-bar-parts) (parts status-bar)
  (let* ((nparts (length parts))
         (partv  (make-array nparts
                      :element-type '(signed-byte 32)
                      :initial-contents parts )) )
      (|SendMessage/is| status-bar SB_SETPARTS nparts partv)
      parts ) )


;;;; (setf status-bar-simple-p)
(defun (setf status-bar-simple-p) (simplep status-bar)
  (|SendMessage/ii| status-bar SB_SIMPLE (if simplep 1 0) 0) )


;;;; (setf status-bar-text)
(defun (setf status-bar-text) (text status-bar &optional (type SB_SIMPLEID))
  (|SendMessage/is| status-bar SB_SETTEXT type text) )


(defmethod window-procedure ((w hello-window) uMsg wParam lParam)
    (declare (ignore wParam lParam))
  (labels (
    ;; onCreate
    (onCreate ()
      (let ((font (make-font :family #+nil "Consolas"
                                     "Times New Roman"
                                     #+nil "Vardana"
                             :italic t
                             :underline t
                             :height 50 )))
        (realize-font font)
        (setf (slot-value w 'font) font) )
      (let ((status-bar (make-instance 'status-bar)))
        (setf (slot-value w 'status-bar) status-bar)
        (create-window status-bar
                       :class WC_STATUSBAR
                       :ctrl-id 1
                       :parent  w
                       :style (logior WS_CHILD WS_VISIBLE)
                       :x 0
                       :y 0
                       :width 0
                       :height 0 )
        (setf (status-bar-simple-p status-bar) t)
        (setf (status-bar-text status-bar) "Ready")
        (|GetClientRect| status-bar (window-rect status-bar)) ) )

    ;; onDraw
    (onDraw ()
      (let ((rc (make-rect))
            (status-bar (slot-value w 'status-bar)) )

        (replace rc (window-rect w))
        (decf (rect-bottom rc) (window-height status-bar))

        (|FillRect| w rc +brush.btnface+)
        (draw-text w 0 0 0 rc "foobar" 0 6)

        (multiple-value-bind (cx cy) (get-text-extent w "foobar" 0 6)
          (let ((s (format nil "~Dx~D" cx cy)))
            (draw-text w 0 cy 0 rc s 0 (length s)) ) )

        (let* ((font (slot-value w 'font))
               (old-font (|SelectFont| w font)) )
          (|SetTextColor| w #x00CC00)
          (|ExtTextOut| w 100 100 0 rc "Foo Bar Baz.")
          (|ExtTextOut| w 100 160 0 rc "This is a text.")
          (|SelectObject| w old-font) ) ) )

    ;; onSize
    (onSize ()
      (let* ((rc (window-rect w))
             (status-bar (slot-value w 'status-bar))
             (cy (window-height status-bar)) )
        (|GetClientRect| w rc)

        (let ((cx (rect-width rc)))
          (setf (status-bar-parts status-bar)
              (list (- cx 200)
                    (- cx 150)
                    (- cx 100)
                    (- cx  50)
                    cx ) ) )

        (setf (status-bar-simple-p status-bar) 1)
        (setf (status-bar-text status-bar)
            (format nil "onSize ~S" w) )

        (|SetWindowPos| status-bar +null-window+
                        (rect-left rc)
                        (- (rect-bottom rc) cy)
                        (rect-width rc)
                        cy
                        SWP_NOZORDER )
        (|GetClientRect| status-bar (slot-value status-bar 'rect)) ) )
    )
    ;;
    (case uMsg
      (#.WM_CREATE (onCreate))
      (#.WM_PAINT  (onDraw))
      (#.WM_SIZE   (onSize)) ) ) )


(defvar *foo* (make-instance 'hello-window))

;; #define ERROR_INVALID_WINDOW_HANDLE      1400L
;; #define ERROR_INVALID_MENU_HANDLE        1401L
;; #define ERROR_INVALID_CURSOR_HANDLE      1402L
;; #define ERROR_INVALID_ACCEL_HANDLE       1403L
;; #define ERROR_INVALID_HOOK_HANDLE        1404L
;; #define ERROR_INVALID_DWP_HANDLE         1405L
;; #define ERROR_TLW_WITH_WSCHILD           1406L
;; #define ERROR_CANNOT_FIND_WND_CLASS      1407L
;; #define ERROR_WINDOW_OF_OTHER_THREAD     1408L

(defun message-pump ()
  (let ((msgbuf (slot-value *context* 'msgbuf)))
    (|GetMessage| msgbuf +null-window+ 0 0)
    (|TranslateMessage| msgbuf)
    (|DispatchMessage|  msgbuf) ) )

(defmethod idle-procedure ((window hello-window) counter)
    (declare (ignore context))
  (let ((rc (window-rect window))
        (status-bar (slot-value window 'status-bar)) )
    (setf (status-bar-simple-p status-bar) nil)
    (setf (status-bar-text status-bar 0) "Ready")

    (setf (status-bar-text status-bar 1)
        (format nil "c=~D" counter) )

    (setf (status-bar-text status-bar 2)
        (format nil "t:~D" (rect-top rc)) )

    (setf (status-bar-text status-bar 3)
        (format nil "r:~D" (rect-right rc)) )

    (setf (status-bar-text status-bar 4)
        (format nil "b:~D" (rect-bottom rc)) )

    nil ) )


(defun test-it ()
  (when (zerop (slot-value *foo* 'hwnd))
    (initialize)
    (create-window *foo*
      :style (logior WS_OVERLAPPEDWINDOW WS_VISIBLE)
      :title "Foo" )
    (setf (slot-value *context* 'applications) (list *foo*)) ) )


#|

cd "/proj/evcl3/platform/win/lisp/windowing"
ld "win32-test"

|#

