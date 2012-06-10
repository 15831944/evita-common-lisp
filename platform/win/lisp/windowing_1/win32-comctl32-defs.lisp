(in-package :win32)

(defconstant CCM_FIRST  #x2000)
(defconstant CCM_LAST   (+ CCM_FIRST #x200))

(defconstant CCM_SETBKCOLOR          (+ CCM_FIRST 1))
(defconstant CCM_SETCOLORSCHEME      (+ CCM_FIRST 2))
(defconstant CCM_GETCOLORSCHEME      (+ CCM_FIRST 3))
(defconstant CCM_GETDROPTARGET       (+ CCM_FIRST 4))
(defconstant CCM_SETUNICODEFORMAT    (+ CCM_FIRST 5))
(defconstant CCM_GETUNICODEFORMAT    (+ CCM_FIRST 6))
(defconstant CCM_SETVERSION          (+ CCM_FIRST #x7))
(defconstant CCM_GETVERSION          (+ CCM_FIRST #x8))
(defconstant CCM_SETNOTIFYWINDOW     (+ CCM_FIRST #x9))
(defconstant CCM_SETWINDOWTHEME      (+ CCM_FIRST #xb))
(defconstant CCM_DPISCALE            (+ CCM_FIRST #xc))

;;;; Header
(defconstant WC_HEADER "SysHeader32")

;;;; Status Bar
(defconstant WC_STATUSBAR "msctls_statusbar32")

(defconstant SBARS_SIZEGRIP #x0100)
(defconstant SBARS_TOOLTIPS #x0800)

(defconstant SB_SETTEXT           (+ WM_USER 11))
(defconstant SB_GETEXTLENGTH      (+ WM_USER 12))
(defconstant SB_GETTEXT           (+ WM_USER 13))

(defconstant SB_SETPARTS          (+ WM_USER 4))
(defconstant SB_GETPARTS          (+ WM_USER 6))
(defconstant SB_GETBORDERS        (+ WM_USER 7))
(defconstant SB_SETMINHEIGHT      (+ WM_USER 8))
(defconstant SB_SIMPLE            (+ WM_USER 9))
(defconstant SB_GETRECT           (+ WM_USER 10))
(defconstant SB_ISSIMPLE          (+ WM_USER 14))
(defconstant SB_SETICON           (+ WM_USER 15))
(defconstant SB_SETTIPTEXT        (+ WM_USER 17))
(defconstant SB_GETTIPTEXT        (+ WM_USER 19))
(defconstant SB_GETICON           (+ WM_USER 20))
(defconstant SB_SETUNICODEFORMAT  CCM_SETUNICODEFORMAT)
(defconstant SB_GETUNICODEFORMAT  CCM_GETUNICODEFORMAT)
(defconstant SB_SETBKCOLOR        CCM_SETBKCOLOR)

(defconstant SBT_OWNERDRAW            #x1000)
(defconstant SBT_NOBORDERS            #x0100)
(defconstant SBT_POPOUT               #x0200)
(defconstant SBT_RTLREADING           #x0400)
(defconstant SBT_NOTABPARSING         #x0800)

(defconstant SB_SIMPLEID #xff)
