(in-package :win32)

(defconstant COLOR_SCROLLBAR 0)
(defconstant COLOR_BACKGROUND 1)
(defconstant COLOR_ACTIVECAPTION 2)
(defconstant COLOR_INACTIVECAPTION 3)
(defconstant COLOR_MENU 4)
(defconstant COLOR_WINDOW 5)
(defconstant COLOR_WINDOWFRAME 6)
(defconstant COLOR_MENUTEXT 7)
(defconstant COLOR_WINDOWTEXT 8)
(defconstant COLOR_CAPTIONTEXT 9)
(defconstant COLOR_ACTIVEBORDER 10)
(defconstant COLOR_INACTIVEBORDER 11)
(defconstant COLOR_APPWORKSPACE 12)
(defconstant COLOR_HIGHLIGHT 13)
(defconstant COLOR_HIGHLIGHTTEXT 14)
(defconstant COLOR_BTNFACE 15)
(defconstant COLOR_BTNSHADOW 16)
(defconstant COLOR_GRAYTEXT 17)
(defconstant COLOR_BTNTEXT 18)
(defconstant COLOR_INACTIVECAPTIONTEXT 19)
(defconstant COLOR_BTNHIGHLIGHT 20)
(defconstant COLOR_3DDKSHADOW 21)
(defconstant COLOR_3DLIGHT 22)
(defconstant COLOR_INFOTEXT 23)
(defconstant COLOR_INFOBK 24)
(defconstant COLOR_HOTLIGHT 26)
(defconstant COLOR_GRADIENTACTIVECAPTION 27)
(defconstant COLOR_GRADIENTINACTIVECAPTION 28)
(defconstant COLOR_MENUHILIGHT 29)
(defconstant COLOR_MENUBAR 30)

;;; ExtTextOut
(defconstant ETO_OPAQUE         #x0002)
(defconstant ETO_CLIPPED        #x0004)
(defconstant ETO_GLYPH_INDEX    #x0010)
(defconstant ETO_RTLREADING     #x0080)
(defconstant ETO_NUMERICSLOCAL  #x0400)
(defconstant ETO_NUMERICSLATIN  #x0800)
(defconstant ETO_IGNORELANGUAGE #x1000)
(defconstant ETO_PDY            #x2000)

;;; SetTextAlign
(defconstant TA_NOUPDATECP   0)
(defconstant TA_UPDATECP     1)
(defconstant TA_LEFT         0)
(defconstant TA_RIGHT        2)
(defconstant TA_CENTER       6)
(defconstant TA_TOP          0)
(defconstant TA_BOTTOM       8)
(defconstant TA_BASELINE    24)
(defconstant TA_RTLREADING 256)


;;; LOGFONT

;;; LOGFONT.lfOutPrecision
(defconstant OUT_DEFAULT_PRECIS          0)
(defconstant OUT_STRING_PRECIS           1)
(defconstant OUT_CHARACTER_PRECIS        2)
(defconstant OUT_STROKE_PRECIS           3)
(defconstant OUT_TT_PRECIS               4)
(defconstant OUT_DEVICE_PRECIS           5)
(defconstant OUT_RASTER_PRECIS           6)
(defconstant OUT_TT_ONLY_PRECIS          7)
(defconstant OUT_OUTLINE_PRECIS          8)
(defconstant OUT_SCREEN_OUTLINE_PRECIS   9)
(defconstant OUT_PS_ONLY_PRECIS          10)

;;; LOGFONT.lfClipPrecision
(defconstant CLIP_DEFAULT_PRECIS     0)
(defconstant CLIP_CHARACTER_PRECIS   1)
(defconstant CLIP_STROKE_PRECIS      2)
(defconstant CLIP_MASK               #xf)
(defconstant CLIP_LH_ANGLES          (ash 1 4))
(defconstant CLIP_TT_ALWAYS          (ash 2 4))
(defconstant CLIP_DFA_DISABLE        (ash 4 4))
(defconstant CLIP_EMBEDDED           (ash 8 4))

;;; LOGFONT.lfQuality
(defconstant DEFAULT_QUALITY 0)
(defconstant DRAFT_QUALITY 1)
(defconstant PROOF_QUALITY 2)
(defconstant NONANTIALIASED_QUALITY  3)
(defconstant ANTIALIASED_QUALITY     4)
(defconstant CLEARTYPE_QUALITY       5)
(defconstant CLEARTYPE_NATURAL_QUALITY 6)


;;; LOGFONT.tmPitchAndFamily
(defconstant DEFAULT_PITCH           0)
(defconstant FIXED_PITCH             1)
(defconstant VARIABLE_PITCH          2)
(defconstant MONO_FONT               8)

(defconstant FF_DONTCARE         (ash 0 4)) ; Don't care or don't know.
(defconstant FF_ROMAN            (ash 1 4))  ; Variable stroke width, serifed. Times Roman, Century Schoolbook, etc.
(defconstant FF_SWISS            (ash 2 4))  ; Variable stroke width, sans-serifed. Helvetica, Swiss, etc.
(defconstant FF_MODERN           (ash 3 4))  ; Constant stroke width, serifed or sans-serifed. Pica, Elite, Courier, etc.
(defconstant FF_SCRIPT           (ash 4 4))  ; Cursive, etc.
(defconstant FF_DECORATIVE       (ash 5 4))  ; Old English, etc.


(defconstant LF_FACESIZE 32)

(defclass LOGFONT (foreign-object)
 ((lfHeight         :type int32)    ; +0
  (lfWidth          :type int32)    ; +4
  (lfEscapement     :type int32)    ; +8
  (lfOrientation    :type int32)    ; +12
  (lfWeight         :type int32)    ; +16
  (lfItalic         :type uint8)    ; +20
  (lfUnderline      :type uint8)    ; +21
  (lfStrikeOut      :type uint8)    ; +22
  (lfCharSet        :type uint8)    ; +23
  (lfOutPrecision   :type uint8)    ; +24
  (lfClipPrecision  :type uint8)    ; +25
  (lfQuality        :type uint8)    ; +26
  (lfPitchAndFamily :type uint8)    ; +27
  (lfFaceName       :type (vec uint16 #.LF_FACESIZE)) ) ; +28
  (:metaclass foreign-class) )


(defclass TEXTMETRIC (foreign-object)
  ((tmHeight            :type int32)    ; +0
   (tmAscent            :type int32)    ; +4
   (tmDescent           :type int32)    ; +8
   (tmInternalLeading   :type int32)    ; +12
   (tmExternalLeading   :type int32)    ; +16
   (tmAveCharWidth      :type int32)    ; +20
   (tmMaxCharWidth      :type int32)    ; +24
   (tmWeight            :type int32)    ; +28
   (tmOverhang          :type int32)    ; +32
   (tmDigitizedAspectX  :type int32)    ; +36
   (tmDigitizedAspectY  :type int32)    ; +40
   (tmFirstChar         :type uint16)   ; +44
   (tmLastChar          :type uint16)   ; +46
   (tmDefaultChar       :type uint16)   ; +48
   (tmBreakChar         :type uint16)   ; +50
   (tmItalic            :type uint8)    ; +52
   (tmUnderlined        :type uint8)    ; +53
   (tmStruckOut         :type uint8)    ; +54
   (tmPitchAndFamily    :type uint8)    ; +55
   (tmCharSet           :type uint8) )  ; +56
                                        ; +57
  (:metaclass foreign-class) )
