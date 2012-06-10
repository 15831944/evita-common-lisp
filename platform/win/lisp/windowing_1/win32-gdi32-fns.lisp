(in-package :win32)

(define-api |CreateFontIndirect| HFONT (LOGFONT)
  "gdi32.dll" "CreateFontIndirectW" )

(define-api |GetTextMetrics| LastError (HDC TEXTMETRIC)
  "gdi32.dll" "GetTextMetricsW" )

(define-api |SelectBrush| HGDIOBJ (HDC HBRUSH)
  "gdi32.dll" "SelectObject" )

(define-api |SelectFont| HGDIOBJ (HDC HFONT)
  "gdi32.dll" "SelectObject" )

(define-api |SelectObject| VOID (HDC HGDIOBJ)
  "gdi32.dll" "SelectObject" )

(define-api |SelectPen| HGDIOBJ (HDC HPEN)
  "gdi32.dll" "SelectObject" )


;; #define CLR_INVALID     0xFFFFFFFF
(define-api |SetBkColor| COLORREF (HDC COLORREF)
  "gdi32.dll" "SetBkColor" )

;;#define TRANSPARENT         1
;;#define OPAQUE              2
;;#define BKMODE_LAST         2

(define-api |SetBkMode| int (HDC int32)
  "gdi32.dll" "SetBkMode" )


(define-api |SetTextColor| COLORREF (HDC COLORREF)
  "gdi32.dll" "SetTextColor" )
