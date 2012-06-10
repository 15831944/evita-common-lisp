(in-package :win32)

(define-api |CloseWindow| LastError (HWND)
  "user32.dll" "CloseWindow" )

(define-api |DestroyWindow| LastError (HWND)
  "user32.dll" "DestroyWindow" )

(define-api |DispatchMessage| LRESULT (LPMSG)
  "user32.dll" "DispatchMessageW")

(define-api |GetClientRect| LastError (HWND LPRECT)
  "user32.dll" "GetClientRect" )

(define-api |GetMessage| int (LPMSG HWND uint32 uint32)
  "user32.dll" "GetMessageW" )

(define-api |GetWindowRect| LastError (HWND LPRECT)
  "user32.dll" "GetWindowRect" )

(define-api |FillRect| LastError (HDC LPRECT HBRUSH)
  "user32.dll" "FillRect" )

(define-api |SendMessage/ii| LRESULT (HWND uint32 int32 int32)
  "user32.dll" "SendMessageW" )

(define-api |SendMessage/is| LRESULT (HWND uint32 int32 LPTSTR)
  "user32.dll" "SendMessageW" )


(define-api |SetWindowPos| LastError (HWND HWND int32 int32 int32 int32 uint32)
  "user32.dll" "SetWindowPos" )

(define-api |ShowWindow| BOOL (HWND int)
  "user32.dll" "ShowWindow" )

(define-api |TranslateMessage| BOOL (LPMSG)
  "user32.dll" "TranslateMessage" )

(define-api |UpdateWindow| LastError (HWND)
  "user32.dll" "UpdateWindow" )
