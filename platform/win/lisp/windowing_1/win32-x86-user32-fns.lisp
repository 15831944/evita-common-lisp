(in-package :win32)


(x86:defasm |CreateWindow| ()
  (frame #.(ash (+ 1 3 12) 2))

  ;; ToKernel Frame
  (push #.(ash 12 4))
  (push #x4b724e00) ; Frame::Type_ToKernel
  (push ($rtcb #x2c))
  (mov ($rtcb #x2c) $rsp)

  (sar  $r0 2)
  (lea  $r1 ($r1 #.(- (sizeof 'simple-string) (tagof 'simple-string))))
  (lea  $r2 ($r2 #.(- (sizeof 'simple-string) (tagof 'simple-string))))

  ;; Handle dwStyle
  (test $r3 3)
  (jne style.bignum)
  (sar  $r3 2)
  (jmp style.done)
 style.bignum
  (mov  $r3 ($r3 #.(- (sizeof 'bignum) (tagof 'bignum))))

 style.done
  (mov ($rtcb #.(+ 56 (* 4 4))) $r4)

  (xor  $rn $rn)
  (push $rn)      ; [11] LPVOID lpParam

  (mov  $rn ($rtcb #.(+ 56 (* 4 10))))
  (sar  $rn 2)
  (push $rn)      ; [10] HINSTANCE hInstance

  (mov  $rn ($rtcb #.(+ 56 (* 4 9))))
  (sar  $rn 2)
  (push $rn)      ; [9] HMENU hMenu

  (mov $rn ($rtcb #.(+ 56 (* 4 8))))
  (mov $rn ($rn (offsetof window hwnd)))
  (sar $rn 2)
  (push $rn)      ; [8] HWND hwndParent

  (mov $rn #x80000000)  ; CW_USEDEFAULT

  (mov $r4 ($rtcb #.(+ 56 (* 4 7))))
  (cmp $r4 'nil)
  (cmove $r4 $rn)
  (je push-height)
  (sar  $r4 2)
 push-height
  (push $r4)      ; [7] int nHeight

  (mov $r4 ($rtcb #.(+ 56 (* 4 6))))
  (cmp $r4 'nil)
  (cmove $r4 $rn)
  (je push-width)
  (sar  $r4 2)
 push-width
  (push $r4)      ; [6] int nWidth

  (mov $r4 ($rtcb #.(+ 56 (* 4 5))))
  (cmp $r4 'nil)
  (cmove $r4 $rn)
  (je push-y)
  (sar  $r4 2)
 push-y
  (push $r4)      ; [5] int y

  (mov $r4 ($rtcb #.(+ 56 (* 4 4))))
  (cmp $r4 'nil)
  (cmove $r4 $rn)
  (je push-x)
  (sar  $r4 2)
 push-x
  (push $r4)      ; [4] int     x

  (push $r3)      ; [3] DWORD   dwStyle
  (push $r2)      ; [2] LPCTSTR lpWindowName
  (push $r1)      ; [1] LPCTSTR lpClassName
  (push $r0)      ; [0] DWORD   dwExStyle
  (call ((:dll "user32.dll" "CreateWindowExW")))
  (sal $r0 2)
  (pop  ($rtcb #x2c))
  (add  $rsp 8)
  (ret) )


(x86:defasm |GetDC| (HWND)
  (frame #.(ash (+ 1 3 1 1) 2))

  (push $r0)
  (push 4)
  (push #x4b724e00) ; Frame::Type_ToKernel
  (push ($rtcb #x2c))
  (mov ($rtcb #x2c) $rsp)

  (mov $r0 ($r0 (offsetof window hwnd)))
  (sar $r0 2)
  (push $r0)
  (call ((:dll "user32.dll" "GetDC")))
  (pop ($rtcb #x2c))

  (mov $r1 ($rsp 8))
  (mov $r1 ($r1 (offsetof window hdc)))
  (mov ($r1 (offsetof si::handle si::value)) $r0)

  (mov $r0 $r1)
  (add $rsp 12)
  (ret) )


#+nil
(x86:defasm internal-get-message ()
  (lea $rn ($rsp -16))
  (push $rn)
  (push #x4b724e00) ; Frame::Type_ToKernel
  (push ($rtcb #x2c))
  (mov ($rtcb #x2c) $rsp)
  (xor $rn $rn)
  (push $rn)    ; UINT wMsgFilterMax
  (push $rn)    ; UINT wMsgFilterMax
  (push $rn)    ; HWND hWnd
  (lea $r0 ($r0 #.(- (sizeof 'simple-string) (tagof 'simple-string))))
  (push $r0)    ; LPMSG lpMsg
  (call ((:dll "user32" "GetMessageW")))
  (cmp eax -1)
  (jne done)
  (call ((:dll "kernel32" "GetLastError")))
  done
  (shl  eax 2)
  (pop  ($rtcb #x2c))
  (add  $rsp 8)
  (ret) )


;;;; create-window
(defun create-window (window &key
                        (class   "LispWindowClass")
                        (ctrl-id 0)
                        (exstyle 0)
                        height
                        (parent +null-window+)
                        (style 0)
                        (title "")
                        width
                        x
                        y )
  (format t "; create-window: ~S ~S ~S~%" window class parent)
  (setf (slot-value *context* 'creation) window)
  (let ((hwnd (|CreateWindow|
                exstyle                 ; [0] dwExStyle eax
                class                   ; [1] pszClass  edx
                title                   ; [2] pszTitle  ebx
                style                   ; [3] dwStyle   esi
                x                       ; [4] x         edi
                y                       ; [5] y
                width                   ; [6] cy
                height                  ; [7] cx
                parent                  ; [8] hwndParent
                ctrl-id                 ; [9] hMenu/CtrlId
                (slot-value *context* 'instance) ) ))
    ;; If hwnd isn't LispWindowClass, slot "hwnd" isn't set
    (setf (slot-value window 'hwnd) hwnd)
    (format t "; create-window: ~S last-error=~D~%" window (|GetLastError|))
    window ) )
