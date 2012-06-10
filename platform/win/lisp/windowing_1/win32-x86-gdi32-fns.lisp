(in-package :win32)

(x86:defasm |CreateFontIndirect| (LOGFONT*)
  (frame #.(ash (+ 1 3 1) 2))

  (push #.(ash 1 2))
  (push #x4b724e00) ; Frame::Type_ToKernel
  (push ($rtcb #x2c))
  (mov  ($rtcb #x2c) $rsp)

  (lea $r0 ($r0 #.(- (sizeof 'simple-string) (tagof 'simple-string))))
  (push $r0)
  (call ((:dll "gdi32.dll" "CreateFontIndirectW")))

  (pop ($rtcb #x2c))
  (add $rsp 8)
  (jmp #'.box-int) )


(x86:defasm |ExtTextOut| (hdc x y options rc string)
  (frame #.(ash (+ 1 3 8) 2))

  ;; Push ToKernel frame
  (push #.(ash 8 2))
  (push #x4b724e00) ; Frame::Type_ToKernel
  (push ($rtcb #x2c))
  (mov  ($rtcb #x2c) $rsp)

  (sar $r1 2)   ; x
  (sar $r2 2)   ; y
  (sar $r3 2)   ; fuOptions
  (lea $r4 ($r4 #.(- (sizeof 'simple-string) (tagof 'simple-string))))

  (xor $rn $rn)
  (push $rn)    ; [7] const int* lpDx

  (mov  $rn ($rtcb #.(+ 56 (* 5 4))))
  (mov  $rn ($rn (offsetof simple-string length)))
  (sar $rn 2)
  (push $rn)    ; [6] UINT cwch

  (mov  $rn ($rtcb #.(+ 56 (* 5 4))))
  (lea $rn ($rn #.(- (sizeof 'simple-string) (tagof 'simple-string))))
  (push $rn)    ; [5] LPCTSTR lpString
  (push $r4)    ; [4] const RECT* lprc
  (push $r3)    ; [3] uint fuOptions
  (push $r2)    ; [2] int y
  (push $r1)    ; [1] int x
  (mov $r0 ($r0 (offsetof drawable hdc)))
  (mov $r0 ($r0 (offsetof si::handle si::value)))
  (push $r0)    ; [0] HDC hdc
  (call ((:dll "gdi32.dll" "ExtTextOutW")))

  ;; Pop ToKernel frame
  (pop  ($rtcb #x2c))
  (add  $rsp 8)
  (ret) )


(x86:defasm !draw-text (hdc x y options rc string start end)
  ;; $r0 = hdc
  ;; $r1 = x
  ;; $r2 = y
  ;; $r3 = options
  ;; $r4 = rc
  ;; [$rtcb 56 + 5 * 4] string
  ;; [$rtcb 56 + 6 * 4] start
  ;; [$rtcb 56 + 7 * 4] end
  (frame #.(ash (+ 1 3 8) 2))

  (sub $rsp #.(ash (+ 3 8) 2))
  ;;  -4 RA of this function        <---+
  ;;  +0 CARG[0] HDC hdc                |
  ;;  +4 CARG[1] int x                  |
  ;;  +8 CARG[2] int y                  |
  ;; +12 CARG[3] uint fuOptions         |
  ;; +16 CARG[4] const RECT* lprc       |
  ;; +20 CARG[5] LPCTSTR lpString       |
  ;; +24 CARG[6] uint cwch              |
  ;; +28 CARG[7] const int* lpDX        |
  ;; +32 FP'                            |
  ;; +36 Frame::Type_ToKernel           |
  ;; +40 size of C arguments        o---+
  ;; +44 RA

  ;;; CARG[0] HDC hdc
  (mov $r0 ($r0 (offsetof drawable hdc)))
  (mov $r0 ($r0 (offsetof si::handle si::value)))
  (mov ($rsp) $r0)

  ;; CARG[1] int x
  (shr $r1 2)
  (mov ($rsp 4) $r1)

  ;; CARG[2] int y
  (shr $r2 2)
  (mov ($rsp 8) $r2)

  ;; CARG[3] uint fuOption
  (shr $r3 2)
  (mov ($rsp 12) $r3)

  ;; CARG[4] const RECT* lprc
  (lea $r4 ($r4 #.(- (sizeof 'simple-string) (tagof 'simple-string))))
  (mov ($rsp 16) $r4)

  (mov $r1 ($rtcb #.(+ 56 (* 5 4))))    ; $r1 = string
  (mov $r2 ($rtcb #.(+ 56 (* 6 4))))    ; $r2 = start
  (mov $r0 ($rtcb #.(+ 56 (* 7 4))))    ; $r0 = end

  ;; CARG[6] uint cwch
  (sub $r0 $r2)
  (shr $r0 2)
  (mov ($rsp 24) $r0)

  ;; CARG[5] LPCTSTR lpString
  (shr $r2 1)
  (lea $r1 ($r1 $r2 #.(- (sizeof 'simple-string) (tagof 'simple-string))))
  (mov ($rsp 20) $r1)

  ;; +28 CARG[7] const int* lpDX
  (xor $r0 $r0)
  (mov ($rsp 28) $r0)

  ;; Push ToKernel frame - This frame will be used when AV is occured
  ;; in DLL procedure.
  (mov ($rsp 40) #.(ash 8 2))   ; location of RA from FP.
  (mov ($rsp 36) #x4b724e00)    ; Frame::Type_ToKernel
  (mov $rn ($rtcb #x2c))
  (lea $r0 ($rsp 32))
  (mov ($r0) $rn)
  (mov ($rtcb #x2c) $r0)

  (call ((:dll "gdi32.dll" "ExtTextOutW")))

  ;; Pop ToKernel frame
  (pop  ($rtcb #x2c))

  ;; Check result of ExtTextOutW
  (test $r0 $r0)
  (mov $r0 'nil)
  (jne done)
  (call ((:dll "kernel32.dll" "GetLastError")))
  (shl $r0 2)

 done
  (add  $rsp 8)
  (ret) )


;;;; get-text-extent
(x86:defasm !get-text-extent (hdc string start end)
  ;; $r0 = hdc
  ;; $r1 = string
  ;; $r2 = start
  ;; $r3 = end

  ;; 1=RA, 3=ToKernel, 4=CARGS, 2=SIZE
  (frame #.(ash (+ 1 3 4 2) 2))

  ;; Push ToKernel frame
  (push #.(ash 4 2))
  (push #x4b724e00) ; Frame::Type_ToKernel
  (push ($rtcb #x2c))
  (mov  ($rtcb #x2c) $rsp)

  (sub $rsp 8)
  (push $rsp)   ; [3] LPSIZE lpSize

  (mov $rn $r3) ; $rn = end
  (sub $rn $r2) ; $rn = end - start
  (shr $rn 2)
  (push $rn)    ; [2] int chString

  (shr $r2 1)
  (lea $r1 ($r1 $r2 #.(- (sizeof 'simple-string) (tagof 'simple-string))))
  (push $r1)    ; [1] LPCTSTR lpString

  (mov $r0 ($r0 (offsetof drawable hdc)))
  (mov $r0 ($r0 (offsetof si::handle si::value)))
  (push $r0)    ; [0] HDC hdc

  (call ((:dll "gdi32.dll" "GetTextExtentPoint32W")))

  ;; Retrive SIZE.cx and SIZE.cy as fixnum.
  (mov $r0 ($rsp))
  (mov $r1 ($rsp 4))
  (shl $r0 2)
  (shl $r1 2)
  (add $rsp 8)

  (stc)
  (mov $rn '2)

  ;; Pop ToKernel frame
  (pop  ($rtcb #x2c))
  (lea  $rsp ($rsp 8))
  (ret) )
