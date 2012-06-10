(in-package :win32)

;;;; get-instance-handle
(x86:defasm get-instance-handle ()
  (frame #.(ash (+ 1 3 1) 2))

  ;; ToKernel Frame
  (push 4)
  (push #x4b724e00) ; Frame::Type_ToKernel
  (push ($rtcb #x2c))
  (mov ($rtcb #x2c) $rsp)

  (xor  $r0 $r0)
  (push $r0)
  (call ((:dll "kernel32.dll" "GetModuleHandleW")))
  (shl  $r0 2)
  (pop  ($rtcb #x2c))
  (add  $rsp 8)
  (ret) )


;;;; !make-handle
(x86:defasm !make-handle (foreign-value)
  (frame 8)

  (sub $rsp 4)

  (mov ($rsp) $r0)
  (mov $r0 '#.(si::class-description 'si::handle))
  (mov $rn '1)
  (call #'si::.allocate-binobj)

  (mov $r1 ($rsp))
  (mov ($r0 (offsetof si::handle si::value)) $r1)

  (add $rsp 4)
  (ret) )
