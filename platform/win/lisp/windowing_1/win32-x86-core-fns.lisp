(in-package :win32)



;;;; initialize-windowing
(x86:defasm initialize-windowing (context)
  (frame #.(ash (+ 1 3) 2))

  ;; ToKernel Frame
  (push 0)
  (push #x4b724e00) ; Frame::Type_ToKernel
  (push ($rtcb #x2c))
  (mov ($rtcb #x2c) $rsp)
  (mov ecx $r0)
  (call ((:dll "." "initialize_windowing")))
  (pop  ($rtcb #x2c))
  (add  $rsp 8)
  (ret) )
