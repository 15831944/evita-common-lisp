(in-package :win32)

;;; [G]
(fli:define "kernel32.dll" "GetProcessHeap" HANDLE ())

;;; [H]
(fli:define "kernel32.dll" "HeapAlloc" void* (HANDLE uint32 uint32))
(fli:define "kernel32.dll" "HeapFree"  LastError (HANDLE uint32 void*))
