(in-package :win32)

(define-api |GetLastError| int ()
  "kernel32.dll" "GetLastError" )

(define-api |GetProcessHeap| fixnum ()
  "kernel32.dll" "GetProcessHeap" )

(define-api |HeapCreate| fixnum (uint32 uint32 uint32)
  "kernel32.dll" "HeapCreate" )

(define-api |HeapAlloc| LPVOID (fixnum uint32 uint32)
  "kernel32.dll" "HeapAlloc" )

(define-api |HeapDestroy| LastError (fixnum)
  "kernel32.dll" "HeapDestroy" )

(define-api |HeapFree| LastError (fixnum uint32 LPVOID)
  "kernel32.dll" "HeapFree" )
