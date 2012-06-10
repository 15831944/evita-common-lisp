#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - main
// platform/win/win_dllcrt.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/platform/win/win_dllcrt.cpp#1 $
//

#if NDEBUG //&& _M_IX86

extern "C"
{

// Note: For x64, we need to use /OPT:NOREF
extern const int _fltused = 0;

#if _M_X64 || _M_IX86
//#if _M_IX86
typedef void (__cdecl *_PVFV)(void);

extern _PVFV __xi_a[], __xi_z[];    // C initializers
extern _PVFV __xc_a[], __xc_z[];    // C++ initializers
extern _PVFV __xp_a[], __xp_z[];    // C pre-terminators
extern _PVFV __xt_a[], __xt_z[];    // C terminators

#if 1
// See crt0init.c
#pragma section(".CRT$XIA", long, read)
#pragma section(".CRT$XIZ", long, read)

#pragma section(".CRT$XCA", long, read)
#pragma section(".CRT$XCZ", long, read)

#pragma section(".CRT$XPA", long, read)
#pragma section(".CRT$XPZ", long, read)

#pragma section(".CRT$XTA", long, read)
#pragma section(".CRT$XTZ", long, read)

__declspec(allocate(".CRT$XIA")) _PVFV __xi_a[] = { NULL };
__declspec(allocate(".CRT$XIZ")) _PVFV __xi_z[] = { NULL };

__declspec(allocate(".CRT$XCA")) _PVFV __xc_a[] = { NULL };
__declspec(allocate(".CRT$XCZ")) _PVFV __xc_z[] = { NULL };

__declspec(allocate(".CRT$XPA")) _PVFV __xp_a[] = { NULL };
__declspec(allocate(".CRT$XPZ")) _PVFV __xp_z[] = { NULL };

__declspec(allocate(".CRT$XTA")) _PVFV __xt_a[] = { NULL };
__declspec(allocate(".CRT$XTZ")) _PVFV __xt_z[] = { NULL };

#pragma comment(linker, "/merge:.CRT=.rdata")

#endif // _M_IX86

#define initterm(mp_a, mp_z) \
{ \
    for (_PVFV* pf = mp_a; pf < mp_z; pf++) \
    { \
        if (NULL != *pf) (**pf)(); \
    } \
}

extern "C" BOOL WINAPI
DllMain(HANDLE, DWORD, void*);

BOOL WINAPI
_DllMainCRTStartup(
    HANDLE  hInstance,
    DWORD   dwReason,
    LPVOID  pReserved )
{
    switch (dwReason)
    {
    case DLL_PROCESS_ATTACH:
    case DLL_THREAD_ATTACH:
        // do C initializations
        initterm( __xi_a, __xi_z );

        // do C++ initializations
        initterm( __xc_a, __xc_z );
        break;
    } // switch dwReason

    BOOL fRet = DllMain(hInstance, dwReason, pReserved);

    switch (dwReason) 
    {
    case DLL_PROCESS_DETACH:
    case DLL_THREAD_DETACH:
        // pre-termination for exit
        initterm(__xp_a, __xp_z);

        // termination for _exit
        initterm(__xt_a, __xt_z);
        break;
    } // switch dwReason

    return fRet;
} // _DllMainCRTStartup

#endif // _M_IX86

} // extern "C"

#endif // NDEBUG
