#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - initialization
// genesis/gs_init.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/platform/win/win_crt.cpp#3 $
//
// Description:
//  Installs following C implemented lisp function for genesis.
//
#if NDEBUG
void* __cdecl operator new(size_t cb) { return malloc(cb); }
void __cdecl operator delete(void* pv) { free(pv); }

#if 1
int __cdecl evcl_memcmp(const void* src1, const void* src2, size_t count)
{
    const uint8* s = reinterpret_cast<const uint8*>(src1);
    const uint8* p = reinterpret_cast<const uint8*>(src2);
    const uint8* e = s + count;
    while (s < e)
    {
        int diff = *s - *p;
        if (0 != diff) return diff;
        s++;
        p++;
    } // while
    return 0;
} // memcmp
#endif

#if _M_IX86
void* __cdecl evcl_memcpy(void* dst, const void* src, size_t count)
{
    const uint8* s = reinterpret_cast<const uint8*>(src);
    const uint8* e = s + count;
    uint8* d = reinterpret_cast<uint8*>(dst);
    while (s < e) *d++ = *s++;
    return dst;
} // memcpy
#endif

#if _M_IX86
extern "C" void* __cdecl evcl_memmove(void* dst, const void* src, size_t count)
{
    const uint8* s = reinterpret_cast<const uint8*>(src);
    const uint8* e = s + count;
    uint8* d = reinterpret_cast<uint8*>(dst);
    if (s < d && d < e)
    {
        d += count;
        while (e > s) *--d = *--e;
    }
    else
    {
        while (s < e) *d++ = *s++;
    }
    return dst;
} // evcl_memmove
#endif // _M_IX86

#if _M_IX86
void* __cdecl evcl_memset(void* dst, int c, size_t count)
{
    uint8* d = reinterpret_cast<uint8*>(dst);
    uint8* e = d + count;
    while (d < e) *d++ = static_cast<uint8>(c);
    return dst;
} // evcl_memset
#endif // 0

extern "C"
{

void __cdecl abort() { ::ExitProcess(1); }
int __cdecl atexit(void ( __cdecl *)(void)) { return 0; }
int __cdecl _purecall(void) { return 0; }

__declspec(noalias) void __cdecl free(void* pv)
{
    ::HeapFree(::GetProcessHeap(), 0, pv);
} // free

__declspec(noalias) __declspec(restrict) void*  __cdecl malloc(size_t cb)
{
    return ::HeapAlloc(::GetProcessHeap(), 0, cb);
} // malloc

} // extern "C"

#endif // NDEBUG
