//////////////////////////////////////////////////////////////////////////////
//
// evcl - listener - winapp - pre-compiled header
// precomp.h
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/listener/winapp/precomp.h#2 $
//
#if !defined(INCLUDE_listener_winapp_precomp_h)
#define INCLUDE_listener_winapp_precomp_h

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

// Requires at least Windows Vista
#define _WIN32_WINNT 0x600

#define STRICT
#define INC_OLE2
#define WIN32_LEAN_AND_MEAN
#define ISOLATION_AWARE_ENABLED 1

// warning C6385: Invalid data: accessing 'rgchFullModulePath'
#pragma warning(disable: 6385)
#include <windows.h>
#pragma warning(default: 6385)
#include <ShellAPI.h>   // HDROP
#include <stddef.h>     // ptrdiff_t
#include <windowsx.h>   // GET_X_LPARAM

// Theme (Visual Style )
#include <uxtheme.h>
#if _MSC_VER < 1700 // Before Visual Studio 2012
#include <vssym32.h>
#define final
#endif

#ifndef DragQueryFile
    #error "We must include ShellAPI.h"
#endif

#if ! defined(_WIN64)
    #undef GetWindowLongPtr

    inline LONG_PTR GetWindowLongPtr(HWND hwnd, int idx)
    {
        return static_cast<LONG_PTR>(GetWindowLong(hwnd, idx));
    } // GetWindowLongPtr

    #undef SetWindowLongPtr
    inline LONG_PTR SetWindowLongPtr(HWND hwnd, int idx, LONG_PTR val)
    {
        return SetWindowLong(hwnd, idx, static_cast<LONG>(val));
    } // SetWindowLongPtr
#endif // ! defined(_WIN64)

// undocumented SWP flags. See http://www.winehq.org.
#if !defined(SWP_NOCLIENTSIZE)
    #define SWP_NOCLIENTSIZE    0x0800
    #define SWP_NOCLIENTMOVE    0x1000
#endif // !defined(SWP_NOCLIENTSIZE)

// Windows Vista
#if ! defined(LVS_EX_TRANSPARENTBKGND)
#define LVS_EX_TRANSPARENTBKGND 0x00400000
#define LVS_EX_TRANSPARENTSHADOWTEXT 0x00800000
#endif

#define WM_SYSTIMER 0x118

// warning C4481: nonstandard extension used: override specifier 'override'
#pragma warning(disable:4481)

// warning C4373: previous versions of the compiler did not override when
// parameters only differed by const/volatile qualifiers
#pragma warning(disable:4373)

typedef char int8;
typedef unsigned char uint8;
typedef unsigned int uint;
typedef unsigned int uint32;
typedef wchar_t char16;

#define unless(mp_exp)  if (! (mp_exp))
#define when(mp_exp)    if (mp_exp)
#define NoReturn __declspec(noreturn)

//#define ASSERT(mp_expr) __assume(mp_expr)
#include "./z_debug.h"

#define foreach(mp_ty, mp_enum, mp_init) \
    for (mp_ty mp_enum(mp_init); ! mp_enum.AtEnd(); mp_enum.Next())

#define lengthof(a) ( sizeof(a) / sizeof(*(a)) )

#define DISALLOW_COPY_AND_ASSIGN(mp_type) \
  private: mp_type(const mp_type&); \
  private: void operator=(const mp_type&)

// warning C6400: Using 'lstrcmpiW' to perform a case-insensitive compare to constant string 
#pragma warning(disable: 6246)

// warning C6246: Local declaration of 'oEnum' hides declaration of the same name in outer scope
#pragma warning(disable: 6400)

// ObjectInHeap
class ObjectInHeap
{
    public: void* operator new(size_t cb, HANDLE h)
        { return ::HeapAlloc(h, 0, cb); }

    private: void* operator new(size_t)
        { return NULL; }

    private: void operator delete(void*)
        {}
}; // ObjectInHeap

template<class T> void
swap(T& rx, T& ry)
{
    T temp = rx;
    rx = ry;
    ry = temp;
} // swap

#if ! defined(SUPPORT_IME)
    #define SUPPORT_IME 1
#endif // ! defined(SUPPORT_IME)


class FileTime : public FILETIME
{
    public: FileTime()
    {
        dwLowDateTime  = 0;
        dwHighDateTime = 0;
    } // FileTime

    public: FILETIME& operator =(const FILETIME& ft)
    {
        dwLowDateTime  = ft.dwLowDateTime;
        dwHighDateTime = ft.dwHighDateTime;
        return *this;
    } // opeator =

    public: int Compare(const FILETIME* p) const
    {
        int iDiff = dwHighDateTime - p->dwHighDateTime;
        if (0 != iDiff) return iDiff;
        return dwLowDateTime - p->dwLowDateTime;
    } // Compare
}; // FileTime


//////////////////////////////////////////////////////////////////////
//
// FileHandle
//
class FileHandle
{
    public: HANDLE h;

    public: FileHandle(HANDLE hFile = INVALID_HANDLE_VALUE)
    {
        h = hFile;
    } // FileHandle

    public: ~FileHandle()
    {
        Release();
    } // ~FileHandle

    public: operator HANDLE() const { return h; }

    public: FileHandle& operator =(HANDLE hHandle)
    {
        Attach(hHandle);
        return *this;
    } // operator =

    public: void Attach(HANDLE hHandle)
    {
        Release();
        h  = hHandle;
    } // Attach

    public: HANDLE Detach()
        { HANDLE h1 = h; h = INVALID_HANDLE_VALUE; return h1; }

    public: void Release()
    {
        if (INVALID_HANDLE_VALUE != h)
        {
            ::CloseHandle(h);
            h = INVALID_HANDLE_VALUE;
        }
    } // Release
}; // FileHandle


//////////////////////////////////////////////////////////////////////
//
// Handle
//
class Handle
{
    public: HANDLE h;

    public: Handle(HANDLE hFile = NULL)
    {
        h = hFile;
    } // Handle

    public: ~Handle()
    {
        Release();
    } // ~Handle

    public: operator HANDLE() const { return h; }

    public: Handle& operator =(HANDLE hHandle)
    {
        Attach(hHandle);
        return *this;
    } // operator =

    public: void Attach(HANDLE hHandle)
    {
        Release();
        h  = hHandle;
    } // Attach

    public: HANDLE Detach()
        { HANDLE h1 = h; h = NULL; return h1; }

    public: void Release()
    {
        if (NULL != h)
        {
            ::CloseHandle(h);
            h = NULL;
        }
    } // Release
}; // Handle


//////////////////////////////////////////////////////////////////////
//
// RegKey
//  Smart handle of HKEY.
//
class RegKey
{
    public: HKEY h;

    public: RegKey() : h(NULL) {}

    public: ~RegKey()
    {
        if (NULL != h)
        {
            ::RegCloseKey(h);
        }
    } // ~RegKey

    public: operator HKEY() const { return h; }
}; // RegKey


#include "./ed_defs.h"

typedef Edit::Count Count;
typedef Edit::Posn  Posn;

#include <commctrl.h>


#if NDEBUG
    #if defined (_M_IX86)
        #pragma function(memcpy)
        //#pragma function(memset)
    #endif // defined (_M_IX86)
    extern "C" void* myCopyMemory(void*, const void*, size_t);
    extern "C" void* myMoveMemory(void*, const void*, size_t);
    extern "C" void* myZeroMemory(void*, size_t);
#else
    #define myCopyMemory(d, s, n) ::CopyMemory(d, s, n)
    #define myMoveMemory(d, s, n) ::MoveMemory(d, s, n)
    #define myZeroMemory(d, n)    ::ZeroMemory(d, n)

#endif // NDEBUG


char16* lstrchrW(const char16*, char16);
char16* lstrrchrW(const char16*, char16);

#define DEBUG_DESTROY 1

#endif //!defined(INCLUDE_listener_winapp_precomp_h)
