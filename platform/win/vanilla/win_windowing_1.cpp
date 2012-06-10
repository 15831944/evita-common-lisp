#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - platform - win - windowing
// platform/win/vanilla/win_windowing.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/platform/win/vanilla/win_windowing_1.cpp#1 $
//
// Description:
//  Installs following C implemented lisp function for genesis.
//
#include "../../../big/big_lisp.h"

namespace MiniLisp
{
    Val internal_find_package(Val);
} // MiniLisp

using namespace MiniLisp;

extern HINSTANCE g_hInstance;

static Val CLASSD_win32_window;
static Val CLASSD_win32_context;
static Int TLV_win32_AcontextA;

#undef SetWindowLongPtr
inline LONG_PTR SetWindowLongPtr(
    HWND        hwnd,
    int         iIndex,
    LONG_PTR    lValue )
{
    return SetWindowLongPtrW(hwnd, iIndex, (LONG) lValue);
} // SetWindowLongPtr

class win32_window
{
    public: Val m_classd;
    public: Val m_hdc;
    public: Val m_handle;
    public: static Val ClassD() { return CLASSD_win32_window; }
}; // win32_window


class win32_context
{
    public: Val m_classd;
    public: Val m_applications;
    public: Val m_creation;
    public: Val m_event_function;
    public: Val m_idle_function;
    public: Val m_instance;
    public: Val m_msgbuf;
    public: Val m_plist;
    public: static Val ClassD() { return CLASSD_win32_context; }
}; // win32_context


class Handle           : public Kernel::Record_<Layout::C_handle> {};
class WindowingContext : public Kernel::Record_<win32_context> {};
class Window           : public Kernel::Record_<win32_window> {};


static Val find_symbol(const char16* pwszName, Val package)
{
    StackString_<> name(pwszName);
    Val status;
    return CommonLisp::find_symbol(name, package, &status);
} // find_symbol


//////////////////////////////////////////////////////////////////////
//
// on_windowing_idle
//
bool on_windowing_idle(uint nCount)
{
    if (0 == TLV_win32_AcontextA)
    {
        return false;
    }

    Val context = TLV(win32_AcontextA);
    if (nil == context)
    {
        return false;
    }

    WindowingContext* pContext = context->Decode<WindowingContext>();

    Val morep = funcall(
        pContext->m_idle_function,
        context,
        Fixnum::Encode(nCount) );

    return nil != morep;
} // on_windowing_idle


//////////////////////////////////////////////////////////////////////
//
// windowProc
//
// The window procedure for all lisp window.
//
static LRESULT CALLBACK
windowProc(
    HWND    hwnd,
    UINT    uMsg,
    WPARAM  wParam,
    LPARAM  lParam )
{
    WindowingContext* pContext = TLV(win32_AcontextA)->
        Decode<WindowingContext>();

    Kernel::GcAnchor* pGcAnchor =
        reinterpret_cast<GcAnchor*>(
            static_cast<INT_PTR>(
                ::GetWindowLongPtr(hwnd, GWLP_USERDATA) ));

    Val winobj;
    if (NULL != pGcAnchor)
    {
        winobj = pGcAnchor->m_value;
    }
    else
    {
        winobj = pContext->m_creation;

        pGcAnchor = Kernel::Memory::AllocGcAnchor(winobj);

        ::SetWindowLongPtr(
            hwnd,
            GWLP_USERDATA,
            reinterpret_cast<LONG_PTR>(pGcAnchor) );

        winobj->Decode<Window>()->m_handle =
            make_int(reinterpret_cast<Int>(hwnd));
    }

    switch (uMsg)
    {
    case WM_NCDESTROY:
    {
        Kernel::Memory::FreeGcAnchor(pGcAnchor);
        break;
    } // WM_NCDESTROY

    case WM_PAINT:
    {
        PAINTSTRUCT ps;
        HDC hdc = ::BeginPaint(hwnd, &ps);

        winobj->Decode<Window>()->m_hdc->Decode<Handle>()->m_value =
            reinterpret_cast<Val>(hdc);

        funcall(pContext->m_event_function,
            winobj,
            Fixnum::Encode(uMsg),
            Fixnum::Encode(0),
            Fixnum::Encode(&ps) );

        winobj->Decode<Window>()->m_hdc->Decode<Handle>()->m_value =
            Fixnum::Encode(0);

        ::EndPaint(hwnd, &ps);
        return 0;
    } // WM_PAINT
    } // switch

    Val value = funcall(
        pContext->m_event_function,
        winobj,
        Fixnum::Encode(uMsg),
        make_int(static_cast<Int>(wParam)),
        make_int(static_cast<Int>(lParam)) );

    if (nil == value)
    {
        return ::DefWindowProc(hwnd, uMsg, wParam, lParam);
    }

    if (value->Is<Fixnum>())
    {
        return Fixnum::Decode_(value);
    }

    return value->Decode<Bignum>()->m_rgBigit[0];
} // windowProc


//////////////////////////////////////////////////////////////////////
//
// initialize_windowing
//
Val initialize_windowing(Val context)
{
    StackString_<> STR_win32(L"WIN32");
    Val PKG_win32 = find_package(STR_win32);

    TLV_win32_AcontextA = deftlv(
        find_symbol(L"*CONTEXT*", PKG_win32),
        nil,
        nil,
        nil );

    Val CLASS_win32_context = find_class(
        find_symbol(L"CONTEXT", PKG_win32) );

    CLASSD_win32_context =
        CLASS_win32_context->Decode<StructureClass>()->
            m_instanced;

    Val CLASS_win32_window = find_class(
        find_symbol(L"WINDOW", PKG_win32) );

    CLASSD_win32_window =
        CLASS_win32_window->Decode<StructureClass>()->
            m_instanced;

    WindowingContext* pContext = context->
        Decode<WindowingContext>();

    pContext->m_instance = make_int(
        reinterpret_cast<Int>(g_hInstance) );

    WNDCLASSEX oWC;
    ::ZeroMemory(&oWC, sizeof(oWC));
    oWC.cbSize        = sizeof(oWC);
    oWC.style         = CS_DBLCLKS;
    oWC.lpfnWndProc   = windowProc;
    oWC.hInstance     = g_hInstance;
    oWC.hCursor       = ::LoadCursor(NULL, IDC_ARROW);
    oWC.lpszClassName = L"LispWindowClass";
    return make_int(::RegisterClassEx(&oWC));
} // windowing_initialize
