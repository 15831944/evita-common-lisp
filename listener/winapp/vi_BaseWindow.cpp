#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - listener - winapp - winmain
// listener/winapp/winmain.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/listener/winapp/vi_BaseWindow.cpp#3 $
//
#include "./vi_BaseWindow.h"

extern HINSTANCE g_hInstance;
extern HINSTANCE g_hResource;

ATOM BaseWindow::sm_atomWndClass;
BaseWindow* BaseWindow::sm_pCreateWnd;


BaseWindow::~BaseWindow()
{
    #if DEBUG_DESTROY
        DEBUG_PRINTF("%p\n", this);
    #endif

    ASSERT(NULL == m_hwnd);
} // BaseWindow::~BaseWindow

bool BaseWindow::CreateWindowEx(
    DWORD   dwExStyle,
    LPCWSTR pwszText,
    DWORD   dwStyle,
    HWND    hwndParent,
    int     x,
    int     y,
    int     cx,
    int     cy )
{
    sm_pCreateWnd = this;

    HWND hwnd = ::CreateWindowEx(
        dwExStyle,
        MAKEINTATOM(sm_atomWndClass),
        pwszText,
        dwStyle,
        x, y, cx, cy,
        hwndParent,
        NULL,   // hMenu
        g_hInstance,
        0 );    // lParam
    return NULL != hwnd;
} // BaseWindow::CreateWindowEx


// BaseWindow::windowProc
LRESULT CALLBACK BaseWindow::windowProc(
    HWND    hwnd,
    UINT    uMsg,
    WPARAM  wParam,
    LPARAM  lParam )
{
    BaseWindow* pWnd = reinterpret_cast<BaseWindow*>(
        static_cast<LONG_PTR>(::GetWindowLongPtrW(hwnd, GWLP_USERDATA)) );

    if (NULL == pWnd)
    {
        pWnd = sm_pCreateWnd;
        pWnd->m_hwnd = hwnd;
        ::SetWindowLongPtrW(
            hwnd,
            GWLP_USERDATA,
            static_cast<LONG>(reinterpret_cast<LONG_PTR>(pWnd)) );
    } // if NULL == pWnd

    if (WM_NCDESTROY == uMsg)
    {
        pWnd->m_hwnd = NULL;
        delete pWnd;
        return 0;
    }

    return pWnd->onMessage(uMsg, wParam, lParam);
} // BaseWindow::windowProc


// BaseWindow::Init
int BaseWindow::Init()
{
    WNDCLASSEXW oWC;
        oWC.cbSize          = sizeof(oWC);
        oWC.style           = CS_DBLCLKS |
                              CS_BYTEALIGNCLIENT;
        oWC.lpfnWndProc     = windowProc;
        oWC.cbClsExtra      = 0;
        oWC.cbWndExtra      = 0;
        oWC.hInstance       = g_hInstance;
        oWC.hIcon           =
            ::LoadIconW(g_hResource, MAKEINTRESOURCE(IDI_APPLICATION));
        oWC.hCursor         = NULL;
        oWC.hbrBackground   = (HBRUSH) (COLOR_WINDOW + 1);
        oWC.lpszMenuName    = NULL;
        oWC.lpszClassName   = L"EvitaWindow";
        oWC.hIconSm         =
            ::LoadIconW(g_hResource, MAKEINTRESOURCE(IDI_APPLICATION));

    sm_atomWndClass = ::RegisterClassExW(&oWC);
    if (sm_atomWndClass == 0)
    {
        return ::GetLastError();
    }

    return 0;
} // BaseWindow::init
