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
static BaseWindow* s_focus_pseudo_window;
static BaseWindow* s_set_focus_caller;

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

void BaseWindow::Destroy() {
  ASSERT(IsRealized());
  ::DestroyWindow(m_hwnd);
}

void BaseWindow::DidSetFocus() {
  DEBUG_PRINTF("%p\n", this);
  s_focus_pseudo_window = this;
}

BaseWindow* BaseWindow::MapHwndToWindow(HWND const hwnd) {
  return reinterpret_cast<BaseWindow*>(
    static_cast<LONG_PTR>(::GetWindowLongPtrW(hwnd, GWLP_USERDATA)));
}

void BaseWindow::SetFocus() {
  ASSERT(m_hwnd);
  //ASSERT(!s_set_focus_caller);
  if (auto const focus_window = s_focus_pseudo_window) {
    s_focus_pseudo_window = nullptr;
    focus_window->DidKillFocus();
  }

  if (::GetFocus() == m_hwnd) {
    DidSetFocus();
    return;
  }

  s_set_focus_caller = this;
  ::SetFocus(m_hwnd);
}

// BaseWindow::windowProc
LRESULT CALLBACK BaseWindow::windowProc(
    HWND    hwnd,
    UINT    uMsg,
    WPARAM  wParam,
    LPARAM  lParam )
{
    BaseWindow* pWnd = MapHwndToWindow(hwnd);

    if (NULL == pWnd)
    {
        pWnd = sm_pCreateWnd;
        pWnd->m_hwnd = hwnd;
        ::SetWindowLongPtrW(
            hwnd,
            GWLP_USERDATA,
            static_cast<LONG>(reinterpret_cast<LONG_PTR>(pWnd)) );
    } // if NULL == pWnd

    switch (uMsg) {
    case WM_KILLFOCUS:
      if (auto const window = s_focus_pseudo_window) {
        s_focus_pseudo_window = nullptr;
        window->DidKillFocus();
        return 0;
      }
      pWnd->DidKillFocus();
      return 0;

    case WM_NCDESTROY:
        pWnd->m_hwnd = NULL;
        delete pWnd;
        return 0;

    case WM_SETFOCUS: {
      if (auto const caller = s_set_focus_caller) {
        s_set_focus_caller = nullptr;
        caller->DidSetFocus();
        s_focus_pseudo_window = caller;
      } else {
        pWnd->DidSetFocus();
        s_focus_pseudo_window = nullptr;
      }
      return 0;
    }
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
