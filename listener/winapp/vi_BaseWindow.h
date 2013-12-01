//////////////////////////////////////////////////////////////////////////////
//
// evcl - listener - edit buffer
// listener/winapp/ed_buffer.h
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/listener/winapp/vi_BaseWindow.h#1 $
//
#if !defined(INCLUDE_visual_basewnd_h)
#define INCLUDE_visual_basewnd_h

#include "./li_util.h"

#define MY_VK_CONTROL   0x100
#define MY_VK_SHIFT     0x200

class BaseWindow;
class FrameWindow;
class Pane;


//////////////////////////////////////////////////////////////////////
//
// BaseWindow
//
class BaseWindow
{
    protected: static ATOM sm_atomWndClass;
    static BaseWindow* sm_pCreateWnd;

    protected: HWND m_hwnd;

    public:    BaseWindow() : m_hwnd(NULL) {}
    protected: virtual ~BaseWindow();

    public: operator HWND() const { return m_hwnd; }

    public: bool operator==(const BaseWindow* other) const {
      return this == other;
    }

    public: bool operator==(HWND hwnd) const {
      return m_hwnd == hwnd;
    }

    // [C]
    public: bool CreateWindowEx(
        DWORD   dwExStyle,
        LPCWSTR pwszText,
        DWORD   dwStyle,
        HWND    hwndParent = NULL,
        int     x = CW_USEDEFAULT,
        int     y = CW_USEDEFAULT,
        int     w = CW_USEDEFAULT,
        int     h = CW_USEDEFAULT );

    // [D]
    public: void Destroy()
    {
        ASSERT(IsRealized());
        ::DestroyWindow(m_hwnd);
    } // Destroy

    // [I]
    public: static int Init();
    public: bool IsRealized() const { return NULL != m_hwnd; }

    // [O]
    public: virtual bool OnIdle(uint) { return false; }

    protected: virtual LRESULT onMessage(
        UINT    uMsg,
        WPARAM  wParam,
        LPARAM  lParam )
    {
        return ::DefWindowProc(m_hwnd, uMsg, wParam, lParam);
    } // windowProc

    // [S]
    public: LRESULT SendMessage(
        uint    uMsg,
        WPARAM  wParam = 0,
        LPARAM  lParam = 0 )
    {
        return ::SendMessage(m_hwnd, uMsg, wParam, lParam);
    } // SendMessage

    // [W]
    static LRESULT CALLBACK windowProc(
        HWND    hwnd,
        UINT    uMsg,
        WPARAM  wParam,
        LPARAM  lParam );
}; // BaseWindow

#endif //!defined(INCLUDE_visual_basewnd_h)
