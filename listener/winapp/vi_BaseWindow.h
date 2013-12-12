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
  public: class MessageResult {
    private: LRESULT lResult_;
    private: bool is_handled_;
    public: MessageResult() : lResult_(0), is_handled_(false) {}
    public: MessageResult(LRESULT lResult)
        : lResult_(lResult), is_handled_(true) {
    }
    public: bool is_handled() const { return is_handled_; }
    public: LRESULT lResult() const { return lResult_; }
  };

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

    public: bool operator!=(const BaseWindow* other) const {
      return this != other;
    }

    public: bool operator!=(HWND hwnd) const {
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
    public: virtual void Destroy();
    public: virtual void DidKillFocus() {}
    public: virtual void DidSetFocus();

    // [I]
    public: static int Init();
    public: virtual bool IsRealized() const { return m_hwnd; }

    // [M]
    protected: static BaseWindow* MapHwndToWindow(HWND);

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

    public: virtual void SetFocus();

    // [W]
    static LRESULT CALLBACK windowProc(
        HWND    hwnd,
        UINT    uMsg,
        WPARAM  wParam,
        LPARAM  lParam );

    DISALLOW_COPY_AND_ASSIGN(BaseWindow);
}; // BaseWindow

#endif //!defined(INCLUDE_visual_basewnd_h)
