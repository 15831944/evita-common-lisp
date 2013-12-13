//////////////////////////////////////////////////////////////////////////////
//
// evcl - listener - edit buffer
// listener/winapp/ed_buffer.h
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/listener/winapp/vi_Widget.h#1 $
//
#if !defined(INCLUDE_widgets_native_widget_h)
#define INCLUDE_widgets_native_widget_h

#include "./li_util.h"
#include "widgets/widget.h"

#define MY_VK_CONTROL   0x100
#define MY_VK_SHIFT     0x200

namespace widgets {

//////////////////////////////////////////////////////////////////////
//
// NaitiveWindow
//
class NaitiveWindow {
  protected: HWND hwnd_;
  private: Widget* widget_;

  public: explicit NaitiveWindow(const Widget& widget);
  protected: NaitiveWindow();
  protected: virtual ~NaitiveWindow();

  public: operator HWND() const { return hwnd_; }

  public: bool operator==(const NaitiveWindow* other) const {
    return this == other;
  }

  public: bool operator==(HWND hwnd) const {
    return hwnd_ == hwnd;
  }

  public: bool operator!=(const NaitiveWindow* other) const {
    return this != other;
  }

  public: bool operator!=(HWND hwnd) const {
    return hwnd_ == hwnd;
  }

  // [C]
  public: bool CreateWindowEx(DWORD dwExStyle, DWORD dwStyle,
                              HWND parent_hwnd, const Rect& rect);

  // [D]
  public: void Destroy();
  public: LRESULT DefWindowProc(UINT message, WPARAM wParam, LPARAM lParam);

  // [I]
  public: static int Init();
  public: virtual bool IsRealized() const { return hwnd_; }

  // [M]
  protected: static NaitiveWindow* MapHwnToNaitiveWindow(HWND);

  // [O]
  protected: virtual LRESULT OnMessage(UINT message, WPARAM wParam,
                                       LPARAM lParam);

  // [S]
  public: LRESULT SendMessage(uint uMsg, WPARAM wParam = 0,
                              LPARAM lParam = 0) {
    return ::SendMessage(hwnd_, uMsg, wParam, lParam);
  }

  // [W]
  private: static LRESULT CALLBACK WindowProc(HWND hwnd, UINT uMsg,
                                              WPARAM wParam, LPARAM lParam);

  DISALLOW_COPY_AND_ASSIGN(NaitiveWindow);
};

} // namespace widgets

#endif //!defined(INCLUDE_widgets_native_widget_h)
