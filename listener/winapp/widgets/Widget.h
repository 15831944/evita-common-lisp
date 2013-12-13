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
#if !defined(INCLUDE_widgets_widget_h)
#define INCLUDE_widgets_widget_h

#include "./li_util.h"
#include "./vi_defs.h"

#define MY_VK_CONTROL   0x100
#define MY_VK_SHIFT     0x200

namespace widgets {

class ContainerWidget;
class NaitiveWindow;

//////////////////////////////////////////////////////////////////////
//
// Widget
//
class Widget {
  private: ContainerWidget* container_widget_;
  private: NaitiveWindow* naitive_window_;
  private: bool realized_;
  private: Rect rect_;
  private: int showing_;

  protected: Widget();
  protected: ~Widget();

  public: ContainerWidget& container_widget() const {
    ASSERT(container_widget_);
    return *container_widget_;
  }

  public: bool operator==(const Widget* other) const {
    return this == other;
  }

  public: bool operator!=(const Widget* other) const {
    return this != other;
  }

  public: bool has_focus() const;
  public: bool is_realized() const { return realized_; }
  public: bool is_showing() const { return showing_; }
  protected: virtual bool is_top_level() const { return false; }
  protected: NaitiveWindow* naitive_window() const {
    return naitive_window_;
  }
  public: const Rect& rect() const { return rect_; }
  public: static Widget& top_level_widget();

  // [A]
  public: HWND AssociatedHwnd() const;

  // [D]
  public: void Destroy();
  protected: virtual void DidCreateNaitiveWindow();
  protected: virtual void DidDestroyNaitiveWindow();
  public: virtual void DidHide() {}
  public: virtual void DidKillFocus() {}
  public: virtual void DidRealize() {}
  public: virtual void DidResize() {}
  public: virtual void DidSetFocus() {}
  public: virtual void DidShow() {}

  // [G]
  public: static const char16* GetClass_() { return L"Widget"; }

  public: void Hide();

  // [O]
  public: virtual bool OnIdle(uint) { return false; }
  public: virtual LRESULT OnMessage(UINT uMsg, WPARAM wParam,
                                    LPARAM lParam);

  // [R]
  public: void Realize(const ContainerWidget& container, const Rect& rect);
  public: void Realize(const ContainerWidget& container, const Rect& rect,
                       DWORD dwExStyle, DWORD dwStyle);
  public: void Realize(DWORD dwExStyle, DWORD dwStyle, int cx, int cy);
  public: void ResizeTo(const Rect& rect);

  // [S]
  public: virtual void SetFocus();
  public: void Show();

  // [W]
  public: virtual void WillDestroyWidget();
  public: virtual void WillDestroyNaitiveWindow();

  DISALLOW_COPY_AND_ASSIGN(Widget);
};

} // namespace widgets

#endif //!defined(INCLUDE_widgets_widget_h)
