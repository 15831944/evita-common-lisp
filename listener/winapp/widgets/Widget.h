// Copyright (C) 1996-2013 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
#if !defined(INCLUDE_widgets_widget_h)
#define INCLUDE_widgets_widget_h

#include "./li_util.h"
#include "gfx/rect.h"

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
  private: gfx::Rect rect_;
  private: int showing_;

  protected: Widget();
  protected: ~Widget();

  public: bool operator==(const Widget* other) const {
    return this == other;
  }

  public: bool operator!=(const Widget* other) const {
    return this != other;
  }

  public: ContainerWidget& container_widget() const {
    ASSERT(container_widget_);
    return *container_widget_;
  }
  public: bool has_focus() const;
  public: bool is_realized() const { return realized_; }
  public: bool is_showing() const { return showing_; }
  protected: virtual bool is_top_level() const { return false; }
  protected: NaitiveWindow* naitive_window() const {
    return naitive_window_;
  }
  public: const gfx::Rect& rect() const { return rect_; }
  public: static ContainerWidget& top_level_widget();

  // [A]
  public: HWND AssociatedHwnd() const;

  // [C]
  public: virtual bool Contains(const Widget& widget) const;

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
  public: virtual const char* GetClass() const { return "Widget"; }
  public: virtual HCURSOR GetCursorAt(const gfx::Point& point) const;

  // [H]
  public: virtual void Hide();

  // [O]
  public: virtual bool OnIdle(uint idle_count);
  public: virtual void OnLeftButtonDown(uint flags, const gfx::Point& point);
  public: virtual void OnLeftButtonUp(uint flags, const gfx::Point& point);
  public: virtual LRESULT OnMessage(uint uMsg, WPARAM wParam, LPARAM lParam);
  public: virtual void OnMouseMove(uint flags, const gfx::Point& point);
  public: virtual void OnPaint(const gfx::Rect rect);

  // [R]
  // Realize widget, one of container must be realized with native widnow.
  public: void Realize(const ContainerWidget& container,
                       const gfx::Rect& rect);

  // Realize widget with native window.
  public: void Realize(const ContainerWidget& container,
                       const gfx::Rect& rect,
                       DWORD dwExStyle, DWORD dwStyle);

  // Realize top-level widget with native window.
  public: void Realize(DWORD dwExStyle, DWORD dwStyle, const gfx::Size& size);
  public: void ReleaseCapture();
  public: void ResizeTo(const gfx::Rect& rect);

  // [S]
  public: void SetCapture();
  public: virtual void SetFocus();
  public: virtual void Show();

  // [T]
  public: virtual const ContainerWidget* ToContainer() const {
    return nullptr;
  }
  public: virtual ContainerWidget* ToContainer() { return nullptr; }

  // [W]
  public: virtual void WillDestroyWidget();
  public: virtual void WillDestroyNaitiveWindow();
  public: virtual LRESULT WindowProc(UINT uMsg, WPARAM wParam,
                                     LPARAM lParam);

  DISALLOW_COPY_AND_ASSIGN(Widget);
};

} // namespace widgets

#endif //!defined(INCLUDE_widgets_widget_h)
