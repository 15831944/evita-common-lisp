// Copyright (C) 1996-2013 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
#if !defined(INCLUDE_widgets_widget_h)
#define INCLUDE_widgets_widget_h

#include "base/tree/node.h"
#include "./li_util.h"
#include "gfx/rect.h"

namespace widgets {

class ContainerWidget;
class NaitiveWindow;

//////////////////////////////////////////////////////////////////////
//
// Widget
//
class Widget : public base::tree::Node_<Widget, ContainerWidget> {
  private: NaitiveWindow* naitive_window_;
  private: bool realized_;
  private: gfx::Rect rect_;
  private: int showing_;

  protected: Widget();
  protected: ~Widget();

  public: ContainerWidget& container_widget() const {
    ASSERT(parent_node());
    return *parent_node();
  }
  public: bool has_focus() const;
  public: virtual bool is_container() const { return false; }
  public: bool is_realized() const { return realized_; }
  public: bool is_showing() const { return showing_; }
  // Expose |is_top_level()| for iterator.
  public: virtual bool is_top_level() const { return false; }
  protected: NaitiveWindow* naitive_window() const {
    return naitive_window_;
  }
  public: const gfx::Rect& rect() const { return rect_; }
  public: static ContainerWidget& top_level_widget();

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
  public: void ReleaseCapture() const;
  public: void ResizeTo(const gfx::Rect& rect);

  // [S]
  public: void SetCapture() const;
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

#define DEBUG_WIDGET_PRINTF(mp_format, ...) \
  DEBUG_PRINTF(DEBUG_WIDGET_FORMAT " " mp_format, \
    DEBUG_WIDGET_ARG(this), __VA_ARGS__)

#define DEBUG_WIDGET_FORMAT "%s@%p"
#define DEBUG_WIDGET_ARG(mp_widget) \
  ((mp_widget) ? (mp_widget)->GetClass() : "null"), (mp_widget)

} // namespace widgets

#endif //!defined(INCLUDE_widgets_widget_h)