#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - listener - winapp - Widget
// listener/winapp/winmain.cpp
//
// Copyright (C) 1996-2013 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/listener/winapp/vi_Pane.cpp#1 $
//
#include "widgets/widget.h"

#include "widgets/container_widget.h"
#include "widgets/naitive_window.h"

namespace widgets {

namespace {
class TopLevelWidget : public Widget {
  private: virtual bool is_top_level() const { return true; }
};
}

Widget::Widget()
    : container_widget_(nullptr),
      naitive_window_(nullptr),
      realized_(false),
      showing_(0) {
}

Widget::~Widget() {
  ASSERT(!naitive_window_);
}

bool Widget::has_focus() const {
  return container_widget().focus_widget() == this;
}

Widget& Widget::top_level_widget() {
  static Widget* top_level_widget;
  if (!top_level_widget)
    top_level_widget = new TopLevelWidget();
  return *top_level_widget;
}

HWND Widget::AssociatedHwnd() const {
  auto runner = this;
  while (runner && !runner->is_top_level()) {
    if (auto window = runner->naitive_window_)
      return *window;
    runner = runner->container_widget_;
  }
  return nullptr;
}

void Widget::Destroy() {
  if (naitive_window_) {
    ::DestroyWindow(*naitive_window_);
    return;
  }
  WillDestroyWidget();
  container_widget_->WillDestroyWidget(*this);
  delete this;
}

void Widget::DidCreateNaitiveWindow() {
}

void Widget::DidDestroyNaitiveWindow() {
  ASSERT(naitive_window_);
  naitive_window_ = nullptr;
}

void Widget::Hide() {
  showing_ = 0;
  if (naitive_window_)
    ::ShowWindow(*naitive_window_, SW_HIDE);
}

LRESULT Widget::OnMessage(UINT uMsg, WPARAM wParam, LPARAM lParam) {
  ASSERT(naitive_window_);
  switch (uMsg) {
    case WM_CREATE:
      ::GetClientRect(*naitive_window_, &rect_);
      DidCreateNaitiveWindow();
      return 0;

    case WM_DESTROY:
      WillDestroyNaitiveWindow();
      return 0;

    case WM_KILLFOCUS:
      DidKillFocus();
      return 0;

    case WM_NCDESTROY:
      DidDestroyNaitiveWindow();
      return 0;

    case WM_SETFOCUS:
      DidSetFocus();
      return 0;

    case WM_WINDOWPOSCHANGED: {
      // DefWindowProc sents WM_SIZE and WM_MOVE, so handling
      // WM_WINDPOSCHANGED is faster than DefWindowProc.
      //
      // #define SWP_NOSIZE          0x0001
      // #define SWP_NOMOVE          0x0002
      // #define SWP_NOZORDER        0x0004
      // #define SWP_NOREDRAW        0x0008
      // #define SWP_NOACTIVATE      0x0010
      // #define SWP_FRAMECHANGED    0x0020  /* The frame changed: send WM_NCCALCSIZE */
      // #define SWP_SHOWWINDOW      0x0040
      // #define SWP_HIDEWINDOW      0x0080
      // #define SWP_NOCOPYBITS      0x0100
      // #define SWP_NOOWNERZORDER   0x0200  /* Don't do owner Z ordering */
      // #define SWP_NOSENDCHANGING  0x0400  /* Don't send WM_WINDOWPOSCHANGING */
      //
      // #define SWP_DRAWFRAME       SWP_FRAMECHANGED
      // #define SWP_NOREPOSITION    SWP_NOOWNERZORDER
      //
      //#if(WINVER >= 0x0400)
      // #define SWP_DEFERERASE      0x2000
      // #define SWP_ASYNCWINDOWPOS  0x4000
      //#endif /* WINVER >= 0x0400 */

      // undocumented SWP flags. See http://www.winehq.org.
      #if !defined(SWP_NOCLIENTSIZE)
          #define SWP_NOCLIENTSIZE    0x0800
          #define SWP_NOCLIENTMOVE    0x1000
      #endif // !defined(SWP_NOCLIENTSIZE)
      // Create   0x10001843  NOCLIENTMOVE NOCLIENTSIZE SHOWWINDOW NOMOV NOSIZE
      // Minimize 0x00008130
      // Restore  0x00008124
      // Move     0x00000A15
      // Destroy  0x20001897  NOCLIENTMOVE NOCLIENTSIZE HIDEWINDOW NOACTIVATE
      //                      NOZORDER NOMOVE NOSIZE
      //if (wp->flags & SWP_NOSIZE) return 0;

      auto const wp = reinterpret_cast<WINDOWPOS*>(lParam);

      if (wp->flags & SWP_HIDEWINDOW) {
        // We don't take care hidden window.
        return 0;
      }

      if (!(wp->flags & 0x10000000) && (wp->flags & SWP_NOSIZE))
        return 0;

      if (::IsIconic(*naitive_window_)) {
        // We don't take care miminize window.
        return 0;
      }

      ::GetClientRect(*naitive_window_, &rect_);
      DidResize();
      return 0;
    }
  }
  return naitive_window_->DefWindowProc(uMsg, wParam, lParam);
}

void Widget::Realize(const ContainerWidget& container_widget,
                     const Rect& rect) {
  ASSERT(!naitive_window_);
  ASSERT(!realized_);
  container_widget_ = const_cast<ContainerWidget*>(&container_widget);
  realized_ = true;
  rect_ = rect;
  DidRealize();
  container_widget_->DidRealizeWidget(*this);
}

void Widget::Realize(const ContainerWidget& container_widget,
                     const Rect& rect, DWORD dwExStyle, DWORD dwStyle) {
  ASSERT(!naitive_window_);
  ASSERT(!realized_);
  Realize(container_widget, rect);
  naitive_window_ = new NaitiveWindow(*this);
  naitive_window_->CreateWindowEx(dwExStyle, dwStyle, AssociatedHwnd(), rect);
}

void Widget::Realize(DWORD dwExStyle, DWORD dwStyle, int cx, int cy) {
  ASSERT(!naitive_window_);
  ASSERT(!realized_);
  realized_ = true;
  naitive_window_ = new NaitiveWindow(*this);
  naitive_window_->CreateWindowEx(dwExStyle, dwStyle, AssociatedHwnd(),
                                  Rect(CW_USEDEFAULT, CW_USEDEFAULT, cx, cy));
}

void Widget::ResizeTo(const Rect& rect) {
  ASSERT(realized_);
  if (naitive_window_) {
    ::SetWindowPos(*naitive_window_, nullptr, rect.left, rect.right,
                   rect.width(), rect.height(), SWP_NOACTIVATE);
  } else {
    rect_ = rect;
    DidResize();
  }
}

void Widget::SetFocus() {
  container_widget().SetFocusTo(*this);
}

void Widget::Show() {
  ++showing_;
  if (naitive_window_)
    ::ShowWindow(*naitive_window_, SW_HIDE);
}

void Widget::WillDestroyWidget() {
}

void Widget::WillDestroyNaitiveWindow() {
}

} // namespace widgets
