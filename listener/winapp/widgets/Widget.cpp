#include "precomp.h"
// Copyright (C) 1996-2013 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
#include "widgets/widget.h"

#include "widgets/container_widget.h"
#include "widgets/naitive_window.h"

namespace widgets {

namespace {
class TopLevelWidget : public ContainerWidget {
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
  auto runner = this;
  while (runner && !runner->is_top_level()) {
    if (auto window = runner->naitive_window_) {
      if (::GetFocus() != *window)
        return false;
      if (runner == this)
        return true;
      if (auto container = runner->ToContainer())
        return container->focus_widget() == this;
      return false;
    }
    runner = runner->container_widget_;
  }
  return false;
}

ContainerWidget& Widget::top_level_widget() {
  static ContainerWidget* top_level_widget;
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
  CAN_NOT_HAPPEN();
}

bool Widget::Contains(const Widget&) const {
  return false;
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

HCURSOR Widget::GetCursorAt(const gfx::Point&) const {
  return nullptr;
}

void Widget::Hide() {
  showing_ = 0;
  if (naitive_window_)
    ::ShowWindow(*naitive_window_, SW_HIDE);
  else
    DidHide();
}

bool Widget::OnIdle(uint) {
  return false;
}

void Widget::OnLeftButtonDown(uint, const gfx::Point&) {
}

void Widget::OnLeftButtonUp(uint, const gfx::Point&) {
}

LRESULT Widget::OnMessage(UINT message, WPARAM wParam, LPARAM lParam) {
  switch (message) {
    case WM_LBUTTONDOWN:
      OnLeftButtonDown(static_cast<uint>(wParam), 
                       gfx::Point(MAKEPOINTS(lParam)));
      return 0;

    case WM_LBUTTONUP:
      OnLeftButtonUp(static_cast<uint>(wParam), 
                     gfx::Point(MAKEPOINTS(lParam)));
      return 0;

    case WM_MOUSEMOVE:
      OnMouseMove(static_cast<uint>(wParam), 
                  gfx::Point(MAKEPOINTS(lParam)));
      return 0;
  }
  if (naitive_window_)
    return naitive_window_->DefWindowProc(message, wParam, lParam);
  return container_widget_->OnMessage(message, wParam, lParam);
}

void Widget::OnMouseMove(uint, const gfx::Point&) {
}

void Widget::OnPaint(const gfx::Rect) {
}

void Widget::Realize(const ContainerWidget& container_widget,
                     const gfx::Rect& rect) {
  ASSERT(!naitive_window_);
  ASSERT(!realized_);
  ASSERT(container_widget.is_realized());
  container_widget_ = const_cast<ContainerWidget*>(&container_widget);
  realized_ = true;
  rect_ = rect;
  auto const parent_style = ::GetWindowLong(AssociatedHwnd(), GWL_STYLE);
  if (parent_style & WS_VISIBLE)
    ++showing_;
  DidRealize();
  container_widget_->DidRealizeWidget(*this);
}

void Widget::Realize(const ContainerWidget& container_widget,
                     const gfx::Rect& rect, DWORD dwExStyle, DWORD dwStyle) {
  ASSERT(!naitive_window_);
  ASSERT(!realized_);
  ASSERT(container_widget.is_realized());
  container_widget_ = const_cast<ContainerWidget*>(&container_widget);
  realized_ = true;
  naitive_window_ = new NaitiveWindow(*this);
  naitive_window_->CreateWindowEx(dwExStyle, dwStyle, AssociatedHwnd(),
                                  rect.left_top(), rect.size());
}

void Widget::Realize(DWORD dwExStyle, DWORD dwStyle, const gfx::Size& size) {
  ASSERT(!naitive_window_);
  ASSERT(!realized_);
  container_widget_ = &top_level_widget();
  realized_ = true;
  naitive_window_ = new NaitiveWindow(*this);
  naitive_window_->CreateWindowEx(dwExStyle, dwStyle, nullptr,
                                  gfx::Point(CW_USEDEFAULT, CW_USEDEFAULT), 
                                  size);
}

void Widget::ReleaseCapture() {
  container_widget().ReleaseCaptureFrom(*this);
}

void Widget::ResizeTo(const gfx::Rect& rect) {
  ASSERT(realized_);
  if (naitive_window_) {
    ::SetWindowPos(*naitive_window_, nullptr, rect.left, rect.right,
                   rect.width(), rect.height(), SWP_NOACTIVATE);
  } else {
    rect_ = rect;
    DidResize();
  }
}

void Widget::SetCapture() {
  container_widget().SetCaptureTo(*this);
}

void Widget::SetFocus() {
  container_widget().SetFocusTo(*this);
}

void Widget::Show() {
  ++showing_;
  if (naitive_window_) {
    ::ShowWindow(*naitive_window_, SW_SHOW);
  } else if (showing_ == 1) {
    DidShow();
    OnPaint(rect());
  }
}

void Widget::WillDestroyWidget() {
}

void Widget::WillDestroyNaitiveWindow() {
}

LRESULT Widget::WindowProc(UINT message, WPARAM wParam, LPARAM lParam) {
  ASSERT(naitive_window_);
  switch (message) {
    case WM_CREATE:
      if (reinterpret_cast<CREATESTRUCT*>(lParam)->style & WS_VISIBLE)
        ++showing_;
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
  return OnMessage(message, wParam, lParam);
}

} // namespace widgets
