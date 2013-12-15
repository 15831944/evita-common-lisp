#include "precomp.h"
// Copyright (C) 1996-2013 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
#include "widgets/widget.h"

#include "base/tree/ancestors_or_self.h"
#include "base/tree/descendants_or_self.h"
#include "widgets/container_widget.h"
#include "widgets/naitive_window.h"

#define DEBUG_SHOW _DEBUG

namespace widgets {

namespace {
class TopLevelWidget : public ContainerWidget {
  public: TopLevelWidget() {
  }
  private: virtual bool is_top_level() const { return true; }
  DISALLOW_COPY_AND_ASSIGN(TopLevelWidget);
};
}

Widget::Widget(std::unique_ptr<NaitiveWindow>&& naitive_window)
    : naitive_window_(std::move(naitive_window)),
      realized_(false),
      shown_(0) {
}

Widget::Widget()
    : Widget(NaitiveWindow::Create()) {
}

Widget::~Widget() {
  #if DEBUG_DESTROY
    DEBUG_WIDGET_PRINTF("realized=%d show=%d " DEBUG_RECT_FORMAT "\n",
        realized_, shown_, DEBUG_RECT_ARG(rect_));
  #endif
  ASSERT(!naitive_window_);
}

bool Widget::has_focus() const {
  for (auto& runner: base::tree::ancestors_or_self(*this)) {
    if (auto const window = runner.naitive_window_.get()) {
      if (::GetFocus() != *window)
        return false;
      if (runner == this)
        return true;
      if (auto const container = runner.ToContainer())
        return container->focus_widget() == this;
    }
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
  for (auto& runner: base::tree::ancestors_or_self(*this)) {
    if (auto const window = runner.naitive_window_.get())
      return *window;
  }
  CAN_NOT_HAPPEN();
}

void Widget::CreateNaitiveWindow() const {
}

void Widget::Destroy() {
  #if DEBUG_DESTROY
    DEBUG_WIDGET_PRINTF("realized=%d show=%d " DEBUG_RECT_FORMAT "\n",
        realized_, shown_, DEBUG_RECT_ARG(rect_));
  #endif
  if (naitive_window_) {
    ::DestroyWindow(*naitive_window_.get());
    return;
  }
  WillDestroyWidget();
  container_widget().WillDestroyChildWidget(*this);
  delete this;
}

void Widget::DidCreateNaitiveWindow() {
}

void Widget::DidDestroyNaitiveWindow() {
  #if DEBUG_DESTROY
    DEBUG_WIDGET_PRINTF("realized=%d show=%d " DEBUG_RECT_FORMAT "\n",
        realized_, shown_, DEBUG_RECT_ARG(rect_));
  #endif
  ASSERT(!naitive_window_);
  // Since naitive window, which handles UI, is destroyed, this widget should
  // be destroyed too.
  Destroy();
}

HCURSOR Widget::GetCursorAt(const gfx::Point&) const {
  return nullptr;
}

void Widget::Hide() {
  #if DEBUG_SHOW
    DEBUG_WIDGET_PRINTF("show=%d\n", shown_);
  #endif
  shown_ = 0;
  if (naitive_window_) {
    ::ShowWindow(*naitive_window_.get(), SW_HIDE);
  } else {
    if (has_focus())
      container_widget().SetFocus();
    DidHide();
  }
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
  return container_widget().OnMessage(message, wParam, lParam);
}

void Widget::OnMouseMove(uint, const gfx::Point&) {
}

void Widget::OnPaint(const gfx::Rect) {
}

void Widget::Realize(const ContainerWidget& container_widget,
                     const gfx::Rect& rect) {
  ASSERT(!realized_);
  ASSERT(container_widget.is_realized());
  const_cast<ContainerWidget&>(container_widget).AppendChild(*this);
  ASSERT(container_widget.Contains(*this));
  realized_ = true;
  rect_ = rect;
  if (naitive_window_) {
    CreateNaitiveWindow();
    return;
  }
  auto const parent_style = ::GetWindowLong(AssociatedHwnd(), GWL_STYLE);
  if (parent_style & WS_VISIBLE)
    ++shown_;
  DidRealize();
  this->container_widget().DidRealizeWidget(*this);
}

void Widget::RealizeTopLevelWidget() {
  ASSERT(naitive_window_);
  ASSERT(!realized_);
  top_level_widget().AppendChild(*this);
  realized_ = true;
  CreateNaitiveWindow();
}

void Widget::ReleaseCapture() const {
  if (naitive_window_) {
    ::ReleaseCapture();
    return;
  }
  container_widget().ReleaseCaptureFrom(*this);
}

void Widget::ResizeTo(const gfx::Rect& rect) {
  ASSERT(realized_);
  if (naitive_window_) {
    ::SetWindowPos(*naitive_window_.get(), nullptr, rect.left, rect.right,
                   rect.width(), rect.height(), SWP_NOACTIVATE);
  } else {
    rect_ = rect;
    DidResize();
  }
}

void Widget::SetCapture() const {
  if (naitive_window_) {
    ::SetCapture(*naitive_window_.get());
    return;
  }
  container_widget().SetCaptureTo(*this);
}

void Widget::SetFocus() {
  // This wieget might be hidden during creating window.
  if (naitive_window_) {
    ::SetFocus(*naitive_window_.get());
    return;
  }
  container_widget().SetFocusTo(*this);
}

void Widget::Show() {
  ++shown_;
  if (naitive_window_) {
    ::ShowWindow(*naitive_window_.get(), SW_SHOW);
  } else if (shown_ == 1) {
    DidShow();
    OnPaint(rect());
  }
}

void Widget::WillDestroyWidget() {
  #if DEBUG_DESTROY
    DEBUG_WIDGET_PRINTF("realized=%d show=%d " DEBUG_RECT_FORMAT "\n",
        realized_, shown_, DEBUG_RECT_ARG(rect_));
  #endif
}

void Widget::WillDestroyNaitiveWindow() {
  #if DEBUG_DESTROY
    DEBUG_WIDGET_PRINTF("realized=%d show=%d " DEBUG_RECT_FORMAT "\n",
        realized_, shown_, DEBUG_RECT_ARG(rect_));
  #endif
}

LRESULT Widget::WindowProc(UINT message, WPARAM wParam, LPARAM lParam) {
  ASSERT(naitive_window_);
  switch (message) {
    case WM_CREATE:
      if (reinterpret_cast<CREATESTRUCT*>(lParam)->style & WS_VISIBLE)
        ++shown_;
      ::GetClientRect(*naitive_window_.get(), &rect_);
      DidCreateNaitiveWindow();
      return 0;

    case WM_DESTROY:
      WillDestroyNaitiveWindow();
      return 0;

    case WM_KILLFOCUS:
      DidKillFocus();
      return 0;

    case WM_NCDESTROY:
      ASSERT(naitive_window_);
      // NativeWindow::WindowProc() will delete |native_window_|.
      naitive_window_.release();
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
        for (auto& widget: base::tree::descendants_or_self(*this)) {
          widget.shown_ = 0;
        }
        return 0;
      }

      if (wp->flags & SWP_SHOWWINDOW) {
        for (auto& widget: base::tree::descendants_or_self(*this)) {
          widget.shown_ = 1;
        }
      }

      if (!(wp->flags & 0x10000000) && (wp->flags & SWP_NOSIZE))
        return 0;

      if (::IsIconic(*naitive_window_.get())) {
        // We don't take care miminize window.
        return 0;
      }

      ::GetClientRect(*naitive_window_.get(), &rect_);
      DidResize();
      return 0;
    }
  }
  return OnMessage(message, wParam, lParam);
}

} // namespace widgets
