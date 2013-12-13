#include "precomp.h"
// Copyright (C) 1996-2013 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
#include "widgets/container_widget.h"

#include "widgets/naitive_window.h"
#include <algorithm>

#define DEBUG_FOCUS _DEBUG
#define DEBUG_PAINT _DEBUG


namespace widgets {

ContainerWidget::ContainerWidget()
    : focus_widget_(nullptr),
      capture_widget_(nullptr) {
}

ContainerWidget::~ContainerWidget() {
}

bool ContainerWidget::Contains(const Widget& widget) const {
  if (widget.container_widget() == this) {
    ASSERT(std::find(child_widgets_.begin(), child_widgets_.end(),
                      &widget) != child_widgets_.end());
    return true;
  }

  for (auto const child: child_widgets_) {
    if (child->Contains(widget))
      return true;
  }

  return false;
}

void ContainerWidget::DidRealizeWidget(const Widget& widget) {
  child_widgets_.push_back(const_cast<Widget*>(&widget));
}

void ContainerWidget::DispatchPaintMessage() {
  gfx::Rect exposed_rect;
  if (!::GetUpdateRect(*naitive_window(), &exposed_rect, false))
    return;
  #if DEBUG_PAINT
    DEBUG_PRINTF("Start %s@%p (%d,%d)x(%d,%d)\n", GetClass(), this,
        exposed_rect.left, exposed_rect.top,
        exposed_rect.width(), exposed_rect.height());
  #endif
  OnPaint(exposed_rect);
  #if DEBUG_PAINT
    DEBUG_PRINTF("End %s@%p (%d,%d)x(%d,%d)\n", GetClass(), this,
        exposed_rect.left, exposed_rect.top,
        exposed_rect.width(), exposed_rect.height());
  #endif

   for (auto child : child_widgets_) {
    if (!child->is_showing())
      continue;
    auto const rect = exposed_rect.Intersect(child->rect());
    if (rect) {
      #if DEBUG_PAINT
        DEBUG_PRINTF("Start %s@%p focus=%d (%d,%d)x(%d,%d)\n",
            child->GetClass(), child, child->has_focus(),
            rect.left, rect.top, rect.width(), rect.height());
      #endif
      child->OnPaint(rect);
      #if DEBUG_PAINT
        DEBUG_PRINTF("End %s@%p (%d,%d)x(%d,%d)\n",
            child->GetClass(), child,
            rect.left, rect.top, rect.width(), rect.height());
      #endif
    }
  }
}

Widget* ContainerWidget::GetWidgetAt(const gfx::Point& point) const {
  for (auto const child: child_widgets_) {
    if (child->rect().Contains(point))
      return child;
  }
  return nullptr;
}

void ContainerWidget::Hide() {
  for (auto const child: child_widgets_) {
   child->Hide();
  }
  Widget::Hide();
}

bool ContainerWidget::OnIdle(uint idle_count) {
  auto more = false;
  for (auto const child: child_widgets_) {
    if (child->OnIdle(idle_count))
      more = true;
  }
  return more;
}

void ContainerWidget::ReleaseCaptureFrom(const Widget& widget) {
  ASSERT(widget == capture_widget_);
  capture_widget_ = nullptr;
  ::ReleaseCapture();
}

void ContainerWidget::SetCaptureTo(const Widget& widget) {
  ASSERT(!capture_widget_);
  ::SetCapture(*naitive_window());
  capture_widget_ = const_cast<Widget*>(&widget);
}

bool ContainerWidget::SetCursor() {
  gfx::Point point;
  if (!::GetCursorPos(&point))
    return false;
  if (::ScreenToClient(*naitive_window(), &point))
    return false;
  auto const widget = GetWidgetAt(point);
  if (!widget)
    return false;
  auto const hCursor = GetCursorAt(point);
  if (!hCursor)
    return false;
  ::SetCursor(hCursor);
  return true;
}

void ContainerWidget::SetFocusTo(const Widget& widget) {
  #if DEBUG_FOCUS
    DEBUG_PRINTF("%s@%p native_focus=%d new=%s@%p cur=%s@%p\n",
        GetClass(), this,
        ::GetFocus() == AssociatedHwnd(),
        widget.GetClass(), &widget,
        focus_widget_ ? focus_widget_->GetClass() : "null", focus_widget_);
  #endif

  if (!naitive_window()) {
    container_widget().SetFocusTo(widget);
    return;
  }

  ASSERT(Contains(widget));

  auto const hwnd = static_cast<HWND>(*naitive_window());
  if (::GetFocus() != hwnd) {
    ASSERT(!focus_widget_);
    ::SetFocus(hwnd);
    return;
  }

  if (widget == focus_widget_)
    return;

  if (auto previous = focus_widget_) {
    focus_widget_ = nullptr;
    previous->DidKillFocus();
  }
  focus_widget_ = const_cast<Widget*>(&widget);
  focus_widget_->DidSetFocus();
}

void ContainerWidget::Show() {
  Widget::Show();
  for (auto const child: child_widgets_) {
   child->Show();
  }
}

void ContainerWidget::WillDestroyWidget(const Widget& widget) {
  auto it = std::find(child_widgets_.begin(), child_widgets_.end(),
                      &widget);
  ASSERT(it != child_widgets_.end());
  child_widgets_.erase(it);
}

LRESULT ContainerWidget::WindowProc(UINT message, WPARAM wParam,
                                   LPARAM lParam) {
  switch(message) {
    case WM_KILLFOCUS:
      #if DEBUG_FOCUS
        DEBUG_PRINTF("WM_KILLFOCUS %p cur=%s@%p\n", this,
            focus_widget_ ? focus_widget_->GetClass() : "null", focus_widget_);
      #endif
      if (auto widget = focus_widget_) {
        focus_widget_ = nullptr;
        widget->DidKillFocus();
        return 0;
      }
      break;

    case WM_PAINT:
      DispatchPaintMessage();
      ::ValidateRect(*naitive_window(), nullptr);
      return 0;

    case WM_SETCURSOR:
      if (LOWORD(lParam) == HTCLIENT && SetCursor())
        return true;
      break;

    case WM_SETFOCUS:
      #if DEBUG_FOCUS
        DEBUG_PRINTF("WM_SETFOCUS %p cur=%s%p\n", this,
            focus_widget_ ? focus_widget_->GetClass() : "null", focus_widget_);
      #endif
      if (auto widget = focus_widget_) {
        widget->DidSetFocus();
        return 0;
      }
      break;
  }

  if (focus_widget_) {
    if (message >= WM_KEYFIRST && message <= WM_KEYLAST)
      return focus_widget_->OnMessage(message, wParam, lParam);
  }

  if (message >= WM_MOUSEFIRST && message <= WM_MOUSELAST) {
    if (capture_widget_) {
      capture_widget_->OnMessage(message, wParam, lParam);
    } else {
      const gfx::Point point(MAKEPOINTS(lParam));
      if (auto widget = GetWidgetAt(point))
        return widget->OnMessage(message, wParam, lParam);
    }
  }

  return Widget::WindowProc(message, wParam, lParam);
}

} // namespace widgets
