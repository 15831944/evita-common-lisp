#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - listener - winapp - winmain
// listener/winapp/winmain.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/listener/winapp/vi_Widget.cpp#3 $
//
#include "widgets/container_widget.h"

#include "widgets/naitive_window.h"
#include <algorithm>

namespace widgets {

ContainerWidget::ContainerWidget()
    : focus_widget_(nullptr) {
}

ContainerWidget::~ContainerWidget() {
}

bool ContainerWidget::contains(const Widget& widget) const {
  return std::find(child_widgets_.begin(), child_widgets_.end(), 
                   &widget) != child_widgets_.end();
}

LRESULT ContainerWidget::OnMessage(UINT message, WPARAM wParam,
                                   LPARAM lParam) {
  switch(message) {
    case WM_KILLFOCUS:
      if (auto widget = focus_widget_) {
        focus_widget_ = nullptr;
        widget->DidKillFocus();
        return 0;
      }
      break;

    case WM_SETFOCUS:
      if (auto widget = focus_widget_) {
        widget->DidSetFocus();
        return 0;
      }
      break;
  }
  return Widget::OnMessage(message, wParam, lParam);
}

void ContainerWidget::DidRealizeWidget(const Widget& widget) {
  child_widgets_.push_back(const_cast<Widget*>(&widget));
}

void ContainerWidget::SetFocusTo(const Widget& widget) {
  ASSERT(contains(widget));
  if (auto previous = focus_widget_) {
    previous->DidKillFocus();
    focus_widget_ = nullptr;
  }
  focus_widget_ = const_cast<Widget*>(&widget);
  auto const hwnd = AssociatedHwnd();
  ASSERT(hwnd);
  if (::GetFocus() == hwnd) {
    focus_widget_->DidSetFocus();
    return;
  }
  ::SetFocus(hwnd);
}

void ContainerWidget::WillDestroyWidget(const Widget& widget) {
  auto it = std::find(child_widgets_.begin(), child_widgets_.end(),
                      &widget);
  ASSERT(it != child_widgets_.end());
  child_widgets_.erase(it);
}

} // namespace widgets
