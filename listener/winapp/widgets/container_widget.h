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
#if !defined(INCLUDE_widgets_container_widget_h)
#define INCLUDE_widgets_container_widget_h

#include "widgets/widget.h"
#include <vector>

namespace widgets {

class ContainerWidget : public Widget {
  private: std::vector<Widget*> child_widgets_;
  private: Widget* focus_widget_;

  public: ContainerWidget();
  public: virtual ~ContainerWidget();

  private: bool contains(const Widget& widget) const;
  public: Widget* focus_widget() const { return focus_widget_; }

  // [D]
  public: virtual void DidRealizeWidget(const Widget& widget);

  // [G]
  public: static const char16* GetClass_() { return L"ContainerWidget"; }

  // [O]
  protected: virtual LRESULT OnMessage(UINT uMsg, WPARAM  wParam,
                                       LPARAM lParam) override;

  // [S]
  public: void SetFocusTo(const Widget&);

  // [W]
  public: void WillDestroyWidget(const Widget&);

  DISALLOW_COPY_AND_ASSIGN(ContainerWidget);
};

} // namespace widgets

#endif //!defined(INCLUDE_widgets_container_widget_h)
