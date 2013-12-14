// Copyright (C) 1996-2013 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
#if !defined(INCLUDE_widgets_container_widget_h)
#define INCLUDE_widgets_container_widget_h

#include "widgets/widget.h"
#include <vector>

namespace widgets {

class ContainerWidget : public Widget {
  private: Widget* capture_widget_;
  // List of containing widgets in order of bottom to fron.
  private: std::vector<Widget*> child_widgets_;
  private: Widget* focus_widget_;

  public: ContainerWidget();
  public: virtual ~ContainerWidget();

  public: Widget* focus_widget() const { return focus_widget_; }
  public: virtual bool is_container() const override { return true; }

  // [C]
  private: virtual bool Contains(const Widget& widget) const override;

  // [D]
  public: virtual void DidRealizeWidget(const Widget& widget);
  private: void DispatchPaintMessage();

  // [G]
  public: virtual const char* GetClass() const override {
    return "ContainerWidget";
  }
  private: ContainerWidget& GetHostContainer() const;
  private: Widget* GetWidgetAt(const gfx::Point& point) const;

  // [H]
  public: virtual void Hide();

  // [O]
  protected: virtual bool OnIdle(uint idle_count) override;

  // [R]
  public: void ReleaseCaptureFrom(const Widget&);

  // [S]
  public: void SetCaptureTo(const Widget&);
  private: bool SetCursor();
  public: void SetFocusTo(const Widget&);
  public: virtual void Show();

  // [T]
  public: virtual const ContainerWidget* ToContainer() const override {
    return this;
  }
  public: virtual ContainerWidget* ToContainer() override {
    return this;
  }

  // [W]
  public: void WillDestroyWidget(const Widget&);
  protected: virtual LRESULT WindowProc(UINT uMsg, WPARAM  wParam,
                                        LPARAM lParam) override;

  DISALLOW_COPY_AND_ASSIGN(ContainerWidget);
};

} // namespace widgets

#endif //!defined(INCLUDE_widgets_container_widget_h)
