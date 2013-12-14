// Copyright (C) 1996-2013 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
#if !defined(INCLUDE_widgets_traverse_h)
#define INCLUDE_widgets_traverse_h

namespace widgets {
namespace adoptors {

namespace impl {

template<typename Type>
class AbstractWidgetIterator_ {
  protected: typedef Type* PtrType;
  protected: typedef Type& RefType;
  protected: PtrType widget_;
  public: AbstractWidgetIterator_(PtrType widget) : widget_(widget) {
    ASSERT(widget_);
  }

  public: operator RefType() const {
    ASSERT(widget_);
    ASSERT(!widget_->is_top_level());
    return *widget_;
  }

  public: RefType operator*() const {
    ASSERT(widget_);
    ASSERT(!widget_->is_top_level());
    return *widget_;
  }

  public: PtrType operator->() const {
    ASSERT(widget_);
    ASSERT(!widget_->is_top_level());
    return widget_;
  }

  public: bool operator==(const AbstractWidgetIterator_& other) const {
    return widget_ == other.widget_;
  }

  public: bool operator!=(const AbstractWidgetIterator_& other) const {
    return widget_ != other.widget_;
  }
};

template<typename Type>
class TraverseAncestory_ {
  private: typedef Type* PtrType;
  private: typedef Type& RefType;
  public: class Iterator
      : public AbstractWidgetIterator_<Type> {
    public: Iterator(PtrType widget)
        : AbstractWidgetIterator_(widget) {
    }
    public: Iterator& operator++() {
      ASSERT(widget_);
      ASSERT(!widget_->is_top_level());
      widget_ = &widget_->container_widget();
      return *this;
    }
  };
  private: RefType widget_;

  public: TraverseAncestory_(RefType widget) : widget_(widget) {
  }

  public: Iterator begin() const { return Iterator(&widget_); }
  public: Iterator end() const {
    return Iterator(&Widget::top_level_widget());
  }

  DISALLOW_COPY_AND_ASSIGN(TraverseAncestory_);
};
} // namespace impl

impl::TraverseAncestory_<const ContainerWidget>
ancestory(const ContainerWidget& container) {
  return impl::TraverseAncestory_<const ContainerWidget>(container);
}

impl::TraverseAncestory_<ContainerWidget>
ancestory(ContainerWidget& container) {
  return impl::TraverseAncestory_<ContainerWidget>(container);
}

} // namespace adoptors
} // namespace widgets

#endif //!defined(INCLUDE_widgets_traverse_h)
