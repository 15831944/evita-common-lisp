#if !defined(INCLUDE_base_com_ptr_h)
#define INCLUDE_base_com_ptr_h

namespace base {

#if defined(_DEBUG)
#define COM_VERIFY(expr) { \
  auto const macro_hr = (expr); \
  if (FAILED(macro_hr)) \
    Debugger::Fail("hr=%08X\r\n%s\r\n", macro_hr, #expr); \
}
#else
#define COM_VERIFY(expr) expr;
#endif

template<class T> class ComPtr {
  private: T* ptr_;
  public: explicit ComPtr(T* ptr = nullptr) : ptr_(ptr) {}
  public: explicit ComPtr(T& ptr) : ptr_(&ptr) {}
  public: ComPtr(ComPtr& other) : ptr_(other.ptr_) {
    if (ptr_)
      ptr_->AddRef();
  }
  public: ComPtr(ComPtr&& other) : ptr_(other.ptr_) {
    other.ptr_ = nullptr;
  }
  public: ~ComPtr() {
    if (ptr_) {
      if (!ptr_->Release())
        DEBUG_PRINTF("%p destructed\r\n", ptr_)
    }
  }
  public: operator T*() const { return ptr_; }
  public: operator bool() const { return ptr_; }
  public: T* operator->() const { return ptr_; }
  public: T** operator&() { return &ptr_; }
  public: bool operator!() const { return !ptr_; }

  public: bool operator==(const ComPtr& other) const {
    return ptr_ == other.ptr_;
  }

  public: bool operator==(T* other) const {
    return ptr_ == other;
  }

  public: bool operator!=(const ComPtr& other) const {
    return ptr_ != other.ptr_;
  }

  public: bool operator!=(T* other) const {
    return ptr_ != other;
  }

  public: ComPtr& operator=(const ComPtr& other) {
    ptr_ = other.ptr_;
    if (ptr_)
      ptr_->AddRef();
  }

  public: ComPtr& operator=(ComPtr&& other) {
    ptr_ = other.ptr_;
    other.ptr_ = nullptr;
    if (ptr_)
      ptr_->AddRef();
  }

  public: IUnknown** locationUnknown() {
    return reinterpret_cast<IUnknown**>(&ptr_);
  }
};

class ComInit {
  public: ComInit() { COM_VERIFY(::CoInitialize(nullptr)); }
  public: ~ComInit() { ::CoUninitialize(); }
};

} // namespace base

#endif //!defined(INCLUDE_base_com_ptr_h)
