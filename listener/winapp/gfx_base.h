// Graphics
//
// Copyright (C) 2013 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
#if !defined(INCLUDE_gfx_base_h)
#define INCLUDE_gfx_base_h

#include "./li_util.h"

#include <d2d1.h>
#include <dwrite.h>
#include <wincodec.h>

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
  public: explicit ComPtr(ComPtr&& other) : ptr_(other.ptr) {
    other.ptr_ = nullptr;
  }
  public: ~ComPtr() {
    if (ptr_)
      ptr_->Release();
  }
  public: operator T*() const { return ptr_; }
  public: T* operator->() const { return ptr_; }
  public: T** operator&() { return &ptr_; }

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

  public: IUnknown** locationUnknown() {
    return reinterpret_cast<IUnknown**>(&ptr_);
  }
};

class ComInit {
  public: ComInit() { COM_VERIFY(::CoInitialize(nullptr)); }
  public: ~ComInit() { ::CoUninitialize(); }
};

} // namespace base

namespace gfx {

typedef D2D1::ColorF ColorF;

class Bitmap;
class Brush;
class FactorySet;
class Font;
class Graphics;
class TextFormat;
class TextLayout;

class Object {
  protected: Object() {}
  protected: ~Object() {}
  DISALLOW_COPY_AND_ASSIGN(Object);
};

template<class T>
class SimpleObject_ : public Object {
  private: const base::ComPtr<T> ptr_;
  protected: SimpleObject_(T* ptr) : ptr_(ptr) {}
  public: operator T*() const { return ptr_; }
  public: T* operator->() const { return ptr_; }
};

class PointF : public D2D1_POINT_2F {
  public: PointF(const PointF& other) {
    x = other.x;
    y = other.y;
  }
  public: PointF(float x, float y) {
    this->x = x;
    this->y = y;
  }
  public: PointF(int x, int y) {
    this->x = static_cast<float>(x);
    this->y = static_cast<float>(y);
  }
  public: PointF() {
    this->x = 0;
    this->y = 0;
  }

  public: PointF& operator=(const PointF& other) {
    x = other.x;
    y = other.y;
    return *this;
  }

  public: PointF operator-(const PointF& other) const {
    return PointF(x - other.x, y - other.y);
  }

  public: PointF operator+(const PointF& other) const {
    return PointF(x + other.x, y + other.y);
  }

  public: PointF operator*(const PointF& other) const {
    return PointF(x * other.x, y * other.y);
  }

  public: PointF operator/(float divisor) const {
    return PointF(x / divisor, y / divisor);
  }
};

class RectF : public D2D1_RECT_F {
  public: RectF() {
    left = top = right = bottom = 0;
  }
  public: RectF(float left, float top, float right, float bottom) {
    this->left = left;
    this->top = top;
    this->right = right;
    this->bottom = bottom;
  }
  public: RectF(int left, int top, int right, int bottom) {
    this->left = static_cast<float>(left);
    this->top = static_cast<float>(top);
    this->right = static_cast<float>(right);
    this->bottom = static_cast<float>(bottom);
  }
  public: explicit RectF(const RECT& rc) {
    this->left = static_cast<float>(rc.left);
    this->top = static_cast<float>(rc.top);
    this->right = static_cast<float>(rc.right);
    this->bottom = static_cast<float>(rc.bottom);
  }
  public: RectF(const PointF& left_top, const PointF& right_bottom) {
    left = left_top.x;
    top = left_top.y;
    right = right_bottom.x;
    bottom = right_bottom.y;
  }

  public: operator RECT() const {
    RECT rc;
    rc.left = static_cast<long>(left);
    rc.top = static_cast<long>(top);
    rc.right = static_cast<long>(right);
    rc.bottom = static_cast<long>(bottom);
    return rc;
  }

  public: RectF operator*(const PointF& point) const {
    return RectF(left * point.x, top * point.y,
                 right * point.x, bottom * point.y);
  }

  public: RectF& operator*=(const PointF& point) {
    left *= point.x;
    top *= point.y;
    right *= point.x;
    bottom *= point.y;
    return *this;
  }
};

class Bitmap : public SimpleObject_<ID2D1Bitmap> {
  public: Bitmap(const Graphics& gfx, HICON hIcon);
};

class Brush : public SimpleObject_<ID2D1SolidColorBrush> {
  public: Brush(const Graphics& gfx, ColorF color);
};

class FactorySet : public RefCounted_<FactorySet>,
                   public base::ComInit,
                   public Object {
  private: base::ComPtr<ID2D1Factory> d2d1_factory_;
  private: base::ComPtr<IDWriteFactory> dwrite_factory_;
  private: base::ComPtr<IWICImagingFactory> image_factory_;
  private: PointF dpi_scale_;

  public: FactorySet();
  public: ~FactorySet() {}
  public: static FactorySet& instance();
  public: static ID2D1Factory& d2d1() {
    return *instance().d2d1_factory_;
  }
  public: static IDWriteFactory& dwrite() {
    return *instance().dwrite_factory_;
  }
  public: static IWICImagingFactory& image() {
    return *instance().image_factory_;
  }
  public: static const PointF dpi_scale() {
    return instance().dpi_scale_;
  }
};

class Font : public SimpleObject_<IDWriteFont> {
  private: const base::ComPtr<IDWriteFont> font_;
  private: const base::ComPtr<IDWriteFontFace> font_face_;
  private: const DWRITE_FONT_METRICS metrics_;
  public: Font(const char16* family_name);
  public: IDWriteFontFace* font_face() const { return font_face_; }
  public: const DWRITE_FONT_METRICS& metrics() const { return metrics_; }
  public: bool HasCharacter(char16) const;
};

class TextFormat : public SimpleObject_<IDWriteTextFormat> {
  private: const ScopedRefCount_<FactorySet> factory_set_;
  public: TextFormat(const LOGFONT& log_font);
  public: OwnPtr<TextLayout> CreateLayout(const char16*, int) const;
};

class TextLayout : public SimpleObject_<IDWriteTextLayout> {
  public: TextLayout(const LOGFONT& log_font);
  public: SIZE GetMetrics() const;
};

class Graphics : public Object{
  private: ScopedRefCount_<FactorySet> factory_set_;
  private: base::ComPtr<ID2D1HwndRenderTarget> render_target_;
  private: mutable void* work_;

  public: Graphics();
  public: ~Graphics();

  public: ID2D1HwndRenderTarget* operator->() const {
    return render_target_;
  }

  public: const FactorySet& factory_set() const { return *factory_set_; }

  public: template<typename T> T* work() const { 
    return reinterpret_cast<T*>(work_); 
  }
  public: void set_work(void* ptr) const { work_ = ptr; }

  public: RectF scaledRect(int left, int top, int right, int bottom) const {
    return RectF(left, top, right, bottom) * factory_set_->dpi_scale();
  }

  public: PointF scaledPoint(int x, int y) const {
    return PointF(x, y) * factory_set_->dpi_scale();
  }

  public: PointF scaledPoint(float x, float y) const {
    return PointF(x, y) * factory_set_->dpi_scale();
  }

  public: RectF scaledRect(const RECT& rc) const {
    return RectF(rc) * factory_set_->dpi_scale();
  }

  // [D]
  public: void DrawLine(const Brush& brush, int sx, int sy, int ex, int ey,
                        float strokeWidth = 1) const {
    render_target_->DrawLine(PointF(sx, sy), PointF(ex, ey), brush,
                             strokeWidth);
  }

  public: void DrawLine(const Brush& brush,
                        float sx, float sy,
                        float ex, float ey,
                        float strokeWidth = 1) const {
    render_target_->DrawLine(PointF(sx, sy), PointF(ex, ey), brush,
                             strokeWidth);
  }

  public: void DrawRectangle(const Brush& brush, const RECT& rc,
                             float strokeWidth = 1) const {
    render_target_->DrawRectangle(RectF(rc), brush, strokeWidth);
  }

  public: void DrawRectangle(const Brush& brush, const RectF& rect,
                             float strokeWidth = 1) const {
    render_target_->DrawRectangle(rect, brush, strokeWidth);
  }

  public: void DrawText(const TextFormat& text_format,
                        const Brush& brush,
                        const RECT& rc,
                        const char16* pwch, size_t cwch) const {
    render_target_->DrawText(pwch, cwch, text_format, RectF(rc), brush);
  }

  // [F]
  public: void FillRectangle(const Brush& brush, int left, int top,
                             int right, int bottom) const {
    render_target_->FillRectangle(RectF(left, top, right, bottom), brush);
  }

  public: void FillRectangle(const Brush& brush, float left, float top,
                             float right, float bottom) const {
    render_target_->FillRectangle(RectF(left, top, right, bottom), brush);
  }

  public: void FillRectangle(const Brush& brush, const RECT& rc) const {
    render_target_->FillRectangle(RectF(rc), brush);
  }

  public: void FillRectangle(const Brush& brush, const RectF& rect) const {
    render_target_->FillRectangle(rect, brush);
  }

  // [I]
  public: void Init(HWND hwn);

  // [R]
  public: void Resize(const RECT rc) const {
    auto size = D2D1::SizeU(rc.right - rc.left, rc.bottom - rc.top);
    render_target_->Resize(size);
  }
};

// Helper functions
inline ColorF blackColor() {
  return ColorF(ColorF::Black);
}

inline ColorF grayColor() {
  return ColorF(ColorF::LightGray);
}

inline ColorF sysColor(int name, float alpha = 1) {
  auto const colorRef = ::GetSysColor(name);
  return ColorF(static_cast<float>(GetRValue(colorRef)) / 255,
                static_cast<float>(GetGValue(colorRef)) / 255,
                static_cast<float>(GetBValue(colorRef)) / 255,
                alpha);
}

inline ColorF whiteColor() {
  return ColorF(ColorF::White);
}

} // namespace gfx

#endif //!defined(INCLUDE_gfx_base_h)
