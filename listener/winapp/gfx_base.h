// Graphics
//
// Copyright (C) 2013 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
#if !defined(INCLUDE_gfx_base_h)
#define INCLUDE_gfx_base_h

#include "base_com_ptr.h"
#include "./li_util.h"

#include <d2d1.h>
#include <dwrite.h>
#include <wincodec.h>

namespace gfx {

typedef D2D1::ColorF ColorF;

class Bitmap;
class Brush;
class FactorySet;
class FontFace;
class Graphics;
class PointF;
class RectF;
class SizeF;
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
  protected: SimpleObject_(base::ComPtr<T>&& ptr) : ptr_(std::move(ptr)) {}
  public: operator T*() const { return ptr_; }
  public: T* operator->() const { return ptr_; }
};

class SizeF : public D2D1_SIZE_F {
  public: SizeF() {
    width = height = 0;
  }
  public: SizeF(float width, float height) {
    this->width = width;
    this->height = height;
  }
  public: SizeF(int width, int height) {
    this->width = static_cast<float>(width);
    this->height = static_cast<float>(height);
  }
  public: bool operator==(const SizeF& other) const {
    return width == other.width && height == other.height;
  }
  public: bool operator!=(const SizeF& other) const {
    return width != other.width && height != other.height;
  }

  public: SizeF operator+(const SizeF& other) const {
    return SizeF(width + other.width, height + other.height);
  }

  public: SizeF operator+(float scalar) const {
    return SizeF(width + scalar, height + scalar);
  }

  public: SizeF operator-(const SizeF& other) const {
    return SizeF(width - other.width, height - other.height);
  }

  public: SizeF operator-(float scalar) const {
    return SizeF(width - scalar, height - scalar);
  }

  public: SizeF operator*(const SizeF& other) const {
    return SizeF(width * other.width, height * other.height);
  }

  public: SizeF operator*(float scalar) const {
    return SizeF(width * scalar, height * scalar);
  }

  public: SizeF operator/(const SizeF& other) const {
    return SizeF(width / other.width, height / other.height);
  }

  public: SizeF operator/(float scalar) const {
    return SizeF(width / scalar, height / scalar);
  }
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
  public: PointF(const POINT& point) {
    x = static_cast<float>(point.x);
    y = static_cast<float>(point.y);
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

  public: PointF operator+(const PointF& other) const {
    return PointF(x + other.x, y + other.y);
  }

  public: PointF operator+(const SizeF& size) const {
    return PointF(x + size.width, y + size.height);
  }

  public: PointF operator-(const PointF& other) const {
    return PointF(x - other.x, y - other.y);
  }

  public: PointF operator-(const SizeF& size) const {
    return PointF(x - size.width, y - size.height);
  }

  public: PointF operator*(const PointF& other) const {
    return PointF(x * other.x, y * other.y);
  }

  public: PointF operator*(const SizeF& size) const {
    return PointF(x * size.width, y * size.height);
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
  public: RectF(const PointF& left_top, const SizeF& size) {
    left = left_top.x;
    top = left_top.y;
    right = left + size.width;
    bottom = top + size.height;
  }

  public: operator RECT() const {
    RECT rc;
    rc.left = static_cast<long>(left);
    rc.top = static_cast<long>(top);
    rc.right = static_cast<long>(right);
    rc.bottom = static_cast<long>(bottom);
    return rc;
  }

  public: RectF operator*(const SizeF& size) const {
    return RectF(left * size.width, top * size.height,
                 right * size.width, bottom * size.height);
  }

  public: RectF& operator*=(const SizeF& size) {
    left *= size.width;
    top *= size.height;
    right *= size.width;
    bottom *= size.height;
    return *this;
  }

  public: bool operator!() const { return is_empty(); }

  public: float height() const { return bottom - top; }

  public: bool is_empty() const {
    return !width() && !height();
  }

  public: PointF left_top() const {
    return PointF(left, top);
  }

  public: PointF right_bottom() const {
    return PointF(right, bottom);
  }

  public: float width() const { return right - left; }

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
  private: SizeF dpi_scale_;

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
  public: static PointF Scale(const PointF& point) {
    return point * instance().dpi_scale_;
  }
  public: static SizeF Scale(const SizeF& size) {
    return size * instance().dpi_scale_;
  }
  public: static float ScaleX(float x) {
    return x * instance().dpi_scale_.width;
  }
  public: static float ScaleY(float y) {
    return y * instance().dpi_scale_.height;
  }
};

class FontFace : public SimpleObject_<IDWriteFontFace> {
  private: const DWRITE_FONT_METRICS metrics_;
  public: FontFace(const char16* family_name);
  public: const DWRITE_FONT_METRICS& metrics() const { return metrics_; }
};

class TextFormat : public SimpleObject_<IDWriteTextFormat> {
  private: const ScopedRefCount_<FactorySet> factory_set_;
  public: TextFormat(const LOGFONT& log_font);
  public: base::OwnPtr<TextLayout> CreateLayout(const char16*, int) const;
};

class TextLayout : public SimpleObject_<IDWriteTextLayout> {
  public: TextLayout(const LOGFONT& log_font);
  public: SIZE GetMetrics() const;
};

class Graphics : public Object {
  private: ScopedRefCount_<FactorySet> factory_set_;
  private: base::ComPtr<ID2D1HwndRenderTarget> render_target_;
  private: mutable void* work_;

  public: Graphics();
  public: ~Graphics();

  public: ID2D1HwndRenderTarget* operator->() const {
    return &render_target();
  }

  public: const FactorySet& factory_set() const { return *factory_set_; }

  public: ID2D1HwndRenderTarget& render_target() const {
    ASSERT(!!render_target_);
    return *render_target_;
  }

  public: template<typename T> T* work() const { 
    return reinterpret_cast<T*>(work_); 
  }
  public: void set_work(void* ptr) const { work_ = ptr; }

  public: SizeF Scale(const SizeF& size) const {
    return FactorySet::Scale(size);
  }

  // [D]
  public: void DrawLine(const Brush& brush, int sx, int sy, int ex, int ey,
                        float strokeWidth = 1) const {
    render_target().DrawLine(PointF(sx, sy), PointF(ex, ey), brush,
                             strokeWidth);
  }

  public: void DrawLine(const Brush& brush,
                        float sx, float sy,
                        float ex, float ey,
                        float strokeWidth = 1) const {
    render_target().DrawLine(PointF(sx, sy), PointF(ex, ey), brush,
                             strokeWidth);
  }

  public: void DrawRectangle(const Brush& brush, const RECT& rc,
                             float strokeWidth = 1) const {
    render_target().DrawRectangle(RectF(rc), brush, strokeWidth);
  }

  public: void DrawRectangle(const Brush& brush, const RectF& rect,
                             float strokeWidth = 1) const {
    render_target().DrawRectangle(rect, brush, strokeWidth);
  }

  public: void DrawText(const TextFormat& text_format,
                        const Brush& brush,
                        const RECT& rc,
                        const char16* pwch, size_t cwch) const {
    render_target().DrawText(pwch, cwch, text_format, RectF(rc), brush);
  }

  // [F]
  public: void FillRectangle(const Brush& brush, int left, int top,
                             int right, int bottom) const {
    render_target().FillRectangle(RectF(left, top, right, bottom), brush);
  }

  public: void FillRectangle(const Brush& brush, float left, float top,
                             float right, float bottom) const {
    render_target().FillRectangle(RectF(left, top, right, bottom), brush);
  }

  public: void FillRectangle(const Brush& brush, const RECT& rc) const {
    render_target().FillRectangle(RectF(rc), brush);
  }

  public: void FillRectangle(const Brush& brush, const RectF& rect) const {
    render_target().FillRectangle(rect, brush);
  }

  // [I]
  public: void Init(HWND hwn);

  // [R]
  public: void Resize(const RECT& rc) const;
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
