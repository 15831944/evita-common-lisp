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

//////////////////////////////////////////////////////////////////////
//
// Geometry objects
//

template<typename BaseType, typename UnitType>
class Size_ : public BaseType {
  public: typedef UnitType UnitType;

  public: Size_() {
    width = height = 0;
  }
  public: Size_(const BaseType& other) {
    width = other.width;
    height = other.height;
  }
  public: Size_(UnitType width, UnitType height) {
    this->width = width;
    this->height = height;
  }
  public: Size_(int width, int height) {
    this->width = static_cast<UnitType>(width);
    this->height = static_cast<UnitType>(height);
  }
  public: bool operator==(const Size_& other) const {
    return width == other.width && height == other.height;
  }
  public: bool operator!=(const Size_& other) const {
    return width != other.width && height != other.height;
  }

  public: Size_ operator+(const Size_& other) const {
    return Size_(width + other.width, height + other.height);
  }

  public: Size_ operator+(UnitType scalar) const {
    return Size_(width + scalar, height + scalar);
  }

  public: Size_ operator-(const Size_& other) const {
    return Size_(width - other.width, height - other.height);
  }

  public: Size_ operator-(UnitType scalar) const {
    return Size_(width - scalar, height - scalar);
  }

  public: Size_ operator*(const Size_& other) const {
    return Size_(width * other.width, height * other.height);
  }

  public: Size_ operator*(UnitType scalar) const {
    return Size_(width * scalar, height * scalar);
  }

  public: Size_ operator/(const Size_& other) const {
    return Size_(width / other.width, height / other.height);
  }

  public: Size_ operator/(UnitType scalar) const {
    return Size_(width / scalar, height / scalar);
  }

  public: bool is_empty() const { return width <= 0 || height <= 0; }
  public: bool is_zero() const { return !width && !height; }
};

template<typename BaseType, typename SizeType>
class Point_ : public BaseType {
  public: typedef typename SizeType::UnitType UnitType;
  public: Point_(const Point_& other) {
    x = other.x;
    y = other.y;
  }
  public: Point_(UnitType x, UnitType y) {
    this->x = x;
    this->y = y;
  }
  public: Point_(int x, int y) {
    this->x = static_cast<UnitType>(x);
    this->y = static_cast<UnitType>(y);
  }
  public: Point_(const POINT& point) {
    x = static_cast<UnitType>(point.x);
    y = static_cast<UnitType>(point.y);
  }
  public: Point_() {
    this->x = static_cast<UnitType>(0);
    this->y = static_cast<UnitType>(0);
  }

  public: Point_& operator=(const Point_& other) {
    x = other.x;
    y = other.y;
    return *this;
  }

  public: Point_ operator+(const Point_& other) const {
    return Point_(x + other.x, y + other.y);
  }

  public: Point_ operator+(const SizeType& size) const {
    return Point_(x + size.width, y + size.height);
  }

  public: Point_ operator-(const Point_& other) const {
    return Point_(x - other.x, y - other.y);
  }

  public: Point_ operator-(const SizeType& size) const {
    return Point_(x - size.width, y - size.height);
  }

  public: Point_ operator*(const Point_& other) const {
    return Point_(x * other.x, y * other.y);
  }

  public: Point_ operator*(const SizeType& size) const {
    return Point_(x * size.width, y * size.height);
  }

  public: Point_ operator/(UnitType divisor) const {
    return Point_(x / divisor, y / divisor);
  }
};

template<typename BaseType, typename PointType, typename SizeType>
class Rect_ : public BaseType {
  public: typedef typename SizeType::UnitType UnitType;
  public: Rect_() {
    left = top = right = bottom = 0;
  }
  public: Rect_(UnitType left, UnitType top, UnitType right, UnitType bottom) {
    this->left = left;
    this->top = top;
    this->right = right;
    this->bottom = bottom;
  }
  public: Rect_(int left, int top, int right, int bottom) {
    this->left = static_cast<UnitType>(left);
    this->top = static_cast<UnitType>(top);
    this->right = static_cast<UnitType>(right);
    this->bottom = static_cast<UnitType>(bottom);
  }
  public: explicit Rect_(const RECT& rc) {
    this->left = static_cast<UnitType>(rc.left);
    this->top = static_cast<UnitType>(rc.top);
    this->right = static_cast<UnitType>(rc.right);
    this->bottom = static_cast<UnitType>(rc.bottom);
  }
  public: Rect_(const PointType& left_top, const PointType& right_bottom) {
    left = left_top.x;
    top = left_top.y;
    right = right_bottom.x;
    bottom = right_bottom.y;
  }
  public: Rect_(const PointType& left_top, const SizeType& size) {
    left = left_top.x;
    top = left_top.y;
    right = left + size.width;
    bottom = top + size.height;
  }
  public: explicit Rect_(const SizeType& size) {
    left = 0;
    top = 0;
    right = size.width;
    bottom = size.height;
  }

  public: operator RECT() const {
    RECT rc;
    rc.left = static_cast<long>(left);
    rc.top = static_cast<long>(top);
    rc.right = static_cast<long>(right);
    rc.bottom = static_cast<long>(bottom);
    return rc;
  }

  public: Rect_ operator*(const SizeType& size) const {
    return Rect_(left * size.width, top * size.height,
                 right * size.width, bottom * size.height);
  }

  public: Rect_& operator*=(const SizeType& size) {
    left *= size.width;
    top *= size.height;
    right *= size.width;
    bottom *= size.height;
    return *this;
  }

  public: operator bool() const { return !is_empty(); }
  public: bool operator!() const { return is_empty(); }

  public: UnitType height() const { return bottom - top; }

  public: bool is_empty() const {
    return width() <= 0 || height() <= 0;
  }

  public: bool is_zero() const { return !width() && !height(); }

  public: PointType left_top() const {
    return PointType(left, top);
  }

  public: PointType right_bottom() const {
    return PointType(right, bottom);
  }

  public: SizeType size() const { return SizeType(width(), height()); }
  public: UnitType width() const { return right - left; }
};

typedef Size_<D2D1_SIZE_F, float> SizeF;
typedef Size_<D2D1_SIZE_U, uint> SizeU;
typedef Point_<D2D1_POINT_2F, SizeF> PointF;
typedef Point_<D2D1_POINT_2U, SizeU> PointU;
typedef Rect_<D2D1_RECT_F, PointF, SizeF> RectF;
typedef Rect_<D2D1_RECT_U, PointU, SizeU> RectU;

//////////////////////////////////////////////////////////////////////
//
// Graphics objects
//
typedef D2D1::ColorF ColorF;
class Bitmap;
class Brush;
class FactorySet;
class FontFace;
class Graphics;
class TextFormat;
class TextLayout;

class Bitmap : public SimpleObject_<ID2D1Bitmap> {
  public: Bitmap(const Graphics& gfx, HICON hIcon);
  public: Bitmap(const Graphics& gfx, SizeU size);
  public: explicit Bitmap(const Graphics& gfx);
};

class Brush : public SimpleObject_<ID2D1SolidColorBrush> {
  public: Brush(const Graphics& gfx, ColorF color);
  #if _DEBUG
    public: ~Brush();
  #endif
};

class DpiHandler {
  private: SizeF dpi_;
  private: SizeF pixels_per_dip_;
  public: const SizeF& pixels_per_dip() const { return pixels_per_dip_; }
  public: SizeF AlignToPixel(const SizeF& size) const;
  public: SizeF CeilToPixel(const SizeF& size) const;
  public: SizeF FloorToPixel(const SizeF& size) const;
  protected: void UpdateDpi(const SizeF&);
};

class FactorySet : public RefCounted_<FactorySet>,
                   public base::ComInit,
                   public DpiHandler,
                   public Object {
  private: base::ComPtr<ID2D1Factory> d2d1_factory_;
  private: base::ComPtr<IDWriteFactory> dwrite_factory_;
  private: base::ComPtr<IWICImagingFactory> image_factory_;

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

  public: static SizeF AlignToPixel(const SizeF& size) {
    return instance().DpiHandler::AlignToPixel(size);
  }
  public: static SizeF CeilToPixel(const SizeF& size) {
    return instance().DpiHandler::CeilToPixel(size);
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

class Graphics : public Object, public DpiHandler {
  private: mutable bool drawing_;
  private: ScopedRefCount_<FactorySet> factory_set_;
  private: HWND hwnd_;
  private: ID2D1HwndRenderTarget* render_target_;
  private: mutable void* work_;

  public: class DrawingScope {
    private: const Graphics& gfx_;
    public: DrawingScope(const Graphics& gfx) : gfx_(gfx) {
      gfx_.BeginDraw();
    }
    public: ~DrawingScope() {
      // TODO: DrawingScope should take mutable Graphics.
      const_cast<Graphics&>(gfx_).EndDraw();
    }
    DISALLOW_COPY_AND_ASSIGN(DrawingScope);
  };
  friend class DrawingScope;

  public: Graphics();
  public: ~Graphics();

  public: operator ID2D1HwndRenderTarget*() const {
    return &render_target();
  }
  public: ID2D1HwndRenderTarget* operator->() const {
    return &render_target();
  }

  // |drawing()| is for debugging.
  public: bool drawing() const { return drawing_; }
  public: const FactorySet& factory_set() const { return *factory_set_; }

  public: ID2D1HwndRenderTarget& render_target() const {
    ASSERT(!!render_target_);
    return *render_target_;
  }

  public: template<typename T> T* work() const { 
    return reinterpret_cast<T*>(work_); 
  }
  public: void set_work(void* ptr) const { work_ = ptr; }

  // [B]
  public: void BeginDraw() const;

  // [D]
  public: void DrawLine(const Brush& brush, int sx, int sy, int ex, int ey,
                        float strokeWidth = 1) const {
    ASSERT(drawing_);
    render_target().DrawLine(PointF(sx, sy), PointF(ex, ey), brush,
                             strokeWidth);
  }

  public: void DrawLine(const Brush& brush,
                        float sx, float sy,
                        float ex, float ey,
                        float strokeWidth = 1) const {
    ASSERT(drawing_);
    render_target().DrawLine(PointF(sx, sy), PointF(ex, ey), brush,
                             strokeWidth);
  }

  public: void DrawRectangle(const Brush& brush, const RECT& rc,
                             float strokeWidth = 1) const {
    DrawRectangle(brush, RectF(rc), strokeWidth);
  }

  public: void DrawRectangle(const Brush& brush, const RectF& rect,
                             float strokeWidth = 1) const {
    ASSERT(drawing_);
    ASSERT(!!rect);
    render_target().DrawRectangle(rect, brush, strokeWidth);
  }

  public: void DrawText(const TextFormat& text_format,
                        const Brush& brush,
                        const RECT& rc,
                        const char16* pwch, size_t cwch) const {
    ASSERT(drawing_);
    auto rect = RectF(rc);
    ASSERT(!!rect);
    render_target().DrawText(pwch, cwch, text_format, rect, brush);
  }

  // [E]
  // Returns true if succeeded.
  public: bool EndDraw();

  // [F]
  public: void FillRectangle(const Brush& brush, int left, int top,
                             int right, int bottom) const {
    render_target().FillRectangle(RectF(left, top, right, bottom), brush);
  }

  public: void FillRectangle(const Brush& brush, float left, float top,
                             float right, float bottom) const {
    FillRectangle(brush, RectF(left, top, right, bottom));
  }

  public: void FillRectangle(const Brush& brush, const RECT& rc) const {
    FillRectangle(brush, RectF(rc));
  }

  public: void FillRectangle(const Brush& brush, const RectF& rect) const {
    ASSERT(drawing_);
    ASSERT(!!rect);
    render_target().FillRectangle(rect, brush);
  }

  public: void Flush() const;

  // [I]
  public: void Init(HWND hwnd);

  // [R]
  private: void Reinitialize();
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
