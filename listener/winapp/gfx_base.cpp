#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// Graphics
//
// Copyright (C) 2013 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
#include "gfx_base.h"

#include <utility>

#pragma comment(lib, "d2d1.lib")
#pragma comment(lib, "dwrite.lib")
#pragma comment(lib, "windowscodecs.lib")

namespace gfx {

namespace {

#if 0
class NullFont : IDWriteFont {
  public: STDMETHOD_(ULONG, AddRef)() override { return 1; }
  public: STDMETHOD_(ULONG, Release)() override { return 1; }
  public: STDMETHOD(QueryInterface)(REFIID, void**) override { 
    return E_NOTIMPL;
  }

  //IDWriteFont
  public: STDMETHOD(CreateFontFace)(IDWriteFontFace**) override {
    return E_NOTIMPL;
  }
  public: STDMETHOD(GetFaceNames)(IDWriteLocalizedStrings**) override {
    return E_NOTIMPL;
  }
  public: STDMETHOD(GetFontFamily)(IDWriteFontFamily**) override {
    return E_NOTIMPL;
  }
  public: STDMETHOD(GetInformationalStrings)(DWRITE_INFORMATIONAL_STRING_ID,
                                             IDWriteLocalizedStrings**,
                                             BOOL*) override {
    return E_NOTIMPL;
  }
  public: STDMETHOD_(void, GetMetrics)(DWRITE_FONT_METRICS* metrics) override {
    ::ZeroMemory(&metrics, sizeof(metrics));
  }
  public: STDMETHOD_(DWRITE_FONT_SIMULATIONS, GetSimulations)() override {
    return DWRITE_FONT_SIMULATIONS_NONE;
  }
  public: STDMETHOD_(DWRITE_FONT_STRETCH, GetStretch)() override {
    return DWRITE_FONT_STRETCH_NORMAL;
  }
  public: STDMETHOD_(DWRITE_FONT_STYLE, GetStyle)() override {
    return DWRITE_FONT_STYLE_NORMAL;
  }
  public: STDMETHOD_(DWRITE_FONT_WEIGHT, GetWeight)() override {
    return DWRITE_FONT_WEIGHT_NORMAL;
  }
  public: STDMETHOD(HasCharacter)(UINT32, BOOL*) override {
    return E_NOTIMPL;
  }
  public: STDMETHOD_(BOOL, IsSymbolFont)() override {
    return false;
  }
};
#endif

base::ComPtr<ID2D1Bitmap> CreateBitmap(const Graphics& gfx, HICON hIcon) {
  base::ComPtr<IWICBitmap> icon;
  COM_VERIFY(gfx::FactorySet::image().CreateBitmapFromHICON(
      hIcon, &icon));
 base::ComPtr<IWICFormatConverter> converter;
 COM_VERIFY(gfx::FactorySet::image().
    CreateFormatConverter(&converter));
 COM_VERIFY(converter->Initialize(
      icon,
      GUID_WICPixelFormat32bppPBGRA,
      WICBitmapDitherTypeNone,
      nullptr,
      0,
      WICBitmapPaletteTypeMedianCut));
  base::ComPtr<ID2D1Bitmap> bitmap;
  COM_VERIFY(gfx->CreateBitmapFromWicBitmap(converter, nullptr, &bitmap));
  return std::move(bitmap);
}

ID2D1Factory& CreateD2D1Factory() {
  ID2D1Factory* factory;
  COM_VERIFY(::D2D1CreateFactory(
      D2D1_FACTORY_TYPE_SINGLE_THREADED,
      &factory));
  return *factory;
}

IDWriteFactory& CreateDWriteFactory() {
  IDWriteFactory* factory;
  COM_VERIFY(::DWriteCreateFactory(
      DWRITE_FACTORY_TYPE_SHARED,
      __uuidof(IDWriteFactory),
      reinterpret_cast<IUnknown**>(&factory)));
  return *factory;
}

base::ComPtr<IDWriteFontFace> CreateFontFace(const char16* family_name) {
  base::ComPtr<IDWriteFontCollection> font_collection;
  COM_VERIFY(gfx::FactorySet::dwrite().
      GetSystemFontCollection(&font_collection, false));

  uint32 index;
  BOOL exists;
  COM_VERIFY(font_collection->FindFamilyName(family_name, &index, &exists));
  if (!exists)
   return CreateFontFace(L"Courier New");

  base::ComPtr<IDWriteFontFamily> font_family;
  COM_VERIFY(font_collection->GetFontFamily(index, &font_family));

  base::ComPtr<IDWriteFont> font;
  COM_VERIFY(font_family->GetFirstMatchingFont(
    DWRITE_FONT_WEIGHT_NORMAL,
    DWRITE_FONT_STRETCH_NORMAL,
    DWRITE_FONT_STYLE_NORMAL, // normal, italic or oblique
    &font));

  base::ComPtr<IDWriteFontFace> font_face;
  COM_VERIFY(font->CreateFontFace(&font_face));
  return std::move(font_face);
}

IWICImagingFactory& CreateImageFactory() {
  IWICImagingFactory* factory;
  COM_VERIFY(::CoCreateInstance(
      CLSID_WICImagingFactory,
      nullptr,
      CLSCTX_INPROC_SERVER,
      IID_PPV_ARGS(&factory)));
  return *factory;
}

base::ComPtr<ID2D1SolidColorBrush> CreateSolidColorBrush(const Graphics& gfx,
                                                         ColorF color) {
  base::ComPtr<ID2D1SolidColorBrush> brush;
  COM_VERIFY(gfx->CreateSolidColorBrush(color, &brush));
  return brush;
}

base::ComPtr<IDWriteTextFormat> CreateTextFormat(
    const FactorySet&,
    const LOGFONT& log_font) {
  ASSERT(log_font.lfHeight < 0);
  auto const height = FactorySet::ScaleY(
    static_cast<float>(-log_font.lfHeight) * 96.0f / 72.0f);
  base::ComPtr<IDWriteTextFormat> text_format;
  COM_VERIFY(FactorySet::dwrite().CreateTextFormat(
    log_font.lfFaceName,
    nullptr,
    DWRITE_FONT_WEIGHT_REGULAR,
    DWRITE_FONT_STYLE_NORMAL,
    DWRITE_FONT_STRETCH_NORMAL,
    height,
    L"en-us",
    &text_format));
  return std::move(text_format);
}

DWRITE_FONT_METRICS GetFontMetrics(IDWriteFontFace* font) {
  DWRITE_FONT_METRICS metrics;
  font->GetMetrics(&metrics);
  return metrics;
}

} // namespace

Bitmap::Bitmap(const Graphics& gfx, HICON hIcon)
    : SimpleObject_(CreateBitmap(gfx, hIcon)) {
}

Brush::Brush(const Graphics& gfx, ColorF color)
    : SimpleObject_(CreateSolidColorBrush(gfx, color)) {
}

FactorySet::FactorySet()
      : d2d1_factory_(CreateD2D1Factory()),
        dwrite_factory_(CreateDWriteFactory()),
        image_factory_(CreateImageFactory()) {
  float dpi_x, dpi_y;
  d2d1_factory_->GetDesktopDpi(&dpi_x, &dpi_y);
  // All render targets except for ID2D1HwndRenderTarget, DPI values are
  // 96 DPI.
  float const default_dpi = 96.0f;
  dpi_scale_ = SizeF(dpi_x, dpi_y) / default_dpi;
}

FontFace::FontFace(const char16* family_name)
    : SimpleObject_(CreateFontFace(family_name)),
      ALLOW_THIS_IN_INITIALIZER_LIST(metrics_(GetFontMetrics(*this))) {
}

FactorySet& FactorySet::instance() {
  static FactorySet* instance;
  if (!instance)
    instance = new FactorySet();
  return *instance;
}

// Graphics

Graphics::Graphics()
    : factory_set_(FactorySet::instance()),
      work_(nullptr),
      drawing_(false) {
}

Graphics::~Graphics() {
}

void Graphics::Flush() const {
  ASSERT(drawing());
  D2D1_TAG tag1, tag2;
  COM_VERIFY(render_target_->Flush(&tag1, &tag2));
  ASSERT(!tag1 && !tag2);
}

void Graphics::Init(HWND hwnd) {
  RECT rc;
  ::GetClientRect(hwnd, &rc);
  auto const pixel_format = D2D1::PixelFormat(
      DXGI_FORMAT_B8G8R8A8_UNORM,
      D2D1_ALPHA_MODE_PREMULTIPLIED);
  auto const size = D2D1::SizeU(rc.right - rc.left, rc.bottom - rc.top);
  COM_VERIFY(FactorySet::d2d1().CreateHwndRenderTarget(
      D2D1::RenderTargetProperties(D2D1_RENDER_TARGET_TYPE_DEFAULT,
                                   pixel_format),
      D2D1::HwndRenderTargetProperties(hwnd, size),
      &render_target_));
}

void Graphics::Resize(const RECT& rc) const {
  auto size = D2D1::SizeU(rc.right - rc.left, rc.bottom - rc.top);
  render_target().Resize(size);
}

// TextFormat

TextFormat::TextFormat(const LOGFONT& log_font)
    : SimpleObject_(CreateTextFormat(FactorySet::instance(), log_font)),
    factory_set_(FactorySet::instance()) {
}

} // namespace gfx
