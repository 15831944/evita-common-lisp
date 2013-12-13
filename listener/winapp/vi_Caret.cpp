#include "precomp.h"
// Copyright (C) 1996-2013 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
#include "./vi_Caret.h"

#include "./gfx_base.h"
#include <math.h>
#include <utility>

#define DEBUG_SHOW 0

static const auto kBlinkInterval = 250; // milliseconds

//////////////////////////////////////////////////////////////////////
//
// BackingStore
//
class Caret::BackingStore {
  private: base::OwnPtr<gfx::Bitmap> bitmap_;
  private: ID2D1RenderTarget* render_target_;
  private: gfx::RectF rect_;

  public: BackingStore();
  public: ~BackingStore();
  public: void Restore(const gfx::Graphics& gfx);
  public: void Save(const gfx::Graphics& gfx, const gfx::RectF& rect);
};

Caret::BackingStore::BackingStore()
    : render_target_(nullptr) {
}

Caret::BackingStore::~BackingStore() {
}

void Caret::BackingStore::Restore(const gfx::Graphics& gfx) {
  if (!rect_)
    return;
  ASSERT(!!bitmap_);
  gfx->DrawBitmap(*bitmap_, rect_);
  auto const hr = gfx->Flush();
  if (FAILED(hr)) {
    DEBUG_PRINTF("DrawBitmap failed hr=0x%08X\n", hr);
  }
}

void Caret::BackingStore::Save(const gfx::Graphics& gfx, 
                               const gfx::RectF& rect) {
  ASSERT(!rect.is_empty());
  rect_ = gfx::RectF(::floorf(rect.left), ::floorf(rect.top),
                     ::ceilf(rect.right), ::ceilf(rect.bottom));
  gfx::RectU screen_rect = gfx::RectU(static_cast<uint>(rect_.left),
                                      static_cast<uint>(rect_.top),
                                      static_cast<uint>(rect_.right),
                                      static_cast<uint>(rect_.bottom));
  base::OwnPtr<gfx::Bitmap> bitmap(*new gfx::Bitmap(gfx,
                                                    screen_rect.size()));
  COM_VERIFY((*bitmap)->CopyFromRenderTarget(nullptr, gfx, &screen_rect));
  bitmap_ = std::move(bitmap);
  render_target_ = &*gfx;
}

//////////////////////////////////////////////////////////////////////
//
// Caret
//
Caret::Caret()
  : backing_store_(new BackingStore()),
    showing_(false),
    taken_(false) {
}

Caret::~Caret() {
  ASSERT(!showing_);
  ASSERT(!taken_);
}

void Caret::Draw(const gfx::Graphics& gfx) {
  ASSERT(!!rect_);
  backing_store_->Save(gfx, rect_);
  gfx::Brush fill_brush(gfx, gfx::ColorF::Black);
  gfx.FillRectangle(fill_brush, rect_);
  showing_ = true;
}

void Caret::Give(const gfx::Graphics& gfx) {
  #if DEBUG_SHOW
    DEBUG_PRINTF("gfx=%p\n", *gfx);
  #endif
  ASSERT(taken_);
  gfx::Graphics::DrawingScope drawing_scope(gfx);
  backing_store_->Restore(gfx);
  taken_ = false;
}

void Caret::Hide(const gfx::Graphics& gfx) {
  if (!taken_)
    return;
  if (!showing_)
    return;
  backing_store_->Restore(gfx);
  showing_ = false;
}

void Caret::Show(const gfx::Graphics& gfx, const gfx::RectF& new_rect) {
  ASSERT(!!new_rect);
  ASSERT(taken_);
  ASSERT(!showing_);
  #if DEBUG_SHOW
    DEBUG_PRINTF("Move caret at (%d,%d) from (%d,%d)\n", 
        static_cast<uint>(new_rect.left), static_cast<uint>(new_rect.top),
        static_cast<uint>(rect_.left), static_cast<uint>(rect_.top));
  #endif
  rect_ = new_rect;
  Draw(gfx);
}

void Caret::Take(const gfx::Graphics& gfx) {
  #if DEBUG_SHOW
    DEBUG_PRINTF("gfx=%p taken=%d\n", *gfx, taken_);
  #endif
  taken_ = true;
  if (!rect_)
    return;
  gfx::Graphics::DrawingScope drawing_scope(gfx);
  Draw(gfx);
}
