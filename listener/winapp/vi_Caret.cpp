#include "precomp.h"
// Copyright (C) 1996-2013 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
#include "./vi_Caret.h"

#include "./gfx_base.h"
#include "widgets/widget.h"
#include <math.h>
#include <utility>

#define DEBUG_BLINK 0
#define DEBUG_DRAW 0
#define DEBUG_SHOW 0

static const auto kBlinkInterval = 250; // milliseconds

//////////////////////////////////////////////////////////////////////
//
// BackingStore
//
class Caret::BackingStore {
  private: base::OwnPtr<gfx::Bitmap> bitmap_;
  private: void* render_target_;
  private: gfx::RectF rect_;

  public: BackingStore();
  public: ~BackingStore();
  public: void Restore(const gfx::Graphics& gfx);
  public: void Save(const gfx::Graphics& gfx, const gfx::RectF& rect);
        
  DISALLOW_COPY_AND_ASSIGN(BackingStore);
};

Caret::BackingStore::BackingStore()
    : render_target_(nullptr) {
}

Caret::BackingStore::~BackingStore() {
}

void Caret::BackingStore::Restore(const gfx::Graphics& gfx) {
  if (!rect_ || render_target_ != &*gfx)
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
Caret::Caret(const widgets::Widget& owner)
  : backing_store_(new BackingStore()),
    blink_count_(0),
    gfx_(nullptr),
    owner_(owner),
    shown_(false),
    should_blink_(false),
    taken_(false),
    updated_at_(0) {
}

Caret::~Caret() {
  ASSERT(!taken_);
}

base::scoped_refptr<Caret> Caret::Create(const widgets::Widget& owner) {
  return std::move(base::scoped_refptr<Caret>(new Caret(owner)));
}

void Caret::Give() {
  #if DEBUG_SHOW
    DEBUG_PRINTF("gfx=%p\n", gfx_);
  #endif
  ASSERT(taken_);
  ::KillTimer(owner_.AssociatedHwnd(), reinterpret_cast<UINT_PTR>(this));
  Release();
  gfx::Graphics::DrawingScope drawing_scope(*gfx_);
  Hide();
  taken_ = false;
}

void Caret::Hide() {
  #if DEBUG_DRAW || DEBUG_SHOW
    DEBUG_PRINTF("taken=%d shown=%d\n", taken_, shown_);
  #endif
  if (!taken_)
    return;
  if (!shown_)
    return;
  backing_store_->Restore(*gfx_);
  shown_ = false;
}

void Caret::OnTimer() {
  if (!taken_ || !should_blink_ || !rect_)
    return;
  auto const now = ::GetTickCount();
  #if DEBUG_BLINK
    DEBUG_PRINTF("show=%d blink_count=%d %dms\n",
        shown_, blink_count_, now - updated_at_);
  #endif
  updated_at_ = now;
  blink_count_ = (blink_count_ + 1) % 4;
  gfx::Graphics::DrawingScope drawing_scope(*gfx_);
  if (blink_count_)
    Show();
  else
    Hide();
}

void Caret::Show() {
  #if DEBUG_DRAW
    DEBUG_PRINTF("taken=%d shown=%d\n", taken_, shown_);
  #endif
  ASSERT(!!rect_);
  if (shown_)
    return;
  backing_store_->Save(*gfx_, rect_);
  gfx::Brush fill_brush(*gfx_, gfx::ColorF::Black);
  gfx_->FillRectangle(fill_brush, rect_);
  shown_ = true;
}

void Caret::Take(const gfx::Graphics& gfx) {
  #if DEBUG_SHOW
    DEBUG_PRINTF("gfx=%p taken=%d\n", &gfx, taken_);
  #endif
  taken_ = true;
  gfx_ = &gfx;
  AddRef();
  ::SetTimer(owner_.AssociatedHwnd(), reinterpret_cast<UINT_PTR>(this),
             kBlinkInterval, &Caret::TimerProc);
  if (!rect_)
    return;
  gfx::Graphics::DrawingScope drawing_scope(*gfx_);
  Show();
}

void Caret::TimerProc(HWND, UINT, UINT_PTR self, DWORD) {
  reinterpret_cast<Caret*>(self)->OnTimer();
}

void Caret::Update(const gfx::RectF& new_rect) {
  ASSERT(!!new_rect);
  ASSERT(taken_);
  ASSERT(!shown_);
  #if DEBUG_SHOW
    DEBUG_PRINTF("Update caret to (%d,%d) from (%d,%d)\n", 
        static_cast<uint>(new_rect.left), static_cast<uint>(new_rect.top),
        static_cast<uint>(rect_.left), static_cast<uint>(rect_.top));
  #endif
  if (rect_ == new_rect) {
    should_blink_ = true;
  } else {
    rect_ = new_rect;
    should_blink_ = false;
  }
  Show();
}
