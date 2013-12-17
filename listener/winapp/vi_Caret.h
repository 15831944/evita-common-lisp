//////////////////////////////////////////////////////////////////////////////
//
// evcl - listener - visual - text pane
// listener/winapp/vi_text_pane.h
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/listener/winapp/vi_TextEditWindow.h#2 $
//
#if !defined(INCLUDE_listener_winapp_visual_caret_h)
#define INCLUDE_listener_winapp_visual_caret_h

#include "base/ref_counted.h"
#include "./gfx_base.h"

namespace widgets {
class Widget;
}

// Represents caret in per-thread queue. To blink caret, we must track
// caret size. If we call CreateCaret, caret doesn't blink.
// Note: Caret should be lived until timer is fired.
class Caret : public base::RefCounted<Caret> {
  friend class base::RefCounted<Caret>;
  private: class BackingStore;

  private: const base::OwnPtr<BackingStore> backing_store_;
  private: const gfx::Graphics* gfx_;
  private: const widgets::Widget& owner_;
  private: gfx::RectF rect_;
  private: bool shown_;
  private: bool should_blink_;
  private: bool taken_;

  private: Caret(const widgets::Widget& widget);
  private: ~Caret();
  public: static base::scoped_refptr<Caret>
      Create(const widgets::Widget& widget);
  public: void Hide();
  public: void Give();
  private: void OnTimer();
  private: void Show();
  // TODO: We should pass Widget to Caret::Take() instead of gfx::Graphics.
  public: void Take(const gfx::Graphics& gfx);
  private: static void CALLBACK TimerProc(HWND, UINT, UINT_PTR, DWORD);
  public: void Update(const gfx::RectF& rect);

  DISALLOW_COPY_AND_ASSIGN(Caret);
};

#endif //!defined(INCLUDE_listener_winapp_visual_caret_h)
