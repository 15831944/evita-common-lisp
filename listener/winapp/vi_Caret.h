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

#include "gfx_base.h"

// Represents caret in per-thread queue. To blink caret, we must track
// caret size. If we call CreateCaret, caret doesn't blink.
class Caret {
  private: class BackingStore;

  private: const base::OwnPtr<BackingStore> backing_store_;
  private: gfx::RectF rect_;
  private: bool shown_;
  private: bool taken_;

  public: Caret();
  public: ~Caret();
  private: void Draw(const gfx::Graphics& gfx);
  public: void Hide(const gfx::Graphics& gfx);
  public: void Give(const gfx::Graphics& gfx);
  public: void Show(const gfx::Graphics& gfx, const gfx::RectF& rect);
  public: void Take(const gfx::Graphics& gfx);

  DISALLOW_COPY_AND_ASSIGN(Caret);
};


#endif //!defined(INCLUDE_listener_winapp_visual_caret_h)
