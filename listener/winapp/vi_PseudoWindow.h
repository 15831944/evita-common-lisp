//////////////////////////////////////////////////////////////////////////////
//
// evcl - listener - edit buffer
// listener/winapp/ed_buffer.h
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/listener/winapp/vi_PseudoWindow.h#1 $
//
#if !defined(INCLUDE_visual_PseudoWindow_h)
#define INCLUDE_visual_PseudoWindow_h

#include "./vi_CommandWindow.h"

//////////////////////////////////////////////////////////////////////
//
// PseudoWindow
//
class PseudoWindow : public CommandWindow {
  private: bool has_focus_;

  protected: PseudoWindow();
  protected: ~PseudoWindow();

  public: bool has_focus() { return has_focus_; }
  protected: void set_owner_window(HWND hwnd);

  // [C]
  public: static const char16* GetClass_() { return L"PseudoWindow"; }

  // [D]
  protected: virtual void DidKillFocus() override;
  protected: virtual void DidSetFocus() override;

  // [S]
  public: virtual void SetFocus() override;
};

#endif //!defined(INCLUDE_visual_PseudoWindow_h)
