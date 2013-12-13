//////////////////////////////////////////////////////////////////////////////
//
// evcl - listener - edit buffer
// listener/winapp/ed_buffer.h
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/listener/winapp/vi_Pane.h#1 $
//
#if !defined(INCLUDE_vi_Pane_h)
#define INCLUDE_vi_Pane_h

#include "./vi_CommandWindow.h"
#include "widgets/container_widget.h"

namespace gfx {
class Graphics;
}

class Frame;
struct Point;

class Pane : public CommandWindow_<Pane>, public ChildNode_<Frame, Pane> {
  protected: uint             m_nActiveTick;
  protected: const char16*    m_pwszName;

  // ctor
  protected: Pane();

  // [A]
  public: virtual void Activate();

  // [D]
  public: virtual void DidChangeOwnerFrame() {}

  public: virtual bool DidCreateHwnd(HWND) {
    // TODO: Once make EditPane isn't derived from Pane, we should remove
    // Pane::DidCreateHwnd().
    return false;
  }
  public: virtual bool DidDestroyHwnd(HWND) {
    // TODO: Once make EditPane isn't derived from Pane, we should remove
    // Pane::DidDestoryHwnd().
    return false;
  }

  // [G]
  public: uint GetActiveTick() const { return m_nActiveTick; }
  public: static const char16* GetClass_() { return L"Pane"; }
  public: virtual HCURSOR GetCursorAt(const Point&) const { return nullptr; }

  public: Frame*   GetFrame() const { return m_pParent; }
  public: const char16*  GetName()  const { return m_pwszName; }
  public: virtual int    GetTitle(char16* pwsz, int) = 0;

  // [I]
  public: virtual bool IsPane() const override { return true; }

  public: static bool Is_(const CommandWindow* p) { return p->IsPane(); }

  // [O]
  // TODO: Once we have scroll bar widget, we don't use
  // |OnDeprecatedVScroll()|.
  public: virtual void OnDeprecatedVScroll(uint, HWND) {}
  public: virtual void OnLeftButtonDown(uint, const Point&) {}
  public: virtual void OnLeftButtonUp(uint, const Point&) {}
  protected: LRESULT onMessage(uint, WPARAM, LPARAM);
  public: virtual void OnMouseMove(uint, const Point&) {}

  // [U]
  public: virtual void UpdateStatusBar() {}
};

#endif //!defined(INCLUDE_vi_Pane_h)
