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

namespace gfx {
class Graphics;
}

struct Point;

class Pane : public CommandWindow, public ChildNode_<Frame, Pane> {
  protected: uint             m_nActiveTick;
  protected: const char16*    m_pwszName;

  // ctor
  protected: Pane();

  // [A]
  public: virtual void Activate();

  // [D]
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
  public: virtual HCURSOR GetCursorAt(const Point&) const { return nullptr; }

  public: Frame*   GetFrame() const { return m_pParent; }
  public: const char16*  GetName()  const { return m_pwszName; }
  public: virtual int    GetTitle(char16* pwsz, int) = 0;

  // [H]
  public: virtual bool HasFocus() const { return ::GetFocus() == m_hwnd; }
  public: virtual void Hide() = 0;

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

  // [R]
  public: void virtual Realize() {
    // TODO: Once make EditPane isn't derived from Pane, we should remove
    // Pane::Realize().
    CAN_NOT_HAPPEN();
  }

  public: virtual void Resize(const RECT&) {
    // TODO: We shoule make Pane::Resize() as abstract member function.
    CAN_NOT_HAPPEN();
  }

  // [S]
  public: virtual void SetFocus() = 0;
  public: virtual void Show() = 0;

  // [U]
  public: virtual void UpdateStatusBar() {}
};

#endif //!defined(INCLUDE_vi_Pane_h)
