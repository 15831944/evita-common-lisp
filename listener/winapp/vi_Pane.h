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
#if !defined(INCLUDE_visual_panewnd_h)
#define INCLUDE_visual_panewnd_h

#include "./vi_CommandWindow.h"

//////////////////////////////////////////////////////////////////////
//
// Pane
//
class Pane :
    public CommandWindow,
    public ChildNode_<Frame, Pane>
{
    protected: uint             m_nActiveTick;
    protected: const char16*    m_pwszName;

    // ctor
    protected: Pane() :
        m_nActiveTick(0),
        m_pwszName(L"") {}

    // [A]
    public: void Activate();

    // [G]
    public: uint GetActiveTick() const { return m_nActiveTick; }

    public: Frame*   GetFrame() const { return m_pParent; }
    public: const char16*  GetName()  const { return m_pwszName; }
    public: virtual int    GetTitle(char16* pwsz, int) = 0;

    // [H]
    public: virtual bool HasFocus() const
        { return ::GetFocus() == m_hwnd; }

    // [I]
    public: virtual bool IsPane() const override { return true; }

    public: static bool Is_(const CommandWindow* p)
        { return p->IsPane(); }

    // [O]
    protected: LRESULT onMessage(uint, WPARAM, LPARAM);

    // [R]
    public: void virtual Realize();

    // [U]
    public: virtual void UpdateStatusBar() {}
}; // Pane

#endif //!defined(INCLUDE_visual_panewnd_h)
