#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - listener - winapp - winmain
// listener/winapp/winmain.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/listener/winapp/vi_Pane.cpp#1 $
//
#include "./vi_Pane.h"

#include "./vi_Frame.h"

extern uint g_nDropTargetMsg;


Pane::Pane()
    : m_nActiveTick(0),
      m_pwszName(L"") {
}

void Pane::Activate() {
  ++m_nActiveTick;
  GetFrame()->DidActivePane(this);
}

Pane::MessageResult Pane::ForwardMessage(uint, WPARAM, LPARAM) {
  return MessageResult();
}

LRESULT Pane::onMessage(uint uMsg, WPARAM wParam, LPARAM lParam) {
    switch (uMsg)
    {
    case WM_SETFOCUS:
        m_nActiveTick = ::GetTickCount();
        return 0;

    default:
        if (g_nDropTargetMsg && g_nDropTargetMsg == uMsg)
        {
            return reinterpret_cast<LRESULT>(this);
        }
        break;
    } // switch uMsg

    return Widget::onMessage(uMsg, wParam, lParam);
} // Pane::onMessage
