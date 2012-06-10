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

//////////////////////////////////////////////////////////////////////
//
// Pane::Activate
//
void Pane::Activate()
{
    ASSERT(IsRealized());
    GetFrame()->SetActivePane(this);
} // Activate


//////////////////////////////////////////////////////////////////////
//
// Pane::onMessage
//
LRESULT Pane::onMessage(uint uMsg, WPARAM wParam, LPARAM lParam)
{
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

    return BaseWindow::onMessage(uMsg, wParam, lParam);
} // Pane::onMessage


//////////////////////////////////////////////////////////////////////
//
// Pane::Realize
//
void Pane::Realize()
{
    when (IsRealized()) return;

    Frame* pFrame = GetFrame();

    RECT rc;
    pFrame->GetPaneRect(&rc);

    CreateWindowEx(
        0,
        NULL,
        WS_CHILD,
        *pFrame,
        rc.left,
        rc.top,
        rc.right - rc.left,
        rc.bottom - rc.top );
} // Pane::Realize
