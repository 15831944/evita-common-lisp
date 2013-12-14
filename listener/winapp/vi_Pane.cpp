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
#include "widgets/naitive_window.h"

Pane::Pane(std::unique_ptr<widgets::NaitiveWindow>&& naitive_window)
    : CommandWindow_(std::move(naitive_window)),
      m_nActiveTick(0),
      m_pwszName(L"") {
}

Pane::Pane()
    : Pane(std::move(std::unique_ptr<widgets::NaitiveWindow>())) {
}

void Pane::Activate() {
  ++m_nActiveTick;
  GetFrame()->DidActivePane(this);
}
