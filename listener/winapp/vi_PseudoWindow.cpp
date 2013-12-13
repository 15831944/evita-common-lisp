#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - listener - winapp - PseudoWindow
// listener/winapp/winmain.cpp
//
// Copyright (C) 1996-2013 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/listener/winapp/vi_Pane.cpp#1 $
//
#include "./vi_PseudoWindow.h"

PseudoWindow::PseudoWindow()
    : has_focus_(false) {
}

PseudoWindow::~PseudoWindow() {
  m_hwnd = nullptr;
}

void PseudoWindow::set_owner_window(HWND hwnd) {
  ASSERT(!m_hwnd);
  m_hwnd = hwnd;
}

void PseudoWindow::DidKillFocus() {
  ASSERT(has_focus_);
  has_focus_ = false;
  Widget::DidKillFocus();
}

void PseudoWindow::DidSetFocus() {
  has_focus_ = true;
  Widget::DidSetFocus();
}

void PseudoWindow::SetFocus() {
  Widget::SetFocus();
}
