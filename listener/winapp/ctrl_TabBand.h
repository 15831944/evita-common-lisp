//////////////////////////////////////////////////////////////////////////////
//
// evcl - listener - edit buffer
// listener/winapp/ed_buffer.h
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/listener/winapp/ctrl_TabBand.h#1 $
//
#if !defined(INCLUDE_control_tabBand_h)
#define INCLUDE_control_tabBand_h

#define TABBAND_NOTIFY_CLOSE        (TCN_LAST - 1)
#define TABBAND_NOTIFY_QUERY_CLOSE  (TCN_LAST - 2)

enum TabBandDragAndDrop {
    kHover,
    kDrop,
    kThrow,
};

const char16 TabBand__TabDragMsgStr[] = L"Evita.TabBand.TabDrag";
void TabBand__Init(HINSTANCE);

#endif //!defined(INCLUDE_control_tabBand_h)
