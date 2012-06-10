//////////////////////////////////////////////////////////////////////////////
//
// evcl - listener - edit buffer
// listener/winapp/ed_buffer.h
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cl/cl_defs.h#34 $
//
#if !defined(INCLUDE_listener_winapp_visual_edit_pane_h)
#define INCLUDE_listener_winapp_visual_edit_pane_h

#include "./vi_text_pane.h"

//////////////////////////////////////////////////////////////////////
//
// EditPane
//
class EditPane : public TextPane
{
    public: EditPane(Edit::Buffer* pBuffer, Posn = 0);

    // [O]
    public:    virtual bool    OnIdle();
    private:   void            onKeyDown(uint, BOOL);
    protected: virtual LRESULT onMessage(UINT, WPARAM, LPARAM);
}; // EditPane

#endif //!defined(INCLUDE_listener_winapp_visual_edit_pane_h)
