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
#if !defined(INCLUDE_listener_winapp_app_listener_pane_h)
#define INCLUDE_listener_winapp_app_listener_pane_h

#include "./ap_input_history.h"
#include "./vi_edit_pane.h"


//////////////////////////////////////////////////////////////////////
//
// ListenerPane
//
//  m_fEcho
//    true if subprocess echo input text.
//
class ListenerPane : public EditPane
{
    struct SendInfo
    {
        Posn    m_lEnd;
        Posn    m_lPosn;
    }; // SendInfo

    private: DWORD          m_dwLisp;
    private: HMODULE        m_hDll;
    private: HWND           m_hwndFrame;
    private: HANDLE         m_hLisp;
    private: InputHistory   m_oHistory;
    private: SendInfo       m_oSendInfo;
    private: Edit::Range*   m_pOutput;
    private: void*          m_pvLisp;
    private: char16         m_rgwch[1024];

    // ctor/dtor
    public: ListenerPane(Edit::Buffer*);
    public: ~ListenerPane();

    // [L]
    private: DWORD lispProc();
    private: static DWORD WINAPI lispProc_(void* pv)
        { return reinterpret_cast<ListenerPane*>(pv)->lispProc(); }

    // [O]
    public: virtual bool OnIdle();
    protected: virtual LRESULT onMessage(UINT, WPARAM, LPARAM);

    // [S]
    private: DWORD senderProc();

    private: static DWORD WINAPI senderProc_(void* pv)
        { return reinterpret_cast<ListenerPane*>(pv)->senderProc(); }

    private: void sendText();
    private: void sendTextAux();
    public: uint  Start(HWND, const char16*);
}; // ProcessWindow

#endif //!defined(INCLUDE_listener_winapp_app_listener_pane_h)
