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
#if !defined(INCLUDE_listener_winapp_app_proc_pane_h)
#define INCLUDE_listener_winapp_app_proc_pane_h

#include "./ap_input_history.h"
#include "./vi_edit_pane.h"


enum ListenerMessage
{
    LISTENER_WM_SETSTATUS = WM_USER,
}; // ListenerMessage


//////////////////////////////////////////////////////////////////////
//
// ProcessPane
//
//  m_fEcho
//    true if subprocess echo input text.
//
class ProcessPane : public EditPane
{
    private: DWORD          m_dwSender;
    private: bool           m_fEcho;
    private: HANDLE         m_hProcess;
    private: HANDLE         m_hReceiver;
    private: HANDLE         m_hStdInR;
    private: HANDLE         m_hStdInW;
    private: HANDLE         m_hStdOutR;
    private: HANDLE         m_hStdOutW;
    private: HANDLE         m_hSender;
    private: HWND           m_hwndFrame;
    private: InputHistory   m_oHistory;
    private: Edit::Range*   m_pOutput;

    public: ProcessPane(Edit::Buffer*);
    public: ~ProcessPane();

    // [O]
    public: virtual bool OnIdle();
    protected: virtual LRESULT onMessage(UINT, WPARAM, LPARAM);

    // [R]
    private: DWORD receiverProc();
    private: static DWORD WINAPI receiverProc_(void* pv)
        { return reinterpret_cast<ProcessPane*>(pv)->receiverProc(); }

    // [S]
    private: DWORD senderProc();
    private: static DWORD WINAPI senderProc_(void* pv)
        { return reinterpret_cast<ProcessPane*>(pv)->senderProc(); }
    private: void sendText();
    public: uint Start(HWND, const char16*);
}; // ProcessWindow

#endif //!defined(INCLUDE_listener_winapp_app_proc_pane_h)
