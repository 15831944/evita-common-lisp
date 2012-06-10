#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - platform - win - listener stream
// platform/win/vanilla/win_listener.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/platform/win/vanilla/pipe_win_listener.cpp#1 $
//
// Description:
//  Installs following C implemented lisp function for genesis.
//
#include "../../mini/mini_lisp.h"
#include "../../generic/mini/pl_mini_21_stream.h"

#include "../../listener/winapp/listener.h"


namespace MiniLisp
{

Val make_external_format(Val, Val, Val);
Val schedule_finalization(Val, Val);

} // MiniLisp


using namespace MiniLisp;

class ListenerStream : public Instance_<Layout::C_listener_stream> {};

//////////////////////////////////////////////////////////////////////
//
// ListenerStreamImpl
//
class ListenerStreamImpl :
    public      MiniLisp::StreamImpl
{
    private: uint       m_cwchRead;
    private: size_t     m_cwchWrite;
    private: HANDLE     m_hStdOut;
    private: HWND       m_hwndListener;
    private: uint       m_nRead;
    private: char16     m_rgwchRead[1024];
    private: char16     m_rgwchWrite[1024];
    private: Val        m_unread;

    public: ListenerStreamImpl(
        HANDLE  hStdOut,
        HWND    hwndListener ) :
            m_cwchRead(0),
            m_cwchWrite(0),
            m_hStdOut(hStdOut),
            m_hwndListener(hwndListener),
            m_nRead(0),
            m_unread(nil) {}

    // [C]
    public: virtual void ClearInput(Val) {}
    public: virtual Val  Close(Val, bool) { return t; }

    // [F]
    private: void fill();
    public: virtual void ForceOutput(Val);

    // [L]
    public: virtual Val LineColumn(Val stream)
    {
        ForceOutput(stream);
        LRESULT lColumn = ::SendMessage(
            m_hwndListener, LISTENER_WM_QUERYCOLUMN, 0, 0 );

        return Fixnum::Encode(lColumn);
    } // LineColumn

    public: virtual Val LineNumber(Val) { return nil; }

    public: virtual bool Listen(Val)
    {
        return m_nRead < m_cwchRead;
    } // Listen

    // [O]
    public: virtual Val OutputWidth(Val) { return nil; }

    // [R]
    public: virtual Val ReadChar(Val)
    {
        if (m_unread->Is<Character>())
        {
            Val ch = m_unread;
            m_unread = nil;
            return ch;
        } // if

        return read_char();
    } // ReadChar

    private: Val read_char()
    {
        for (;;)
        {
            if (m_nRead < m_cwchRead)
            {
                uint nIndex = m_nRead;
                m_nRead += 1;
                return Character::Encode(m_rgwchRead[nIndex]);
            }

            fill();
        } // for
    } // read_char

    public: virtual void UnreadChar(Val, char16 wch)
    {
        if (m_unread->Is<Character>())
        {
            error(L"Too many unread-char");
        }

        m_unread = Character::Encode(wch);
    } // UnreadChar

    public: virtual void WriteString(Val, const char16*, size_t);
}; // ListenerStreamImpl


//////////////////////////////////////////////////////////////////////
//
// ListenerStreamImpl::fill
//
void ListenerStreamImpl::fill()
{
    ::PostMessage(m_hwndListener, LISTENER_WN_READY, 0, 0);

    for (;;)
    {
        MSG oMsg;
        int iRet = ::GetMessage(&oMsg, NULL, 0, 0);
        if (iRet == -1)
        {
            // handle error on GetMessage
        }
        else if (iRet == 0)
        {
            // We got WM_QUIT. What should we do?
        }
        else
        {
            ::TranslateMessage(&oMsg);
            ::DispatchMessage(&oMsg);
            switch (oMsg.message)
            {
            case LISTENER_WM_SENDTEXT:
            {
                uint cwch = static_cast<uint>(oMsg.wParam);
                const char16* pwch = reinterpret_cast<char16*>(oMsg.lParam);
                cwch = min(lengthof(m_rgwchRead), cwch);
                ::memcpy(m_rgwchRead, pwch, sizeof(char16) * cwch);
                m_cwchRead = cwch;
                m_nRead = 0;

                // If listener sends characters more than buffer, listener
                // thread will be blocked.

                // Returns number of characters processed.
                ::PostMessage(
                    m_hwndListener,
                    LISTENER_WN_RECEIVETEXT,
                    cwch,
                    0 );
                return;
            } // LISTENER_WM_SENDTEXT
            } // switch message
        } // if
    } // for
} // ListenerStreamImpl::fill


// ListenerStreamImpl::ForceOutput
void ListenerStreamImpl::ForceOutput(Val)
{
    size_t nWrite = 0;
    while (m_cwchWrite > 0)
    {
        DWORD cbWritten;
        BOOL fSucceeded = ::WriteFile(
            m_hStdOut,
            reinterpret_cast<BYTE*>(m_rgwchWrite + nWrite),
            static_cast<DWORD>(sizeof(char16) * m_cwchWrite),
            &cbWritten,
            NULL );
        if (! fSucceeded)
        {
            break;
        }

        nWrite += cbWritten / sizeof(char16);
        m_cwchWrite -= cbWritten / sizeof(char16);
    } // while

    m_cwchWrite = 0;
} // ListenerStreamImpl::ForceOutput


//////////////////////////////////////////////////////////////////////
//
// ListenerStreamImpl::WriteString
//
void ListenerStreamImpl::WriteString(
    Val             stream,
    const char16*   pwch,
    size_t          cwchRest )
{
    while (cwchRest > 0)
    {
        size_t cwchNext = min(m_cwchWrite + cwchRest, lengthof(m_rgwchWrite));
        size_t cwch = cwchNext - m_cwchWrite;
        ::memcpy(
            m_rgwchWrite + m_cwchWrite,
            pwch,
            sizeof(char16) * cwch );

        m_cwchWrite = cwchNext;

        if (m_cwchWrite < lengthof(m_rgwchWrite))
        {
            break;
        }

        ForceOutput(stream);

        cwchRest -= cwch;
        pwch += cwch;
    } // while
} // ListenerStreamImpl::WriteString


//////////////////////////////////////////////////////////////////////
//
// make_listener_stream
//
static Val make_listener_stream(HANDLE hStdOut, HWND hwndListener)
{
    ListenerStreamImpl* pPlatform =
        new ListenerStreamImpl(hStdOut, hwndListener);

    Val stream = MiniThread::Get()->AllocInstance(CLASSD_listener_stream);

    ListenerStream* p = stream->Decode<ListenerStream>();
        p->m_flags = 
            FromInt<Val_>(Stream::Flag_Both | Stream::Flag_Interactive);
        p->m_blob = Fixnum::Encode(pPlatform);

        p->m_external_format = make_external_format(
            TLV(Adefault_charsetA),
            Kcrlf,
            Fixnum::Encode(0) );

    schedule_finalization(stream, Qclose);

    return stream;
} // make_listener_stream


//////////////////////////////////////////////////////////////////////
//
// bind_listener_streams
//
void bind_listener_streams(void* pv)
{
    // FIXME 2007-05-19 We should use initialization of
    // default-charset-default to another place.
    VAR(Adefault_charset_defaultA) =
        gethash(Fixnum::Encode(::GetACP()), VAR(Acharset_tableA));

    set_tlv(
        TLV_Adefault_charsetA,
        VAR(Adefault_charset_defaultA) );

    ListenerInfo* pInfo = reinterpret_cast<ListenerInfo*>(pv);

    Val terminal_io = make_listener_stream(
        pInfo->m_hStdOut,
        pInfo->m_hwndListener );

    set_tlv(TLV_Aterminal_ioA,      terminal_io);
    set_tlv(TLV_Astandard_inputA,   terminal_io);
    set_tlv(TLV_Astandard_outputA,  terminal_io);
    set_tlv(TLV_Aerror_outputA,     terminal_io);
    set_tlv(TLV_Adebug_ioA,         terminal_io);
    set_tlv(TLV_Aquery_ioA,         terminal_io);
    set_tlv(TLV_Atrace_outputA,     terminal_io);
} // bind_listener_streams
