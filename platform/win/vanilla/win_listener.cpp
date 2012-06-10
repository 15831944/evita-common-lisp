#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - platform - win - listener stream
// platform/win/vanilla/win_listener.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/platform/win/vanilla/win_listener.cpp#1 $
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

bool on_windowing_idle(uint);


using namespace MiniLisp;

class ListenerStream : public Instance_<Layout::C_listener_stream> {};

//////////////////////////////////////////////////////////////////////
//
// ListenerStreamImpl
//
class ListenerStreamImpl :
    public      MiniLisp::StreamImpl
{
    private: Val        m_column;
    private: uint       m_cwchRead;
    private: size_t     m_cwchWrite;
    private: HWND       m_hwndListener;
    private: uint       m_nRead;
    private: char16     m_rgwchRead[1024];
    private: char16     m_rgwchWrite[1024*4];
    private: Val        m_unread;

    public: ListenerStreamImpl(
        HWND    hwndListener ) :
            m_column(Fixnum::Encode(0)),
            m_cwchRead(0),
            m_cwchWrite(0),
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

    // FIXME 2007-05-20 Should we use LISTENER_WM_QUERYCOLUMN? If so, we
    // need to call ForceOutput before querying column to listener.
    public: virtual Val LineColumn(Val)
        { return m_column; }

    public: virtual Val LineNumber(Val) { return nil; }

    public: virtual bool Listen(Val)
        { return m_unread != nil || m_nRead < m_cwchRead; }

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
                char16 wch = m_rgwchRead[nIndex];
                if (wch == 0x0A) m_column = Fixnum::Encode(0);
                return Character::Encode(wch);
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

    int iIdle = 1;

    for (;;)
    {
        MSG oMsg;
        if (! ::PeekMessage(&oMsg, NULL, 0, 0, PM_REMOVE))
        {
            if (0 != iIdle)
            {
                uint nCount = 0;
                for (;;)
                {
                    if (! on_windowing_idle(nCount))
                    {
                        break;
                    } // if
                    nCount += 1;
                } // for
            } // if

            iIdle = 0;

            int iRet = ::GetMessage(&oMsg, NULL, 0, 0);
            if (-1 == iRet)
            {
                // handle error on GetMessage
            }
            else if (0 == iRet)
            {
                // We got WM_QUIT. What should we do?
            }
        } // if

        ::TranslateMessage(&oMsg);
        ::DispatchMessage(&oMsg);

        switch (oMsg.message)
        {
        case WM_PAINT:
        case 0x118: // WM_SYSTIME for bliking caret
            break;

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

        default:
            iIdle = 1;
            break;
        } // switch message
    } // for
} // ListenerStreamImpl::fill


// ListenerStreamImpl::ForceOutput
void ListenerStreamImpl::ForceOutput(Val)
{
    if (m_cwchWrite == 0) return;

    // Note: We must wait listener processes sent text.
    ::SendMessage(
        m_hwndListener,
        LISTENER_WM_SENDTEXT,
        m_cwchWrite,
        reinterpret_cast<LPARAM>(m_rgwchWrite) );

    m_cwchWrite = 0;
} // ListenerStreamImpl::ForceOutput


//////////////////////////////////////////////////////////////////////
//
// ListenerStreamImpl::WriteString
//
void ListenerStreamImpl::WriteString(
    Val             stream,
    const char16*   pwchString,
    size_t          cwchRest )
{
    const char16* pwchEnd = pwchString + cwchRest;

    bool fNewline = false;

    for (const char16* pwch = pwchString; pwch < pwchEnd; pwch++)
    {
        if (*pwch == 0x0A)
        {
            m_column = Fixnum::Encode(0);
            fNewline = true;
        }
        else
        {
            m_column = add_xx(m_column, Fixnum::Encode(1));
        } // if
    } // for each char

    {
        const char16* pwch = pwchString;
        while (cwchRest > 0)
        {
            size_t cwchNext = min(
                m_cwchWrite + cwchRest,
                lengthof(m_rgwchWrite) );

            size_t cwch = cwchNext - m_cwchWrite;

            ::memcpy(
                m_rgwchWrite + m_cwchWrite,
                pwch,
                sizeof(char16) * cwch );

            m_cwchWrite = cwchNext;

            if (! fNewline && m_cwchWrite < lengthof(m_rgwchWrite))
            {
                break;
            }

            ForceOutput(stream);

            cwchRest -= cwch;
            pwch += cwch;
        } // while
    }
} // ListenerStreamImpl::WriteString


//////////////////////////////////////////////////////////////////////
//
// make_listener_stream
//
static Val make_listener_stream(HWND hwndListener)
{
    ListenerStreamImpl* pPlatform = new ListenerStreamImpl(hwndListener);

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

    Val terminal_io = make_listener_stream(pInfo->m_hwndListener);

    set_tlv(TLV_Aterminal_ioA,      terminal_io);
    set_tlv(TLV_Astandard_inputA,   terminal_io);
    set_tlv(TLV_Astandard_outputA,  terminal_io);
    set_tlv(TLV_Aerror_outputA,     terminal_io);
    set_tlv(TLV_Adebug_ioA,         terminal_io);
    set_tlv(TLV_Aquery_ioA,         terminal_io);
    set_tlv(TLV_Atrace_outputA,     terminal_io);
} // bind_listener_streams
