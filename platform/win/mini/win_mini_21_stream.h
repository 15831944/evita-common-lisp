//////////////////////////////////////////////////////////////////////////////
//
// evcl - platform - pre-compiled header
// platform/win_mini_21_stream.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/platform/win/mini/win_mini_21_stream.h#9 $
//
#if !defined(INCLUDE_platform_win_mini_21_stream_h)
#define INCLUDE_platform_win_mini_21_stream_h

#include "../../generic/mini/pl_mini_21_stream.h"

namespace MiniLisp
{

class ConsoleStream : public Instance_<Layout::C_console_stream> {};


struct ExtFormat
{
    uint    m_nCodePage;
    uint    m_nFlags;

    ExtFormat(uint nCodePage, uint nFlags) :
        m_nCodePage(nCodePage),
        m_nFlags(nFlags) {}
}; // ExtFormat

//////////////////////////////////////////////////////////////////////
//
// Console Stream Implementation
//
class ConsoleStreamImpl : public StreamImpl
{
    protected: HANDLE  m_hOutput;
    protected: HANDLE  m_hInput;
    protected: Val     m_unread1;
    protected: Val     m_unread2;
    protected: bool    m_fEol;

    protected: enum
    {
        MaxWrite = 1024,
    }; // enum

    public: ConsoleStreamImpl() :
        m_unread1(nil),
        m_unread2(nil),
        m_fEol(true)
    {

        m_hInput = ::CreateFileW(
            L"CONIN$",
            GENERIC_READ | GENERIC_WRITE,
            FILE_SHARE_WRITE | FILE_SHARE_READ,
            NULL,
            OPEN_EXISTING,
            0,
            NULL );
        ASSERT(INVALID_HANDLE_VALUE != m_hInput);

        m_hOutput = ::CreateFileW(
            L"CONOUT$",
            GENERIC_READ | GENERIC_WRITE,
            FILE_SHARE_WRITE | FILE_SHARE_READ,
            NULL,
            OPEN_EXISTING,
            0,
            NULL );
        ASSERT(INVALID_HANDLE_VALUE != m_hOutput);

        init();
    } // ConsoleStreamImpl

    static Thread* sm_pThread;
    void init();

    public: virtual void ClearInput(Val) {}

    public: virtual Val Close(Val, bool)
    {
        if (INVALID_HANDLE_VALUE != m_hInput)
            { ::CloseHandle(m_hInput); m_hInput = INVALID_HANDLE_VALUE; }

        if (INVALID_HANDLE_VALUE != m_hOutput)
            { ::CloseHandle(m_hOutput); m_hOutput = INVALID_HANDLE_VALUE; }

        return t;
    } // Close

    public: virtual void ForceOutput(Val) {}

    public: virtual Val LineColumn(Val)
    {
        CONSOLE_SCREEN_BUFFER_INFO oInfo;
        if (! ::GetConsoleScreenBufferInfo(m_hOutput, &oInfo)) return nil;
        return Fixnum::Encode(oInfo.dwCursorPosition.X);
    } // LineColumn

    public: virtual Val LineNumber(Val)
        { return nil; }

    public: virtual bool Listen(Val)
        { return nil != m_unread1 || ! m_fEol; }

    public: virtual Val OutputWidth(Val)
    {
        CONSOLE_SCREEN_BUFFER_INFO oInfo;
        if (! ::GetConsoleScreenBufferInfo(m_hOutput, &oInfo)) return nil;
        return Fixnum::Encode(oInfo.dwSize.X);
    } // OutputWidth

    public: virtual Val ReadChar(Val stream)
    {
        if (m_unread1->Is<Character>())
        {
            Val ch = m_unread1;
            m_unread1 = m_unread2;
            m_unread2 = nil;
            return ch;
        }

        Val ch = read_char();

        if (Character::Encode(0x0D) == ch)
        {
            ch = read_char();
            if (Character::Encode(0x0A) != ch)
            {
                if (characterp(ch))
                {
                    UnreadChar(stream, Character::ToCode(ch));
                    ch = Character::Encode(0x0D);
                }
            } // if
        } // if

        return ch;
    } // ReadChar

    Val read_char()
    {
        m_fEol = false;

      try_again:
        char16 wchChar;
        DWORD cwchRead;
        BOOL fSucceeded = ::ReadConsoleW(
            m_hInput,
            &wchChar,
            1,
            &cwchRead,
            NULL );
        if (! fSucceeded)
        {
            DEBUG_PRINTF(L"ReadConsole: error=%u\r\n", ::GetLastError());
            goto try_again;
        }

        if (0 == cwchRead)
        {
            // for Ctrl+C
            goto try_again;
        }

        if (0x1A == wchChar && 1 == cwchRead)
        {
            return Keof;
        }

        m_fEol = 0x0A == wchChar;

        return Character::Encode(wchChar);
    } // read_char

    public: virtual void UnreadChar(Val, char16 wch)
    {
        if (m_unread2->Is<Character>())
        {
            error(L"Too many unread-char.");
        }

        m_unread2 = m_unread1;
        m_unread1 = Character::Encode(wch);
    } // UnreadChar

    public: virtual void WriteString(
        Val,
        const char16* pwchString,
        size_t cwchString )
    {
        const char16* pwchEnd = pwchString + cwchString;
        while (pwchString < pwchEnd)
        {
            size_t cwchWrite = cwchString;
            if (cwchWrite > MaxWrite)
            {
                cwchWrite = MaxWrite;
            }

            DWORD cwchWritten;
            BOOL fSucceeded = ::WriteConsoleW(
                m_hOutput,
                pwchString,
                static_cast<DWORD>(cwchWrite),
                &cwchWritten,
                NULL );
            if (! fSucceeded || 0 == cwchWritten)
            {
                return;
            }

            cwchString -= cwchWritten;
            pwchString += cwchWritten;
        } // write
    } // WriteStirng
}; // ConsoleStreamImpl


//////////////////////////////////////////////////////////////////////
//
// File Stream Implementation
//
class FileStreamImpl : public StreamImpl
{
    protected: HANDLE  m_hFile;

    public: FileStreamImpl(HANDLE hFile)
    {
        m_hFile = hFile;
    } // FileStreamImpl

    public: virtual Val Close(Val, bool)
    {
        if (INVALID_HANDLE_VALUE == m_hFile) return nil;
        ::CloseHandle(m_hFile);
        m_hFile = INVALID_HANDLE_VALUE;
        return t;
    } // Close
}; // FileStreamImpl


//////////////////////////////////////////////////////////////////////
//
// File Byte Input Stream
//
class FileByteInStreamImpl : public FileStreamImpl
{
    protected: uint8    m_rgbBuffer[1024];
    protected: uint8*   m_pbRead;
    protected: size_t   m_cbRest;

    public: FileByteInStreamImpl(HANDLE hFile) :
        FileStreamImpl(hFile)
    {
        m_pbRead = m_rgbBuffer;
        m_cbRest = 0;
    } // FileByteInStreamImpl

    public: virtual Val ReadByte(Val)
    {
        if (0 == m_cbRest)
        {
            if (! fill())
            {
                return Keof;
            }
        }

        m_cbRest -= 1;
        uint8 bByte = *m_pbRead++;
        return Fixnum::Encode(bByte);
    } // ReadByte

    // fill
    protected: bool fill()
    {
        DWORD cbRead;
        BOOL fSucceeded = ::ReadFile(
            m_hFile,
            m_rgbBuffer,
            sizeof(m_rgbBuffer),
            &cbRead,
            NULL );
        if (! fSucceeded)
        {
            return false;
        }

        if (0 == cbRead)
        {
            return false;
        }

        m_cbRest = cbRead;
        m_pbRead = m_rgbBuffer;
        return true;
    } // fill
}; // FileByteInStreamImpl


//////////////////////////////////////////////////////////////////////
//
// File Byte Input Stream
//
class FileByteOutStreamImpl : public FileStreamImpl
{
    protected: uint8    m_rgbBuffer[1024];
    protected: size_t  m_cbWrite;

    public: FileByteOutStreamImpl(HANDLE hFile) :
        FileStreamImpl(hFile)
    {
        m_cbWrite = 0;
    } // FileByteInStreamImpl

    public: virtual void ClearOutput(Val)
    {
        m_cbWrite = 0;
    } // ClearOutput

    public: virtual Val Close(Val stream, bool fAbort)
    {
        if (INVALID_HANDLE_VALUE == m_hFile) return nil;

        unless (fAbort) ForceOutput(stream);
        FileStreamImpl::Close(stream, fAbort);
        return t;
    } // Close

    public: virtual void ForceOutput(Val)
    {
        const uint8* pbRunner = m_rgbBuffer;

        while (0 != m_cbWrite)
        {
            DWORD cbWritten;
            BOOL fSucceeded = ::WriteFile(
                m_hFile,
                pbRunner,
                static_cast<DWORD>(m_cbWrite),
                &cbWritten,
                NULL );
            if (! fSucceeded)
            {
                m_cbWrite = 0;
                return;
            }

            if (0 == cbWritten)
            {
                m_cbWrite = 0;
                return;
            }

            m_cbWrite -= cbWritten;
            pbRunner  += cbWritten;
        } // while
    } // ForceOutput

    // WriteBytes
    public: virtual void WriteBytes(
        Val stream,
        const uint8*
        pbData,
        size_t cbData)
    {
        while (cbData + m_cbWrite > sizeof(m_rgbBuffer))
        {
            size_t cbCopy = sizeof(m_rgbBuffer) - m_cbWrite;

            evcl_memmove(
                m_rgbBuffer + m_cbWrite,
                pbData,
                cbCopy );

            pbData += cbCopy;
            cbData -= cbCopy;

            ForceOutput(stream);
        } // while

        evcl_memmove(
            m_rgbBuffer + m_cbWrite,
            pbData,
            cbData );

        m_cbWrite += cbData;
    } // WriteByte
}; // FileByteOutStreamImpl


//////////////////////////////////////////////////////////////////////
//
// File character input stream
//
// Note:
//  FileCharInStreamImpl inherits FileByteInStreamImpl. However, it doesn't
//  use no method defined in FileByteInStreamImpl except for constructor and
//  member variables.
//
// Note:
//  FileCharInStreamImpl can't support encoding with state transition, such
//  as ISO-2022. Since, MultiByteToWideChar doesn't accept state information
//  as an argument.
//
class FileCharInStreamImpl : public FileByteInStreamImpl
{
    protected: char16       m_rgwchBuffer[1024];
    protected: char16*      m_pwchRead;
    protected: size_t       m_cwchRest;
    protected: Val          m_unread1;
    protected: Val          m_unread2;
    protected: ExtFormat    m_oFormat;
    protected: Val          m_line_number;
    protected: Val          m_line_column;

    public: FileCharInStreamImpl(HANDLE hFile, Val ef);

    public: virtual Val LineNumber(Val) { return m_line_number; }
    public: virtual Val LineColumn(Val) { return m_line_column; }

    public: virtual Val ReadByte(Val s) 
        { unsupported(s, Qread_byte); }

    public: virtual Val ReadChar(Val stream)
    {
        if (m_unread1->Is<Character>())
        {
            Val ch = m_unread1;
            m_unread1 = nil;
            return ch;
        }

        Val ch = read_char();

        if (Character::Encode(0x0D) == ch)
        {
            ch = read_char();
            if (Character::Encode(0x0A) != ch)
            {
                if (characterp(ch))
                {
                    UnreadChar(stream, Character::ToCode(ch));
                    ch = Character::Encode(0x0D);
                }
            } // if
        } // if

        if (Character::Encode(0x0A) != ch)
        {
            m_line_column = add_xx(m_line_column, 1);
        }
        else
        {
            m_line_number = add_xx(m_line_number, 1);
            m_line_column = Fixnum::Encode(0);
        }

        return ch;
    } // ReadChar

    public: virtual void UnreadChar(Val, char16 wchChar)
    {
        if (m_unread2->Is<Character>())
        {
            error(L"too many unread");
        }

        m_unread2 = m_unread1;
        m_unread1 = Character::Encode(wchChar);
    } // UnreadChar

    // fill
    protected: bool fill()
    {
        if (0 != m_cbRest)
        {
            evcl_memcpy(
                m_rgbBuffer,
                m_rgbBuffer + sizeof(m_rgbBuffer) - m_cbRest,
                m_cbRest );
        }

        DWORD cbRead;
        BOOL fSucceeded = ::ReadFile(
            m_hFile,
            m_rgbBuffer + m_cbRest,
            static_cast<DWORD>(sizeof(m_rgbBuffer) - m_cbRest),
            &cbRead,
            NULL );
        if (! fSucceeded)
        {
            return false;
        }

        if (0 == cbRead)
        {
            return false;
        }

        m_cbRest += cbRead;

        size_t cbRest = m_cbRest;

        for (;;)
        {
            int cwchRead = ::MultiByteToWideChar(
                m_oFormat.m_nCodePage,
                MB_ERR_INVALID_CHARS,
                reinterpret_cast<LPCSTR>(m_rgbBuffer),
                static_cast<int>(cbRest),
                m_rgwchBuffer,
                lengthof(m_rgwchBuffer) );
            if (cwchRead >= 1)
            {
                evcl_memmove(
                    m_rgbBuffer,
                    m_rgbBuffer + cbRest,
                    sizeof(m_rgbBuffer) - cbRest );
                m_cbRest -= cbRest;
                m_cwchRest = cwchRead;
                break;
            }

            if (1 == cbRest)
            {
                // Can't translate byte into Unicode in m_nCodePage.
                return false;
            }

            {
                DWORD dwError = ::GetLastError();
                if (ERROR_NO_UNICODE_TRANSLATION != dwError)
                {
                    return false;
                }
            }

            cbRest -= 1;
        } // for

        m_pwchRead = m_rgwchBuffer;

        return true;
    } // fill

    // read character
    protected: Val read_char()
    {
        if (0 == m_cwchRest)
        {
            if (! fill())
            {
                return Keof;
            }
        }

        Val xChar = Character::Encode(*m_pwchRead++);
        m_cwchRest -= 1;
        return xChar;
    } // read_char
}; // FileCharInStraem


//////////////////////////////////////////////////////////////////////
//
// File character output stream
//
class FileCharOutStreamImpl : public FileByteOutStreamImpl
{
    protected: ExtFormat    m_oFormat;
    protected: Val          m_line_column;
    protected: Val          m_line_number;
    protected: uint         m_nChars;

    public: FileCharOutStreamImpl(HANDLE, Val);

    public: virtual Val Close(Val stream, bool fAbort)
    {
        if (INVALID_HANDLE_VALUE == m_hFile) return nil;

        unless (fAbort) ForceOutput(stream);

        FileStreamImpl::Close(stream, fAbort);

        if (fAbort)
        {
            funcall(Qdelete_file, stream->Decode<FileStream>()->m_pathname);
        }
        return t;
    } // Close

    public: virtual Val LineNumber(Val) { return m_line_number; }
    public: virtual Val LineColumn(Val) { return m_line_column; }

    public: virtual Val WriteByte(Val s) 
        { unsupported(s, Qwrite_byte); }

    // WriteString
    public: virtual void WriteString(
        Val             stream,
        const char16*   pwchString,
        size_t          cwchString )
    {
        ASSERT(NULL != pwchString);

        const char16* pwchStart = pwchString;
        const char16* pwchScan;
        for (
            pwchScan = pwchString;
            pwchScan < pwchString + cwchString;
            pwchScan++ )
        {
            switch (*pwchScan)
            {
            case 0x0A:
                write_string(stream, pwchStart, pwchScan - pwchStart);
                write_string(stream, L"\x0D\x0A", 2);

                pwchStart = pwchScan + 1;

                m_line_number = add_xx(m_line_number, 1);
                m_line_column = Fixnum::Encode(0);
                m_nChars += 1;
                break;

            case 0x09:
                m_line_column = add_xx(
                    m_line_column,
                    8 - (Fixnum::Decode_(m_line_column) + 1) % 8 );
                break;

            default:
                m_line_column = add_xx(m_line_column, 1);
                break;
            } // switch char

            m_nChars += 1;
        } // for each char

        write_string(stream, pwchStart, pwchScan - pwchStart);
    } // WriteString

    protected: void write_string(
        Val             stream,
        const char16*   pwchString,
        size_t          cwchString )
    {
        while (cwchString >= 1)
        {
            ASSERT(m_cbWrite <= sizeof(m_rgbBuffer));

            // Make sure MBC buffer has space for converting
            // at least one WC character.
            if (m_cbWrite + 10 > sizeof(m_rgbBuffer))
            {
                ForceOutput(stream);

                ASSERT(0 == m_cbWrite);
            }

            uint cbConvert = static_cast<uint>(
                sizeof(m_rgbBuffer) - m_cbWrite );

            uint cwchConvert = static_cast<uint>(cwchString);
                if (cwchConvert > cbConvert)
                {
                    cwchConvert = cbConvert;
                }

            for (;;)
            {
                int cchData = ::WideCharToMultiByte(
                    m_oFormat.m_nCodePage,
                    0,
                    pwchString,
                    cwchConvert,
                    reinterpret_cast<CHAR*>(m_rgbBuffer) + m_cbWrite,
                    cbConvert,
                    NULL,
                    NULL );

                if (cchData >= 1)
                {
                    m_cbWrite += cchData;
                    break;
                }

                DWORD dwError = ::GetLastError();

                if (ERROR_INSUFFICIENT_BUFFER != dwError)
                {
                    error(L"convertion error");
                }

                cwchConvert -= 1;
            } // forever

            pwchString += cwchConvert;
            cwchString -= cwchConvert;
        } // while
    } // write_string
}; // FileCharOutStreamImpl

} // MiniLisp

#endif //!defined(INCLUDE_platform_win_mini_21_stream_h)
