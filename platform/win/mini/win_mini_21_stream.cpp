#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - lisp
// platform/win/mini/win_mini_21_stream.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/platform/win/mini/win_mini_21_stream.cpp#9 $
//
#include "../../../mini/mini_lisp.h"

#include "./win_mini_21_stream.h"

namespace MiniLisp
{

static Val make_console_stream();
Val make_external_format(Val, Val, Val);
Val make_file_stream(HANDLE, Val, Val);
Val realize_file_stream(Val, HANDLE);
Val schedule_finalization(Val, Val);

namespace
{

inline bool external_formatp(Val x)
    { return x->Is<ExternalFormat>(); }

// get_format
static ExtFormat get_format(Val ef)
{
    check_type(ef, external_format);
    const ExternalFormat* pFormat = ef->Decode<ExternalFormat>();
    const Charset* pCharset = pFormat->m_charset->Decode<Charset>();
    return ExtFormat(
        static_cast<uint>(Fixnum::Decode_(pCharset->m_code_page)),
        static_cast<uint>(Fixnum::Decode_(pFormat->m_flags)) );
} // get_format


//////////////////////////////////////////////////////////////////////
//
// Is Console Handle?
//
// Called by:
//  CCharOutStream::Init
//
static BOOL
is_console_handle(
    HANDLE  hFile )
{
    DWORD dwMode;
    return ::GetConsoleMode(hFile, &dwMode);
} // is_console_handle

} // namespace


FileCharInStreamImpl::FileCharInStreamImpl(HANDLE hFile, Val ef) :
    FileByteInStreamImpl(hFile),
    m_unread1(nil),
    m_unread2(nil),
    m_cwchRest(0),
    m_oFormat(get_format(ef)),
    m_line_number(Fixnum::Encode(0)),
    m_line_column(Fixnum::Encode(0)) {}


FileCharOutStreamImpl::FileCharOutStreamImpl(HANDLE hFile, Val ef) :
        FileByteOutStreamImpl(hFile),
        m_line_column(Fixnum::Encode(0)),
        m_line_number(Fixnum::Encode(0)),
        m_nChars(0),
        m_oFormat(get_format(ef)) {}


// bind_standard_streams
Val bind_standard_streams()
{
    // FIXME 2007-05-19 We should use initialization of default-charset-default
    // to another place.
    VAR(Adefault_charset_defaultA) =
        gethash(Fixnum::Encode(::GetACP()), VAR(Acharset_tableA));

    set_tlv(
        TLV_Adefault_charsetA,
        VAR(Adefault_charset_defaultA) );

    Val terminal_io = nil;
    if (::GetConsoleWindow() != NULL)
    {
        terminal_io = make_console_stream();
        set_tlv(TLV_Aterminal_ioA, terminal_io);
    } // if

    Val stdin = make_file_stream(
            ::GetStdHandle(STD_INPUT_HANDLE),
            Kinput,
            Qcharacter );

    set_tlv(TLV_Astandard_inputA, stdin);

    if (stdin == terminal_io)
    {
        set_tlv(TLV_Astandard_outputA, terminal_io);
        set_tlv(TLV_Aerror_outputA,    terminal_io);
    }
    else
    {
        set_tlv(
            TLV_Astandard_outputA,
            make_file_stream(
                ::GetStdHandle(STD_OUTPUT_HANDLE),
                Koutput,
                Qcharacter ));

        set_tlv(
            TLV_Aerror_outputA,
            make_file_stream(
                ::GetStdHandle(STD_ERROR_HANDLE),
                Koutput,
                Qcharacter ));
    } // if

    set_tlv(TLV_Adebug_ioA,      terminal_io);
    set_tlv(TLV_Aquery_ioA,      terminal_io);
    set_tlv(TLV_Atrace_outputA,  terminal_io);

    return nil;
} // bind_standard_streams


//////////////////////////////////////////////////////////////////////
//
// Make console stream
//
static Val make_console_stream()
{
    ConsoleStreamImpl* pPlatform = new ConsoleStreamImpl();

    Val stream =  MiniThread::Get()->AllocInstance(CLASSD_console_stream);

    ConsoleStream* p = stream->Decode<ConsoleStream>();
        p->m_flags = 
            FromInt<Val_>(Stream::Flag_Both | Stream::Flag_Interactive);
        p->m_blob = Fixnum::Encode(pPlatform);

        p->m_external_format = make_external_format(
            TLV(Adefault_charsetA),
            Kcrlf,
            Fixnum::Encode(0) );

    schedule_finalization(stream, Qclose);

    return stream;
} // make_console_stream


// make_external_format
Val make_external_format(Val charset, Val eol, Val flags)
{
    Val ef = MiniThread::Get()->AllocRecord(
        CLASSD_external_format );

    ExternalFormat* p = ef->Decode<ExternalFormat>();
    p->m_charset = charset;
    p->m_eol     = eol;
    p->m_flags   = flags;

    return ef;
} // make_external_format


// make_file_stream
Val make_file_stream(HANDLE hFile, Val direction, Val element_type)
{
    Val flags;

    if (Kinput == direction)
    {
        flags = FromInt<Val_>(Stream::Flag_Input);
    }
    else if (Koutput == direction)
    {
        flags = FromInt<Val_>(Stream::Flag_Output);
    }
    else
    {
        error(L"Unsupported direction: ~S", direction);
    }

    if (is_console_handle(hFile))
    {
        if (Qcharacter != element_type)
        {
            error(L"Invalid element type for console stream: ~S",
                element_type );
        }
        return TLV(Aterminal_ioA);
    }

    Val stream =  MiniThread::Get()->AllocInstance(CLASSD_file_stream);

    FileStream* p = stream->Decode<FileStream>();
        p->m_flags           = flags;
        p->m_element_type    = element_type;
        p->m_pathname        = nil;
        p->m_plist           = nil;

        p->m_external_format = make_external_format(
            TLV(Adefault_charsetA),
            Kdefault,
            Fixnum::Encode(0) );

    realize_file_stream(stream, hFile);

    schedule_finalization(stream, Qclose);
    return stream;
} // make_file_stream


//////////////////////////////////////////////////////////////////////
//
// Realize file stream
//
Val realize_file_stream(Val stream, HANDLE hFile)
{
    Val element_type    = stream->Decode<FileStream>()->m_element_type;
    Val external_format = stream->Decode<FileStream>()->m_external_format;

    if (Qcharacter == element_type)
    {
        if (stream->Decode<Stream>()->IsInputStream())
        {
            void* pv = new FileCharInStreamImpl(hFile, external_format);
            stream->Decode<FileStream>()->m_blob = Fixnum::Encode(pv);
            return stream;
        }
        else if (stream->Decode<Stream>()->IsOutputStream())
        {
            void* pv = new FileCharOutStreamImpl(hFile, external_format);
            stream->Decode<FileStream>()->m_blob = Fixnum::Encode(pv);
            return stream;
        }
    }
    else if (Qunsigned_byte == element_type ||
             equal(ty_unsigned_byte_8, element_type) )
    {
        if (stream->Decode<Stream>()->IsInputStream())
        {
            void* pv = new FileByteInStreamImpl(hFile);
            stream->Decode<FileStream>()->m_blob = Fixnum::Encode(pv);
            return stream;
        }
        else if (stream->Decode<Stream>()->IsOutputStream())
        {
            void* pv = new FileByteOutStreamImpl(hFile);
            stream->Decode<FileStream>()->m_blob = Fixnum::Encode(pv);
            return stream;
        }
    } // if

    ::CloseHandle(hFile);

    error(L"non-supported file stream element-type: ~S", element_type);
} // realize_file_stream

} // Minilisp
