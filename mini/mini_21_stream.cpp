#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - lisp
// genesis/geneis_lisp.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/mini/mini_21_stream.cpp#8 $
//
#include "./mini_lisp.h"

#include "../platform/generic/mini/pl_mini_21_stream.h"

namespace
{

// StringOutputStreamImpl
class StringOutputStreamImpl : public StringOutputStream
{
    public: void WriteString(const char16* pwch, size_t cwch)
    {
        const char16* pwchEnd = pwch + cwch;
        while (pwch < pwchEnd) WriteChar(*pwch++);
    } // WriteString

    public: void WriteChar(char16 wch)
    {
        if (0x0A == wch)
        {
            m_column = Fixnum::Encode(0);
        }
        else
        {
            m_column = add_xx(m_column, 1);
        }

        String* p = m_string->Decode<String>();

        Val offset;
        Val data = string_data(m_string, &offset);

        if (p->m_fill_pointer == p->m_total_size)
        {
            Val oldlen = p->m_total_size;

            p->m_total_size = add_xx(p->m_total_size, 10);
            Val newdata = allocate_string(p->m_total_size);

            ::memcpy(
                newdata->Decode<SimpleString>()->m_rgwchElement,
                data->Decode<SimpleString>()->m_rgwchElement +
                    Fixnum::Decode_(offset),
                sizeof(char16) * Fixnum::Decode_(oldlen) );

            p->m_displaced_to = newdata;
            p->m_offset = Fixnum::Encode(0);

            data = newdata;
            offset = Fixnum::Encode(0);
        } // if

        Int iIndex = Fixnum::Decode_(add_xx(offset, p->m_fill_pointer));
        data->Decode<SimpleString>()->m_rgwchElement[iIndex] =  wch;
        p->m_fill_pointer = add_xx(p->m_fill_pointer, 1);
    } // WriteChar
}; // StringOutputStream

} // namespace

namespace MiniLisp
{

//////////////////////////////////////////////////////////////////////
//
// Ensure Output Stream
//
Val ensure_input_stream(Val stream)
{
    if (nil == stream) stream =  TLV(Astandard_inputA);
    else if (Qt == stream) stream =  TLV(Aterminal_ioA);
    return stream;
} // ensure_input_stream


//////////////////////////////////////////////////////////////////////
//
// Ensure Output Stream
//
Val ensure_output_stream(Val stream)
{
    if (nil == stream) stream =  TLV(Astandard_outputA);
    else if (Qt == stream) stream =  TLV(Aterminal_ioA);
    return stream;
} // ensure_output_stream

// stream_line_number
Val stream_line_number(Val stream)
{
    check_type(stream, stream);
    return StreamImpl::StaticCast(stream)->LineNumber(stream);
} // stream_line_number


// stream_line_column
Val stream_line_colum(Val stream)
{
    check_type(stream, stream);
    return StreamImpl::StaticCast(stream)->LineColumn(stream);
} // stream_line_column

} // MiniLisp

namespace CommonLisp
{

//////////////////////////////////////////////////////////////////////
//
// Close
//  NYI: close: keyword argument abort.
Val close(Val stream)
{
    if (stream->Decode<Stream>()->IsOutputStream())
        { StreamImpl::StaticCast(stream)->ForceOutput(stream); }
    StreamImpl::StaticCast(stream)->Close(stream, false);
    return nil;
} // close


Val force_output(Val stream)
{
    stream = ensure_output_stream(stream);
    StreamImpl::StaticCast(stream)->ForceOutput(stream);
    return nil;
} // force_output


// get_output_stream_string
Val get_output_stream_string(Val stream)
{
    ASSERT(stream->Is<StringOutputStream>());

    Val buffer = stream->Decode<StringOutputStream>()->m_string;

    Val offset;
    Val data = string_data(buffer, &offset);

    Val s = make_string(
        data->Decode<SimpleString>()->GetElements() +
            Fixnum::Decode_(offset),
        Fixnum::Decode_(buffer->Decode<String>()->m_fill_pointer) );

    return s;
} // get_output_stream_string


// listen
bool listen(Val stream)
{
    stream = ensure_input_stream(stream);
    return StreamImpl::StaticCast(stream)->Listen(stream);
} // force_output


// make_string_output_stream
Val make_string_output_stream()
{
    Val stream =  MiniThread::Get()->AllocInstance(
        CLASSD_string_output_stream );

    Val data = allocate_string(Fixnum::Encode(24));

    StringOutputStream* p = stream->Decode<StringOutputStream>();
        p->m_flags  = FromInt<Val_>(Stream::Flag_Output);
        p->m_string = make_vector(CLASSD_string_object, data);
        p->m_column = Fixnum::Encode(0);

    return stream;
} // make_string_output_stream


// read_char
Val read_char(Val stream, Val eof_error_p, Val eof_value)
{
    stream = ensure_input_stream(stream);
    Val ch = StreamImpl::StaticCast(stream)->ReadChar(stream);
    if (characterp(ch)) return ch;
    if (nil != eof_error_p) error(L"end of file on ~S", stream);
    return eof_value;
} // read_char

// read_char_no_hang
Val read_char_no_hang(Val stream, Val eof_error_p, Val eof_value)
{
    stream = ensure_input_stream(stream);
    if (! listen(stream)) return nil;
    return read_char(stream, eof_error_p, eof_value);
} // read_char_no_hang


// streamp
bool platform_stream_p(Val obj)
{
    unless (obj->Is<Instance>()) return false;

    Instance* p = obj->Decode<Instance>();

    return CLASSD_file_stream == p->m_classd ||
            #if _WIN32
                CLASSD_console_stream == p->m_classd ||
                CLASSD_debug_output_stream == p->m_classd;
            #endif // _WIN32;
} // streamp


bool streamp(Val obj)
{
    unless (obj->Is<Instance>()) return false;

    Instance* p = obj->Decode<Instance>();

    return platform_stream_p(obj) ||
           CLASSD_string_output_stream == p->m_classd;
} // streamp

// unread_char
Val unread_char(Val ch, Val stream)
{
    stream = ensure_input_stream(stream);
    StreamImpl::StaticCast(stream)->UnreadChar(stream, Character::ToCode(ch));
    return ch;
} // unread_char


// write_string
void write_string(const char16* pwchString, size_t cwchString, Val stream)
{
    stream = ensure_output_stream(stream);

    Val classd = stream->Decode<Instance>()->m_classd;

    if (classd == CLASSD_string_output_stream)
    {
        stream->Decode<StringOutputStreamImpl>()->
            WriteString(pwchString, cwchString);
    }
    else if (platform_stream_p(stream))
    {
        StreamImpl::StaticCast(stream)->
            WriteString(stream, pwchString, cwchString);
    }
    else
    {
        error(make_type_error(stream, Qstream));
    }
} // write_string


// write_string
Val write_string(Val str, Val stream)
{
    SimpleString* pString = str->Decode<SimpleString>();

    write_string(
        pString->GetElements(),
        Fixnum::Decode_(pString->m_length),
        stream );

    return str;
} // write_string


// input_stream_p
bool input_stream_p(Val stream)
{
    check_type(stream, stream);
    return stream->Decode<Stream>()->IsInputStream();
} // input_stream_p


// output_stream_p
bool output_stream_p(Val stream)
{
    check_type(stream, stream);
    return stream->Decode<Stream>()->IsOutputStream();
} // output_stream_p

} // CommonLisp
