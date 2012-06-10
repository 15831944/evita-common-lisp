#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - platform - generic - stream
// platform/generic/big/pl_big_21_stream.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/platform/generic/big/pl_big_21_stream.cpp#4 $
//
#include "../../../mini/mini_lisp.h"

#include "../mini/pl_mini_21_stream.h"

namespace MiniLisp
{

#define define_platform_method_1(mp_name, mp_Name) \
    Val platform_##mp_name(Val stream) \
        { return StreamImpl::StaticCast(stream)->mp_Name(stream); }

#define define_platform_method_1_nil(mp_name, mp_Name) \
    Val platform_##mp_name(Val stream) \
        { StreamImpl::StaticCast(stream)->mp_Name(stream); return nil; }

define_platform_method_1_nil(stream_clear_input,    ClearInput);
define_platform_method_1_nil(stream_clear_output,   ClearOutput);
define_platform_method_1_nil(stream_finish_output,  FinishOutput);
define_platform_method_1_nil(stream_force_output,   ForceOutput);

define_platform_method_1(stream_fresh_line,         FreshLine);
define_platform_method_1(stream_line_number,        LineNumber);
define_platform_method_1(stream_line_column,        LineColumn);
define_platform_method_1(stream_output_width,       OutputWidth);
define_platform_method_1(stream_peek_char,          PeekChar);
define_platform_method_1(stream_read_byte,          ReadByte);
define_platform_method_1(stream_read_char,          ReadChar);
define_platform_method_1(stream_read_char_no_hang,  ReadCharNoHang);
define_platform_method_1(stream_start_line_p,       StartLineP);


// platform_stream_listen(
Val platform_stream_listen(Val stream)
    { return StreamImpl::StaticCast(stream)->Listen(stream) ? t : nil; }


// platform_stream_close
Val platform_stream_close(MiniThread* p)
{
    Val stream = p->mv_value[0];
    bool fAbort = false;
    if (Fixnum::Encode(3) == p->m_n) fAbort = p->mv_value[2] != nil;
    return StreamImpl::StaticCast(stream)->Close(stream, fAbort);
} // platform_stream_close


// platform_stream_read_bytes
Val platform_stream_read_bytes(MiniThread* p)
{
    Val stream = p->mv_value[0];
    Val vector = p->mv_value[1];
    Val start  = p->mv_value[2];
    Val end    = p->mv_value[3];

    switch (Fixnum::Decode_(p->m_n))
    {
        case 2: start = 0;
        case 3: end = nil;
        case 4: break;
        default: CAN_NOT_HAPPEN();
    } // switch n

    if (CLASSD_unsigned_byte_8_vector != vector->GetClassD())
    {
        error(make_type_error(
            vector,
            CLASSD_unsigned_byte_8_vector->Decode<ClassD>()->m_typespec ) );
    }

    if (nil == end) end = vector->Decode<DataVector>()->m_length;

    Val count = StreamImpl::StaticCast(stream)->ReadBytes(
        stream,
        reinterpret_cast<uint8*>(vector->Decode<DataVector>() + 1) +
            Fixnum::Decode_(start),
        Fixnum::Decode_(end) - Fixnum::Decode_(start) );

    return count;
} // platform_stream_read_bytes


// platform_stream_unread_char
Val platform_stream_unread_char(Val stream, Val ch)
{
    check_type(ch, character);

    StreamImpl::StaticCast(stream)->
        UnreadChar(stream, Character::ToCode(ch));

    return ch;
} // platform_stream_unread_char


// platform_stream_write_byte
Val platform_stream_write_byte(Val stream, Val datum)
{
    unless (fixnump(datum) && static_cast<UInt>(Fixnum::Decode_(datum)) <= 255)
    {
        error(make_type_error(datum, ty_unsigned_byte_8));
    }

    StreamImpl::StaticCast(stream)->
        WriteByte(stream, static_cast<uint8>(Fixnum::Decode_(datum)));

    return datum;
} // platform_stream_write_byte


// platform_stream_write_char
Val platform_stream_write_char(Val stream, Val ch)
{
    StreamImpl::StaticCast(stream)->
        WriteChar(stream, Character::ToCode(ch));
    return ch;
} // platform_stream_write_char


// platform_stream_write_bytes
Val platform_stream_write_bytes(MiniThread* p)
{
    Val stream = p->mv_value[0];
    Val vector = p->mv_value[1];
    Val start  = p->mv_value[2];
    Val end    = p->mv_value[3];

    switch (Fixnum::Decode_(p->m_n))
    {
        case 2: start = 0;
        case 3: end = nil;
        case 4: break;
        default: CAN_NOT_HAPPEN();
    } // switch n

    if (CLASSD_unsigned_byte_8_vector != vector->GetClassD())
    {
        error(make_type_error(
            vector,
            CLASSD_unsigned_byte_8_vector->Decode<ClassD>()->m_typespec ) );
    }

    if (nil == end) end = vector->Decode<DataVector>()->m_length;

    StreamImpl::StaticCast(stream)->WriteBytes(
        stream,
        reinterpret_cast<uint8*>(vector->Decode<DataVector>() + 1) +
            Fixnum::Decode_(start),
        Fixnum::Decode_(end) - Fixnum::Decode_(start) );

    return vector;
} // platform_stream_write_bytes


// platform_stream_write_string
Val platform_stream_write_string(MiniThread* p)
{
    Val stream = p->mv_value[0];
    Val str    = p->mv_value[1];
    Val start  = p->mv_value[2];
    Val end    = p->mv_value[3];

    switch (Fixnum::Decode_(p->m_n))
    {
        case 2: start = Fixnum::Encode(0);
        case 3: end = nil;
        case 4: break;
        default: CAN_NOT_HAPPEN();
    } // switch n

    Val offset;
    Val data = string_data(str, &offset);

    StreamImpl::StaticCast(stream)->WriteString(
        stream,
        data->Decode<SimpleString>()->m_rgwchElement +
            Fixnum::Decode_(add_xx(offset, start)),
        Fixnum::Decode_(add_xx(end, start)) );

    return str;
} // platform_stream_write_string

} // MiniLisp
