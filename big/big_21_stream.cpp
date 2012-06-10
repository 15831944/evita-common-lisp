#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - big- 23 Reader
// big/big_23_reader.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/big/big_21_stream.cpp#1 $
//
#include "./big_lisp.h"

namespace MiniLisp
{
    Val make_external_format(Val, Val, Val);
    Val realize_instance_file_stream(MiniThread*);
} // MiniLisp

namespace
{
static Val make_file_stream(Val filename)
{
    Val stream = MiniThread::Get()->AllocInstance(CLASSD_file_stream);

    {
        FileStream* p = stream->Decode<FileStream>();
        p->m_pathname        = filename;
        p->m_element_type    = Qcharacter;
        p->m_plist           = nil;

        p->m_external_format = make_external_format(
            TLV(Adefault_charsetA),
            Kdefault,
            Fixnum::Encode(0) );
    }

    return stream;
} // make_file_stream;

} // namespace


namespace CommonLisp
{

// open
Val open(Val filename, Val k1, Val v1)
{
    Val stream = make_file_stream(filename);
    values(stream, Kfilename, filename, k1, v1);
    realize_instance_file_stream(MiniThread::Get());
    return stream;
} // open


// open
Val open(Val filename, Val k1, Val v1, Val k2, Val v2)
{
    Val stream = make_file_stream(filename);
    values(stream, Kfilename, filename, k1, v1, k2, v2);
    realize_instance_file_stream(MiniThread::Get());
    return stream;
} // open

} // CommonLisp
