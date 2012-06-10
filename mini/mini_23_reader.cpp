#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - mini - 23 Reader
// mini/mini_23_reader.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/mini/mini_23_reader.cpp#3 $
//
#include "./mini_lisp.h"

namespace CommonLisp
{

// make_readtable
Val make_readtable()
{
    Val readtable =  MiniThread::Get()->AllocRecord(CLASSD_readtable);

    Readtable* pReadtable = readtable->Decode<Readtable>();
        pReadtable->m_case = Kupcase;
        pReadtable->m_vector = make_vector(128);

    return readtable;
} // make_readtable

} // CommonLisp
