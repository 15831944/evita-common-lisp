#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - Compiler Memory Manager
// compiler/cm/cm_mm.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cm/cm_mm.cpp#3 $
//
#include "./cm_mm.h"

namespace Compiler
{

// Mm constructor
Mm::Mm()
{
    m_hHeap = ::HeapCreate(HEAP_NO_SERIALIZE, 0, 0);
} // Mm::Mm


Mm::~Mm()
{
    if (NULL != m_hHeap) ::HeapDestroy(m_hHeap);
} // Mm::Mm




//////////////////////////////////////////////////////////////////////
//
// Alloc
void*
Mm::Alloc(size_t cb)
{
    return ::HeapAlloc(m_hHeap, HEAP_GENERATE_EXCEPTIONS, cb);
} // Mm::Alloc


} // Compiler
