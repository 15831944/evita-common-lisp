#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - big - 49 Internals - Cell
// big/big_49_cell.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/big/big_49_cell.cpp#4 $
//
#include "./big_lisp.h"

namespace MiniLisp
{

bool static_object_p(Val);

// C_static_object_p
//  For debugging image
Val C_static_object_p(Val x)
{
    Area* pArea = Kernel::Memory::MapToArea(x);
    if (NULL == pArea) return t;
    return Area::Age_Static == pArea->GetAge() ? t : nil;
} // C_static_object_p


// Note: stab: we'll replace this with lisp code.
Val Dmake_closed_cell(Val init)
{
    Val cell = MiniThread::Get()->AllocRecord(CLASSD_closed_cell);
    cell->Decode<ClosedCell>()->m_value = init;
    return cell;
} // Dmake_closed_cell

} // MiniLisp
