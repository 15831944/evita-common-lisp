#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - lisp
// genesis/geneis_lisp.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/mini/mini_49_cell.cpp#4 $
//
#include "./mini_lisp.h"

namespace MiniLisp
{


// find_setf_cell
Val find_setf_cell(Val name)
{
    check_type(name, symbol);
    return gethash(name, VAR(Asetf_tableA));
} // find_setf_cell

// find_value_cell
Val find_value_cell(Val name)
{
    check_type(name, symbol);
    return gethash(name, VAR(Avalue_tableA));
} // find_value_cell


//////////////////////////////////////////////////////////////////////
//
// Intern Setf Cell
//
Val intern_setf_cell(Val name)
{
    ASSERT(symbolp(name));

    Val setf_table = VAR(Asetf_tableA);

    Val cell = gethash(name, setf_table);

    if (nil != cell) return cell;

    cell = make_setf_cell(name, nil);
    return setf_gethash(cell, name, setf_table);
} // intern_setf_cell


// intern_value_cell
Val intern_value_cell(Val name)
{
    check_type(name, symbol);

    Val table = value_cell_value(VAR_Avalue_tableA);
    Val cell = gethash(name, table);

    if (nil != cell) return cell;

    cell = make_value_cell(name, QQunbound_marker);
    return setf_gethash(cell, name, table);
} // intern_value_cell


// make_setf_cell
Val make_setf_cell(Val name, Val val)
{
    ASSERT(symbolp(name));
    ASSERT(functionp(val) || nil == val);

    Val cell = MiniThread::Get()->AllocRecord(CLASSD_setf_cell);
    SetfCell* pCell = cell->Decode<SetfCell>();
        pCell->m_name     = name;
        pCell->m_function = val;
    return cell;
} // make_setf_cell

// make_value_cell
Val make_value_cell(Val name, Val val)
{
    ASSERT(symbolp(name));

    Val cell = MiniThread::Get()->AllocRecord(CLASSD_value_cell);
    ValueCell* pCell = cell->Decode<ValueCell>();
        pCell->m_name  = name;
        pCell->m_type  = Kvariable;
        pCell->m_value = val;
    return cell;
} // make_value_cell

} // MiniLisp
