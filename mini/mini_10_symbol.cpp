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
// @(#)$Id: //proj/evcl3/mainline/mini/mini_10_symbol.cpp#5 $
//

#include "./mini_lisp.h"

namespace CommonLisp
{
// boundp
bool boundp(Val name)
{
    Val val;
    {
        Val cell = find_value_cell(name);
        if (value_cell_p(cell))
        {
            val = value_cell_value(cell);
        }
        else if (tlv_record_p(cell))
        {
            val = get_tlv(tlv_record_index(cell));
        }
        else
        {
            return false;
        }
    } // val

    return QQunbound_marker != val;
} // boundp


// symbol_function
Val symbol_function(Val sym)
{
    check_type(sym, symbol);

    Val fun = sym->Decode<Symbol>()->m_function;

    if (! functionp(fun))
    {
        error(make_undefined_function(sym));
    }

    return fun;
} // symbol_function

// symbol_value
Val symbol_value(Val name)
{
    check_type(name, symbol);

    Val val;
    {
        with_shared_latch(VAR(Avalue_table_latchA));

        Val cell = find_value_cell(name);
        if (value_cell_p(cell))
        {
            val = value_cell_value(cell);
        }
        else if (tlv_record_p(cell))
        {
            val = get_tlv(tlv_record_index(cell));
        }
        else if (keywordp(name))
        {
            val = name;
        }
        else
        {
            val = QQunbound_marker;
        }
    } // val

    if (QQunbound_marker == val)
    {
        error(Qunbound_variable, Kname, name);
    }

    return val;
} // symbol_value


// (setf symbol_value)
Val setf_symbol_value(Val val, Val name)
{
    with_exclusive_latch(VAR(Avalue_table_latchA));

    {
        Val cell = find_value_cell(name);
        if (value_cell_p(cell))
        {
            if (value_cell_type(cell) == Kconstant)
            {
                error(Qalter_constant_symbol, Kname, name);
            }

            return setf_value_cell_value(val, cell);
        }
        else if (tlv_record_p(cell))
        {
            return set_tlv(tlv_record_index(cell), val);
        }
    }

    {
        Val cell = make_value_cell(name, val);
        setf_gethash(cell, name, VAR(Avalue_tableA));
        return val;
    }
} // setf_symbol_value


//////////////////////////////////////////////////////////////////////
//
// Make uninterned symbol
//
Val make_symbol(Val name)
{
    check_type(name, simple_string);

    Val symbol = MiniThread::Get()->AllocRecord(CLASSD_symbol);
    Symbol* pSymbol = symbol->Decode<Symbol>();
        pSymbol->m_hash_code = hash_string(name);
        pSymbol->m_name = name;
        pSymbol->m_package = nil;
        pSymbol->m_function = nil;
        pSymbol->m_plist = nil;

    return symbol;
} // make_symbol

} // CommonLisp
