#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - Direct Call Manager
// kernel/ke_direct_call.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/mini/mini_03_direct_call.cpp#4 $
//
#include "./mini_lisp.h"

namespace MiniLisp
{

namespace
{

enum { Default_Caller_Set_Length = 10 };

} // namespace

//    Current = weak object             cons
//      +-----------+                   +--------+
//  [0] |  free o---+---+ <------+      | callee |
//      +-----------+   |        |      +--------+
//  [1] | caller_1  |   |        +------+---o    |
//      +-----------+   |               +--------+
//  [2] | caller_2  |   |
//      +-----------+   |
//        ...           |
//      +-----------+   |
//      |           |<--+
//      +-----------+ 

//////////////////////////////////////////////////////////////////////
//
// intern_callee
//
Val intern_callee(Val name)
{
    #if ! defined(EVCL_BOOT)
    {
        ASSERT(Thread::Get()->Encode() == latch_state(VAR(Acaller_table_latchA)));
    }
    #endif // ! defined(EVCL_BOOT)

    Val htb = VAR(Acaller_tableA);

    Val entry = gethash(name, htb);

    if (nil == entry)
    {
        Val caller_set = make_caller_set(Default_Caller_Set_Length);

        Val callee;
        {
            if (symbolp(name))
            {
                callee = name->Decode<Symbol>()->m_function;
            }
            else if (setf_cell_p(name))
            {
                callee = name->Decode<SetfCell>()->m_function;
            }
            else
            {
                CAN_NOT_HAPPEN();
            }

            if (nil == callee)
            {
                callee = make_undefined_function_function(name);
            }
        } // callee

        entry = cons(callee, caller_set);
        setf_gethash(entry, name, htb);
    } // if

    return entry;
} // intern_callee


//////////////////////////////////////////////////////////////////////
//
// register_caller
//
//  Registers caller to caller set of name and retruns callee.
//
//  BUGBUG: We MUST use latch *caller-table-latch* for register_caller
//  method.
//
//  Note: We don't install undefined-function-function into
//  symbol-function slot or value slot of setf-cell.
//
//  When function name doesn't have definition coresponding slot
//  has NIL instead of function object.
//
Val register_caller(Val name_or_fun, Val caller)
{
    Val name;
    {
        if (! functionp(name_or_fun))
        {
            name = name_or_fun;
        }
        else
        {
            name = name_or_fun->Decode<NativeCodeFunction>()->m_name;
            if (name->Is<Storage>())
            {
                name = name_or_fun->Decode<StandardGenericFunction>()->m_name;
            }
        }
        
        if (consp(name)) name = intern_setf_cell(second(name));
    } // name

    ASSERT(symbolp(name) || setf_cell_p(name));
    ASSERT(functionp(caller));

    with_exclusive_latch(VAR(Acaller_table_latchA));

    Val entry = intern_callee(name);

    Val caller_set = cdr(entry);

    {
        EnumVector oEnum(caller_set);
            oEnum.Next();

        while (! oEnum.AtEnd())
        {
            Val present = oEnum.Get();
            if (present == caller)
            {
                return car(entry);
            }

            if (! functionp(present))
            {
                break;
            }

            oEnum.Next();
        } // for each elt
    }

    Val index = svref(caller_set, Fixnum::Encode(0));

    if (cmp_xx(index, length(caller_set)) >= 0)
    {
        Val old_caller_set = caller_set;

        // BUGBUG: Must be weak vector
        caller_set = make_caller_set(truncate_xx(mul_xx(index, 120), 100));

        ::CopyMemory(
            caller_set->Decode<SimpleVector>()->mv_element,
            old_caller_set->Decode<SimpleVector>()->mv_element,
            Fixnum::Decode_(index) * sizeof(Val) );

        setf_cdr(caller_set, entry);
    } // if

    setf_svref(add_xx(index, 1), caller_set, Fixnum::Encode(0));
    setf_svref(caller, caller_set, index);

    return car(entry);
} // register_caller

} // MiniLisp

namespace CommonLisp
{

// (setf fdefinition)
Val setf_fdefinition(Val fun, Val fname)
{
    check_type(fun, function);
    if (symbolp(fname))
    {
        return setf_symbol_function(fun, fname);
    }
    else if (function_name_p(fname))
    {
        Val cell = intern_setf_cell(second(fname));
        update_callers(cell, fun);
        return cell->Decode<SetfCell>()->m_function = fun;
    }
    else
    {
        error(make_type_error(fname, Qfunction_name));
    }
} // setf_fdefinition


// (setf symbol-function)
Val setf_symbol_function(Val fun, Val sym)
{
    check_type(fun, function);
    check_type(sym, symbol);
    if (nil == sym) error(L"Can't use ~S as function name.", sym);
    update_callers(sym, fun);
    return sym->Decode<Symbol>()->m_function = fun;
} // setf_symbol_function

} // CommonLisp
