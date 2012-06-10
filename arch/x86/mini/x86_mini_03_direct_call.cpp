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
// @(#)$Id: //proj/evcl3/mainline/arch/x86/mini/x86_mini_03_direct_call.cpp#4 $
//
#include "../x86_asm.h"
#include "../kernel/x86_ke_thread.h"

namespace MiniLisp
{

using namespace X86;

//////////////////////////////////////////////////////////////////////
//
// make_not_function_function
//  For direct call
//
//      (:mov %arg_0 name)
//      (:jmp (:tcb SVC_not_error)
//
Val make_not_function_function(Val name)
{
    class Asm : public X86Assembler
    {
        public: Val Run(Val name)
        {
            Defun oDefun(this, name, 0, -1);

            mov($r2, name);
            jmp(ea($tcb, SVC_not_function));

            if (special_operator_p(name))
            {
                oDefun.m_name = list(Kspecial_operator, name);
            }
            else
            {
                oDefun.m_name = list(Kmacro, name);
            }

            oDefun.m_classd = CLASSD_not_function_function;
            return makeFunObj(&oDefun);
        } // Run
    }; // Asm

    if (setf_cell_p(name))
    {
        name = list(Qsetf, value_cell_name(name));
    }

    Asm oAsm;
    return oAsm.Run(name);
} // make_not_function_function


//////////////////////////////////////////////////////////////////////
//
// make_undefined_function_function
//  For direct call
//
//  0000 BB imm32       (:mov :arg0  foo)
//  0005 FF 65 00       (:jmp (:tcb SVC_undefined_function))
//
Val make_undefined_function_function(Val name)
{
    if (setf_cell_p(name))
    {
        name = list(Qsetf, value_cell_name(name));
    }

    class Asm : public X86Assembler
    {
        public: Val Run(Val name)
        {
            Defun oDefun(this, name, 0, -1);

            mov($r0, name);
            jmp(ea($tcb, SVC_undefined_function));

            oDefun.m_classd = CLASSD_undefined_function_function;
            return makeFunObj(&oDefun);
        } // Run
    }; // Asm

    Asm oAsm;
    return oAsm.Run(name);
} // make_undefined_function_function


//////////////////////////////////////////////////////////////////////
//
// update_callers
//
Val update_callers(Val name, Val new_callee)
{
    ASSERT(symbolp(name) || setf_cell_p(name));

    with_exclusive_latch(VAR(Acaller_table_latchA));

    Val entry = intern_callee(name);
    Val caller_set = cdr(entry);

    Val old_callee = car(entry);

    Val end = svref(caller_set, 0);

    Val count = Fixnum::Encode(0);

    for (
        Val index = Fixnum::Encode(1);
        cmp_xx(index, end) < 0;
        index = add_xx(index, 1) )
    {
        Val caller = svref(caller_set, index);
        FunObj* pCaller = caller->Decode<FunObj>();
        foreach (FunObj::EnumAnnon, oEnum, pCaller)
        {
            FunObj::Annon oAnnon = oEnum.Get();
            if (FunObj::Annon::Type_NamedCallee == oAnnon.m_eType)
            {
                Val callee = pCaller->FetchCallee(oAnnon.m_ofs);
                if (callee == old_callee)
                {
                    pCaller->PatchCallee(oAnnon.m_ofs, new_callee);
                    count = add_xx(count, 1);
                } // if
            } // if
        } // for each annon
    } // for index

    setf_car(new_callee, entry);

    return count;
} // update_callers

} // MiniLisp
