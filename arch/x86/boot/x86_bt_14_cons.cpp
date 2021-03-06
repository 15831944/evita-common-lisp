#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - boot - 14 Conses
// arch/x86/boot/x86_bt_14_cons.inc
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86/boot/x86_bt_14_cons.cpp#4 $
//
// Description:
//  This file contains functions for "14 Conses".

#include "./x86_bt_builder.h"

namespace Boot
{

class MakeCxr : public X86Assembler
{
    // Run
    public: void Run(LPCWSTR pwszName, size_t ofs)
    {
        Val name = parse_symbol(pwszName);

        // cxr
        {
            Defun oDefun(this, name, 1, 1);
            emit_prologue(&oDefun);
                Label type_error_cons;

                lea($r1, ea($r0, -Cons::Tag));
                and($r1, 3);     // CF=0
                jne(type_error_cons);
                mov($r0, ea($r0, static_cast<int32>(ofs - Cons::Tag)));
                ret();
              label(type_error_cons);
                mov($r1, Qlist);
                jmp(ea($tcb, SVC_type_error));
            emit_epilogue(&oDefun);
            installFunObj(&oDefun);
        }

        // (setf cxr)
        {
            Defun oDefun(this, list(Qsetf, name), 2, 2);
            emit_prologue(&oDefun);
                Label type_error_cons;

                lea($r2, ea($r1, -Cons::Tag));
                and($r2, 7);     // CF=0
                jne(type_error_cons);
                mov(ea($r1, ofs - Cons::Tag), $r0);
                ret();
              label(type_error_cons);
                mov($r0, $r1);
                mov($r1, Qcons);
                jmp(ea($tcb, SVC_type_error));
            emit_epilogue(&oDefun);
            installFunObj(&oDefun);
        }
    } // Run
}; // MakeCxr


void X86Builder::build_14_Cons()
{

// (atom x) = (not (consp x))
defun("ATOM", 1, 1)
    lea($r1, ea($r0, -Cons::Tag));
    and($r1, 7); // also set CF=0
    mov($r0, nil);
    mov($r2, Qt);
    cmovne($r0, $r2);
    ret();
end_defun()

// (endp x) = (or (null x) (if (consp x) nil (error 'type-error)))
defun("ENDP", 1, 1)
    Label ret_true;
    Label type_error;

    mov($r2, nil);
    cmp($r0, $r2);
    je(ret_true);           // CF=0 if $r0 is nil.

    lea($r1, ea($r0, -Cons::Tag));
    and($r1, 7);     // CF=0
    jne(type_error);
    mov($r0, $r2);
    ret();

  label(ret_true);
    mov($r0, t);
    ret();

  label(type_error);
    mov($r1, Qcons);
    jmp(ea($tcb, SVC_type_error));
end_defun()

// See also "NOT".
defun("NULL", 1, 1)
    mov($r1, nil);
    cmp($r0, $r1);
    mov($r0, Qt);
    cmovne($r0, $r1);
    xor($r1, $r1);      // xor may be faster than clc.
    ret();
end_defun()

// For access non-primary values in variable "/", "//", and "///".
{
    MakeCxr oMakeCxr;
        oMakeCxr.Run(L"CAR", offsetof(Cons, m_car));
        oMakeCxr.Run(L"CDR", offsetof(Cons, m_cdr));
}

// Generate
//  CAAR, CADR, CDAR, CDDR
//  CAAAR, ..., CDDDR
//  CAAAAR, ..., CDDDDR
{
    WCHAR wszFName[4 + 2 + 1 + 1];
        wszFName[0] = 'C';

    WCHAR wsz1[3 + 2 + 1];
        wsz1[0] = 'C';

    WCHAR wsz2[2 + 1 + 1];
        wsz2[0] = 'C';
        wsz2[1] = 'x';
        wsz2[2] = 'R';
        wsz2[3] = 0;
    for (int n = 2; n <= 4; n++)
    {
        wszFName[n+2] = 'R';
        wszFName[n+3] = 0;

        int k = 1 << n;
        for (int m = 0; m < k; m++)
        {
            int mm = m;
            for (int j = 0; j < n; j++)
            {
                wsz1[j] = mm & 1 ? 'D' : 'A';
                wszFName[j+1] =wsz1[j];
                mm >>= 1;
            } // for j

            wszFName[n+1] = 'R';
            wszFName[n+2] = 0;

            wsz2[1] = wsz1[0];

            wsz1[0] = 'C';
            wsz1[n] = 'R';
            wsz1[n+1] = 0;

            Defun oDefun(this, parse_symbol(wszFName), 1, 1);
            emit_prologue(&oDefun);
                call(parse_symbol(wsz1));
                mov($rn, Fixnum::Encode(1));
                call(parse_symbol(wsz2));
                ret();
            emit_epilogue(&oDefun);
            installFunObj(&oDefun);
        } // for i
    } // for i
} // caar, cadr

} // X86Builder::build_14_Cons
} // Boot
