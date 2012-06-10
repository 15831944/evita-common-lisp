#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - boot - 10 Symbols
// arch/x64/boot/x64_bt_10_symbol.inc
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x64/boot/x64_bt_10_symbol.cpp#4 $
//
// Description:
//  This file contains functions for "12 Symbols".

#include "./x64_bt_builder.h"

namespace Boot
{

class SymbolBuilder : public X64Assembler
{
    // BuildReader
    public: void BuildReader(LPCWSTR pwszName, size_t ofs)
    {
        // symbol-name and symbol-package
        defun_(parse_symbol(pwszName), 1, 1, Fixed, 8)
            Label got_symbol;
            Label not_symbol;

            sub($sp, 8);

            cmp($r0, $rnil);
            je(got_symbol);

            lea($r1, ea($r0, -Symbol::Tag));
            and($r1, Symbol::TagMask);
            jne(not_symbol);

            mov($rn, CLASSD_symbol);
            cmp(ea($r0, OffsetOf(Symbol, m_classd)), $rn);
            jne(not_symbol);

          label(got_symbol);
            and($r0, ~Symbol::TagMask);
            mov($r0, ea($r0, ofs));

            add($sp, 8);    // CF=0
            ret();

          label(not_symbol);
            mov($r1, Qsymbol);
            call(ea($rtcb, SVC_type_error));
        end_defun()
    } // BuildReader
}; // SymbolBuilder;


void X64Builder::build_10_Symbol()
{
    // symbolp
    defun("SYMBOLP", 1, 1)
        Label got_nil;
        Label not_symbol;

        cmp($r0, $rnil);
        je(got_nil);        // CF=0

        lea($r1, ea($r0, -Symbol::Tag));
        and($r1, Symbol::TagMask);
        jne(not_symbol);

        mov($rn, CLASSD_symbol);
        cmp(ea($r0, OffsetOf(Symbol, m_classd)), $rn);
        cmovne($r0, $rnil);
        xor($r2, $r2);      // CF=0
        ret();

      label(not_symbol);
        xor($r0, $r0);      // CF=0
        mov($r0, $rnil);
        ret();

      label(got_nil);
        mov($r0, $tcb);
        ret();
    end_defun()

    {
        SymbolBuilder oBuilder;

        oBuilder.BuildReader(L"SYMBOL-NAME", offsetof(Symbol, m_name));
        // m00-fns.lisp calls symbol-package.
        oBuilder.BuildReader(L"SYMBOL-PACKAGE", offsetof(Symbol, m_package));
    }

    // symbol-function
    defun("SYMBOL-FUNCTION", 1, 1)
        Label got_symbol;
        Label not_symbol;
        Label undefined_function;

        sub($sp, 8);

        cmp($r0, $rnil);
        je(got_symbol);

        // Is arg_0 symbol?
        lea($r1, ea($r0, -Symbol::Tag));
        and($r1, Symbol::TagMask);
        jne(not_symbol);

        mov($rn, CLASSD_symbol);
        cmp(ea($r0, OffsetOf(Symbol, m_classd)), $rn);
        jne(not_symbol);

      label(got_symbol);
        // Save $r0
        mov($r2, $r0);

        // $r0 <- symbol.function
        and($r0, ~Symbol::TagMask);
        mov($r0, ea($r0, offsetof(Symbol, m_function)));

        // Is $r0 function?
        lea($r1, ea($r0, -Funcallable::Tag));
        and($r1, Funcallable::TagMask);
        jne(undefined_function);

        add($sp, 8);    // CF=0
        ret();

      label(not_symbol);
        mov($r1, Qsymbol);
        call(ea($rtcb, SVC_type_error));

      label(undefined_function);
        mov($r0, $r2);
        call(ea($rtcb, SVC_undefined_function));
    end_defun()

} // X64Builder::build_10_Symbol

} // Boot

