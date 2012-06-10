#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - boot - x86 builder
// arch/x86/boot/x86_bt_builder.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86/boot/x86_bt_builder.cpp#4 $
//
// Description:
//  This file contains builder for x86 specific objects, such as native
//  code function.
//
#include "./x86_bt_builder.h"

#include "../kernel/x86_ke_thread.h"

namespace Boot
{

// BuildForTarget
void BuildForTarget()
{
    X86Builder oBuilder;
    oBuilder.Run();

    Val PACKAGE_x86 = make_package(
        list(Q(":X86"), Q(":X86X64")),
        Fixnum::Encode(51),
        Fixnum::Encode(2003) );

    use_package(PACKAGE_cl,  PACKAGE_x86);
    use_package(PACKAGE_ext, PACKAGE_x86);

    PACKAGE_target = PACKAGE_x86;

    // For compiler
    Q(":EAX"); Q(":EBX"); Q(":ECX"); Q(":EDX");
    Q(":ESI"); Q(":EDI"); Q(":EBP"); Q(":ESP");

    {
        Val cell = find_value_cell(Q("SI:*DEFAULT-FEATURES*"));

        cell->Decode<ValueCell>()->m_value =
            listA(
                Q(":X86"),
                Q(":WIN32"),
                cell->Decode<ValueCell>()->m_value);
    }
} // BuildForTarget

} // Boot
