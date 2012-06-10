#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - boot - x64 builder
// arch/x64/boot/x64_bt_builder.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x64/boot/x64_bt_builder.cpp#4 $
//
// Description:
//  This file contains builder for x64 specific objects, such as native
//  code function.
//
#include "./x64_bt_builder.h"

namespace Boot
{

Val defconstant(const char16*, Val);

// BuildForTarget
void BuildForTarget()
{
    Val PACKAGE_x64 = make_package(
        list(Q(":X64"), Q(":X86X64")),
        Fixnum::Encode(51),
        Fixnum::Encode(2003) );

    use_package(PACKAGE_cl,  PACKAGE_x64);
    use_package(PACKAGE_ext, PACKAGE_x64);

    X64Builder oBuilder;
    oBuilder.Run();

    PACKAGE_target = PACKAGE_x64;

    {
        Val cell = find_value_cell(Q("SI:*DEFAULT-FEATURES*"));

        cell->Decode<ValueCell>()->m_value =
            listA(
                Q(":X64"),
                Q(":WIN32"),
                Q(":WIN64"),
                cell->Decode<ValueCell>()->m_value);
    }

    // Intern register names for debugging compiler

    #define defreg(mp_name) Q(":" L## #mp_name)

    #define defreg1(mp_A) \
        { defreg(mp_A##L); defreg(mp_A##X); defreg(E##mp_A##X); \
          defreg(mp_A##H); defreg(R##mp_A##X); }

    #define defreg16(mp_BP) \
        { defreg(mp_BP##L); defreg(mp_BP); defreg(E##mp_BP); \
          defreg(R##mp_BP); }

    #define defreg64(mp_R8) \
        { defreg(mp_R8##L); defreg(mp_R8##W); defreg(##mp_R8##D); \
          defreg(mp_R8); }

    defreg1(A); defreg1(B); defreg1(C); defreg1(D);

    defreg16(SI); defreg16(DI); defreg16(BP); defreg16(SP);

    defreg64(R8);  defreg64(R9);  defreg64(R10);  defreg64(R11);
    defreg64(R12); defreg64(R13); defreg64(R14);  defreg64(R15);
} // BuildForTarget

} // Boot
