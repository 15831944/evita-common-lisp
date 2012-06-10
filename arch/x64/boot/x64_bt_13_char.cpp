#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - boot - 13 Characters
// arch/x64/boot/x64_bt_13_char.inc
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x64/boot/x64_bt_13_char.cpp#8 $
//
// Description:
//  This file contains string functions implemented in assembler:
//      si:char=/2
//      si:char/=/2
//      si:char</2
//      si:char<=/2
//      si:char>/2
//      si:char>=/2
//      cl:char-code
//      cl:char-int
//      cl:code-char
#include "./x64_bt_builder.h"

namespace Boot
{

void X64Builder::build_13_Character()
{

class Asm : public X64Assembler
{
    public: void GenCmp(LPCWSTR pwszName, Tttn tttn)
    {
        defun_(parse_symbol(pwszName), 2, 2, Fixed, 48)
            Label type_error_0;
            Label type_error_1;

            sub(rsp, 40);

            lea($rn, ea($rnil, QQchar_min->ToInt()-nil->ToInt()));

            mov($r2, $r0);
            sub($r2, $rn);
            cmp($r2, sizeof(Character) * Character::Max);
            jne(type_error_0);

            mov($r2, $r1);
            sub($r2, $rn);
            cmp($r2, sizeof(Character) * Character::Max);
            jne(type_error_1);

            emit_rex($r0, $r2);
            emit_op(op_CMOVcc_Gv_Ev + (tttn ^ 1));
            emit_u8(modrm(mod_reg, $r0, $rnil));

            add(rsp, 40);
            ret();

          label(type_error_1);
            mov($r0, $r1);

          label(type_error_0);
            mov($r1, Qcharacter);
            call(ea($rtcb, SVC_type_error));
        end_defun();
    } // GenCmp
} oAsm; // CharCmp

    oAsm.GenCmp(L"SI:CHAR=/2",  tttn_E);
    oAsm.GenCmp(L"SI:CHAR/=/2", tttn_NE);

    oAsm.GenCmp(L"SI:CHAR</2",  tttn_B);
    oAsm.GenCmp(L"SI:CHAR<=/2", tttn_BE);

    oAsm.GenCmp(L"SI:CHAR>/2",  tttn_A);
    oAsm.GenCmp(L"SI:CHAR>=/2", tttn_AE);

    #define defpred(mp_NAME, mp_attr) \
        defun_(Q(mp_NAME), 1, 1, Fixed, 8) \
            Label   not_char; \
            lea($rn, ea($rnil, QQchar_min->ToInt()-nil->ToInt())); \
            mov($r1, $r0); \
            sub($r1, $rn); \
            cmp($r1, sizeof(Character) * Character::Max); \
            ja(not_char); \
            test(ea($r0, OffsetOf(Character, m_data)), mp_attr); \
            cmove($r0, $rnil); \
            ret(); \
         label(not_char); \
            mov($r1, Qcharacter); \
            jmp(ea($tcb, SVC_type_error)); \
        end_defun()

    defpred("ALPHA-CHAR-P",     Character::Attr_Alpha)
    defpred("ALPHANUMERICP",    Character::Attr_AlphaNumeric)
    defpred("BOTH-CASE-P",      Character::Attr_BothCase)
    defpred("GRAPHIC-CHAR-P",   Character::Attr_Graphic)
    defpred("LOWER-CASE-P",     Character::Attr_LowerCase)
    defpred("STANDARD-CHAR-P",  Character::Attr_Standard)
    defpred("UPPER-CASE-P",     Character::Attr_UpperCase)
    defpred("WHITESPACE-CHAR-P",Character::Attr_Whitespace)

    // characterp
    defun_(Q("CHARACTERP"), 1, 1, Fixed, 8)
        lea($r4, ea($rnil, QQchar_min->ToInt()-nil->ToInt()));
        mov($r1, $r0);
        sub($r1, $r4);
        cmp($r1, sizeof(Character) * Character::Max);
        cmova($r0, $rnil);
        ret();
    end_defun();

    // char-category
    defun_(Q("CHAR-CATEGORY"), 1, 1, Fixed, 8)
        Label not_char;

        lea($rn, ea($rnil, QQchar_min->ToInt()-nil->ToInt()));
        mov($r1, $r0);
        sub($r1, $rn);
        cmp($r1, sizeof(Character) * Character::Max);
        ja(not_char);

        mov($r0, ea($r0, OffsetOf(Character, m_data)));
        and($r0, Character::Attr_CategoryMask);
        ret();

      label(not_char);
        mov($r1, Qcharacter);
        jmp(ea($rtcb, SVC_type_error));
    end_defun() // char-category

    // char-downcase
    defun_(Q("CL:CHAR-DOWNCASE"), 1, 1, Fixed, 8)
        Label not_char;

        lea($rn, ea($rnil, QQchar_min->ToInt()-nil->ToInt()));
        mov($r1, $r0);
        sub($r1, $rn);
        cmp($r1, sizeof(Character) * Character::Max);
        ja(not_char);

        mov($r0, ea($r0, OffsetOf(Character, m_data)));
        shr($r0, Character::LowerShiftCount - Character::ShiftCount);
        and($r0, Character::Mask * sizeof(Character));
        lea($r0, ea($rn, 0, $r0));
        ret();

      label(not_char);
        mov($r1, Qcharacter);
        jmp(ea($rtcb, SVC_type_error));
    end_defun() // char-downcase

    // char-upcase
    defun_(Q("CL:CHAR-UPCASE"), 1, 1, Fixed, 8)
        Label not_char;

        lea($rn, ea($rnil, QQchar_min->ToInt()-nil->ToInt()));
        mov($r1, $r0);
        sub($r1, $rn);
        cmp($r1, sizeof(Character) * Character::Max);
        ja(not_char);

        mov($r0, ea($r0, OffsetOf(Character, m_data)));
        shr($r0, Character::UpperShiftCount - Character::ShiftCount);
        and($r0, Character::Mask * sizeof(Character));
        lea($r0, ea($rn, 0, $r0));
        ret();

      label(not_char);
        mov($r1, Qcharacter);
        jmp(ea($rtcb, SVC_type_error));
    end_defun() // char-upcase

    // char-code
    defun_(Q("CL:CHAR-CODE"), 1, 1, Fixed, 8)
        Label not_char;

        lea($rn, ea($rnil, QQchar_min->ToInt()-nil->ToInt()));
        mov($r1, $r0);
        sub($r1, $rn);
        cmp($r1, sizeof(Character) * Character::Max);
        ja(not_char);

        mov($r0, $r1);
        shr($r0, Character::ShiftCount - Fixnum::TagBits);
        ret();

      label(not_char);
        mov($r1, Qcharacter);
        jmp(ea($rtcb, SVC_type_error));
    end_defun() // char-code

    // char-int
    defun_(Q("CL:CHAR-INT"), 1, 1, Fixed, 8)
        Label not_char;

        lea($rn, ea($rnil, QQchar_min->ToInt()-nil->ToInt()));
        mov($r1, $r0);
        sub($r1, $rn);
        cmp($r1, sizeof(Character) * Character::Max);
        ja(not_char);

        mov($r0, $r1);
        shr($r0, Character::ShiftCount - Fixnum::TagBits);
        ret();

      label(not_char);
        mov($r1, Qcharacter);
        jmp(ea($rtcb, SVC_type_error));
    end_defun() // char-int

    // code-char integer => (or character null)
    defun_(Q("CL:CODE-CHAR"), 1, 1, Fixed, 8)
        Label got_fixnum;
        Label not_int;
        Label ret_nil;

        test($r0, Fixnum::TagMask);
        je(got_fixnum);

        test($r0, Record::Tag);
        jne(not_int);

        mov($r1, CLASSD_bignum);
        cmp(ea($r0, OffsetOf(Record, m_classd)), $r1);
        jne(not_int);

      label(ret_nil);
        mov($r0, $rnil);
        ret();

      label(got_fixnum);
        cmp($r0, 0xFFFF * Fixnum::One);
        ja(ret_nil);

        ASSERT(Character::ShiftCount == Fixnum::TagBits + 1);
        lea($r0, ea($rnil, QQchar_min->ToInt() - nil->ToInt(), $r0, scale_2));

        xor($r1, $r1);   // CF=0
        ret();

      label(not_int);
        mov($r1, Qinteger);
        jmp(ea($rtcb, SVC_type_error));
    end_defun()

} // X64Builder::build_13_Character

} // Boot
