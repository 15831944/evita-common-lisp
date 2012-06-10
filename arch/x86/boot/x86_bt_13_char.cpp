#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - boot - 13 Characters
// arch/x86/boot/x86_bt_13_char.inc
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86/boot/x86_bt_13_char.cpp#9 $
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
#include "./x86_bt_builder.h"

namespace Boot
{

void X86Builder::build_13_Character()
{
    #define defcmp(mp_name, mp_tttn) \
        defun(mp_name, 2, 2) \
            Label not_char_0; \
            Label not_char_1; \
            lea($r2, ea($r0, static_cast<int32>(-QQchar_min->ToInt()))); \
            cmp($r2, sizeof(Character) * Character::Max); \
            ja(not_char_0); \
            lea($r2, ea($r1, static_cast<int32>(-QQchar_min->ToInt()))); \
            cmp($r2, sizeof(Character) * Character::Max); \
            ja(not_char_1); \
            mov($r2, nil); \
            cmp($r0, $r1); \
            emit_op(op_CMOVcc_Gv_Ev + (mp_tttn ^ 1)); \
            emit_u8(modrm(mod_reg, $r0, $r2)); \
            or($r0, $r0); \
            ret(); \
          label(not_char_1); \
            mov($r0, $r1); \
          label(not_char_0); \
            mov($r1, Qcharacter); \
            call(ea($tcb, SVC_type_error)); \
        end_defun()


    defcmp("SI:CHAR=/2",  tttn_E);
    defcmp("SI:CHAR/=/2", tttn_NE);

    defcmp("SI:CHAR</2",  tttn_B);
    defcmp("SI:CHAR<=/2", tttn_BE);

    defcmp("SI:CHAR>/2",  tttn_A);
    defcmp("SI:CHAR>=/2", tttn_AE);

    #define defpred(mp_NAME, mp_attr) \
        defun(mp_NAME, 1, 1) \
            Label   not_char; \
            lea($r1, ea($r0, static_cast<int32>(-QQchar_min->ToInt()))); \
            cmp($r1, sizeof(Character) * Character::Max); \
            ja(not_char); \
            mov($r2, nil); \
            test(ea($r0, OffsetOf(Character, m_data)), mp_attr); \
            cmove($r0, $r2); \
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
    defun("CHARACTERP", 1, 1)
        mov($r2, nil);
        lea($r1, ea($r0, static_cast<int32>(-QQchar_min->ToInt())));
        cmp($r1, sizeof(Character) * Character::Max);
        cmova($r0, $r2);
        ret();
    end_defun();

    // char-category
    defun("CHAR-CATEGORY", 1, 1)
        Label   not_char;

        lea($r1, ea($r0, static_cast<int32>(-QQchar_min->ToInt())));
        cmp($r1, sizeof(Character) * Character::Max);
        ja(not_char);

        mov($r0, ea($r0, OffsetOf(Character, m_data)));
        and($r0, Character::Attr_CategoryMask);
        ret();

     label(not_char);
        mov($r1, Qcharacter);
        jmp(ea($tcb, SVC_type_error));
    end_defun()

    // char-downcase
    defun("CHAR-DOWNCASE", 1, 1)
        Label   not_char;

        lea($r1, ea($r0, static_cast<int32>(-QQchar_min->ToInt())));
        cmp($r1, sizeof(Character) * Character::Max);
        ja(not_char);

        mov($r1, ea($r0, OffsetOf(Character, m_data)));
        mov($r2, $r1);
        shr($r2, Character::CaseShiftCount);
        shl($r2, Character::ShiftCount);
        add($r2, static_cast<int32>(QQchar_min->ToInt()));
        test($r1, Character::Attr_UpperCase);
        cmovne($r0, $r2);

        ret();

     label(not_char);
        mov($r1, Qcharacter);
        jmp(ea($tcb, SVC_type_error));
    end_defun()

    // char-upcase
    defun("CHAR-UPCASE", 1, 1)
        Label   not_char;

        lea($r1, ea($r0, static_cast<int32>(-QQchar_min->ToInt())));
        cmp($r1, sizeof(Character) * Character::Max);
        ja(not_char);

        mov($r1, ea($r0, OffsetOf(Character, m_data)));
        mov($r2, $r1);
        shr($r2, Character::CaseShiftCount);
        shl($r2, Character::ShiftCount);
        add($r2, static_cast<int32>(QQchar_min->ToInt()));
        test($r1, Character::Attr_LowerCase);
        cmovne($r0, $r2);

        ret();

     label(not_char);
        mov($r1, Qcharacter);
        jmp(ea($tcb, SVC_type_error));
    end_defun()

    // char-code character => integer
    defun("CL:CHAR-CODE", 1, 1)
        Label not_char;

        lea($r1, ea($r0, static_cast<int32>(-QQchar_min->ToInt())));
        cmp($r1, sizeof(Character) * Character::Max);
        ja(not_char);

        sub($r0, QQchar_min);
        shr($r0, Character::ShiftCount - Fixnum::TagBits);
        ret();

      label(not_char);
        mov($r1, Qcharacter);
        jmp(ea($tcb, SVC_type_error));
    end_defun()

    // char-int character => integer
    defun("CL:CHAR-INT", 1, 1)
        Label not_char;

        lea($r1, ea($r0, static_cast<int32>(-QQchar_min->ToInt())));
        cmp($r1, sizeof(Character) * Character::Max);
        ja(not_char);

        add($r0, QQchar_min);
        shr($r0, Character::ShiftCount - Fixnum::TagBits);
        ret();

      label(not_char);
        mov($r1, Qcharacter);
        jmp(ea($tcb, SVC_type_error));
    end_defun()

    // code-char integer => (or character null)
    defun("CL:CODE-CHAR", 1, 1)
        Label got_fixnum;
        Label not_int;
        Label ret_nil;

        test($r0, Fixnum::TagMask);
        je(got_fixnum);

        test($r0, Record::Tag);
        jne(not_int);

        cmp(ea($r0, OffsetOf(Record, m_classd)), CLASSD_bignum);
        jne(not_int);

      label(ret_nil);
        mov($r0, nil);
        ret();

      label(got_fixnum);
        cmp($r0, Character::Max * Fixnum::One);
        ja(ret_nil);

        ASSERT(sizeof(Character) == 8);
        lea($r0, ea(none,
                    static_cast<int32>(QQchar_min->ToInt()),
                    $r0, scale_2 ) );
        ret();

      label(not_int);
        mov($r1, Q("CHAR-CODE"));
        call(ea($tcb, SVC_type_error));
    end_defun()
} // X86Builder::build_13_Character

} // Boot
