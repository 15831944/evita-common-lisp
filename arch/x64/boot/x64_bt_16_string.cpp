#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - boot - 16 Strings
// arch/x64/boot/x64_bt_16_stirng.inc
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x64/boot/x64_bt_16_string.cpp#6 $
//
// Description:
//  This file contains string functions implemented in assembler:
//      si:string=/2

#include "./x64_bt_builder.h"

namespace Boot
{

void X64Builder::build_16_String()
{

defun("CL:SCHAR", 2, 2)
    Label got_fixnum;
    Label not_integer;
    Label not_string;
    Label bad_index;
    Label unbound_index;

    sub(rsp, 8);

    lea($r2, ea($r0, -SimpleString::Tag));
    and($r2, Val_::Mask_Tag);
    jne(not_string);

    mov($r2, CLASSD_simple_string);
    cmp(ea($r0, OffsetOf(SimpleString, m_classd)), $r2);
    jne(not_string);


    test($r1, Fixnum::TagMask);
    jne(bad_index);

    cmp($r1, ea($r0, OffsetOf(SimpleString, m_length)));
    jae(unbound_index);

    shr($r1, Fixnum::TagBits - 1);
    movzx($r0, word_ptr_($r0, OffsetOf(SimpleString, m_rgwchElement), $r1));

    shl($r0, Character::ShiftCount);
    lea($r0, ea($rnil, QQchar_min->ToInt()-nil->ToInt(), $r0));

    add(rsp, 8);    // CF=0
    ret();

  label(not_string);
    mov($r1, Qsimple_string);
    call(ea($rtcb, SVC_type_error));

  label(bad_index);
    lea($r2, ea($r1, -Bignum::Tag));
    and($r2, Val_::Mask_Tag);
    jne(not_integer);

    mov($r3, CLASSD_bignum);
    cmp(ea($r1, OffsetOf(Bignum, m_classd)), $r3);
    je(unbound_index);

  label(unbound_index);
    call(Q("SI:SEQUENCE-INDEX-ERROR"));

  label(not_integer);
    mov($r0, $r1);
    mov($r1, Qinteger);
    call(ea($rtcb, SVC_type_error));
end_defun()


defun_setf(L"CL:SCHAR", 3, 3)
    Label not_char;
    Label not_integer;
    Label not_string;
    Label bad_index;
    Label unbound_index;

    sub(rsp, 8);

    // Is $r0 character?
    lea($rn, ea($rnil, QQchar_min->ToInt()-nil->ToInt()));
    mov($r4, $r0);
    sub($r4, $rn);
    cmp($r4, sizeof(Character) * Character::Max);
    ja(not_char);

    // Is $r1 simple-string?
    lea($r3, ea($r1, -SimpleString::Tag));
    and($r3, Val_::Mask_Tag);
    jne(not_string);

    mov($r3, CLASSD_simple_string);
    cmp(ea($r1, OffsetOf(SimpleString, m_classd)), $r3);
    jne(not_string);

    // Is $r2 fixnum?
    test($r2, Fixnum::TagMask);
    jne(bad_index);

    cmp($r2, ea($r1, OffsetOf(SimpleString, m_length)));
    jae(unbound_index);

    shr($r4, Character::ShiftCount);

    ASSERT(Fixnum::TagBits >= 2);
    shr($r2, Fixnum::TagBits - 1);

    mov(word_ptr_($r1, OffsetOf(SimpleString, m_rgwchElement), $r2), $r4w);

    add(rsp, 8);
    ret();

  label(not_char);
    mov($r1, Qcharacter);
    call(ea($rtcb, SVC_type_error));

  label(not_string);
    mov($r0, $r1);
    mov($r1, Qsimple_string);
    call(ea($rtcb, SVC_type_error));

  label(bad_index);
    lea($r3, ea($r2, -Bignum::Tag));
    and($r3, Val_::Mask_Tag);
    jne(not_integer);

    mov($r3, CLASSD_bignum);
    cmp(ea($r2, OffsetOf(Bignum, m_classd)), $r3);
    jne(not_integer);

  label(unbound_index);
    mov($r0, $r1);
    mov($r1, $r2);
    mov($rn, Fixnum::Encode(2));
    call(Q("SI:SEQUENCE-INDEX-ERROR"));

  label(not_integer);
    mov($r1, $r2);
    mov($r1, Qinteger);
    call(ea($rtcb, SVC_type_error));
end_defun()

} // X64Builder::build_16_Stirng

} // Boot
