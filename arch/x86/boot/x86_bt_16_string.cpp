#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - boot - 16 Strings
// arch/x86/boot/x86_bt_16_stirng.inc
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86/boot/x86_bt_16_string.cpp#6 $
//
// Description:
//  This file contains string functions implemented in assembler:
//      si:string=/2
//
#include "./x86_bt_builder.h"

namespace Boot
{

void X86Builder::build_16_String()
{

defun("CL:SCHAR", 2, 2)
    Label got_fixnum;
    Label not_integer;
    Label not_string;
    Label bad_index;
    Label unbound_index;

    lea($r2, ea($r0, -SimpleString::Tag));
    and($r2, 7);
    jne(not_string);

    cmp(ea($r0, offsetof(SimpleString, m_classd) - SimpleString::Tag),
        CLASSD_simple_string );
    jne(not_string);


    test($r1, Fixnum::TagMask);
    jne(bad_index);

    cmp(
        $r1,
        ea($r0, offsetof(SimpleString, m_length) - SimpleString::Tag) );
    jae(unbound_index);

    shr($r1, 1);
    movzxw(
        $r0,
        ea($r0,
           offsetof(SimpleString, m_rgwchElement) - SimpleString::Tag,
           $r1 ) );

    shl($r0, Character::ShiftCount);
    add($r0, QQchar_min);
    ret();

  label(not_string);
    mov($r1, Qsimple_string);
    jmp(ea($tcb, SVC_type_error));

  label(bad_index);
    lea($r2, ea($r1, -Bignum::Tag));
    and($r2, 7);
    jne(not_integer);

    cmp(ea($r1, offsetof(Bignum, m_classd) - Bignum::Tag),
        CLASSD_bignum );
    je(unbound_index);

  label(unbound_index);
    call(Q("SI:SEQUENCE-INDEX-ERROR"));

  label(not_integer);
    mov($r0, $r1);
    mov($r1, Qinteger);
    call(ea($tcb, SVC_type_error));
end_defun()


defun_setf(L"CL:SCHAR", 3, 3)
    Label not_char;
    Label not_integer;
    Label not_string;
    Label bad_index;
    Label unbound_index;

    // Is $r0 character?
    lea($r4, ea($r0, -QQchar_min->ToInt()));
    cmp($r4, sizeof(Character) * Character::Max);
    ja(not_char);

    // Is $r1 simple-string?
    lea($r3, ea($r1, -SimpleString::Tag));
    and($r3, 7);
    jne(not_string);

    cmp(ea($r1, offsetof(SimpleString, m_classd) - SimpleString::Tag),
        CLASSD_simple_string );
    jne(not_string);

    // Is $r2 fixnum?
    test($r2, Fixnum::TagMask);
    jne(bad_index);

    cmp(
        $r2,
        ea($r1, offsetof(SimpleString, m_length) - SimpleString::Tag) );
    jae(unbound_index);

    // compute character code
    shr($r4, Character::ShiftCount);

    // scale index
    shr($r2, 1);

    // store character
    emit_op(op_OPDSIZ);
    mov(
        ea($r1,
           offsetof(SimpleString, m_rgwchElement) - SimpleString::Tag,
           $r2 ),
        $r4 );
    ret();

  label(not_char);
    mov($r1, Qcharacter);
    jmp(ea($tcb, SVC_type_error));

  label(not_string);
    mov($r0, $r1);
    mov($r1, Qsimple_string);
    jmp(ea($tcb, SVC_type_error));

  label(bad_index);
    lea($r3, ea($r2, -Bignum::Tag));
    and($r3, 7);
    jne(not_integer);

    cmp(ea($r2, offsetof(Bignum, m_classd) - Bignum::Tag),
        CLASSD_bignum );
    jne(not_integer);

  label(unbound_index);
    mov($r0, $r1);
    mov($r1, $r2);
    mov($rn, Fixnum::Encode(2));
    call(Q("SI:SEQUENCE-INDEX-ERROR"));

  label(not_integer);
    mov($r1, $r2);
    mov($r1, Qinteger);
    call(ea($tcb, SVC_type_error));
end_defun()

} // X86Builder::build_16_Stirng

} // Boot
