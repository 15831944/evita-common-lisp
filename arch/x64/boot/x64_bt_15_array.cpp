#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - boot - 15 Arrays
// arch/x64/boot/x64_bt_15_array.inc
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x64/boot/x64_bt_15_array.cpp#4 $
//
// Description:
//  This file contains string functions implemented in assembler:
//      arrayp
//      bit-vector-p
//      simple-bit-vector-p
//      simple-vector-p
//      vectorp

#include "./x64_bt_builder.h"

namespace Boot
{

void X64Builder::build_15_Array()
{

defun("SI::SEQUENCE-INDEX-ERROR", 2, 2)
    mov($r4, $r1);
    mov($r3, Q(":INDEX"));
    mov($r2, $r0);
    mov($r1, Q(":SEQUENCE"));
    mov($r0, Q("SI::SEQUENCE-INDEX-ERROR"));
    mov($rn, Fixnum::Encode(5));
    jmp(ea($rtcb, SVC_error));
end_defun();


defun_(Q("CL:SVREF"), 2, 2, Fixed, 8)
    Label bad_index;
    Label not_integer;
    Label not_simple_vector;
    Label unbound_index;

    // Is $r0 simple-vector?
    lea($r2, ea($r0, -SimpleVector::Tag));
    and($r2, Val_::Mask_Tag);
    jne(not_simple_vector);

    mov($r2, CLASSD_simple_vector);
    cmp(ea($r0, -SimpleVector::Tag), $r2);
    jne(not_simple_vector);

    // Is $r1 fixnum?
    test($r1, Fixnum::TagMask);
    jne(bad_index);

    // Is bound index?
    cmp(
        ea($r0, offsetof(SimpleVector, m_length) - SimpleVector::Tag),
        $r1 );
    jbe(unbound_index);

    // Fetch datum from $r0[$r1].
    mov(
        $r0,
        ea($r0,
           offsetof(SimpleVector, mv_element) - SimpleVector::Tag,
           $r1 ));
    ret();

  label(not_simple_vector);
    mov($r1, Qsimple_vector);
    jmp(ea($rtcb, SVC_type_error));

  label(bad_index);
    lea($r2, ea($r1, -Bignum::Tag));
    and($r2, Val_::Mask_Tag);
    jne(not_integer);

    mov($r2, CLASSD_bignum );
    cmp(ea($r1, offsetof(Bignum, m_classd) - Bignum::Tag), $r2);
    jne(not_integer);

  label(unbound_index);
    jmp(Q("SI:SEQUENCE-INDEX-ERROR"));

  label(not_integer);
    mov($r0, $r1);
    mov($r1, Qinteger);
    jmp(ea($rtcb, SVC_type_error));
end_defun()


defun_(list(Qsetf, Q("CL:SVREF")), 3, 3, Fixed, 8)
    Label not_integer;
    Label not_vector;
    Label bad_index;
    Label unbound_index;

    // Is $r1 simple-vector?
    lea($r3, ea($r1, -SimpleVector::Tag));
    test($r3, Val_::Mask_Tag);
    jne(not_vector);

    mov($r3, CLASSD_simple_vector );
    cmp(ea($r1, offsetof(SimpleVector, m_classd) - SimpleVector::Tag), $r3);
    jne(not_vector);

    // Is $r2 fixnum?
    test($r2, Fixnum::TagMask);
    jne(bad_index);

    // Is bound index?
    cmp(
        ea($r1, offsetof(SimpleVector, m_length) - SimpleVector::Tag),
        $r2 );
    jbe(unbound_index);

    // Set datum into $r1[$r2].
    mov(
        ea($r1,
           offsetof(SimpleVector, mv_element) - SimpleVector::Tag,
           $r2 ),
        $r0 );
    ret();

  label(not_vector);
    mov($r0, $r1);
    mov($r1, Qsimple_vector);
    jmp(ea($rtcb, SVC_type_error));

  label(bad_index);
    lea($r3, ea($r2, -Bignum::Tag));
    and($r3, Val_::Mask_Tag);
    jne(not_integer);

    mov($r3, CLASSD_bignum);
    cmp(ea($r2, offsetof(Bignum, m_classd) - Bignum::Tag), $r3);
    jne(not_integer);

  label(unbound_index);
    mov($r0, $r1);
    mov($r1, $r2);
    mov($rn, Fixnum::Encode(2));
    jmp(Q("SI:SEQUENCE-INDEX-ERROR"));

  label(not_integer);
    mov($r1, $r2);
    mov($r1, Qinteger);
    jmp(ea($rtcb, SVC_type_error));
end_defun()

} // X64Builder::build_15_Array
} // Boot
