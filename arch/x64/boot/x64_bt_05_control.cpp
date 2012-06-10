#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - boot - 5 Data and Control Flow
// arch/x64/boot/x64_bt_05_control.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x64/boot/x64_bt_05_control.cpp#8 $
//
// Description:
//  This file contains functions for "03 Evaluation and Compilation".
//
#include "./x64_bt_builder.h"

#include "../kernel/x64_ke_thread.h"

namespace Boot
{

using namespace X64;

void X64Builder::build_05_Control()
{
Q(".FUNCALL");
Q(".NOT-FUNCTION");
Q(".UNDEFINED-FUNCTION");
Q(".UNBOUND-VARIABLE");
Q(".NTH-VALUE");
Q(".VALUES*");

Q("EQ");
Q("FUNCALL");
Q("IDENTITY");
Q("NOT");
Q("VALUES");
Q("VALUES-LIST");


////////////////////////////////////////////////////////////
//
// .funcall
//      $rtcb.m_fn <- callable
//
defun_(Q(".FUNCALL"), 0, -1, Fixed, 8)
    Label check_value;
    Label got_symbol;
    Label not_function;
    Label test_function;
    Label test_setf;
    Label undefined_function;

    mov(ea_mv_value(0), $r0);
    mov(ea_mv_value(1), $r1);

    mov($r0, $rfn);

    // test callee is symbol
    cmp($r0, $rnil);
    je(got_symbol);

    lea($r1, ea($r0, -Symbol::Tag));
    and($r1, Symbol::TagMask);
    jne(test_function);

    mov($r1, CLASSD_symbol);
    cmp(ea($r0, OffsetOf(Symbol, m_classd)), $r1);
    jne(test_setf);

  label(got_symbol);
    mov($r1, $r0);
    and($r1, ~Symbol::TagMask);
    mov($r1, ea($r1, offsetof(Symbol, m_function)));

  label(check_value);
    cmp($r1, $rnil);
    je(undefined_function);

    mov($r0, $r1);

    // test callee is function
  label(test_function);
    lea($r1, ea($r0, -Funcallable::Tag));
    and($r1, Funcallable::TagMask);
    jne(not_function);

    lea($rfn, ea($r0, sizeof(FunObj) - Funcallable::Tag));
    mov($r0, ea_mv_value(0));
    mov($r1, ea_mv_value(1));
    jmp($rfn);

    // test callee is setf
  label(test_setf);
    mov($r1, CLASSD_setf_cell );
    cmp(ea($r0, OffsetOf(SetfCell, m_classd)), $r1);
    jne(not_function);

    mov($r1, ea($r0, OffsetOf(SetfCell, m_function)));
    jmp(check_value);

    // Signal undefined-function error
  label(undefined_function);
    mov($r0, $rfn);
    jmp(ea($rtcb, SVC_undefined_function));

    // Signal type-error function
  label(not_function);
    mov($r0, $rfn);
    mov($r1, Qfunction);     // BUGBUG: Should be callable.
    jmp(ea($rtcb, SVC_type_error));
end_defun()


//////////////////////////////////////////////////////////////////////
//
// .not-function
//      $r2 = name
//  See make_not_function_function
defun_(Q(".NOT-FUNCTION"), 0, -1, Fixed, 8)
    mov($r1, Kname);
    mov($r0, Q("SI:NOT-FUNCTION"));
    mov($rn, Fixnum::Encode(3));
    jmp(ea($rtcb, SVC_error));
end_defun()



//////////////////////////////////////////////////////////////////////
//
//  CALL {some-function}
//  CMOVNC  $rn <- tcb.fixnum_1
//  MOV  $rfn <- nth
//  CALL .NTH-VALUE
//
//  Returns nth value specified in $r4. If 0 <= $r4 <= 3, value is taken
//  from register, otherwise value is taken from tcb.mv_value[$r4].
//
defun_(Q(".NTH-VALUE"), 0, -1, Fixed, 8)
    Label ret_$r0; Label ret_$r1; Label ret_$r2; Label ret_$r3;
    Label ret_$r4; Label ret_$r5; Label ret_$r6; Label ret_$r7;
    Label ret_$r8; Label ret_$r9;
    Label ret_$rtbl;
    Label ret_mem;
    Label ret_nil;
    Label not_nth;

    test($rfn, Fixnum::TagMask);
    jne(not_nth);

    test($rfn, $rfn);
    jl(not_nth);

    cmp($rfn, Fixnum::Encode(X64Mach::Multiple_Values_Limit));
    jge(not_nth);               // CF=0 if branch

    // Is nth greater than or equal to #values? (unsigned comparison)
    cmp($rfn, $rn);
    jge(ret_nil);               // CF=0 if branch

    cmp($rfn, Fixnum::Encode(11));
    jge(ret_mem);               // CF=0 if branch

    lea($rn, ea(ret_$rtbl));
    add($rfn, $rn);             // CF=0
    jmp(ea($rfn));

  label(ret_$r0);  ret();
  label(ret_$r1);  mov($r0, $r1);  ret();
  label(ret_$r2);  mov($r0, $r2);  ret();
  label(ret_$r3);  mov($r0, $r3);  ret();
  label(ret_$r4);  mov($r0, $r4);  ret();
  label(ret_$r5);  mov($r0, $r5);  ret();
  label(ret_$r6);  mov($r0, $r6);  ret();
  label(ret_$r7);  mov($r0, $r7);  ret();
  label(ret_$r8);  mov($r0, $r8);  ret();
  label(ret_$r9);  mov($r0, $r9);  ret();

  label(ret_mem); mov($r0, ea_mv_value($rfn)); ret();

  label(ret_nil); mov($r0, nil); ret();

  label(not_nth);
    mov($r0, $rfn);
    mov(
        $r1,
        list(Qinteger,
                0,
                Fixnum::Encode(X64Mach::Multiple_Values_Limit - 1) ) );
    jmp(ea($rtcb, SVC_type_error));

  label(ret_$rtbl);
    dq(ret_$r0); dq(ret_$r1); dq(ret_$r2);  dq(ret_$r3);
    dq(ret_$r4); dq(ret_$r5); dq(ret_$r6);  dq(ret_$r7);
    dq(ret_$r8); dq(ret_$r9);
end_defun()


//////////////////////////////////////////////////////////////////////
//
// .undefined-function
//      $r0 = name
defun_(Q(".UNDEFINED-FUNCTION"), 0, -1, Fixed, 8)
    mov($r2, $r0);
    mov($r1, Kname);
    mov($r0, Q("UNDEFINED-FUNCTION"));
    mov($rn, Fixnum::Encode(3));
    jmp(ea($rtcb, SVC_error));
end_defun()


//////////////////////////////////////////////////////////////////////
//
// .unbound-variable
//      $r0 = name
defun_(Q(".UNBOUND-VARIABLE"), 0, -1, Fixed, 8)
    mov($r2, $r0);
    mov($r1, Kname);
    mov($r0, Q("UNBOUND-VARIABLE"));
    mov($rn, Fixnum::Encode(3));
    jmp(ea($rtcb, SVC_error));
end_defun()


//////////////////////////////////////////////////////////////////////
//
// .values*
//  This function is almost same as values but the last argument
//  must be list and elemetns of the list to values.
//
//  This function always sets CF=1 even if number of values is one.
//
//  [1] Set register parameters into $rtcb.mv_value[k].
//  [2] Get $rtcb.mv_value[k] into registers.
//
defun(".VALUES*", 1, X64Mach::Lambda_Parameters_Limit)
    Label exit_0, exit_1, exit_2, exit_3, exit_4;
    Label exit_5, exit_6, exit_7, exit_8, exit_9;

    Label exit_more_than_9;
    Label exit_tbl;

    Label from_0, from_1, from_2, from_3, from_4;
    Label from_5, from_6, from_7, from_8, from_9;
    Label from_10;
    Label from_tbl;

    Label cont_0,  cont_1, cont_2, cont_3, cont_4;
    Label cont_5,  cont_6, cont_7, cont_8, cont_9;

    Label not_list;
    Label loop;
    Label loop_end;

    Label too_many_arguments;

    sub($sp, 8);

    mov(ea_mv_value(0), $r0);
    mov(ea_mv_value(1), $r1);

    sub($rn, Fixnum::One);
    cmp($rn, Fixnum::Encode(10));
    jge(from_10);
    lea($r0, ea(from_tbl));
    jmp(ea($rn, 0, $r0));

    // Set the last argument(list) to $r1.
  label(from_0);  mov($r1, ea_mv_value(0));  jmp(cont_0);
  // from_1 is as same as loop.
  label(from_2);  mov($r1, $r2);  jmp(cont_2);
  label(from_3);  mov($r1, $r3);  jmp(cont_3);
  label(from_4);  mov($r1, $r4);  jmp(cont_4);
  label(from_5);  mov($r1, $r5);  jmp(cont_5);
  label(from_6);  mov($r1, $r6);  jmp(cont_6);
  label(from_7);  mov($r1, $r7);  jmp(cont_7);
  label(from_8);  mov($r1, $r8);  jmp(cont_8);
  label(from_9);  mov($r1, $r9);  jmp(cont_9);
  label(from_10); mov($r1, ea_mv_value($rn));
                  mov(ea_mv_value( 9), $r9);
  label(cont_9);  mov(ea_mv_value( 8), $r8);
  label(cont_8);  mov(ea_mv_value( 7), $r7);
  label(cont_7);  mov(ea_mv_value( 6), $r6);
  label(cont_6);  mov(ea_mv_value( 5), $r5);
  label(cont_5);  mov(ea_mv_value( 4), $r4);
  label(cont_4);  mov(ea_mv_value( 3), $r3);
  label(cont_3);  mov(ea_mv_value( 2), $r2);

  label(cont_2); label(cont_1); label(cont_0);
  label(from_1);
  label(loop);
    // loop_end if $r1 == nil
    cmp($r1, $rnil);
    je(loop_end);

    // Can we have one more value?
    cmp($rn, Fixnum::Encode(X64Mach::Lambda_Parameters_Limit));
    jge(too_many_arguments);

    // not_list if $r1 isn't cons
    lea($r0, ea($r1, -Cons::Tag));
    and($r0, 15);
    jne(not_list);

    // $r0 = car($r1)
    mov($r0, ea($r1, offsetof(Cons, m_car) - Cons::TagX));

    // mv_value[$rn] = $r0
    mov(ea_mv_value($rn), $r0);

    // $rn += 1
    add($rn, Fixnum::One);

    // $r1 = cdr($r1)
    mov($r1, ea($r1,  offsetof(Cons, m_cdr) - Cons::TagX));
    jmp(loop);

  label(loop_end);
    cmp($rn, Fixnum::Encode(10));
    jge(exit_more_than_9);
    lea($r2, ea(exit_tbl));
    jmp(ea($r2, 0, $rn));

    // Set register values
  label(exit_more_than_9); mov($r9,  ea_mv_value(9));
  label(exit_9);  mov($r8,  ea_mv_value(8));
  label(exit_8);  mov($r7,  ea_mv_value(7));
  label(exit_7);  mov($r6,  ea_mv_value(6));
  label(exit_6);  mov($r5,  ea_mv_value(5));
  label(exit_5);  mov($r4,  ea_mv_value(4));
  label(exit_4);  mov($r3,  ea_mv_value(3));
  label(exit_3);  mov($r2,  ea_mv_value(2));
  label(exit_2);  mov($r1,  ea_mv_value(1));
  label(exit_1);  mov($r0,  ea_mv_value(0));
  label(exit_0);

    add($sp, 8);
    stc();
    ret();

  label(not_list);
    mov($r0, $r1);
    mov($r1, Qlist);
    call(ea($rtcb, SVC_type_error));

  label(too_many_arguments);
    mov($r0, Qtoo_many_values);
    mov($rn, Fixnum::Encode(1));
    call(ea($rtcb, SVC_error));

  label(from_tbl);
    dq(from_0);  dq(from_1); dq(from_2);  dq(from_3);
    dq(from_4);  dq(from_5); dq(from_6);  dq(from_7);
    dq(from_8);  dq(from_9);

  label(exit_tbl);
    dq(exit_0);  dq(exit_1); dq(exit_2); dq(exit_3); dq(exit_4);
    dq(exit_5);  dq(exit_6); dq(exit_7); dq(exit_8); dq(exit_9);
end_defun() // .values*


//////////////////////////////////////////////////////////////////////
//
// eq
//
defun_(Q("EQ"), 2, 2, Fixed, 8)
    cmp($r0, $r1);
    mov($r0, t);
    cmovne($r0, $rnil);
    xor($r1, $r1);
    ret();
end_defun() // eq


//////////////////////////////////////////////////////////////////////
//
// funcall
//
defun("FUNCALL", 1, X64Mach::Lambda_Parameters_Limit)
    Label not_function;
    Label got_symbol;
    Label test_function;
    Label undefined_function;

    mov(ea_mv_value(1), $r1);
    mov(ea_mv_value(2), $r2);
    mov(ea_mv_value(3), $r3);
    mov(ea_mv_value(4), $r4);
    mov(ea_mv_value(5), $r5);
    mov(ea_mv_value(6), $r6);
    mov(ea_mv_value(7), $r7);
    mov(ea_mv_value(8), $r8);
    mov(ea_mv_value(9), $r9);

    // Is symbol?
    cmp($r0, $rnil);
    je(got_symbol);

    lea($r1, ea($r0, -Symbol::Tag));
    test($r1, Symbol::TagMask);
    jne(test_function);

    mov($r1, CLASSD_symbol);
    cmp(ea($r0, OffsetOf(Symbol, m_classd)), $r1);
    jne(not_function);

  label(got_symbol);
    mov($r1, $r0);
    and($r1, ~Symbol::TagMask);
    mov($r1, ea($r1, offsetof(Symbol, m_function)));
    cmp($r1, $rnil);
    je(undefined_function);
    mov($r0, $r1);

  label(test_function);
    lea($r1, ea($r0, -Funcallable::Tag));
    and($r1, Funcallable::TagMask);
    jne(not_function);

    add($r0, sizeof(FunObj) - Funcallable::Tag); // compute entry point
    mov(ea($sp, -8), $r0);                  // store entry point
    sub($rn, Fixnum::One);
    mov($rfn, $rn);                         // save argument count

    // Shift arguments
    shr(rcx, Fixnum::TagBits);
    lea(rsi, ea_mv_value(1));
    lea(rdi, ea_mv_value(0));
    emit_op(op_REP);
    emit_op(op_MOVSQ);

    mov($r0,  ea_mv_value(0));
    mov($r1,  ea_mv_value(1));
    mov($r2,  ea_mv_value(2));
    mov($r3,  ea_mv_value(3));
    mov($r4,  ea_mv_value(4));
    mov($r5,  ea_mv_value(5));
    mov($r6,  ea_mv_value(6));
    mov($r7,  ea_mv_value(7));
    mov($r8,  ea_mv_value(8));
    mov($r9,  ea_mv_value(9));
    mov($rn,  $rfn);
    jmp(ea($sp, -8));

  label(not_function);
    mov($r1, Q("FUNCTION-DESIGNATOR"));
    sub($sp, 8);
    call(ea($rtcb, SVC_type_error));

  label(undefined_function);
    sub($sp, 8);
    call(ea($rtcb, SVC_undefined_function));
end_defun() // funcall


defun_(Q("IDENTITY"), 1, 1, Fixed, 8)
    ret();
end_defun()

// See also "NULL".
defun_(Q("NOT"), 1, 1, Fixed, 8)
    cmp($r0, $rnil);
    mov($r0, Qt);
    cmovne($r0, $rnil);
    xor($r1, $r1);      // xor may be faster than clc.
    ret();
end_defun()

defun_(Q("VALUES"), 0, -1, Fixed, 8)
    stc();      // tell caller ecx holds number of values.
    ret();
end_defun()

// values-list
defun_(Q("VALUES-LIST"), 1, 1, Fixed, 8)
    jmp(Q(".VALUES*"));
end_defun()

} // X64Builder::build_05_Control

} // Boot
