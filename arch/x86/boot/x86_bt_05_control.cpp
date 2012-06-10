#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - boot - 03 Eval
// arch/x86/boot/x86_bt_07_object.inc
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86/boot/x86_bt_05_control.cpp#6 $
//
// Description:
//  This file contains functions for "03 Evaluation and Compilation".
//
#include "./x86_bt_builder.h"

#include "../kernel/x86_ke_thread.h"

namespace Boot
{
using namespace X86;

void X86Builder::build_05_Control()
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
//  thread.m_fn <- callable
//
defun(".FUNCALL", 0, -1)
    Label got_symbol;
    Label not_function;
    Label test_function;
    Label test_setf;
    Label undefined_function;

    mov(ea_mv_value(0), $r0);
    mov(ea_mv_value(1), $r1);

    mov($r0, ea_m_fn());

    // test callee is symbol
    cmp($r0, nil);
    je(got_symbol);

    lea($r1, ea($r0, -Symbol::Tag));
    test($r1, Symbol::TagMask);
    jne(test_function);

    cmp(ea($r1, offsetof(Symbol, m_classd)), CLASSD_symbol);
    jne(test_setf);

  label(got_symbol);
    mov($r0, $r0);
    and($r1, ~Symbol::TagMask);
    mov($r1, ea($r1, offsetof(Symbol, m_function)));
    cmp($r1, nil);
    je(undefined_function);

    mov($r0, $r1);

    // test callee is function
  label(test_function);
    lea($r1, ea($r0, -FunObj::Tag));
    and($r1, Val_::Mask_Tag);
    jne(not_function);

    add($r0, sizeof(FunObj) - FunObj::Tag);
    mov(ea(esp, -4), $r0);
    mov($r0, ea_mv_value(0));
    mov($r1, ea_mv_value(1));
    jmp(ea(esp, -4));

    // test callee is setf
  label(test_setf);
    cmp(ea($r1, offsetof(SetfCell, m_classd)), CLASSD_setf_cell);
    jne(not_function);

    mov($r1, ea($r1, OffsetOf(SetfCell, m_function)));
    cmp($r1, nil);
    je(undefined_function);

    mov($r0, $r1);
    jmp(test_function);

    // Signal undefined-function error
  label(undefined_function);
    mov($r0, ea_m_fn());
    jmp(ea($tcb, SVC_undefined_function));

    // Signal type-error function
  label(not_function);
    mov($r0, ea_m_fn());
    mov($r1, Qfunction);     // BUGBUG: Should be callable.
    jmp(ea($tcb, SVC_type_error));
end_defun()

// .not-function
//      $r2 = name
//  See make_not_function_function
defun(".NOT-FUNCTION", 0, -1)
    mov($r1, Q(":NAME"));
    mov($r0, Q("SI:NOT-FUNCTION"));
    mov($rn, Fixnum::Encode(3));
    jmp(ea($tcb, SVC_error));
end_defun()

// .undefined-function
//      $r0 = name
defun(".UNDEFINED-FUNCTION", 0, -1)
    mov($r2, $r0);
    mov($r1, Kname);
    mov($r0, Q("UNDEFINED-FUNCTION"));
    mov($rn, Fixnum::Encode(3));
    jmp(ea($tcb, SVC_error));
end_defun()

// x86::.unbound-variable
//      $r0 = name
defun(".UNBOUND-VARIABLE", 0, -1)
    mov($r2, $r0);
    mov($r1, Kname);
    mov($r0, Q("UNBOUND-VARIABLE"));
    mov($rn, Fixnum::Encode(3));
    jmp(ea($tcb, SVC_error));
end_defun()

// .values*
//  This function is almost same as values but the last argument
//  must be list and elemetns of the list to values.
//
//  This function always sets CF=1 even if number of values is one.
//
defun(".VALUES*", 1, X86::X86Mach::Lambda_Parameters_Limit)
    Label exit_0, exit_1, exit_2, exit_3, exit_4;
    Label exit_more_than_5;
    Label exit_tbl;

    Label from_0, from_2, from_3, from_4, from_5;
    Label from_tbl;

    Label cont_3, cont_4;

    Label not_list;
    Label loop;
    Label loop_end;

    Label too_many_arguments;

    mov(ea_mv_value(0), $r0);
    mov(ea_mv_value(1), $r1);
    sub($rn, Fixnum::One);
    cmp($rn, Fixnum::Encode(4));
    jg(from_5);
    jmp(ea($rn, from_tbl));

  label(from_0); mov($r1, $r0); jmp(loop);
  label(from_2); mov($r1, $r2); jmp(loop);
  label(from_3); mov($r1, $r3); jmp(cont_3);
  label(from_4); mov($r1, $r4); jmp(cont_4);

  label(from_5); mov($r1, ea_mv_value($rn));
                 mov(ea_mv_value(4), $r4);
  label(cont_4); mov(ea_mv_value(3), $r3);
  label(cont_3); mov(ea_mv_value(2), $r2);

  label(loop);
    // loop_end if $r1 == nil
    cmp($r1, nil);
    je(loop_end);

    // Can we have one more value?
    cmp($rn, Fixnum::Encode(X86::X86Mach::Lambda_Parameters_Limit));
    jge(too_many_arguments);

    // not_list if $r1 isn't cons
    lea($r0, ea($r1, -Cons::Tag));
    and($r0, 7);
    jne(not_list);

    // $r0 = car($r1)
    mov($r0, ea($r1, offsetof(Cons, m_car) - Cons::Tag));

    // mv_value[$rn] = $r0
    mov(ea_mv_value($rn), $r0);

    // $rn += 1
    add($rn, Fixnum::One);

    // $r1 = cdr($r1)
    mov($r1, ea($r1,  offsetof(Cons, m_cdr) - Cons::Tag));
    jmp(loop);

  label(loop_end);
    cmp($rn, Fixnum::Encode(5));
    jge(exit_more_than_5);
    jmp(ea($rn, exit_tbl));

    // Set register values
  label(exit_more_than_5); mov($r4, ea_mv_value(4));
  label(exit_4); mov($r3, ea_mv_value(3));
  label(exit_3); mov($r2, ea_mv_value(2));
  label(exit_2); mov($r1, ea_mv_value(1));
  label(exit_1); mov($r0, ea_mv_value(0));
  label(exit_0);
    stc();
    ret();

  label(not_list);
    mov($r0, $r1);
    mov($r1, Qlist);
    call(ea($tcb, SVC_type_error));

  label(too_many_arguments);
    mov($r0, Qtoo_many_values);
    mov($rn, Fixnum::Encode(1));
    call(ea($tcb, SVC_error));

  label(from_tbl);
    dd(from_0);
    dd(loop);
    dd(from_2);
    dd(from_3);
    dd(from_4);

  label(exit_tbl);
    dd(exit_0);
    dd(exit_1);
    dd(exit_2);
    dd(exit_3);
    dd(exit_4);
end_defun()

defun("EQ", 2, 2)
    cmp(eax, edx);
    mov(eax, t);
    mov(edx, nil);
    cmovne(eax, edx);
    xor(edx, edx);      // xor may be faster than clc.
    ret();
end_defun()

defun("FUNCALL", 1, X86::X86Mach::Lambda_Parameters_Limit)
    Label do_call;
    Label got_symbol;
    Label not_function;
    Label undefined_function;

    mov(ea_mv_value(1), $r1);
    mov(ea_mv_value(2), $r2);
    mov(ea_mv_value(3), $r3);
    mov(ea_mv_value(4), $r4);

    // Is funcallable object?
    lea($r1, ea($r0, -Funcallable::Tag));
    test($r1, Funcallable::TagMask);
    je(do_call);

    // Is symbol?
    cmp($r0, nil);
    je(got_symbol);

    lea($r1, ea($r0, -Symbol::Tag));
    test($r1, Symbol::TagMask);
    jne(not_function);

 label(got_symbol);
    mov($r1, $r0);
    and($r1, ~Symbol::TagMask);
    mov($r1, ea($r1, offsetof(Symbol, m_function)));
    cmp($r1, nil);
    je(undefined_function);

    mov($r0, $r1);

  label(do_call);
    add($r0, sizeof(FunObj) - FunObj::Tag);
    mov(ea(esp, -4), $r0);
    sub($rn, Fixnum::One);  // -1 for fn
    mov(ea_m_n(), $rn);

    shr(ecx, 2);
    lea(esi, ea_mv_value(1));
    lea(edi, ea_mv_value(0));

    emit_op(op_REP);
    emit_op(op_MOVSD);

    mov($r0, ea_mv_value(0));
    mov($r1, ea_mv_value(1));
    mov($r2, ea_mv_value(2));
    mov($r3, ea_mv_value(3));
    mov($r4, ea_mv_value(4));
    mov($rn, ea_m_n());
    jmp(ea(esp, -4));

  label(not_function);
    mov($r1, Q("FUNCTION-DESIGNATOR"));
    call(ea($tcb, SVC_type_error));

  label(undefined_function);
    call(ea($tcb, SVC_undefined_function));
end_defun()


defun("IDENTITY", 1, 1)
    ret();
end_defun()

// See also "NULL".
defun("NOT", 1, 1)
    mov(edx, nil);
    cmp(eax, edx);
    mov(eax, Qt);
    cmovne(eax, edx);
    xor(edx, edx);      // xor may be faster than clc.
    ret();
end_defun()

defun("VALUES", 0, -1)
    stc();      // tell caller ecx holds number of values.
    ret();
end_defun()

// values-list
defun("VALUES-LIST", 1, 1)
    jmp(Q(".VALUES*"));
end_defun()

//////////////////////////////////////////////////////////////////////
//
//  CALL {some-function}
//  CMOVNC  $rn <- tcb.fixnum_1
//  MOV     $tcb.mv_value[4] <- $r4
//  MOV     $r4 <- nth
//  CALL    .NTH-VALUE
//
//  Returns nth value specified in $r4. If 0 <= $r4 <= 3, value is taken
//  from register, otherwise value is taken from tcb.mv_value[$r4].
//
defun(".NTH-VALUE", 0, -1)
    Label ret_reg_0;
    Label ret_reg_1;
    Label ret_reg_2;
    Label ret_reg_3;
    Label ret_reg_tbl;
    Label ret_mem;
    Label ret_nil;
    Label not_nth;

    test($r4, 3);
    jne(not_nth);

    test($r4, $r4);
    jl(not_nth);

    cmp($r4, Fixnum::Encode(X86::X86Mach::Multiple_Values_Limit));
    jge(not_nth);

    // Is nth greater than or equal to #values? (unsigned comparison)
    cmp($r4, $rn);
    jge(ret_nil);       // CF=0 if branch

    cmp($r4, Fixnum::Encode(4));
    jge(ret_mem);       // CF=0 if branch

    or($r4, $r4);       // CF=0
    jmp(ea($r4, ret_reg_tbl));

  label(ret_reg_0);
    ret();

  label(ret_reg_1);
    mov($r0, $r1);
    ret();

  label(ret_reg_2);
    mov($r0, $r2);
    ret();

  label(ret_reg_3);
    mov($r0, $r3);
    ret();

  label(ret_mem);
    mov($r0, ea_mv_value($r4));
    ret();

  label(ret_nil);
    mov($r0, nil);
    ret();

  label(not_nth);
    mov($r0, $r4);
    mov(
        $r1,
        list(Qinteger,
                0,
                Fixnum::Encode(X86::X86Mach::Multiple_Values_Limit - 1) ) );
    jmp(ea($tcb, SVC_type_error));

  label(ret_reg_tbl);
    dd(ret_reg_0);
    dd(ret_reg_1);
    dd(ret_reg_2);
    dd(ret_reg_3);
end_defun()

} // X86Builder::build_05_Control

} // Boot
