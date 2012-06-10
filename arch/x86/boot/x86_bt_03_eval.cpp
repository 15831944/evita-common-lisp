#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - boot - 03 Eval
// arch/x86/boot/x86_bt_03_eval.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86/boot/x86_bt_03_eval.cpp#7 $
//
// Description:
//  This file contains functions for "03 Evaluation and Compilation".
//
#include "./x86_bt_builder.h"

#include "../kernel/x86_ke_thread.h"

namespace Boot
{
using namespace X86;

void X86Builder::build_03_Eval()
{

Q(".ARITY-ERROR");
Q(".CHECK-KEYS");
Q(".PARSE-KEYS");
Q(".RESTIFY");
Q(".STACK-RESTIFY");

//  .arity-error
defun(".ARITY-ERROR", 0, -1)
    frame_type(Restify, 0);

    mov(ea_mv_value(0), $r0);
    mov(ea_mv_value(1), $r1);
    mov(ea_mv_value(2), $r2);
    mov(ea_mv_value(3), $r3);
    mov(ea_mv_value(4), $r4);

    // Set class of condition to be signaled.
    mov($r0, Qtoo_many_arguments);
    mov($r1, Qtoo_few_arguments);
    cmovl($r0, $r1);

    // Get callee and store it into %thread.fn
    pop($r2);         // $r2 <- return address of callee
    and($r2, ~15);
    sub($r2, sizeof(FunObj));

    Label callee_done;
    Label callee_loop;
    Label callee_next;

    // Avoid FunObj::Cookie in 16 byte alignment.
    emit_op(op_NOP);

  label(callee_loop);
    cmp(ea($r2, offsetof(FunObj, m_nCookie)), FunObj::Cookie);
    je(callee_done);
  label(callee_next);
    sub($r2, 16);       // 16 is alignment of function object
    jmp(callee_loop);

  label(callee_done);
    add($r2, Funcallable::Tag); // $r2 = callee

    ////////////////////////////////////////////////////////////
    //
    // Make restify frame
    //  argument list on stack => $r4
    //
    Label args_done;
    Label args_loop;

    mov($r1, esp);    // $r1 <= RP
    mov($r4, nil);

    test($rn, $rn);
    jz(args_done);

    // Adjust stack for align 8
    mov(ea(esp, -4), $r4);  // make align pad gc-able
    and(esp, ~7);

    lea($r3, ea_mv_value($rn));

  label(args_loop);
    sub($r3, sizeof(Val));
    push($r4);            // set cdr <- $r4
    push(ea($r3));        // set car <- [esi]
    lea($r4, ea(esp, Tag_Cons8));
    sub($rn, sizeof(Val));
    jnz(args_loop);

  label(args_done);
    // $r4 = args

    push($r1);        // RP(RA address pointer)

    ////////////////////////////////////////////////////////////
    //
    // Call function "error".
    //
    mov($r1, Kfunction);
    mov($r3, Karguments);
    mov($rn, Fixnum::Encode(5));
    call(ea($tcb, SVC_error));
end_defun() // x86::.arity_error


//////////////////////////////////////////////////////////////////////
//
// .check-keys
//  $r0 = rest (list)
//  $r1 = keys (simple-vector)
defun(".CHECK-KEYS", 2, 2)
    frame_type(Fixed, 8);

    Label check_end;
    Label check_got;
    Label check_test;

    sub(esp, 8);
    mov(ea(esp, 0), nil);   // allow-other-keys
    mov(ea(esp, 4), $r0); // rest
    mov($r2, $r1);      // keys
    mov($r1, $r0);      // save rest
    mov($r3, $r0);      // runner
    jmp(check_test);

  label_def(check_loop);
    mov($r0, ea($r3, offsetof(Cons, m_car) - Cons::Tag));
    mov($r3, ea($r3, offsetof(Cons, m_cdr) - Cons::Tag));

    // Is runner cons?
    lea($r4, ea($r3, -Cons::Tag));
    and($r4, Cons::Tag >> 1);
    jne(check_end);

    // Is key :allow-other-keys?
    cmp($r0, Q(":ALLOW-OTHER-KEYS"));
    je(check_got);

    mov($r3, ea($r3, offsetof(Cons, m_cdr) - Cons::Tag));

  label(check_test);
    // Is runner cons?
    lea($r0, ea($r3, -Cons::Tag));
    and($r0, Cons::Tag >> 1);
    je(check_loop);
    jmp(check_end);

  label(check_got);
    mov($r0, ea($r3, offsetof(Cons, m_car) - Cons::Tag));
    mov(ea(esp, 0), $r0);

  label(check_end);

    ////////////////////////////////////////////////////////////
    //
    // Parse keywords argumetns
    //
    Label parse_$rend;
    Label parse_$rnot_cons;
    Label parse_$rodd;
    Label parse_$rstep;
    Label parse_$rtest;

    Label parse_key_got;
    Label parse_key_invalid;
    Label parse_key_test;

    mov($r3, $r1);  // runner
    xor($r0, $r0);  // flags
    mov(ea_mv_value(0), $r0);

    // $r1 = end of keys
    mov($r1, ea($r2,
        offsetof(SimpleVector, m_length) - SimpleVector::Tag ) );

    lea($r1, ea($r1,
        offsetof(SimpleVector, mv_element) - SimpleVector::Tag, $r2 ) );

    // $r2 = start of keys
    add($r2, offsetof(SimpleVector, mv_element) - SimpleVector::Tag);

    jmp(parse_$rtest);

  label_def(parse_$rtop);
    lea($r4, ea($r3, -Cons::Tag));
    and($r4, Cons::Tag >> 1);
    jne(parse_$rnot_cons);

    mov($rn, ea($r3, offsetof(Cons, m_car) - Cons::Tag));
    mov($r3, ea($r3, offsetof(Cons, m_cdr) - Cons::Tag));

    cmp($r3, nil);
    je(parse_$rodd);

    lea($r4, ea($r3, -Cons::Tag));
    and($r4, Cons::Tag >> 1);
    jne(parse_$rnot_cons);

    ////////////////////////////////////////////////////////////
    //
    // Check key in table
    //  $r0   bit
    //  $r1   end of keys.mv_element
    //  $r2   start keys.mv_element
    //  $r3   runner
    //  $r4   ptr to keys.mv_element
    //  $rn   key
    //
    mov($r4, $r2);
    mov($r0, Fixnum::One);
    jmp(parse_key_test);

  label_def(parse_key_loop);
    cmp($rn, ea($r4));
    je(parse_key_got);

    shl($r0, 1);
    add($r4, sizeof(Val));

  label(parse_key_test);
    cmp($r4, $r1);
    jne(parse_key_loop);

    // found unrecognized key
    cmp($rn, Q(":ALLOW-OTHER-KEYS")); // key == :allow-other-keys
    je(parse_$rstep);

    cmp(ea(esp, 0), nil);   // allow-other-keys?
    jne(parse_$rstep);

    // Is key symbol?
    lea($r0, ea($rn, -Symbol::Tag));
    and($r0, 13);
    jne(parse_key_invalid);

    // (error 'si:unrecognized-keyword-argument :key key :keys keys)
    lea($r4,
        ea($r2,
           -static_cast<Int>(offsetof(SimpleVector, mv_element)) +
           SimpleVector::Tag ) );

    mov($r3, Q(":KEYS"));
    mov($r2, $rn);
    mov($r1, Q(":KEY"));
    mov($r0, Qunrecognized_keyword_argument);
    mov($rn, Fixnum::Encode(5));
    call(ea($tcb, SVC_error));

  label(parse_key_invalid);
    mov($r4, Qsymbol);
    mov($r3, Kexpected_type);
    mov($r2, $rn);
    mov($r1, Kdatum);
    mov($r0, Qinvalid_keyword_argument);
    mov($rn, Fixnum::Encode(5));
    call(ea($tcb, SVC_error));

  label(parse_key_got);
    test(ea_mv_value(0), $r0);    // Do we have the key?
    jne(parse_$rstep);

    // get key value
    sub($r4, $r2);
    mov($rn, ea($r3, offsetof(Cons, m_car) - Cons::Tag));

    // store to thread.mv_value
    mov(ea($tcb, offsetof(Thread, mv_value[1]), $r4), $rn);

    // Set flag for key
    or(ea_mv_value(0), $r0);

    ////////////////////////////////////////////////////////////
    //
    // arg loop
    //
  label(parse_$rstep);
    mov($r3, ea($r3, offsetof(Cons, m_cdr) - Cons::Tag));

  label(parse_$rtest);
    cmp($r3, nil);
    jne(parse_$rtop);

  label(parse_$rend);
    mov($rn, ea($r2, -4));
    add($rn, Fixnum::One);

    mov($r0, ea_mv_value(0));
    mov($r1, ea_mv_value(1));
    mov($r2, ea_mv_value(2));
    mov($r3, ea_mv_value(3));
    mov($r4, ea_mv_value(4));

    add(esp, 8);
    stc();
    ret();

  label(parse_$rodd);
    cmp(ea(esp, 0), nil);   // allow-other-keys?
    jne(parse_$rend);

    mov($r0, Qodd_number_of_keyword_arguments);
    mov($r1, Q(":ARGUMENTS"));
    mov($r2, ea(esp, 4)); // rest
    mov($rn, Fixnum::Encode(3));
    call(ea($tcb, SVC_error));

  label(parse_$rnot_cons);
    cmp(ea(esp, 0), nil);   // allow-other-keys?
    jne(parse_$rend);

    mov($r0, ea(esp, 4)); // rest
    mov($r1, Qcons);
    call(ea($tcb, SVC_type_error));
end_defun() // check-keys


////////////////////////////////////////////////////////////
//
// .parse-keys
//  $r0 = rest (list)
//  $r1 = keys (simple-vector)
defun(".PARSE-KEYS", 2, 2);
    Label parse_key_loop_got;
    Label parse_key_loop_test;
    Label parse_key_loop_loop;
    Label parse_$rloop_end;
    Label parse_$rloop_odd;
    Label parse_$rloop_step;
    Label parse_$rloop_test;

    // $r2 = start of keys
    lea($r2, ea($r1,
        offsetof(SimpleVector, mv_element) - SimpleVector::Tag ) );

    // $r3 = end of keys
    mov($r3, ea($r1,
        offsetof(SimpleVector, m_length) - SimpleVector::Tag ) );
    add($r3, $r2);

    mov($r1, $r0);      // runner = rest

    xor($r4, $r4);
    mov(ea_mv_value(0), $r4);

    jmp(parse_$rloop_test);

  label(parse_key_loop_got);
    test(ea_mv_value(0), $rn);    // Do we already have the key?
    jne(parse_$rloop_step);       // Yes, we have.

    or(ea_mv_value(0), $rn);      // Set flag for the key

    // get value from list
    mov($r0, ea($r1, offsetof(Cons, m_car) - Cons::Tag));

    // save it to thread.mv_value
    sub($r4, $r2);
    mov(ea($tcb, offsetof(Thread, mv_value[1]), $r4), $r0);
    jmp(parse_$rloop_step);

    ////////////////////////////////////////////////////////////
    //
    // arg loop
    //
  label_def(parse_$rloop);
    mov($r0, ea($r1, offsetof(Cons, m_car) - Cons::Tag));
    mov($r1, ea($r1, offsetof(Cons, m_cdr) - Cons::Tag));

    lea($r4, ea($r1, -Cons::Tag));
    and($r4, Cons::Tag >> 1);
    jne(parse_$rloop_odd);

    ////////////////////////////////////////////////////////////
    //
    // parse_key_loop
    //  $r0   key
    //  $r2   start of key
    //  $r3   end of key
    //  $r4   key runner
    //  reg_n   bit
    //
    //  $r1   arg runner
    mov($rn, Fixnum::One);        // bit = 1
    mov($r4, $r2);              // key_runner = key_start
    jmp(parse_key_loop_test);

  label(parse_key_loop_loop);
    cmp($r0, ea($r4));          // key == *key_runner
    je(parse_key_loop_got);

    shl($rn, 1);                  // bit <<= 1
    add($r4, sizeof(Val));        // key_runner++

  label(parse_key_loop_test);
    cmp($r4, $r3);              // key_runner == key_start
    jne(parse_key_loop_loop);

    ////////////////////////////////////////////////////////////
    //
    // arg loop
    //
  label(parse_$rloop_step);
    mov($r1, ea($r1, offsetof(Cons, m_cdr) - Cons::Tag));

  label(parse_$rloop_test);
    lea($r0, ea($r1, -Cons::Tag));
    and($r0, Cons::Tag >> 1);
    je(parse_$rloop);

    ////////////////////////////////////////////////////////////
    //
    // End of parse key
    //
  label(parse_$rloop_odd);
  label(parse_$rloop_end);
    mov($rn, ea($r2, -4));
    add($rn, Fixnum::One);

    mov($r0, ea_mv_value(0));
    mov($r1, ea_mv_value(1));
    mov($r2, ea_mv_value(2));
    mov($r3, ea_mv_value(3));
    mov($r4, ea_mv_value(4));

    stc();
    ret();
end_defun() // parse-keys

////////////////////////////////////////////////////////////
//
// .restify
//
// Arguments and Values:
//   m_fn   <- start position of rest parameters.
//   ecx    <- nparams (in fixnum)
//   eax    -> mv_value[start]
//   all registers are preserved.
//
defun(".RESTIFY", 0, -1);
    Label alloc_cons_area;
    Label cont;
    Label exit;
    Label loop;
    Label rest_0;
    Label restore_eax;

    mov(ea_m_n(), $rn);
    mov(ea_mv_value(4), $r4);
    mov(ea_mv_value(3), $r3);
    mov(ea_mv_value(2), $r2);
    mov(ea_mv_value(1), $r1);
    mov(ea_mv_value(0), $r0);

    // How many number of rest parameters do we get?
    mov(ecx, ea_m_fn());
    mov(eax, ea_m_n());
    mov(esi, eax);
    sub(eax,  ecx);
    test(eax, eax);
    jle(rest_0);

    // pThread->m_rgcbObject[T_Cons] += cElts * sizeof(SCons)
    add(eax, eax);
    add(ea_mv_size(Cons::Tag), eax);

    // pArea = pThread->m_pConsArea
    mov(ebx, ea($tcb, offsetof(Context, m_pConsArea)));

 label(cont);
    // edx <- pArea->m_ofsFree
    // edi <- pArea->m_ofsFree + cbElts
    mov(edx, ea(ebx, offsetof(Area, m_ofsFree)));
    lea(edi, ea(edx, 0, eax));

    // pArea + pArea->m_ofsFree + cbElts > pArea->m_cbArea
    cmp(edi, ea(ebx, offsetof(Area, m_cbArea)));
    ja(alloc_cons_area);

    mov(ea(ebx, offsetof(Area, m_ofsFree)), edi);
    add(edi, ebx);
    add(edx, ebx);

    lea(esi, ea($tcb, offsetof(Context, mv_value), esi));
    mov(eax, nil);

 label(loop);
    // eax <- last cons
    // edx <- pArea + pArea->m_ofsFree
    // edi <- pArea + pArea->m_ofsFree + cbElts
    // esi <- &pThread->m_rgxValue[i]
    sub(esi, sizeof(Val));
    sub(edi, sizeof(Cons));
    mov(ea(edi, offsetof(Cons, m_cdr)), eax);
    mov(eax, ea(esi));
    mov(ea(edi, offsetof(Cons, m_car)), eax);
    lea(eax, ea(edi, Cons::Tag));
    cmp(edi, edx);
    jne(loop);

 label(exit);
    // eax -> list of rest parameters
    // ecx -> start position of rest parameter
    mov(ea($tcb, offsetof(Context, mv_value), ecx), eax);
    mov($r0, ea_mv_value(0));
    mov($r1, ea_mv_value(1));
    mov($r3, ea_mv_value(3));
    mov($r2, ea_mv_value(2));
    mov($r4, ea_mv_value(4));
    mov($rn, ea_m_n());
    ret();

label(rest_0);
    mov(eax, nil);
    jmp(exit);

label(alloc_cons_area);
    // eax <- netls * sizeof(SCons);
    // ecx <- start position
    // ebx -> pArea
    mov($r0, Fixnum::Encode(sizeof(Cons)));
    call(ea($tcb, SVC_alloc_cons_area));
    mov(ebx, eax);
    mov(ecx, ea_m_fn());
    mov(eax, ea_m_n());
    mov(esi, eax);
    sub(eax, ecx);;
    add(eax, eax);
    jmp(cont);
end_defun(); // restify

////////////////////////////////////////////////////////////
//
//  .stack-restify
//
//  Arguments and Values:
//    m_fn  <- start position of rest parameters.
//    ecx   <- nparams (in fixnum)
//    eax   -> mv_value[start]
//    all registers are preserved.
// 
//  Restify Frame:
//  (low)       +----------------+
//      ESP  -> |    slot[0]     |
//              +----------------+
//                    ...
//              +----------------+
//              | slot[cSlots-1] |
//              +----------------+
//              |    RP  o-------+------+
//              +----------------+      |
//              |    rest[0]     |      |
//              +----------------+      |
//              |    next        |      |
//              +----------------+      |
//              |    rest[1]     |      |
//              +----------------+      |
//              |    next        |      |
//              +----------------+      |
//                    ...               |
//              +----------------+      |
//              | rest[cRests-1] |      |
//              +----------------+      |
//              |    nil         |      |
//              +----------------+      |
//              |    pad[0]      |      |
//              +----------------+      |
//              |  RA of caller  |  <---+
//              +----------------+
//  (high)
defun(".STACK-RESTIFY", 0, -1)
    Label save_reg_0;
    Label save_reg_1;
    Label save_reg_2;
    Label save_reg_3;
    Label save_reg_4;
    Label save_reg_5;

    Label restore_1;
    Label restore_2;
    Label restore_3;
    Label restore_4;
    Label restore_5;

    Label exit;
    Label exit_tbl;
    Label rest_0;
    Label loop;

    Label save_reg_tbl;

    mov(ea_m_n(), $rn);

    // Uncache register parameters.
    cmp($rn, Fixnum::Encode(5));
    ja(save_reg_5);
    jmp(ea($rn, save_reg_tbl));

  label(save_reg_5); mov(ea_mv_value(4), $r4);
  label(save_reg_4); mov(ea_mv_value(3), $r3);
  label(save_reg_3); mov(ea_mv_value(2), $r2);
  label(save_reg_2); mov(ea_mv_value(1), $r1);
  label(save_reg_1); mov(ea_mv_value(0), $r0);

  label(save_reg_0);

    // How many number of rest parameters do we get?
    mov(eax, ecx);
    mov(esi, ecx);
    mov(ecx, ea_m_fn());
    sub(eax, ecx);
    test(eax, eax);
    jle(rest_0);


    // number of arguments -> esi
    // esi <- &pThread->mv_value[n]
    lea(esi, ea($tcb, offsetof(Context, mv_value), esi));

    add(eax, eax);

    // esp-> RA
    //       caller   <- esp
    pop(edx);           // edx <- RA
    mov(ebx, esp);      // ebx <- esp

    // Align esp to 8
    mov(ea(esp, -4), eax);
    and(esp, ~7);

    // esp-> RA
    //       RP o---------+
    // edx-> rest[0]      |
    //       next[0]      |
    //       rest[1]      |
    //       next[1]      |
    //       ...          |
    //       rest[n-1]    |
    //       nil          |
    // edi-> pad[0]       |
    // ebx-> caller <-----+
    mov(edi, esp);
    sub(esp, eax);
    mov(eax, esp);
    push(ebx);  // RP
    push(edx);  // RA

    mov(edx, eax);
    mov(eax, nil);

    // From the last cons to the first cons.
  label(loop);
    sub(edi, sizeof(Cons));
    mov(ea(edi, offsetof(Cons, m_cdr)), eax);
    sub(esi, sizeof(Val));
    mov(eax, ea(esi));
    mov(ea(edi, offsetof(Cons, m_car)), eax);

    lea(eax, ea(edi, Cons::Tag));

    cmp(edi, edx);
    jne(loop);

  label(exit);
    // eax <- list of rest parameters
    // ecx <- start position of rest parameter
    mov(ea($tcb, offsetof(Context, mv_value), ecx), eax);

    mov($r0, ea_mv_value( 0));
    mov($r1, ea_mv_value( 1));
    mov($r2, ea_mv_value( 2));
    mov($r3, ea_mv_value( 3));
    mov($r4, ea_mv_value( 4));

    mov($rn, ea_m_n());
    ret();

  label(rest_0);
    //                    RA      <- esp
    // esp' -> RA         RP = esp+8
    //         caller     caller
    mov(eax, ea(esp));
    sub(esp, 4);
    mov(ea(esp), eax);
    lea(eax, ea(esp, 8));
    mov(ea(esp, 4), eax);
    mov(eax, nil);
    jmp(exit);

    align(4);
  label(save_reg_tbl);
    dd(save_reg_0);  // #x00
    dd(save_reg_1);  // #x04
    dd(save_reg_2);  // #x08
    dd(save_reg_3);  // #x0C
    dd(save_reg_4);  // #x10
    dd(save_reg_5);  // #x10
end_defun(); // stack-restify

} // X86Builder::build_03_Eval()

} // Boot
