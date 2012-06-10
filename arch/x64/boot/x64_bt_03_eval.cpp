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
// @(#)$Id: //proj/evcl3/mainline/arch/x64/boot/x64_bt_03_eval.cpp#7 $
//
// Description:
//  This file contains functions for "03 Evaluation and Compilation".
//
#include "./x64_bt_builder.h"

#include "../kernel/x64_ke_thread.h"

namespace Boot
{

using namespace X64;

void X64Builder::build_03_Eval()
{

Q(".ARITY-ERROR");
Q(".CHECK-KEYS");
Q(".PARSE-KEYS");
Q(".RESTIFY");
Q(".STACK-RESTIFY");

//////////////////////////////////////////////////////////////////////
//
//  .arity-error
//
//          +-------------------+
//   esp-16 | RA of .arity-error|               (rsp % 16 = 8)
//          +-------------------+
//    esp-8 |       FP          |               (rsp % 16 = 0)
//          +-------------------+
//    esp-0 |       RP       o--+---+
//          +-------------------+   |
//          |      rest[0]      |   |
//          +-------------------+   |
//          |       cdr[0]      |   |
//          +-------------------+   |
//                  ...             |
//          +-------------------+   |
//          |     rest[k-1]     |   |
//          +-------------------+   |
//          |       nil         |   |
//          +-------------------+   |
//          |  pad for align 16 |   |           (rsp % 16 = 0)
//          +-------------------+   |
//          |    RA of caller   |<--+ = RP      (rsp % 16 = 8)
//          +-------------------+
//
// See Also:
//  .arity-error
//
// Description:
// This function is called by callee before establish function frame. We
// consider arity checking is part of function invocation rather than normal
// processing.
//
// Stack pointer is NOT aligned 16 byte at entry of this function, e.g.
// rsp % 16 = 0. On normal function entry, rsp % 16 = 8 as regarding to
// calling convention.
//
// At exit or control transfer to function error, function frame of
// .arity-error is established as caller calls .arity-error instead of
// original callee.
//
defun_(Q(".ARITY-ERROR"), 0, -1, Restify, 16)
    mov(ea_mv_value(0), $r0);
    mov(ea_mv_value(1), $r1);
    mov(ea_mv_value(2), $r2);
    mov(ea_mv_value(3), $r3);
    mov(ea_mv_value(4), $r4);
    mov(ea_mv_value(5), $r5);
    mov(ea_mv_value(6), $r6);
    mov(ea_mv_value(7), $r7);
    mov(ea_mv_value(8), $r8);
    mov(ea_mv_value(9), $r9);

    // Set class of condition to be signaled.
    mov($r0, Qtoo_many_arguments);
    mov($r1, Qtoo_few_arguments);
    cmovl($r0, $r1);

    // Get callee and store it into %thread.fn
    pop($r2);           // $r2 <- return address of callee
    and($r2, ~15);
    sub($r2, sizeof(FunObj));

    Label callee_done;
    Label callee_loop;
    Label callee_next;

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

    mov($r1, $sp);          // $r1 <- RP
    push($rnil);            // pad for align 16
    mov($r4, nil);

    test($rn, $rn);
    jz(args_done);

    lea($r3, ea_mv_value($rn));

  label(args_loop);
    sub($r3, sizeof(Val));
    push($r4);            // set cdr <- $r4
    push(ea($r3));        // set car <- [esi]
    lea($r4, ea($sp, Cons::TagX));
    sub($rn, sizeof(Val));
    jnz(args_loop);

  label(args_done);
    // $r4 = args

    push($r1);      // RP(RA address pointer)
    push($rfp);     // FP
    lea($rfp, ea(rcx, 16));

    ////////////////////////////////////////////////////////////
    //
    // Call function "error".
    //
    mov($r1, Kfunction);
    mov($r3, Karguments);
    mov($rn, Fixnum::Encode(5));
    call(ea($rtcb, SVC_error));
end_defun() // .arity_error


//////////////////////////////////////////////////////////////////////
//
// .check-keys
//  $r0 = rest (list)
//  $r1 = keys (simple-vector)
defun(".CHECK-KEYS", 2, 2)
    Label check_end;
    Label check_got;
    Label check_test;

    sub(rsp, 8);

    mov($r5, $rnil);    // allow-other-keys
    mov($r6, $r0);      // rest (for error)
    mov($r2, $r1);      // keys
    mov($r1, $r0);      // save rest
    mov($r3, $r0);      // runner
    mov($r8, Q(":ALLOW-OTHER-KEYS"));
    jmp(check_test);

  label_def(check_loop);
    mov($r0, ea($r3, offsetof(Cons, m_car) - Cons::TagX));
    mov($r3, ea($r3, offsetof(Cons, m_cdr) - Cons::TagX));

    // Is runner cons?
    lea($r4, ea($r3, -Cons::TagX));
    and($r4, 15);
    jne(check_end);

    // Is key :allow-other-keys?
    cmp($r0, $r8);
    je(check_got);

    mov($r3, ea($r3, offsetof(Cons, m_cdr) - Cons::TagX));

  label(check_test);
    // Is runner cons?
    lea($r0, ea($r3, -Cons::TagX));
    and($r0, 15);
    je(check_loop);
    jmp(check_end);

  label(check_got);
    mov($r0, ea($r3, offsetof(Cons, m_car) - Cons::TagX));
    mov($r5, $r0);

  label(check_end);

    ////////////////////////////////////////////////////////////
    //
    // Parse keywords argumetns
    //
    Label parse_arg_end;
    Label parse_arg_not_cons;
    Label parse_arg_odd;
    Label parse_arg_step;
    Label parse_arg_test;

    Label parse_key_got;
    Label parse_key_invalid;
    Label parse_key_test;

    mov($r3, $r1);  // runner
    xor($r7, $r7);  // flags

    // $r1 = end of keys
    mov($r1, ea($r2,
        offsetof(SimpleVector, m_length) - SimpleVector::Tag ) );

    lea($r1, ea($r1,
        offsetof(SimpleVector, mv_element) - SimpleVector::Tag, $r2 ) );

    // $r2 = start of keys
    add($r2, offsetof(SimpleVector, mv_element) - SimpleVector::Tag);

    jmp(parse_arg_test);

  label_def(parse_arg_top);
    lea($r4, ea($r3, -Cons::TagX));
    and($r4, 15);
    jne(parse_arg_not_cons);

    mov($rn, ea($r3, offsetof(Cons, m_car) - Cons::TagX));
    mov($r3, ea($r3, offsetof(Cons, m_cdr) - Cons::TagX));

    cmp($r3, $rnil);
    je(parse_arg_odd);

    lea($r4, ea($r3, -Cons::TagX));
    and($r4, 15);
    jne(parse_arg_not_cons);

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
    cmp($rn, $r8);      // key == :allow-other-keys
    je(parse_arg_step);

    cmp($r5, $rnil);   // allow-other-keys?
    jne(parse_arg_step);

    // Is key symbol?
    lea($r0, ea($rn, -Symbol::Tag));
    and($r0, 11);
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
    call(ea($rtcb, SVC_error));

  label(parse_key_invalid);
    mov($r4, Qsymbol);
    mov($r3, Kexpected_type);
    mov($r2, $rn);
    mov($r1, Kdatum);
    mov($r0, Qinvalid_keyword_argument);
    mov($rn, Fixnum::Encode(5));
    call(ea($rtcb, SVC_error));

  label(parse_key_got);
    test($r7, $r0);    // Do we have the key?
    jne(parse_arg_step);

    // get key value
    sub($r4, $r2);
    mov($rn, ea($r3, offsetof(Cons, m_car) - Cons::TagX));

    // store to thread.mv_value
    mov(ea($rtcb, offsetof(Thread, mv_value[1]), $r4), $rn);

    // Set flag for key
    or($r7, $r0);

    ////////////////////////////////////////////////////////////
    //
    // arg loop
    //
  label(parse_arg_step);
    mov($r3, ea($r3, offsetof(Cons, m_cdr) - Cons::TagX));

  label(parse_arg_test);
    cmp($r3, $rnil);
    jne(parse_arg_top);

  label(parse_arg_end);
    mov($rn, ea($r2, -8));
    add($rn, Fixnum::One);

    mov($r0, $r7);
    mov($r1,  ea_mv_value(1));
    mov($r2,  ea_mv_value(2));
    mov($r3,  ea_mv_value(3));
    mov($r4,  ea_mv_value(4));
    mov($r5,  ea_mv_value(5));
    mov($r6,  ea_mv_value(6));
    mov($r7,  ea_mv_value(7));
    mov($r8,  ea_mv_value(8));
    mov($r9,  ea_mv_value(9));

    add(rsp, 8);
    stc();
    ret();

  label(parse_arg_odd);
    cmp($r5, $rnil);   // allow-other-keys?
    jne(parse_arg_end);

    mov($r0, Qodd_number_of_keyword_arguments);
    mov($r1, Q(":ARGUMENTS"));
    mov($r2, $r6); // rest
    mov($rn, Fixnum::Encode(3));
    call(ea($rtcb, SVC_error));

  label(parse_arg_not_cons);
    cmp($r5, $rnil);   // allow-other-keys?
    jne(parse_arg_end);

    mov($r0, $r6); // rest
    mov($r1, Qcons);
    call(ea($rtcb, SVC_type_error));
end_defun() // check-keys


////////////////////////////////////////////////////////////
//
// .parse-keys
//      $r0 = rest (list)
//      $r1 = keys (simple-vector)
//
defun(".PARSE-KEYS", 2, 2);
    Label parse_key_loop_got;
    Label parse_key_loop_test;
    Label parse_key_loop_loop;
    Label parse_arg_loop_end;
    Label parse_arg_loop_odd;
    Label parse_arg_loop_step;
    Label parse_arg_loop_test;

    sub(rsp, 8);

    // $r2 = start of keys
    lea($r2, ea($r1,
        offsetof(SimpleVector, mv_element) - SimpleVector::Tag ) );

    // $r3 = end of keys
    mov($r3, ea($r1,
        offsetof(SimpleVector, m_length) - SimpleVector::Tag ) );
    add($r3, $r2);

    mov($r1, $r0);      // runner = rest

    xor($r5, $r5);      // clear flags

    jmp(parse_arg_loop_test);

  label(parse_key_loop_got);
    test($r5, $rn);             // Do we already have the key?
    jne(parse_arg_loop_step);   // Yes, we have.

    or($r5, $rn);               // Set flag for the key

    // get value from list
    mov($r0, ea($r1, offsetof(Cons, m_car) - Cons::TagX));

    // save it to thread.mv_value
    sub($r4, $r2);
    mov(ea($rtcb, offsetof(Thread, mv_value[1]), $r4), $r0);
    jmp(parse_arg_loop_step);

    ////////////////////////////////////////////////////////////
    //
    // arg loop
    //
  label_def(parse_arg_loop);
    mov($r0, ea($r1, offsetof(Cons, m_car) - Cons::TagX));
    mov($r1, ea($r1, offsetof(Cons, m_cdr) - Cons::TagX));

    lea($r4, ea($r1, -Cons::Tag));
    and($r4, 15);
    jne(parse_arg_loop_odd);

    ////////////////////////////////////////////////////////////
    //
    // parse_key_loop
    //  $r0   key
    //  $r2   start of key
    //  $r3   end of key
    //  $r4   key runner
    //  $r5   flags
    //  $rn   bit
    //
    //  $r1   arg runner
    mov($rn, Fixnum::One);        // bit = 1
    mov($r4, $r2);              // key_runner = key_start
    jmp(parse_key_loop_test);

  label(parse_key_loop_loop);
    cmp($r0, ea($r4));          // key == *key_runner
    je(parse_key_loop_got);

    shl($rn, 1);                  // bit <<= 1
    add($r4, sizeof(Val));      // key_runner++

  label(parse_key_loop_test);
    cmp($r4, $r3);              // key_runner == key_start
    jne(parse_key_loop_loop);

    ////////////////////////////////////////////////////////////
    //
    // arg loop
    //
  label(parse_arg_loop_step);
    mov($r1, ea($r1, offsetof(Cons, m_cdr) - Cons::TagX));

  label(parse_arg_loop_test);
    lea($r0, ea($r1, -Cons::Tag));
    and($r0, 15);
    je(parse_arg_loop);

    ////////////////////////////////////////////////////////////
    //
    // End of parse key
    //
  label(parse_arg_loop_odd);
  label(parse_arg_loop_end);
    mov($rn, ea($r2, -8));      // keys.m_length
    add($rn, Fixnum::One);

    mov($r0,  $r5);
    mov($r1,  ea_mv_value(1));
    mov($r2,  ea_mv_value(2));
    mov($r3,  ea_mv_value(3));
    mov($r4,  ea_mv_value(4));
    mov($r5,  ea_mv_value(5));
    mov($r6,  ea_mv_value(6));
    mov($r7,  ea_mv_value(7));
    mov($r8,  ea_mv_value(8));
    mov($r9,  ea_mv_value(9));

    add(rsp, 8);

    stc();
    ret();
end_defun() // parse-keys


////////////////////////////////////////////////////////////
//
// .restify
//
// Arguments and Values:
//   m_fn   <- start position of rest parameters.
//   $rn    <- nparams (in fixnum)
//   $r0    -> mv_value[start]
//   all registers are preserved.
//
defun_(Q(".RESTIFY"), 0, -1, Fixed, 32);
    Label alloc_cons_area;
    Label cont;
    Label exit;
    Label loop;
    Label rest_0;
    Label restore_rax;

    mov(ea_m_n(),        $rn);
    mov(ea_mv_value(0), $r0);
    mov(ea_mv_value(1), $r1);
    mov(ea_mv_value(2), $r2);
    mov(ea_mv_value(3), $r3);
    mov(ea_mv_value(4), $r4);
    mov(ea_mv_value(5), $r5);
    mov(ea_mv_value(6), $r6);
    mov(ea_mv_value(7), $r7);
    mov(ea_mv_value(8), $r8);
    mov(ea_mv_value(9), $r9);

    // How many number of rest parameters do we get?
    mov($rn, ea_m_fn());
    mov(rax, ea_m_n());
    mov(rsi, rax);
    sub(rax,  $rn);
    test(rax, rax);
    jle(rest_0);

    // pThread->m_rgcbObject[T_Cons] += cElts * sizeof(SCons)
    add(rax, rax);
    add(ea_mv_size(Cons::Tag), rax);

    // pArea = pThread->m_pConsArea
    mov(rbx, ea($rtcb, offsetof(Context, m_pConsArea)));

 label(cont);
    // rdx <- pArea->m_ofsFree
    // rdi <- pArea->m_ofsFree + cbElts
    mov(rdx, ea(rbx, offsetof(Area, m_ofsFree)));
    lea(rdi, ea(rdx, 0, rax));

    // pArea + pArea->m_ofsFree + cbElts > pArea->m_cbArea
    cmp(rdi, ea(rbx, offsetof(Area, m_cbArea)));
    ja(alloc_cons_area);

    mov(ea(rbx, offsetof(Area, m_ofsFree)), rdi);
    add(rdi, rbx);
    add(rdx, rbx);

    lea(rsi, ea($rtcb, offsetof(Context, mv_value), rsi));
    mov(rax, $rnil);

 label(loop);
    // rax <- last cons
    // rdx <- pArea + pArea->m_ofsFree
    // rdi <- pArea + pArea->m_ofsFree + cbElts
    // rsi <- &pThread->m_rgxValue[i]
    sub(rsi, sizeof(Val));
    sub(rdi, sizeof(Cons));
    mov(ea(rdi, offsetof(Cons, m_cdr)), rax);
    mov(rax, ea(rsi));
    mov(ea(rdi, offsetof(Cons, m_car)), rax);
    lea(rax, ea(rdi, Cons::TagX));
    cmp(rdi, rdx);
    jne(loop);

 label(exit);
    // $rax -> list of rest parameters
    // $rn  -> start position of rest parameter
    mov(ea($rtcb, offsetof(Context, mv_value), $rn), rax);

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

    mov($rn, ea_m_n());
    ret();

label(rest_0);
    mov($r0, $rnil);
    jmp(exit);

label(alloc_cons_area);
    // $r0 <- netls * sizeof(SCons);
    // $rn <- start position
    // rbx -> pArea
    //
    //          +--------------+
    //   rsp+24 |RA of .restify| rsp%16=8
    //          +--------------+
    //   rsp+16 | home of rcx  |
    //          +--------------+
    //   rsp+8  | home of rdx  |
    //          +--------------+
    //   rsp+0  | RA of caller | rsp%16=0
    //          +--------------+
    //   rsp-8  | RA of caller | rsp%16=8
    //          +--------------+
    sub(rsp, 16);
    mov(rcx, $rtcb);
    mov(rdx, sizeof(Cons));
    call(ea($rtcb, SVC_alloc_cons_area));
    add(rsp, 16);
    mov(rbx, rax);

    mov(rcx, ea_m_fn());
    mov(rax, ea_m_n());
    mov(rsi, rax);
    sub(rax, rcx);;
    add(rax, rax);
    jmp(cont);
end_defun(); // .restify


//////////////////////////////////////////////////////////////////////
//
// .stack-restify
//
//  Syntax:
//      mov $rtcb.m_fn <- number of fixed parameters
//      call .stack-restify
//
//          +-------------------+
//   esp-16 |   RA of callee    |
//          +-------------------+ 
//    esp-8 |  pad for align 16 |
//          +-------------------+
//    esp-0 |       RP       o--+---+
//          +-------------------+   |
//          |      rest[0]      |   |
//          +-------------------+   |
//          |       cdr[0]      |   |
//          +-------------------+   |
//                  ...             |
//          +-------------------+   |
//          |     rest[k-1]     |   |
//          +-------------------+   |
//          |       nil         |   |
//          +-------------------+   |
//          |  pad for align 16 |   |
//          +-------------------+   |
//          |   RA of caller    |<--+ = RP
//          +-------------------+
//
// See Also:
//  .stack-restify, uninitialized-funcallable-instance
//
// Note:
//  This function can't be unwindable.
//
defun_(Q(".STACK-RESTIFY"), 0, -1, Fixed, 16)
    Label exit;
    Label loop;
    Label rest_0;
    Label restore_rax;

    mov(ea_m_n(), $rn);
    mov(ea_mv_value(0), $r0);
    mov(ea_mv_value(1), $r1);
    mov(ea_mv_value(2), $r2);
    mov(ea_mv_value(3), $r3);
    mov(ea_mv_value(4), $r4);
    mov(ea_mv_value(5), $r5);
    mov(ea_mv_value(6), $r6);
    mov(ea_mv_value(7), $r7);
    mov(ea_mv_value(8), $r8);
    mov(ea_mv_value(9), $r9);

    // How many number of rest parameters do we get?
    mov(rax, $rn);
    mov(rsi, $rn);
    mov(rcx, ea_m_fn());
    sub(rax, rcx);
    test(rax, rax);
    jle(rest_0);


    // number of arguments -> rsi
    // rsi <- &pThread->mv_value[n]
    lea(rsi, ea($rtcb, offsetof(Context, mv_value), rsi));

    add(rax, rax);

    // rsp-> RA
    //       caller   <- rsp
    mov(rdx, ea(rsp));      // rdx <- RA
    lea(rbx, ea(rsp, 8));   // rbx <- rsp

    // rsp-> RA
    //       FP
    // rfp-> RP o---------+
    // rdx-> rest[0]      |
    //       cdr[0]       |
    //       rest[1]      |
    //       cdr[1]       |
    //       ...          |
    //       rest[n-1]    |
    //       nil          |
    // rdi-> callee       |     mod 16=0
    // rbx-> caller <-----+     mod 16=8
    mov(rdi, rsp);
    sub(rsp, rax);
    mov(rax, rsp);
    push(rbx);  // RP   mod 16=8
    push($rfp); // FP   mod 16=0
    lea($rfp, ea(rsp, 16));
    push(rdx);  // RA   mod 16=8

    mov(rdx, rax);
    mov(rax, $rnil);

    // From the last cons to the first cons.
  label(loop);
    sub(rdi, sizeof(Cons));
    mov(ea(rdi, offsetof(Cons, m_cdr)), rax);
    sub(rsi, sizeof(Val));
    mov(rax, ea(rsi));
    mov(ea(rdi, offsetof(Cons, m_car)), rax);

    lea(rax, ea(rdi, Cons::TagX));

    cmp(rdi, rdx);
    jne(loop);

  label(exit);
    // rax <- list of rest parameters
    // rcx <- start position of rest parameter
    mov(ea($rtcb, offsetof(Context, mv_value), rcx), rax);

    mov($r0, ea_mv_value(0));
    mov($r1, ea_mv_value(1));
    mov($r2, ea_mv_value(2));
    mov($r3, ea_mv_value(3));
    mov($r4, ea_mv_value(4));
    mov($r5, ea_mv_value(5));
    mov($r6, ea_mv_value(6));
    mov($r7, ea_mv_value(7));
    mov($r8, ea_mv_value(8));
    mov($r9, ea_mv_value(9));

    mov($rn, ea_m_n());
    ret();

  label(rest_0);
    //                    rsp[0]  callee  mod 16=0
    //                    rsp[8]  FP      mod 16=8
    // rsp' -> callee     rsp[16] RP      mod 16=0
    //         caller     caller          mod 16=8
    mov(rax, ea(rsp));      // rax <- RA of callee
    lea(rdx, ea(rsp, 8));   // rdx <- RP == slot of RA of caller
    sub(rsp, 24);
    mov(ea(rsp), rax);
    mov(ea(rsp, 8), $rfp);
    mov(ea(rsp, 16), rdx);
    lea($rfp, ea(rsp, 16));
    mov(rax, $rnil);
    jmp(exit);
end_defun() // .stack-restify
} // X64Builder::build_03_Eval

} // Boot
