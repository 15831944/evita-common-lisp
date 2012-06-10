#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - boot - 07 Objects
// arch/x86/boot/x86_bt_07_object.inc
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86/boot/x86_bt_07_object.cpp#8 $
//
// Description:
//  This file contains functions for "07 Objects".
//      clos:standard-instance-access
//      (setf clos:standard-instance-access)
//
//      clos:funcallable-standard-instance-access
//      (setf clos:funcallable-standard-instance-access)
//
#include "./x86_bt_builder.h"

namespace Boot
{

class BuildAccessor : public X86Assembler
{
    public: void Run(LPCWSTR pwsz, int nTag, int nMask, Int ofs, Val ty)
    {
        // (standard-instance-access instance index)
        defun_(parse_symbol(pwsz), 2, 2)
          Label error_$r0;
          Label error_$r1;

            lea($r2, ea($r0, -nTag));
            test($r2, nMask);
            jne(error_$r0);

            test($r1, Fixnum::TagMask); // CF=0
            jne(error_$r1);

            mov($r0, ea($r0, ofs - nTag));
            mov($r0, ea($r0, OffsetOf(Storage, mv_element), $r1));
            ret();

          label(error_$r0);
            mov($r1, ty);
            call(ea($tcb, SVC_type_error));

          label(error_$r1);
            mov($r0, $r1);
            mov($r1, Q("ext:sequence-index"));
            call(ea($tcb, SVC_type_error));
        end_defun()


        // (setf funcallable-standard-instance-access)
        defun_setf(pwsz, 3, 3)
          Label error_$r1;
          Label error_$r2;

            lea($r3, ea($r1, -nTag));
            test($r3, nMask);
            jne(error_$r1);

            test($r2, Fixnum::TagMask); // CF=0
            jne(error_$r2);

            mov($r1, ea($r1, ofs - nTag));
            mov(ea($r1, OffsetOf(Storage, mv_element), $r2), $r0);
            ret();

          label(error_$r1);
            mov($r0, $r1);
            mov($r1, ty);
            call(ea($tcb, SVC_type_error));

          label(error_$r2);
            mov($r0, $r2);
            mov($r1, Q("ext:sequence-index"));
            call(ea($tcb, SVC_type_error));
        end_defun()
    } // Run
} oAsm;

void X86Builder::build_07_Object()
{

oAsm.Run(
    L"CLOS:STANDARD-INSTANCE-ACCESS",
    Instance::Tag,
    7,
    offsetof(Instance, m_storage),
    Qstandard_object );

oAsm.Run(
    L"CLOS:FUNCALLABLE-STANDARD-INSTANCE-ACCESS",
    FuncallableInstance::Tag,
    15,
    offsetof(FuncallableInstance, m_storage),
    Qfuncallable_standard_object );


// CLASS-OF is almost as same as TYPE-OF. We can implement class-of as
//  (class-of x) = (find-class (type-of x)) or
//  (type-of x) = (class-name (class-of x)
// For fast execution of class-of, we implment class-of in assembler.
defun("CLASS-OF", 1, 1)
    Label tagtbl;

    mov($r1, $r0);

    and($r1, 15);
    add($r1, $r1);  // $r1 = ($r1 & 15) * 2
    add($r1, $r1);  // $r1 = ($r1 & 15) * 4
    jmp(ea($r1, tagtbl));

  label_def(tag_fixnum);
    mov($r0, CLASS_fixnum);
    ret();

  label_def(tag_null);
    mov($r0, CLASS_null);
    ret();

  label_def(tag_instance);
    mov($r0, ea($r0, OffsetOf(Instance, m_classd)));
    mov($r0, ea($r0, OffsetOf(ClassD, m_class)));
    ret();

  label_def(tag_record);
    mov($r0, ea($r0, OffsetOf(Record, m_classd)));
    mov($r0, ea($r0, OffsetOf(ClassD, m_class)));
    ret();

  label_def(tag_cons);
    mov($r0, CLASS_cons);
    ret();

  label_def(tag_function);
    mov($r0, ea($r0, OffsetOf(Function, m_classd)));
    mov($r0, ea($r0, OffsetOf(ClassD, m_class)));
    ret();

  label_def(tag_invalid);
    mov($r0, CLASS_invalid_object);
    ret();

    align(4);
  label(tagtbl);
    dd(tag_fixnum);         //  0 0000
    dd(tag_record);         //  1 0001
    dd(tag_null);           //  2 0010
    dd(tag_invalid);        //  3 0011

    dd(tag_fixnum);         //  4 0100
    dd(tag_function);       //  5 0101
    dd(tag_cons);           //  6 0110
    dd(tag_instance);       //  7 0111

    dd(tag_fixnum);         //  8 1000
    dd(tag_record);         //  9 1001
    dd(tag_invalid);        // 10 1010
    dd(tag_invalid);        // 11 1011

    dd(tag_fixnum);         // 12 1100
    dd(tag_invalid);        // 13 1101
    dd(tag_cons);           // 14 1110
    dd(tag_instance);       // 15 1111
end_defun()


// classd-of
defun("CLASSD-OF", 1, 1)
    Label tagtbl;

    mov($r1, $r0);

    and($r1, 15);
    add($r1, $r1);  // $r1 = ($r1 & 15) * 2
    add($r1, $r1);  // $r1 = ($r1 & 15) * 4
    jmp(ea($r1, tagtbl));

  label_def(tag_fixnum);
    mov($r0, CLASSD_fixnum);
    ret();

  label_def(tag_null);
    mov($r0, CLASSD_null);
    ret();

  label_def(tag_instance);
    mov($r0, ea($r0, OffsetOf(Instance, m_classd)));
    ret();

  label_def(tag_record);
    mov($r0, ea($r0, OffsetOf(Record, m_classd)));
    ret();

  label_def(tag_cons);
    mov($r0, CLASSD_cons);
    ret();

  label_def(tag_function);
    mov($r0, ea($r0, OffsetOf(Function, m_classd)));
    ret();

  label_def(tag_invalid);
    mov($r0, CLASSD_invalid_object);
    ret();

    align(4);
  label(tagtbl);
    dd(tag_fixnum);         //  0 0000
    dd(tag_record);         //  1 0001
    dd(tag_null);           //  2 0010
    dd(tag_invalid);        //  3 0011

    dd(tag_fixnum);         //  4 0100
    dd(tag_function);       //  5 0101
    dd(tag_cons);           //  6 0110
    dd(tag_instance);       //  7 0111

    dd(tag_fixnum);         //  8 1000
    dd(tag_record);         //  9 1001
    dd(tag_invalid);        // 10 1010
    dd(tag_invalid);        // 11 1011

    dd(tag_fixnum);         // 12 1100
    dd(tag_invalid);        // 13 1101
    dd(tag_cons);           // 14 1110
    dd(tag_instance);       // 15 1111
end_defun()

//////////////////////////////////////////////////////////////////////
//
// si::uninitialized-funcallable-instance
//  Similar to x86::.arity-error
//
defun("SI::UNINITIALIZED-FUNCALLABLE-INSTANCE", 0, -1)
    frame_type(Restify, 0);

    mov(ea_mv_value(0), $r0);
    mov(ea_mv_value(1), $r1);
    mov(ea_mv_value(2), $r2);
    mov(ea_mv_value(3), $r3);
    mov(ea_mv_value(4), $r4);
    mov(ea_m_n(),       $rn);

    // Get callee and store it into %thread.fn
    pop($r2);         // $r2 <- return address of callee
    sub($r2, sizeof(FunObj));
    and($r2, ~15);

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
    add($r2, FunObj::Tag); // $r2 = callee

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
    sub($r3, 4);
    push($r4);            // set cdr <- $r4
    push(ea($r3));        // set car <- [esi]
    lea($r4, ea(esp, Tag_Cons8));
    sub($rn, 4);
    jnz(args_loop);

  label(args_done);
    // $r4 = args

    push($r1);        // RP(RA address pointer)

    ////////////////////////////////////////////////////////////
    //
    // Call function "error".
    //
    mov($r0, Q("SI::UNINITIALIZED-FUNCALLABLE-INSTANCE"));
    mov($r1, Kfunction);
    mov($r3, Karguments);
    mov($rn, Fixnum::Encode(5));
    call(ea($tcb, SVC_error));
end_defun()

} // build_07_Object

} // Boot