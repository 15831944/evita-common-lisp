#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - boot - 07 Objects
// arch/x64/boot/x64_bt_07_object.inc
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x64/boot/x64_bt_07_object.cpp#9 $
//
// Description:
//  This file contains functions for "07 Objects".
//
#include "./x64_bt_builder.h"

#include "../kernel/x64_ke_thread.h"

namespace Boot
{

using namespace X64;

class BuildAccessor : public X64Assembler
{
    public: void Run(LPCWSTR pwsz, int nTag, int nMask, Int ofs, Val ty)
    {
        // (standard-instance-access instance index)
        defun_(parse_symbol(pwsz), 2, 2, Fixed, 16)
          Label error_$r0;
          Label error_$r1;

            sub($sp, 8);

            lea($r2, ea($r0, -nTag));
            test($r2, nMask);
            jne(error_$r0);

            test($r1, Fixnum::TagMask); // CF=0
            jne(error_$r1);

            mov($r0, ea($r0, ofs - nTag));
            mov($r0,
                ea($r0,
                   offsetof(Storage, mv_element) - Storage::Tag,
                   $r1 ) );

            add($sp, 8);
            ret();

          label(error_$r0);
            mov($r1, ty);
            call(ea($rtcb, SVC_type_error));

          label(error_$r1);
            mov($r0, $r1);
            mov($r1, Q("ext:sequence-index"));
            call(ea($rtcb, SVC_type_error));
        end_defun()


        // (setf standard-instance-access)
        defun_setf(pwsz, 3, 3)
          Label error_$r1;
          Label error_$r2;

            sub($sp, 8);

            lea($r3, ea($r1, -nTag));
            test($r3, nMask);
            jne(error_$r1);

            test($r2, Fixnum::TagMask); // CF=0
            jne(error_$r2);

            mov($r1, ea($r1, ofs - nTag));
            mov(ea($r1,
                   offsetof(Storage, mv_element) - Storage::Tag,
                   $r2 ),
                $r0 );

            add($sp, 8);
            ret();

          label(error_$r1);
            mov($r0, $r1);
            mov($r1, ty);
            call(ea($rtcb, SVC_type_error));

          label(error_$r2);
            mov($r0, $r2);
            mov($r1, Q("ext:sequence-index"));
            call(ea($rtcb, SVC_type_error));
        end_defun()
    } // Run
};

void X64Builder::build_07_Object()
{

Q("UNINITIALIZED-FUNCALLABLE-INSTANCE");


BuildAccessor oAsm;

oAsm.Run(
    L"CLOS:STANDARD-INSTANCE-ACCESS",
    Instance::Tag,
    15,
    offsetof(Instance, m_storage),
    Qstandard_object );

oAsm.Run(
    L"CLOS:FUNCALLABLE-STANDARD-INSTANCE-ACCESS",
    FuncallableInstance::Tag,
    15,
    offsetof(FuncallableInstance, m_storage),
    Qfuncallable_standard_object );


//////////////////////////////////////////////////////////////////////
//
// class-of
//
// CLASS-OF is almost as same as TYPE-OF. We can implement class-of as
//  (class-of x) = (find-class (type-of x)) or
//  (type-of x) = (class-name (class-of x)
// For fast execution of class-of, we implment class-of in assembler.
//
defun_(Q("CLASS-OF"), 1, 1, Fixed, 8)
    Label tagtbl;

    mov($r1, $r0);

    and($r1, Val_::Mask_Tag);
    lea($r2, ea(tagtbl));
    jmp(ea($r2, 0, $r1, scale_8));

  label_def(tag_fixnum);
    mov($r0, CLASS_fixnum);
    ret();

  label_def(tag_instance);
    mov($r0, ea($r0, OffsetOf(Instance, m_classd)));
    mov($r0, ea($r0, OffsetOf(ClassD, m_class)));
    ret();

  label_def(tag_record);
    mov($r0, ea($r0, OffsetOf(Record, m_classd)));
    mov($r0, ea($r0, OffsetOf(ClassD, m_class)));
    ret();

  label_def(tag_single_float);
    #if USE_SINGLE_FLOAT_TAG
        mov($r0, CLASS_single_float);
    #else // USE_SINGLE_FLOAT_TAG
        mov($r0, CLASS_invalid_object);
    #endif // USE_SINGLE_FLOAT_TAG
    ret();

  label_def(tag_null);
    mov($r0, CLASS_null);
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

    align(8);
  label(tagtbl);
    dq(tag_fixnum);         //  0 0000
    dq(tag_record);         //  1 0001
    dq(tag_null);           //  2 0010
    dq(tag_invalid);        //  3 0011

    dq(tag_single_float);   //  4 0100
    dq(tag_instance);       //  5 0101
    dq(tag_invalid);        //  6 0110
    dq(tag_invalid);        //  7 0111

    dq(tag_fixnum);         //  8 1000
    dq(tag_function);       //  9 1001
    dq(tag_cons);           // 10 1010
    dq(tag_invalid);        // 11 1011

    dq(tag_invalid);        // 12 1100
    dq(tag_invalid);        // 13 1101
    dq(tag_invalid);        // 14 1110
    dq(tag_invalid);        // 15 1111
end_defun() // class-of


//////////////////////////////////////////////////////////////////////
//
// classd-of
//
defun_(Q("CLASSD-OF"), 1, 1, Fixed, 8)
    Label tagtbl;

    mov($r1, $r0);

    and($r1, Val_::Mask_Tag);
    lea($r2, ea(tagtbl));
    jmp(ea($r2, 0, $r1, scale_8));

  label_def(tag_fixnum);
    mov($r0, CLASSD_fixnum);
    ret();

  label_def(tag_instance);
    mov($r0, ea($r0, OffsetOf(Instance, m_classd)));
    ret();

  label_def(tag_record);
    mov($r0, ea($r0, OffsetOf(Record, m_classd)));
    ret();

  label_def(tag_single_float);
    #if USE_SINGLE_FLOAT_TAG
        mov($r0, CLASSD_single_float);
    #else // USE_SINGLE_FLOAT_TAG
        mov($r0, CLASSD_invalid_object);
    #endif // USE_SINGLE_FLOAT_TAG
    ret();

  label_def(tag_null);
    mov($r0, CLASSD_null);
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

    align(8);
  label(tagtbl);
    dq(tag_fixnum);         //  0 0000
    dq(tag_record);         //  1 0001
    dq(tag_null);           //  2 0010
    dq(tag_invalid);        //  3 0011

    dq(tag_single_float);   //  4 0100
    dq(tag_instance);       //  5 0101
    dq(tag_invalid);        //  6 0110
    dq(tag_invalid);        //  7 0111

    dq(tag_fixnum);         //  8 1000
    dq(tag_function);       //  9 1001
    dq(tag_cons);           // 10 1010
    dq(tag_invalid);        // 11 1011

    dq(tag_invalid);        // 12 1100
    dq(tag_invalid);        // 13 1101
    dq(tag_invalid);        // 14 1110
    dq(tag_invalid);        // 15 1111
end_defun()


//////////////////////////////////////////////////////////////////////
//
// si::uninitialized-funcallable-instance
//
//          +-------------------+
//   esp-16 | RA of uninitialized-funcallable-instance
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
//          |      RA of fin    |<--+ = RP
//          +-------------------+
//
// See Also:
//  .arity-error, .stack-restify
//
defun_(Q("UNINITIALIZED-FUNCALLABLE-INSTANCE"), 0, -1, Restify, 16)
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
    mov(ea_m_n(), $rn);

    // Get callee and store it into %thread.fn
    mov($r2, ea($sp));  // $r2 <- return address of callee
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

    mov($r1, $sp);      // $r1 <- RP
    push($rnil);        // pad for align 16

    mov($r4, $rnil);

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

    push($r1);          // RP(RA address pointer)
    push($rfp);         // FP
    lea($rfp, ea(rsp, 16));

    ////////////////////////////////////////////////////////////
    //
    // Call function "error".
    //
    mov($r0, Q("UNINITIALIZED-FUNCALLABLE-INSTANCE"));
    mov($r1, Kfunction);
    mov($r3, Karguments);
    mov($rn, Fixnum::Encode(5));
    call(ea($rtcb, SVC_error));
end_defun()

} // X64Builder::build_07_Objects

} // Boot
