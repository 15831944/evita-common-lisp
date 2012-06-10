#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - boot - 49 Internals
// arch/x86/boot/x86_bt_49_internal.inc
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86/boot/x86_bt_49_internal.cpp#12 $
//
// Description:
//  TBD
#include "./x86_bt_builder.h"

namespace Boot
{

void X86Builder::build_49_Internal()
{

    defun("SI::ADDRESS-OF", 1, 1)
      and($r0, ~Fixnum::TagMask); // CF=0
      ret();
    end_defun()

    defun("SI::CURRENT-THREAD", 0, 0)  // CF=0
        lea($r0, ea($tcb, Record::Tag));
        ret();
    end_defun()

    defun("SI::FREE-SLOT-MARKER", 0, 0)    // CF=0
        mov($r0, QQfree_slot_marker);
        ret();
    end_defun();


    defun("SI::REMOVED-SLOT-MARKER", 0, 0)    // CF=0
        mov($r0, QQremoved_slot_marker);
        ret();
    end_defun();


    defun("SI::UNBOUND-MARKER", 0, 0)    // CF=0
        mov($r0, QQunbound_marker);
        ret();
    end_defun();


    defun("SI::TLV", 1, 1)
        Label not_fixnum;

        test($r0, Fixnum::TagMask);     // CF=0
        jne(not_fixnum);
        mov($r0, ea($tcb, offsetof(Thread, mv_tlv), $r0));
        ret();
      label(not_fixnum);
        mov($r1, Qfixnum);
        call(ea($tcb, SVC_type_error));
    end_defun()


    defun_setf(L"SI::TLV", 2, 2)
        Label not_fixnum;

        test($r1, Fixnum::TagMask);     // CF=0
        jne(not_fixnum);
        mov(ea($tcb, offsetof(Thread, mv_tlv), $r1), $r0);
        ret();
      label(not_fixnum);
        mov($r1, Qfixnum);
        call(ea($tcb, SVC_type_error));
    end_defun()

    #if 0
    // static-object-p
    defun("STATIC-OBJECT-P", 1, 1)
        Label ret_true;

        sub(arg_0, reinterpret_cast<Int>(Memory::GetStart()));
        jb(ret_true);
        shr(arg_0, 16);
        // BUGBUG: How do we get Memory::sm_ppAreaMap?
        // mov(arg_1, Memory::sm_ppAreaMap);
        mov(arg_1, ea(reinterpret_cast<void*>(0x20000040)));
        mov(arg_0, ea(arg_1, 0, arg_0, scale_4));
        mov(arg_1, ea(arg_0, offsetof(Area, m_nFlags)));
        and(arg_1, Area::Flags_AgeMask);
        cmp(arg_1, Area::Age_Static);
        je(ret_true);
        xor(val_0, val_0);  // CF=0
        mov(val_0, nil);
      label(ret_true);
        ret();
    end_defun()
    #endif

    #define defun_alloc_area(mp_NAME, mp_name) \
        defun("ALLOC-" mp_NAME L"-AREA", 0, -1) \
            frame_type(Fixed, 12); \
            push(0); \
            push(Kernel::Frame::Type_ToKernel); \
            push(ea_m_fp()); \
            mov(ea_m_fp(), $sp); \
            shr($r0, 2); \
            mov(ecx, $tcb); \
            mov(edx, $r0); \
            call(dlllink(L".", L"SVC_C_alloc_" mp_name L"_area")); \
            pop(ea_m_fp()); \
            add($sp, 8); \
            ret(); \
        end_defun()

    defun_alloc_area(L"BINO", L"bino");
    defun_alloc_area(L"CODE", L"code");
    defun_alloc_area(L"CONS", L"cons");
    defun_alloc_area(L"RECO", L"reco");

} // X86Builder::build_49_Internal

} // Boot
