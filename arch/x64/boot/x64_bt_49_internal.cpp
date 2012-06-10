#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - boot - 49 Internal
// arch/x86/boot/x86_bt_07_object.inc
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x64/boot/x64_bt_49_internal.cpp#13 $
//
// Description:
//  This file contains functions for "03 Evaluation and Compilation".
//
#include "./x64_bt_builder.h"

#include "../kernel/x64_ke_thread.h"

namespace Boot
{

using namespace X64;

void X64Builder::build_49_Internal()
{
    Q("ALLOC-BINO-AREA");
    Q("ALLOC-CODE-AREA");
    Q("ALLOC-CONS-AREA");
    Q("ALLOC-RECO-AREA");

    defun_(Q("SI::ADDRESS-OF"), 1, 1, Fixed, 8)
      shl($r0, 3);
      // Note: We don't care CF. Since, $rn=1.
      ret();
    end_defun()

    defun_(Q("SI::CURRENT-THREAD"), 0, 0, Fixed, 8)  // CF=0
        lea($r0, ea($rtcb, Record::Tag));
        ret();
    end_defun()

    defun_(Q("SI::FREE-SLOT-MARKER"), 0, 0, Fixed, 8)    // CF=0
        mov($r0, QQfree_slot_marker);
        ret();
    end_defun();


    defun_(Q("SI::REMOVED-SLOT-MARKER"), 0, 0, Fixed, 8)    // CF=0
        mov($r0, QQremoved_slot_marker);
        ret();
    end_defun();


    defun_(Q("SI::UNBOUND-MARKER"), 0, 0, Fixed, 8)    // CF=0
        mov($r0, QQunbound_marker);
        ret();
    end_defun();


    defun_(Q("SI::TLV"), 1, 1, Fixed, 8)
        Label not_fixnum;

        test($r0, Fixnum::TagMask);     // CF=0
        jne(not_fixnum);
        mov($r0, ea($rtcb, offsetof(Thread, mv_tlv), $r0));
        ret();
      label(not_fixnum);
        mov($r1, Qfixnum);
        call(ea($rtcb, SVC_type_error));
    end_defun()


    defun_setf(L"SI::TLV", 2, 2)
        Label not_fixnum;

        test($r1, Fixnum::TagMask);     // CF=0
        jne(not_fixnum);
        mov(ea($rtcb, offsetof(Thread, mv_tlv), $r1), $r0);
        ret();
      label(not_fixnum);
        mov($r1, Qfixnum);
        call(ea($rtcb, SVC_type_error));
    end_defun()


    //////////////////////////////////////////////////////////////////////
    //
    // alloc-bino-area
    // alloc-code-area
    // alloc-cons-area
    // alloc-reco-area
    // alloc-symb-area
    //
    //              +---------------------+
    //       $sp-8  |   RA of wrapper     |<----------------+
    //              +---------------------+                 |
    //       $sp+0  |   home of arg[0]    |     rcx         |
    //              +---------------------+                 |
    //       $sp+8  |   home of arg[1]    |     rdx         |
    //              +---------------------+                 |
    //       $sp+16 |   home of arg[0]    |     r8          |
    //              +---------------------+                 |
    //       $sp+24 |   home of arg[1]    |     r9          |
    //              +---------------------+                 |
    //       $sp+32 |      $rtcb.m_fp     | <-- $rtcb.m_fp  |
    //              +---------------------+                 |
    //       $sp+40 |      ToKernel       |                 |
    //              +---------------------+                 |
    //       $sp+48 |    pointer to RA  o-+-----------------+
    //              +---------------------+
    //       $sp+56 | RA of lisp function | <-- $sp at entry
    //              +----------------------
    class DefunAllocArea : public X64Assembler
    {
        public: void Define(Val name, const char16* pwsz)
        {
            struct Local
            {
                int64           m_home[4];  // +0
                ToKernelFrame   m_oFrame;   // +32
                                            // +56
            }; // Local

            defun_(name, 0, -1, Fixed, sizeof(Local) + sizeof(UInt));

            sub($sp, sizeof(Local));

            PUSH_TRANSITION_FRAME(ToKernel, $r3);

            mov(rcx, $rtcb);                // arg[0]
            mov(rdx, $r0);                  // arg[1]
            call(dlllink(L".", pwsz));

            POP_TRANSITION_FRAME($r1);

            add($sp, sizeof(Local));
            ret();

            end_defun();
        } // Define
    } Build;

    Build.Define(Q("ALLOC-BINO-AREA"), L"SVC_C_alloc_bino_area");
    Build.Define(Q("ALLOC-CODE-AREA"), L"SVC_C_alloc_code_area");
    Build.Define(Q("ALLOC-CONS-AREA"), L"SVC_C_alloc_cons_area");
    Build.Define(Q("ALLOC-RECO-AREA"), L"SVC_C_alloc_reco_area");
} // X64Builder::build_49_Internal

} // Boot
