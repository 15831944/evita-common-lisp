#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - boot - 12 Numbers
// arch/x64/boot/x64_bt_12_number.inc
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x64/boot/x64_bt_12_number.cpp#15 $
//
// Description:
//  This file contains functions for "12 Numbers".
//
#include "./x64_bt_builder.h"

namespace Boot
{

class NumberBuilder : public X64Assembler
{

    public: void Build1op(LPCWSTR pwszName, LPCWSTR pwszCName)
    {
        defun_(parse_symbol(pwszName), 1, 1, Fixed, 48)
            sub($sp, 40);

            mov(rcx, $r0);
            call(dlllink(L".", pwszCName));
            mov($r0, rax);

            add($sp, 40);
            ret();
        end_defun()
    } // Build1op

    public: void Build2op(LPCWSTR pwszName, LPCWSTR pwszCName)
    {
        defun_(parse_symbol(pwszName), 2, 2, Fixed, 48)
            sub($sp, 40);

            mov(rcx, $r0);
            mov(rdx, $r1);
            call(dlllink(L".", pwszCName));
            mov($r0, rax);

            add($sp, 40);
            ret();
        end_defun()
    } // Build2op

    public: void BuildCmp(LPCWSTR pwszName, LPCWSTR pwszFn, Tttn tttn)
    {
        defun_(parse_symbol(pwszName), 2, 2, Fixed, 48)
            sub($sp, 40);

            mov(rcx, $r0);
            mov(rdx, $r1);
            call(dlllink(L".", pwszFn));
            mov($r0, rax);

            test($r0, $r0);     // CF=0
            mov($r0, t);

            emit_rex($r0, $rnil);
            emit_op(op_CMOVcc_Gv_Ev + (tttn ^ 1));
            emit_u8(modrm(mod_reg, $r0, $rnil));

            add($sp, 40);
            ret();
        end_defun()
    } // BuildCmp

    public: void BuildEq(LPCWSTR pwszName, LPCWSTR pwszFn, Tttn tttn)
    {
        defun_(parse_symbol(pwszName), 2, 2, Fixed, 48)
            sub($sp, 40);

            mov(rcx, $r0);
            mov(rdx, $r1);
            call(dlllink(L".", pwszFn));
            mov($r0, rax);

            test(al, al);     // CF=0
            mov($r0, t);

            emit_rex($r0, $rnil);
            emit_op(op_CMOVcc_Gv_Ev + tttn);
            emit_u8(modrm(mod_reg, $r0, $rnil));

            add($sp, 40);
            ret();
        end_defun()
    } // BuildCmp

    public: void BuildEvenp(LPCWSTR pwszName, Tttn tttn)
    {
        defun_(parse_symbol(pwszName), 1, 1, Fixed, 16)
            Label not_integer;
            Label try_bignum;

            sub($sp, 8);

            test($r0, Fixnum::TagMask);
            jne(try_bignum);

            test($r0, Fixnum::One); // CF=0

            emit_rex($r0, $rnil);
            emit_op(op_CMOVcc_Gv_Ev + (tttn ^ 1));
            emit_u8(modrm(mod_reg, $r0, $rnil));

            add($sp, 8);
            ret();

          label(try_bignum);
            lea($r1, ea($r0, -Bignum::Tag));
            test($r1, Val_::Mask_Tag);
            jne(not_integer);

            mov($r2, CLASSD_bignum);
            cmp(ea($r1), $r2);
            jne(not_integer);

            test(ea($r0, offsetof(Bignum, m_rgBigit) - Bignum::Tag), 1);

            emit_rex($r0, $rnil);
            emit_op(op_CMOVcc_Gv_Ev + (tttn ^ 1));
            emit_u8(modrm(mod_reg, $r0, $rnil));

            add($sp, 8);
            ret();

          label(not_integer);
            mov($r1, Qinteger);
            call(ea($rtcb, SVC_type_error));
        end_defun()
    } // BuildEvenp
}; // NumberBuilder

void X64Builder::build_12_Number()
{

    NumberBuilder o;

#if 0
    o.Build1op(L"MINUSP", L"C_minusp");
    o.Build1op(L"PLUSP",  L"C_plusp");
    o.Build1op(L"ZEROP",  L"C_zerop");

    o.Build2op(L"SI::+/2", L"C_add");
    o.Build2op(L"SI:://2", L"C_div");
    o.Build2op(L"SI::*/2", L"C_mul");
    o.Build2op(L"SI::-/2", L"C_sub");

    o.Build2op(L"ASH",          L"C_ash");
    o.Build2op(L"SI::GCD/2",    L"C_gcd");
    o.Build2op(L"LOGBITP",      L"C_logbitp");
    o.Build2op(L"SI::LOGAND/2", L"logand_2");
    o.Build2op(L"SI::LOGIOR/2", L"logior_2");
    o.Build2op(L"SI::LOGXOR/2", L"logxor_2");

    o.BuildCmp(L"SI::</2",  L"C_cmp", tttn_L);
    o.BuildCmp(L"SI::<=/2", L"C_cmp", tttn_LE);
    o.BuildCmp(L"SI::>/2",  L"C_cmp", tttn_G);
    o.BuildCmp(L"SI::>=/2", L"C_cmp", tttn_GE);

    o.BuildEq(L"SI::=/2",  L"C_num_eq", tttn_Z);
    o.BuildEq(L"SI::/=/2", L"C_num_eq", tttn_NZ);
#endif

    o.BuildEvenp(L"EVENP", tttn_Z);
    o.BuildEvenp(L"ODDP",  tttn_NZ);

    // box-float32
    //              +---------------------+
    //       $sp-8  |   RA of wrapper     |<----------------+
    //              +---------------------+                 |
    //       $sp+0  |   home of arg[0]    |     rcx         |
    //              +---------------------+                 |
    //       $sp+8  |   home of arg[1]    |     rdx         |
    //              +---------------------+                 |
    //       $sp+16 |   home of arg[2]    |     r8          |
    //              +---------------------+                 |
    //       $sp+24 |   home of arg[3]    |     r9          |
    //              +---------------------+                 |
    //       $sp+32 |      $rtcb.m_fp     | <-- $rtcb.m_fp  |
    //              +---------------------+                 |
    //       $sp+40 |      ToKernel       |                 |
    //              +---------------------+                 |
    //       $sp+48 |      cbArgs       o-+-----------------+
    //              +---------------------+
    //       $sp+56 |       xmm0/32       |
    //              +----------------------
    //       $sp+64 |                     |
    //              +----------------------
    //       $sp+72 | RA of lisp function | <-- $sp at entry
    //              +----------------------
    defun_(Q(".BOX-FLOAT32"), 0, -1, Fixed, 80)
        struct Local
        {
            int64           m_home[4];  // +0
            ToKernelFrame   m_oFrame;   // +32
            int64           m_r0;      // +56
            double          m_xmm0;    // +64
        }; // Local                    // +72

        sub($sp, sizeof(Local));

        movss(ea($sp, offsetof(Local, m_xmm0)), xmm0);

        PUSH_TRANSITION_FRAME(ToKernel, $r3);

        // Call kernel function
        mov(rcx, CLASSD_single_float);
        call(dlllink(L".", L"Zallocate_binobj"));

        movss(xmm0, ea($sp, offsetof(Local, m_xmm0)));

        POP_TRANSITION_FRAME($r1);

        movss(ea(rax, offsetof(SingleFloat, m_flt) - SingleFloat::Tag), xmm0);

        add($sp, sizeof(Local));   // CF=0
        ret();
    end_defun() // box-float32

    // box-float64
    //              +---------------------+
    //       $sp-8  |   RA of wrapper     |<----------------+
    //              +---------------------+                 |
    //       $sp+0  |   home of arg[0]    |     rcx         |
    //              +---------------------+                 |
    //       $sp+8  |   home of arg[1]    |     rdx         |
    //              +---------------------+                 |
    //       $sp+16 |   home of arg[2]    |     r8          |
    //              +---------------------+                 |
    //       $sp+24 |   home of arg[3]    |     r9          |
    //              +---------------------+                 |
    //       $sp+32 |      $rtcb.m_fp     | <-- $rtcb.m_fp  |
    //              +---------------------+                 |
    //       $sp+40 |      ToKernel       |                 |
    //              +---------------------+                 |
    //       $sp+48 |    pointer to RA  o-+-----------------+
    //              +---------------------+
    //       $sp+56 |       xmm0/64       |
    //              +----------------------
    //       $sp+64 |                     |
    //              +----------------------
    //       $sp+72 | RA of lisp function | <-- $sp at entry
    //              +----------------------
    defun_(Q(".BOX-FLOAT64"), 0, -1, Fixed, 80)

        struct Local
        {
            int64           m_home[4];  // +0
            ToKernelFrame   m_oFrame;   // +32
            int64           m_r0;       // +56
            double          m_xmm0;     // +64
        }; // Local                     // +72

        sub($sp, sizeof(Local));

        movsd(ea($sp, offsetof(Local, m_xmm0)), xmm0);

        PUSH_TRANSITION_FRAME(ToKernel, $r3);

        // Call kernel function
        mov(rcx, CLASSD_double_float);
        call(dlllink(L".", L"Zallocate_binobj"));

        POP_TRANSITION_FRAME($r1);

        movsd(xmm0, ea($sp, offsetof(Local, m_xmm0)));
        movsd(ea(rax, offsetof(DoubleFloat, m_dbl) - DoubleFloat::Tag), xmm0);

        add($sp, sizeof(Local));
        ret();
    end_defun() // box-float64

    // box-int
    //  Parameters:
    //    $r0   -- int64
    //  Values:
    //    $r0   -- fixnum or bignum (1 bigit)
    defun_(Q(".BOX-INT"), 0, -1, Fixed, 80)
        Label bignum;

        struct Local
        {
            int64           m_home[4];  // +0
            ToKernelFrame   m_oFrame;   // +32
            int64   m_r0;               // +56
            int64   m_pad;              // +64
        }; // Local                     // +72

        sub($sp, sizeof(Local));

        mov($r2, -Fixnum::MostNegative);
        mov($r3, Fixnum::MostPositive - Fixnum::MostNegative);

        lea($r1, ea($r0, 0, $r2));
        cmp($r1, $r3);
        ja(bignum);

        // Fixnum::MostNegative <= $r0 <= Fixnum::MostPositive
        shl($r0, Fixnum::TagBits);

        add($sp, sizeof(Local));    // CF=0
        ret();

      label(bignum);
        mov(ea($sp, offsetof(Local, m_r0)), $r0);

        PUSH_TRANSITION_FRAME(ToKernel, $r3);

        // Call kernel function
        mov(rcx, CLASSD_bignum);
        mov(rdx, Fixnum::Encode(1));
        call(dlllink(L".", L"Zallocate_binvec"));

        mov($r1, ea($sp, offsetof(Local, m_r0)));

        POP_TRANSITION_FRAME($r3);

        mov(ea($r0, offsetof(Bignum, m_rgBigit) - Bignum::Tag), $r1);

        add($sp, sizeof(Local));   // CF=0
        ret();
    end_defun() // box-int

    // box-uint
    //  Parameters:
    //    $r0   -- uint32
    //  Values:
    //    $r0   -- fixnum or bignum (1 or 2 bigit)
    //
    //  if $r0 is positive         if $r0 is negative
    //  [0] CLASSD_bignum       [0] CLASSD_bignum
    //  [1] '1                  [1] '2
    //  [2] $r0                 [2] $r0
    //  [3] 0                   [3] 0
    //
    defun_(Q(".BOX-UINT"), 0, -1, Fixed, 80)
        Label bignum, done;

        struct Local
        {
            int64           m_home[4];  // +0
            ToKernelFrame   m_oFrame;   // +32
            int64   m_r0;               // +56
            int64   m_pad;              // +64
        }; // Local                     // +72

        sub($sp, sizeof(Local));
        mov($r1, Fixnum::MostPositive);
        cmp($r0, $r1);
        ja(bignum);

        // |$r0| <= Fixnum::MostPositive
        shl($r0, Fixnum::TagBits);
        add($sp, sizeof(Local)); // CF=0
        ret();

      label(bignum);
        mov(ea($sp, 56), $r0);

        PUSH_TRANSITION_FRAME(ToKernel, $r3);

        // Call kernel function
        mov(rcx, CLASSD_bignum);
        mov(rdx, Fixnum::Encode(1));
        call(dlllink(L".", L"Zallocate_binvec"));

        mov($r1, ea($sp, offsetof(Local, m_r0)));

        POP_TRANSITION_FRAME($r3);

        mov(ea($r0, offsetof(Bignum, m_rgBigit[0]) - Bignum::Tag), $r1);
        test($r1, $r1);
        jns(done);

        // parmeter $r0 is negative
        mov(ea($r0, offsetof(Bignum, m_length) - Bignum::Tag),
            Fixnum::Encode(2) );

      label(done);
        add($sp, sizeof(Local));   // CF=0
        ret();
    end_defun() // box-uint

    // unbox-int
    //  Returns LSB of integer object.
    defun_(Q(".UNBOX-INT"), 0, -1, Fixed, 8)
        Label unbox_bignum, not_integer;

        test($r0, Fixnum::TagMask);
        jne(unbox_bignum);

        // unbox fixnum
        shr($r0, Fixnum::TagBits);
        ret();

      label(unbox_bignum);
        lea($r1, ea($r0, -Bignum::Tag));
        test($r1, Bignum::TagMask);
        jne(not_integer);
        mov($r2, CLASSD_bignum);
        cmp(ea($r0, -Bignum::Tag), $r2);
        jne(not_integer);

        // unbox bignum
        mov($r0, ea($r0, offsetof(Bignum, m_rgBigit[0]) - Bignum::Tag));
        ret();

      label(not_integer);
        mov($r1, Qinteger);
        jmp(ea($tcb, SVC_type_error));
    end_defun() // unbox_int
} // X64Builder::build_12_Number

} // Boot
