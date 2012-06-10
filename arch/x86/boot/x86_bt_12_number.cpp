#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - boot - 12 Numbers
// arch/x86/boot/x86_bt_12_number.inc
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86/boot/x86_bt_12_number.cpp#14 $
//
// Description:
//  This file contains functions for "12 Numbers".
//
#include "./x86_bt_builder.h"

namespace Boot
{

class NumberBuilder : public X86Assembler
{
#if 0
    public: void Build1op(LPCWSTR pwszName, LPCWSTR pwszCName)
    {
        defun_(parse_symbol(pwszName), 1, 1)
            mov(ecx, $r0);
            call(dlllink(L".", pwszCName));
            mov($r0, eax);

            xor($r1, $r1);      // CF=0
            ret();
        end_defun()
    } // Build1op

    public: void Build2op(LPCWSTR pwszName, LPCWSTR pwszCName)
    {
        defun_(parse_symbol(pwszName), 2, 2)
            mov(ecx, $r0);
            mov(edx, $r1);
            call(dlllink(L".", pwszCName));
            mov($r0, eax);

            xor($r1, $r1);      // CF=0
            ret();
        end_defun()
    } // Build2op

    public: void BuildCmp(LPCWSTR pwszName, LPCWSTR pwszFn, Tttn tttn)
    {
        defun_(parse_symbol(pwszName), 2, 2)
            mov(ecx, $r0);
            mov(edx, $r1);
            call(dlllink(L".", pwszFn));
            mov($r0, eax);

            test($r0, $r0);     // CF=0
            mov($r0, nil);
            mov($r1, t);
            emit_op(op_CMOVcc_Gv_Ev + tttn);
            emit_u8(modrm(mod_reg, $r0, $r1));
            ret();
        end_defun()
    } // BuildCmp
#endif

    public: void BuildEvenp(LPCWSTR pwszName, Tttn tttn)
    {
        defun_(parse_symbol(pwszName), 1, 1)
            Label not_integer;
            Label try_bignum;

            mov($r2, nil);

            test($r0, Fixnum::TagMask);
            jne(try_bignum);

            test($r0, Fixnum::One);         // CF=0
            emit_op(op_CMOVcc_Gv_Ev + (tttn ^ 1));
            emit_u8(modrm(mod_reg, $r0, $r2));
            ret();

          label(try_bignum);
            lea($r1, ea($r0, -Bignum::Tag));
            test($r1, 7);
            jne(not_integer);

            cmp(ea($r1), CLASSD_bignum);
            jne(not_integer);

            test(ea($r0, OffsetOf(Bignum, m_rgBigit)), 1);
            emit_op(op_CMOVcc_Gv_Ev + (tttn ^ 1));
            emit_u8(modrm(mod_reg, $r0, $r2));
            ret();

          label(not_integer);
            mov($r1, Qinteger);
            jmp(ea($tcb, SVC_type_error));
        end_defun()
    } // BuildEvenp

#if 0
    // BuildSign
    //  minusp, plusp, zerop
    void BuildSign(LPCWSTR pwszName, int iSign)
    {
        // BUGBUG: We need to know constant for div 40.
        ASSERT(40 == sizeof(ClassD));

        defun_(parse_symbol(pwszName), 1, 1)
            Label disptbl;
            Label do_bignum;
            Label do_double_float;
            Label do_double_float_complex;
            Label do_fixnum;
            Label do_ratio;
            Label do_single_float;
            Label do_single_float_complex;
            Label error_not_real;
            Label ret_nil;
            Label try_again;

          label(try_again);
            test($r0, Fixnum::TagMask);
            je(do_fixnum);

            lea($r1, ea($r0, -Record::Tag));
            test($r1, 7);
            jne(error_not_real);

            mov($r1, ea($r0, OffsetOf(Record, m_classd)));
            sub($r1, CLASSD_real_min);
            jb(error_not_real);

            cmp($r1,
                static_cast<int32>(0 == iSign ?
                    CLASSD_number_max - CLASSD_number_min :
                    CLASSD_real_max   - CLASSD_real_min ) );
            ja(error_not_real);

            shr($r1, 3);
            jmp(ea($r1, disptbl));

        if (0 != iSign)
        {
          label(do_bignum);
            mov($r1, ea($r0, OffsetOf(Bignum, m_length)));
            mov($r0,
                ea($r0, OffsetOf(Bignum, m_length), $r1) );
            jmp(do_fixnum);

          label(do_ratio);
            mov($r0, ea($r0, OffsetOf(Ratio, m_num)));
            jmp(try_again);
        } // if iSign

          label(do_double_float);
            movsd(
                xmm0,
                ea($r0, offsetof(DoubleFloat, m_dbl)) );
            xorpd(xmm1, xmm1);
            ucomisd(xmm0, xmm1);
            switch (iSign)
            {
            case  1: jna(ret_nil); break;   // CF=1 or ZF=1
            case  0: jne(ret_nil); break;   // ZF=0
            case -1: ja(ret_nil);  break;   // CF=0 or ZF=1
            default: CAN_NOT_HAPPEN();
            } // switch iSign
            ret();

          label(do_single_float);
            movss(
                xmm0,
                ea($r0, OffsetOf(SingleFloat, m_flt)) );
            xorps(xmm1, xmm1);
            ucomiss(xmm0, xmm1);
            switch (iSign)
            {
            case  1: jna(ret_nil); break;   // CF=1 or ZF=1
            case  0: jne(ret_nil); break;   // ZF=0
            case -1: ja(ret_nil);  break;   // CF=0 or ZF=1
            default: CAN_NOT_HAPPEN();
            } // switch iSign
            ret();

          label(do_fixnum);
            test($r0, $r0);
            switch (iSign)
            {
            case  1: jle(ret_nil); break;   // CF=1 or ZF=1
            case  0: jne(ret_nil); break;   // ZF=0
            case -1: jge(ret_nil); break;   // CF=0 or ZF=1
            default: CAN_NOT_HAPPEN();
            } // switch iSign
            ret();

        if (0 == iSign)
        {
          label(do_double_float_complex);
            movapd(xmm0, ea($r0, OffsetOf(DoubleFloatComplex, m_dblReal)));
            xorps(xmm1, xmm1);
            cmppd(xmm0, xmm1, cmppd_eq);
            ucomiss(xmm0, xmm1);
            je(ret_nil);
            ret();

          label(do_single_float_complex);
            movss(xmm0, ea($r0, OffsetOf(SingleFloatComplex, m_fltReal)));
            xorps(xmm1, xmm1);
            ucomiss(xmm0, xmm1);
            jne(ret_nil);

            movss(xmm0, ea($r0, OffsetOf(SingleFloatComplex, m_fltImag)));
            ucomiss(xmm0, xmm1);
            jne(ret_nil);
            ret();
        }

          label(ret_nil);
            xor($r1, $r1);
            mov($r0, nil);
            ret();

          label(error_not_real);
            mov($r1, Qreal);
            call(ea($tcb, SVC_type_error));

          label(disptbl);
            dd(0 == iSign ? ret_nil : do_bignum);
            dd(0 == iSign ? ret_nil : do_ratio);
            dd(do_double_float);
            dd(do_single_float);

            if (0 == iSign)
            {
                dd(do_double_float_complex);
                dd(ret_nil);
                dd(do_single_float_complex);
            }
        end_defun()
    } // BuildSign
#endif
}; // NumberBuilder

void X86Builder::build_12_Number()
{

    NumberBuilder o;

#if 0
    o.BuildSign(L"MINUSP", -1);
    o.BuildSign(L"PLUSP",   1);
    o.BuildSign(L"ZEROP",   0);
#endif

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

    o.BuildCmp(L"SI::=/2",  L"C_num_eq", tttn_NZ);
    o.BuildCmp(L"SI::/=/2", L"C_num_eq", tttn_Z);
#endif

    o.BuildEvenp(L"EVENP", tttn_Z);
    o.BuildEvenp(L"ODDP",  tttn_NZ);

    // box-float32
    defun(".BOX-FLOAT32", 0, -1)
        struct Frame : Kernel::ToKernelFrame
        {
            float32 m_fltSave;
        }; // Frame

        frame_type(Fixed, sizeof(Frame));

        sub($sp, sizeof(Frame));

        // Push ToKernel frame
        mov($r1, ea_m_fp());
        mov(ea($sp, offsetof(Frame, m_pOuter)), $r1);
        mov(ea($sp, offsetof(Frame, m_type)),   Frame::Type_ToKernel);
        mov(ea($sp, offsetof(Frame, m_cbArgs)), 0);
        mov(ea_m_fp(), $sp);

        movss(ea($sp, offsetof(Frame, m_fltSave)), xmm0);
        mov(ecx, CLASSD_single_float);
        call(dlllink(L".", L"Zallocate_binobj"));
        movss(xmm0, ea($sp, offsetof(Frame, m_fltSave)));
        movss(ea(eax, OffsetOf(SingleFloat, m_flt)), xmm0);

        // Pop ToKernel frame
        mov($r1, ea($sp, offsetof(Frame, m_pOuter)));
        mov(ea_m_fp(), $r1);

        add($sp, sizeof(Frame));
        ret();
    end_defun()

    // box-float64
    defun(".BOX-FLOAT64", 0, -1)
        struct Frame : Kernel::ToKernelFrame
        {
            float64 m_dblSave;
        }; // Frame

        frame_type(Fixed, sizeof(Frame));

        sub($sp, sizeof(Frame));

        // Push ToKernel frame
        mov($r1, ea_m_fp());
        mov(ea($sp, offsetof(Frame, m_pOuter)), $r1);
        mov(ea($sp, offsetof(Frame, m_type)),   Frame::Type_ToKernel);
        mov(ea($sp, offsetof(Frame, m_cbArgs)), 0);
        mov(ea_m_fp(), $sp);

        movsd(ea($sp, offsetof(Frame, m_dblSave)), xmm0);

        mov(ecx, CLASSD_double_float);
        call(dlllink(L".", L"Zallocate_binobj"));
        movsd(xmm0, ea($sp, offsetof(Frame,m_dblSave)));
        movsd(ea(eax, OffsetOf(DoubleFloat, m_dbl)), xmm0);

        // Pop ToKernel frame
        mov($r1, ea($sp, offsetof(Frame, m_pOuter)));
        mov(ea_m_fp(), $r1);

        add($sp, sizeof(Frame));
        ret();
    end_defun() // box-float64

    // box-int
    //  Parameters:
    //    $r0   -- int32
    //  Values:
    //    $r0   -- fixnum or bignum (1 bigit)
    defun(".BOX-INT", 0, -1)
        struct Frame : Kernel::ToKernelFrame
        {
            int32   m_iSave;
        }; // Frame

        frame_type(Fixed, sizeof(Frame));

        Label bignum;

        sub($sp, sizeof(Frame));

        mov($r1, $r0);
        sub($r1, Fixnum::MostNegative);
        cmp($r1, Fixnum::MostPositive - Fixnum::MostNegative);
        ja(bignum);

        // Fixnum::MostNegative <= $r0 <= Fixnum::MostPositive
        shl($r0, Fixnum::TagBits);

        add($sp, sizeof(Frame));
        ret();

      label(bignum);
        mov(ea($sp, offsetof(Frame, m_iSave)), $r0);

        // Push ToKernel frame
        mov($r1, ea_m_fp());
        mov(ea($sp, offsetof(Frame, m_pOuter)), $r1);
        mov(ea($sp, offsetof(Frame, m_type)),   Frame::Type_ToKernel);
        mov(ea($sp, offsetof(Frame, m_cbArgs)), 0);
        mov(ea_m_fp(), $sp);

        mov(ecx, CLASSD_bignum);
        mov(edx, Fixnum::Encode(1));
        call(dlllink(L".", L"Zallocate_binvec"));
        mov($r1, ea($sp, offsetof(Frame, m_iSave)));
        mov(ea($r0, OffsetOf(Bignum, m_rgBigit)), $r1);

        // Pop ToKernel frame
        mov($r1, ea($sp, offsetof(Frame, m_pOuter)));
        mov(ea_m_fp(), $r1);

        add($sp, sizeof(Frame));
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
    defun(".BOX-UINT", 0, -1)
        struct Frame : Kernel::ToKernelFrame
        {
            int32 m_iSave;
        }; // Frame

        frame_type(Fixed, sizeof(Frame));

        Label bignum, done;

        sub($sp, sizeof(Frame));
        cmp($r0, Fixnum::MostPositive);
        ja(bignum);

        // |$r0| <= Fixnum::MostPositive
        shl($r0, Fixnum::TagBits);
        add($sp, sizeof(Frame)); // CF=0
        ret();

      label(bignum);
        mov(ea($sp, offsetof(Frame, m_iSave)), $r0);

        // Push ToKernel frame
        mov($r1, ea_m_fp());
        mov(ea($sp, offsetof(Frame, m_pOuter)), $r1);
        mov(ea($sp, offsetof(Frame, m_type)),   Frame::Type_ToKernel);
        mov(ea($sp, offsetof(Frame, m_cbArgs)), 0);
        mov(ea_m_fp(), $sp);

        mov(ecx, CLASSD_bignum);
        mov(edx, Fixnum::Encode(1));
        call(dlllink(L".", L"Zallocate_binvec"));

        // Pop ToKernel frame
        mov($r1, ea($sp, offsetof(Frame, m_pOuter)));
        mov(ea_m_fp(), $r1);

        mov($r1, ea($sp, offsetof(Frame, m_iSave)));
        mov(ea($r0, OffsetOf(Bignum, m_rgBigit)), $r1);
        test($r1, $r1);
        jns(done);

        // parmeter $r0 is negative
        mov(ea($r0, OffsetOf(Bignum, m_length)),
            Fixnum::Encode(2) );

      label(done);
        add($sp, sizeof(Frame));
        ret();
    end_defun() // box-uint

    // unbox-int
    //  Returns LSB of integer object.
    defun(".UNBOX-INT", 0, -1)
        Label unbox_bignum, not_integer;

        test($r0, Fixnum::TagMask);
        jne(unbox_bignum);

        // unbox fixnum
        sar($r0, Fixnum::TagBits);
        ret();

      label(unbox_bignum);
        test($r0, 1);
        je(not_integer);
        cmp(ea($r0, OffsetOf(Bignum, m_classd)), CLASSD_bignum);
        jne(not_integer);

        // unbox bignum
        mov($r0, ea($r0, OffsetOf(Bignum, m_rgBigit)));
        ret();

      label(not_integer);
        mov($r1, Qinteger);
        jmp(ea($tcb, SVC_type_error));
    end_defun() // unbox_int
} // X86Builder::build_12_Number

} // Boot
