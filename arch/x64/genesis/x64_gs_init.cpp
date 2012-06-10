#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - x64 - main
// arch/x64/genesis/x64_gs_main.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x64/genesis/x64_gs_init.cpp#9 $
//
#include "./x64_gs_init.h"

#include "../kernel/x64_ke_thread.h"    // SVC_error

namespace Genesis
{

using namespace X64;

void X64Initializer::Run()
{
    class Asm : X64Assembler
    {
        public: void Run()
        {
            // decode-float32
            //  Returns binary representation of single-float as int32.
            //
            //  This functtion is used for libm functions.
            //
            defun_(Q("DECODE-FLOAT32"), 1, 1, Fixed, 8)
                Label   not_single_float;

                lea($r1, ea($r0, -SingleFloat::Tag));
                test($r1, SingleFloat::TagMask);
                jne(not_single_float);

                mov($r3, CLASSD_single_float);
                cmp(ea($r1, offsetof(SingleFloat, m_classd)), $r3);
                jne(not_single_float);

                movsxd($r0, dword_ptr_($r1, offsetof(SingleFloat, m_flt)));
                shl($r0, 3);
                xor($r1, $r1);  // CF=0
                ret();

              label(not_single_float);
                mov($r1, Qsingle_float);
                jmp(ea($tcb, SVC_type_error));
            end_defun(); // decode-float32

            // encode-float32 i32 => f32
            //  This functtion is used for libm functions.
            defun_(Q("ENCODE-FLOAT32"), 1, 1, Fixed, 8)
                call(Q(".UNBOX-INT"));
                mov(ea($sp, -8), $r0);
                movss(xmm0, ea($sp, -8));
                jmp(Q(".BOX-FLOAT32"));
            end_defun() // encode-float32

            // decode-float64
            //  Returns binary representation of double-float as two int32.
            //
            //  This functtion is used for libm functions.
            //
            //  See Also: decode-float32
            //  See Also: encode-float32
            //  See Also: encode-float64
            defun_(Q("DECODE-FLOAT64"), 1, 1, Fixed, 8)
                Label   not_double_float;

                lea($r1, ea($r0, -DoubleFloat::Tag));
                test($r1, DoubleFloat::TagMask);
                jne(not_double_float);

                mov($r3, CLASSD_double_float);
                cmp(ea($r1, offsetof(DoubleFloat, m_classd)), $r3);
                jne(not_double_float);

                // Load high 32bit part of double-float
                movsxd($r0, dword_ptr_($r1, offsetof(DoubleFloat, m_dbl)+4));
                shl($r0, 3);

                // Load low 32bit part of double-float
                movsxd($r1, dword_ptr_($r1, offsetof(DoubleFloat, m_dbl)+0));
                shl($r1, 3);

                mov($rn, Fixnum::Encode(2));
                stc();
                ret();

              label(not_double_float);
                mov($r1, Qdouble_float);
                jmp(ea($tcb, SVC_type_error));
            end_defun();

            // encode-float64
            //  This functtion is used for libm functions.
            //
            //  See Also: decode-float32
            //  See Also: encode-float32
            //  See Also: decode-float64
            defun_(Q("ENCODE-FLOAT64"), 2, 2, Fixed, 8)
                Label   not_int0, not_int1;

                test($r0, 7);
                jne(not_int0);

                test($r1, 7);
                jne(not_int1);

                shr($r0, 3);
                shr($r1, 3);

                mov(dword_ptr_($sp, -8), $r1d);
                mov(dword_ptr_($sp, -4), $r0d);
                movsd(xmm0, ea($sp, -8));
                jmp(Q(".BOX-FLOAT64"));

              label(not_int1);
                mov($r0, $r1);

              label(not_int0);
                mov($r1, Qinteger);
                jmp(ea($tcb, SVC_type_error));
            end_defun() // encode-float64

            // float32-abs
            defun("FLOAT32-ABS", 1, 1)
                Label not_single_float;

                mov($r2, CLASSD_single_float);

                lea($r1, ea($r0, -SingleFloat::Tag));
                test($r1, 7);
                jne(not_single_float);

                cmp(ea($r1, offsetof(SingleFloat, m_classd)), $r2);
                jne(not_single_float);

                mov(eax, ea($r1, offsetof(SingleFloat, m_flt)));
                and(eax, 0x7FFFFFFF);
                movd(xmm0, eax);

                jmp(QDbox_float32);

              label(not_single_float);
                mov($r1, ty_single_float);
                jmp(ea($tcb, SVC_type_error));
            end_defun() //float32-abs

            // float64-abs
            defun("FLOAT64-ABS", 1, 1)
                Label not_double_float;

                mov($r2, CLASSD_double_float);

                lea($r1, ea($r0, -DoubleFloat::Tag));
                test($r1, 7);
                jne(not_double_float);
                cmp(ea($r1, offsetof(SingleFloat, m_classd)), $r2);
                jne(not_double_float);

                mov($r0, ea($r1, offsetof(DoubleFloat, m_dbl)));
                mov($r1, 0x7fffffffffffffffull);
                and($r0, $r1);
                movq(xmm0, $r0);

                jmp(QDbox_float64);

              label(not_double_float);
                mov($r1, ty_double_float);
                jmp(ea($tcb, SVC_type_error));
            end_defun() //float64-abs

            // float32-sqrt
            defun("FLOAT32-SQRT", 1, 1)
                Label not_single_float;

                mov($r2, CLASSD_single_float);

                lea($r1, ea($r0, -SingleFloat::Tag));
                test($r1, 7);
                jne(not_single_float);

                cmp(ea($r1, offsetof(SingleFloat, m_classd)), $r2);
                jne(not_single_float);

                sqrtss(xmm0, ea($r1, offsetof(SingleFloat, m_flt)));

                jmp(QDbox_float32);

              label(not_single_float);
                mov($r1, ty_single_float);
                jmp(ea($tcb, SVC_type_error));
            end_defun() //float32-sqrt

            // float64-sqrt
            defun("FLOAT64-SQRT", 1, 1)
                Label not_double_float;

                mov($r2, CLASSD_double_float);

                lea($r1, ea($r0, -DoubleFloat::Tag));
                test($r1, 7);
                jne(not_double_float);
                cmp(ea($r1, offsetof(SingleFloat, m_classd)), $r2);
                jne(not_double_float);

                sqrtsd(xmm0, ea($r1, offsetof(DoubleFloat, m_dbl)));

                jmp(QDbox_float64);

              label(not_double_float);
                mov($r1, ty_double_float);
                jmp(ea($tcb, SVC_type_error));
            end_defun() //float64-sqrt

            // cpuid
            //  Exposes CPUID instruction to lisp. This function is used for
            //  implementing machine-type and machine-version.
            //
            //  This function is x86 specific.
            //
            defun_(Q("CPUID"), 1, 1, Fixed, 8)
                shr($r0, Fixnum::TagBits);
                emit_op(0x0FA2);    // CPUID
                mov($r3, edx);      // esi <- edx
                mov($r1, ebx);      // edx <- ebx
                mov($r2, ecx);      // ebx <- ecx
                shl($r0, Fixnum::TagBits);  // eax
                shl($r1, Fixnum::TagBits);  // edx
                shl($r2, Fixnum::TagBits);  // ebx
                shl($r3, Fixnum::TagBits);  // esi
                mov($rn, Fixnum::Encode(4));
                stc();
                ret();
            end_defun()
        } // Run
    } oAsm;
    oAsm.Run();
} // X64Initializer::Run


//////////////////////////////////////////////////////////////////////
//
// Initialize::Run
//
void Initializer::Run()
{
    prepare();

    {
        X64Initializer oIniter;
        oIniter.Run();
    }

    install_function(QDgo, 1, 1, L"C_go");
    install_function(QDreturn_from, 0, -1, L"C_return_from");
    install_function(QDthrow, 0, -1, L"C_throw");

    init_all();
} // Initializer::Run

} // Genesis
