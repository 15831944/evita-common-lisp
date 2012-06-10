#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - x86 - main
// arch/x86/genesis/x86_gs_main.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86/genesis/x86_gs_init.cpp#9 $
//
#include "./x86_gs_init.h"

#include "../kernel/x86_ke_thread.h"

namespace Generic
{

void __declspec(noreturn) __fastcall C_go(Val);
void __declspec(noreturn) __fastcall C_return_from(Kernel::Thread*);
void __declspec(noreturn) __fastcall C_throw(Kernel::Thread*);

} // Generic

namespace Genesis
{

void X86Initializer::Run()
{
    x86_init_03_Evaluation_and_Compilation();
    x86_init_04_Types_and_Classes();
    x86_init_05_Data_and_Control_Flow();
    x86_init_07_Objects();
    x86_init_09_Conditions();
    x86_init_10_Symbols();
    x86_init_11_Packages();
    x86_init_12_Numbers();
    x86_init_13_Characters();
    x86_init_14_Conses();
    x86_init_15_Arrays();
    x86_init_16_Strings();
    x86_init_17_Sequences();
    x86_init_18_Hash_Tables();
    x86_init_19_Filenames();
    x86_init_20_Files();
    x86_init_21_Streams();
    x86_init_22_Printer();
    x86_init_23_Reader();
    x86_init_24_System_Construction();
    x86_init_25_Environment();

    x86_init_49_Internals();
    x86_init_50_Extensions();
} // X86Initializer::Run


//  X86Initializer::x86_init_03_Evaluation_and_Compilation
void
X86Initializer::x86_init_03_Evaluation_and_Compilation()
{
    class Asm : X86Assembler
    {
        public: void Run()
        {
        } // Run
    } oAsm;
    oAsm.Run();
} // X86Initializer::x86_init_03_Evaluation_and_Compilation

//////////////////////////////////////////////////////////////////////
//
// X86Initializer::x86_init_04_Types_and_Classes
//
void X86Initializer::x86_init_04_Types_and_Classes()
{
    class Asm : X86Assembler
    {
        public: void Run()
        {
        } // Run
    } oAsm;
    oAsm.Run();
} // X86Initializer::x86_init_04_Types_and_Classes


//////////////////////////////////////////////////////////////////////
//
// X86Initializer::x86_init_05_Data_and_Control_Flow
//
void X86Initializer::x86_init_05_Data_and_Control_Flow()
{
    class Initializer05 : Initializer
    {
        public: Initializer05()
        {
            install_function(QDgo, 1, 1, L"C_go");
            install_function(QDreturn_from, 0, -1, L"C_return_from");
            install_function(QDthrow, 0, -1, L"C_throw");
        } // Initializer05
    }; // Initializer05

    Initializer05();
} // X86Initializer::x86_init_05_Control_and_Data_Flow


void X86Initializer::x86_init_07_Objects()
{
} // X86Initializer::x86_init_07_Objects

void X86Initializer::x86_init_09_Conditions()
{
    // nothing special
} // X86Initializer::x86_init_09_Conditions


void X86Initializer::x86_init_10_Symbols()
{
} // X86Initializer::x86_init_10_Symbols


// X86Initializer::x86_init_12_Numbers
void X86Initializer::x86_init_12_Numbers()
{
    class Asm : X86Assembler
    {
        public: void Run()
        {
            // Following functions are used in fdlibm implementaiton.
            // These functions should be compiler intrinsic function.
            //  decode-float32, decode-float64
            //  encode-float32, encode-float64

            // decode-float32 f32 => i32
            defun("DECODE-FLOAT32", 1, 1)
                Label not_single_float;

                lea($r1, ea($r0, -SingleFloat::Tag));
                test($r1, SingleFloat::TagMask);
                jne(not_single_float);

                cmp(ea($r1, offsetof(SingleFloat, m_classd)),
                    CLASSD_single_float );
                jne(not_single_float);

                mov($r0, ea($r1, offsetof(SingleFloat, m_flt)));
                jmp(Q(".BOX-INT"));

              label(not_single_float);
                mov($r1, Qsingle_float);
                jmp(ea($tcb, SVC_type_error));
            end_defun() // decode-float32

            // encode-float32 i32 => f32
            defun("ENCODE-FLOAT32", 1, 1)
                call(Q(".UNBOX-INT"));
                mov(ea($sp, -4), $r0);
                movss(xmm0, ea($sp, -4));
                jmp(Q(".BOX-FLOAT32"));
            end_defun() // encode-float32

            // decode-float64 f64 => hi32, li32
            defun("DECODE-FLOAT64", 1, 1)
                Label not_double_float;

                frame_type(Fixed, 8);
                sub($sp, 8);
                lea($r1, ea($r0, -DoubleFloat::Tag));
                test($r1, DoubleFloat::TagMask);
                jne(not_double_float);
                cmp(ea($r1, 0), CLASSD_double_float);
                jne(not_double_float);
                mov($r2, ea($r1, offsetof(DoubleFloat, m_dbl)+4));
                mov($r0, ea($r1, offsetof(DoubleFloat, m_dbl)));
                mov(ea($sp, 0), $r2);
                call(Q(".BOX-INT"));        // low32
                mov(ea($sp, 4), $r0);

                mov($r0, ea($sp, 0));
                mov($rn, Fixnum::Encode(1));
                // BUGBUG: NYI: set gcmap
                call(Q(".BOX-INT"));        // high32
                mov($r1, ea($sp, 4));
                mov($rn, Fixnum::Encode(2));
                add($sp, 8);
                stc();
                ret();

              label(not_double_float);
                mov($r1, Qdouble_float);
                jmp(ea($tcb, SVC_type_error));
            end_defun() // decode-float64

            // encode-float64 hi32 li32 => double-float
            defun("ENCODE-FLOAT64", 2, 2)
                frame_type(Fixed, 8);
                sub($sp, 8);

                // get high word
                mov(ea($sp, 0), $r1);
                mov($rn, Fixnum::Encode(1));
                // BUGBUG: NYI: set gcmap
                call(Q(".UNBOX-INT"));
                mov(ea($sp, 4), $r0);

                // Get low word
                mov($r0, ea($sp, 0));
                mov($rn, Fixnum::Encode(1));
                // BUGBUG: NYI: set gcmap
                call(Q(".UNBOX-INT"));
                mov(ea($sp, 0), $r0);

                movsd(xmm0, ea($sp, 0));
                add($sp, 8);
                jmp(Q(".BOX-FLOAT64"));
            end_defun() // encode-float64

            // float32-abs
            defun("FLOAT32-ABS", 1, 1)
                Label not_single_float;

                lea($r1, ea($r0, -SingleFloat::Tag));
                test($r1, 7);
                jne(not_single_float);
                cmp(ea($r1, offsetof(SingleFloat, m_classd)),
                    CLASSD_single_float );
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

                lea($r1, ea($r0, -DoubleFloat::Tag));
                test($r1, 7);
                jne(not_double_float);
                cmp(ea($r1, offsetof(DoubleFloat, m_classd)),
                    CLASSD_double_float );
                jne(not_double_float);

                movq(xmm0, ea($r1, offsetof(DoubleFloat, m_dbl)));
                pcmpeqd(xmm1, xmm1);    // all 1's
                psrlq(xmm1, 1);         // Shift out higest bit
                andpd(xmm0, xmm1);

                jmp(QDbox_float64);

              label(not_double_float);
                mov($r1, ty_double_float);
                jmp(ea($tcb, SVC_type_error));
            end_defun() //float64-abs

            // float32-sqrt
            defun("FLOAT32-SQRT", 1, 1)
                Label not_single_float;

                lea($r1, ea($r0, -SingleFloat::Tag));
                test($r1, 7);
                jne(not_single_float);
                cmp(ea($r1, offsetof(SingleFloat, m_classd)),
                    CLASSD_single_float );
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

                lea($r1, ea($r0, -DoubleFloat::Tag));
                test($r1, 7);
                jne(not_double_float);
                cmp(ea($r1, offsetof(SingleFloat, m_classd)),
                    CLASSD_double_float );
                jne(not_double_float);

                sqrtsd(xmm0, ea($r1, offsetof(DoubleFloat, m_dbl)));
                jmp(QDbox_float64);

              label(not_double_float);
                mov($r1, ty_double_float);
                jmp(ea($tcb, SVC_type_error));
            end_defun() //float64-sqrt
        } // Run
    } oAsm;
    oAsm.Run();
} // X86Initializer::x86_init_12_Numbers


// X86Initializer::x86_init_13_Characters
void X86Initializer::x86_init_13_Characters()
{
} // X86Initializer::x86_init_13_Characters


// X86Initializer::x86_init_14_Conses
void X86Initializer::x86_init_14_Conses()
{
} // X86Initializer::x86_init_14_Conses


void X86Initializer::x86_init_22_Printer() {}

void X86Initializer::x86_init_25_Environment()
{
    class Asm : X86Assembler
    {
        public: void Run()
        {
            // cpuid
            //  Exposes CPUID instruction to lisp. This function is used for
            //  implementing machine-type and machine-version.
            //
            //  This function is x86 specific.
            //
            defun("CPUID", 1, 1)
                Label bignum_param;
                Label doit;
                Label type_error;

                struct Frame
                {
                    int m_eax;
                    int m_ebx;
                    int m_ecx;
                    int m_edx;
                }; // Frame

                sub($sp, sizeof(Frame));

                test($r0, Fixnum::TagMask);
                jnz(bignum_param);
                shr($r0, Fixnum::TagBits);
                jmp(doit);

              label(bignum_param);
                lea($r1, ea($r0, -Bignum::Tag));
                test($r1, 7);
                jne(type_error);
                cmp(ea($r1, offsetof(Bignum, m_classd)), CLASSD_bignum);
                jne(type_error);
                mov($r0, ea($r1, offsetof(Bignum, m_rgBigit[0])));

              label(doit);
                xor(ebx, ebx);
                xor(ecx, ecx);
                xor(edx, edx);
                emit_op(0x0FA2);    // CPUID
                mov(ea($sp, offsetof(Frame, m_eax)), eax);
                mov(ea($sp, offsetof(Frame, m_ebx)), ebx);
                mov(ea($sp, offsetof(Frame, m_ecx)), ecx);
                mov(ea($sp, offsetof(Frame, m_edx)), edx);

                mov($rn, Fixnum::Encode(1));
                call(QDbox_uint);
                mov(ea($sp, offsetof(Frame, m_eax)), $r0);

                mov($r0, ea($sp, offsetof(Frame, m_ebx)));
                mov($rn, Fixnum::Encode(1));
                call(QDbox_uint);
                // FIXME 2007-06-24 We must mark Frame.m_eax is alive.
                mov(ea($sp, offsetof(Frame, m_ebx)), $r0);

                mov($r0, ea($sp, offsetof(Frame, m_ecx)));
                mov($rn, Fixnum::Encode(1));
                call(QDbox_uint);
                // FIXME 2007-06-24 We must mark Frame.m_e{a,b}x is alive.
                mov(ea($sp, offsetof(Frame, m_ecx)), $r0);

                mov($r0, ea($sp, offsetof(Frame, m_edx)));
                mov($rn, Fixnum::Encode(1));
                call(QDbox_uint);
                // FIXME 2007-06-24 We must mark Frame.m_e{a,b,c} are alive.

                mov($r3, $r0);
                mov($r2, ea($sp, offsetof(Frame, m_ecx)));
                mov($r1, ea($sp, offsetof(Frame, m_ebx)));
                mov($r0, ea($sp, offsetof(Frame, m_eax)));
                mov($rn, Fixnum::Encode(4));

                add($sp, sizeof(Frame));
                stc();
                ret();

              label(type_error);
                mov($r1, ty_unsigned_byte_32);
                jmp(ea($tcb, SVC_type_error));
            end_defun()
        } // Run
    } oAsm;
    oAsm.Run();
} // X86Initializer::x86_init_25_Environment


//////////////////////////////////////////////////////////////////////
//
// Initialize::Run
//
void Initializer::Run()
{
    prepare();

    {
        X86Initializer oIniter;
        oIniter.Run();
    }

    init_all();
} // Initializer::Run

} // Genesis
