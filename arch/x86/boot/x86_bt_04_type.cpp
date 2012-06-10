#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - boot - 04 Types and Classes
// arch/x86/boot/x86_bt_04_type.inc
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86/boot/x86_bt_04_type.cpp#6 $
//
// Description:
//  This file contains functions for "03 Evaluation and Compilation".
//
#include "./x86_bt_builder.h"

namespace Boot
{

class TypepBuilder : public X86Assembler
{
    public: static void Run()
        { TypepBuilder o; o.run(); }

    // defun_typep
    //  atom
    //  consp
    //  fixnump
    //  listp
    void defun_typep(
        LPCWSTR pwszName,
        int     iSub,
        int     iMask )
    {
        Defun oDefun(this, parse_symbol(pwszName), 1, 1);

            emit_prologue(&oDefun);

            if (0 == iSub)
            {
                mov($r1, $r0);
            }
            else
            {
                lea($r1, ea($r0, -iSub));
            }

            and($r1, iMask);    // also set CF=0

            if (Tag_Null == iSub && 3 == iMask)
            {
                mov($r0, t);
            }

            mov($r2, nil);
            cmovne($r0, $r2);
            ret();

            emit_epilogue(&oDefun);
            installFunObj(&oDefun);
    } // defun_typep


    // defun_typep
    //      Check record tag and classd.
    //
    //  hash-table-p
    //  packagep
    void defun_typep(
        LPCWSTR pwszName,
        Val     classd )
    {
        defun_(parse_symbol(pwszName), 1, 1);
            Label not_type;

            mov($r2, nil);
            lea($r1, ea($r0, -Tag_Record));
            and($r1, Mask_Tag >> 1);    // also set CF=0
            jne(not_type);

            cmp(ea($r0, OffsetOf(Record, m_classd)), classd);
            cmovne($r0, $r2);
            xor($r1, $r1);  // CF=0
            ret();

          label(not_type);
            mov($r0, $r2);
            ret();

        end_defun();
    } // defun_typep


    // deftype_typep2
    //  Check record tag and range of classd.
    //      arrayp
    //      simple-arrayp
    //      vectorp
    void defun_typep2(
        LPCWSTR pwszName,
        Val     classd_min,
        Val     classd_max )
    {
        ASSERT(classd_min <= classd_max);

        defun_(parse_symbol(pwszName), 1, 1);
            Label ret_false;
            Label ret_true;

            if (classd_min == CLASSD_bignum)
            {
                mov($r1, $r0);
                and($r1, Fixnum::TagMask);    // CF=0
                je(ret_true);
            } // if

            mov($r2, nil);

            // Is record pointer?
            lea($r1, ea($r0, -Tag_Record));
            and($r1, Mask_Tag >> 1);    // CF=0
            jne(ret_false);

            mov($r1, ea($r0, offsetof(Record, m_classd) - Tag_Record));

            if (classd_min == classd_max)
            {
                cmp($r1, classd_min);
                cmovne($r0, $r2);
            }
            else
            {
                sub($r1, classd_min);
                cmp(
                    $r1, 
                    static_cast<int32>(
                        classd_max->ToInt() - classd_min->ToInt() ) );
                cmova($r0, $r2);
            } // if

            xor($r1, $r1);  // CF=0
            ret();

          label(ret_false);
            mov($r0, $r2);

          label(ret_true);
            ret();
        end_defun();
    } // defun_typep2

    //  defun_typep_or
    //      bit-vector-p
    //          bit-vector  simple-bit-vector
    //      stringp
    //          string simplestring
    void defun_typep_or(
        LPCWSTR pwszName,
        Val     classd1,
        Val     classd2 )
    {
        defun_(parse_symbol(pwszName), 1, 1);
            Label ret_false;
            Label ret_true;

            mov($r2, nil);
            lea($r1, ea($r0, -Tag_Record));
            and($r1, Mask_Tag >> 1);    // CF=0
            jne(ret_false);

            mov($r1, ea($r0, offsetof(Record, m_classd) - Tag_Record));
            cmp($r1, classd1);
            je(ret_true);

            cmp($r1, classd2);
            cmovne($r0, $r2);
            xor($r1, $r1);              // CF=0

          label(ret_true);
            ret();

          label(ret_false);
            mov($r0, $r2);
            ret();
        end_defun();
    } // defun_typep_or


    void run()
    {
        // 5 Data and Control Flow
        defun_typep(L"COMPILED-FUNCTION-P", Funcallable::Tag, Mask_Tag);
        defun_typep(L"FUNCTIONP", Funcallable::Tag, Mask_Tag);

        defun_typep(L"PACKAGEP", CLASSD_package);
        defun_typep(L"HASH-TABLE-P", CLASSD_hash_table);
        defun_typep(L"READTABLEP", CLASSD_readtable);

        // 12 Numbers
        defun_typep2(L"INTEGERP",  CLASSD_bignum,      CLASSD_bignum);
        defun_typep2(L"RATIONALP", CLASSD_bignum,      CLASSD_ratio);
        defun_typep2(L"FLOATP",    CLASSD_float_min,   CLASSD_float_max);
        defun_typep2(L"REALP",     CLASSD_bignum,      CLASSD_real_max);
        defun_typep2(L"COMPLEXP",  CLASSD_complex_min, CLASSD_complex_max);
        defun_typep2(L"NUMBERP",   CLASSD_bignum,      CLASSD_complex_max);


        // Cons Pointer
        //  ---- ---- ---- -111 cons (align 8 byte)
        //  ---- ---- ---- 1111 cons (align 8 byte)
        //  Tag_Cons  = 7
        //  Mask_Tag = 15
        //  (p - 15) & 7 == 0
        defun_typep(L"CONSP", Cons::Tag, Cons::TagMask);

        // List Pointer = Cons or Nil Pointer
        //  ---- ---- ---- -011 nil
        //  ---- ---- ---- -111 cons
        //  ---- ---- ---- 1111 cons
        // Tag_Null = 3
        defun_typep(L"LISTP", List::Tag, List::TagMask);

        // 15 Arrays
        defun_typep2(L"CL:ARRAYP", CLASSD_array_min,  CLASSD_array_max); 

        defun_typep_or(L"CL:BIT-VECTOR-P",
            CLASSD_simple_bit_vector,
            CLASSD_bit_vector );

        defun_typep(L"CL:SIMPLE-BIT-VECTOR-P",
            CLASSD_simple_bit_vector );

        defun_typep(L"CL:SIMPLE-VECTOR-P",
            CLASSD_simple_vector );

        defun_typep2(L"CL:VECTORP",
            CLASSD_vector_min, CLASSD_vector_max );

        // 16 Strings
        defun_typep(L"SIMPLE-STRING-P", CLASSD_simple_string);
        defun_typep_or(L"STRINGP", CLASSD_simple_string, CLASSD_string);

        // 49 Internals
        #if 0
            defun_typep(L"SI:MARKERP",         Tag_Marker, Mask_Tag >> 1);
            defun_typep(L"SI:VALUE-CELL-P",    CLASSD_value_cell);
            defun_typep(L"SI:RECORDP",         Tag_Record, Mask_Tag >> 1);
            defun_typep(L"SI:CLOSED-CELL-P",    CLASSD_closed_cell);
        #endif

        // 50 Extensions
        defun_typep(L"EXT:LATCHP", CLASSD_latch);
        defun_typep(L"EXT:MUTEXP", CLASSD_mutex);
    } // run
}; // TypepBuilder


// X86Builder::build_04_Type
void X86Builder::build_04_Type()
{
    TypepBuilder::Run();

    // .type-error
    //      $r0 = datum
    //      $r1 = expected_type
    defun(".TYPE-ERROR", 0, -1)
        mov($r4, $r1);
        mov($r3, Kexpected_type);
        mov($r2, $r0);
        mov($r1, Kdatum);
        mov($r0, Qtype_error);
        mov($rn, Fixnum::Encode(5));
        jmp(ea($tcb, SVC_error));
    end_defun()

    // type-of
    defun("TYPE-OF", 1, 1)
        Label tagtbl;

        mov($r1, $r0);
        and($r1, 15);
        add($r1, $r1);  // $r1 = ($r1 & 15) * 2
        add($r1, $r1);  // $r1 = ($r1 & 15) * 4
        jmp(ea($r1, tagtbl));

      label_def(tag_fixnum);
        mov($r0, Qfixnum);
        ret();

      label_def(tag_null);
        mov($r0, Qnull);
        ret();

      label_def(tag_instance);
        mov($r0, ea($r0, OffsetOf(Instance, m_classd)));
        mov($r0, ea($r0, OffsetOf(ClassD, m_typespec)));
        ret();

      label_def(tag_record);
        mov($r0, ea($r0, OffsetOf(Record, m_classd)));
        mov($r0, ea($r0, OffsetOf(ClassD, m_typespec)));
        ret();

      label_def(tag_cons);
        mov($r0, Qcons);
        ret();

      label_def(tag_function);
        mov($r0, ea($r0, OffsetOf(Function, m_classd)));
        mov($r0, ea($r0, OffsetOf(ClassD, m_typespec)));
        ret();

      label_def(tag_invalid);
        mov($r0, Qinvalid_object);
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
    end_defun() // type-of
} // X86Builder::build_04_Type

} // Boot
