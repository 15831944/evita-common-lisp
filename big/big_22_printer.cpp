#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - 22 Printer
// big/big_22_printer.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/big/big_22_printer.cpp#7 $
//

#include "../mini/mini_lisp.h"

namespace MiniLisp
{

Val print_object_default(Val, Val);

Val print_character(Val, Val);
Val print_cons(Val, Val);
Val print_double_float(Val, Val);
Val print_double_float_complex(Val, Val);
Val print_function(Val, Val);
Val print_integer(Val, Val);
Val print_package(Val, Val);
Val print_single_float(Val, Val);
Val print_single_float_complex(Val, Val);
Val print_record(Val, Val);
Val print_simple_string(Val, Val);
Val C_format(Kernel::Thread*);

// print_function
Val print_function(Val obj, Val stream)
{
    switch (CLASSD_INDEX_OF(obj->Decode<Funcallable>()->m_classd))
    {
    case CLASSD_(native_code_function):
        format(stream, L"#<Function ~S ~X~X>",
            obj->Decode<NativeCodeFunction>()->m_name,
            Fixnum::Encode(obj->ToInt() >> 4),
            Fixnum::Encode(obj->ToInt() & 15) );
        break;

    case CLASSD_(native_code_closure):
        format(stream, L"#<Closure ~S ~X~X>",
            obj->Decode<NativeCodeFunction>()->m_name,
            Fixnum::Encode(obj->ToInt() >> 4),
            Fixnum::Encode(obj->ToInt() & 15) );
        break;

    case CLASSD_(standard_generic_function):
        format(stream, L"#<Generic-Function ~S ~X~X>",
            obj->Decode<StandardGenericFunction>()->m_name,
            Fixnum::Encode(obj->ToInt() >> 4),
            Fixnum::Encode(obj->ToInt() & 15) );
        break;

    default:
        format(stream, L"#<~S ~S ~X~X>",
            class_name(class_of(obj)),
            obj->Decode<NativeCodeFunction>()->m_name,
            Fixnum::Encode(obj->ToInt() >> 4),
            Fixnum::Encode(obj->ToInt() & 15) );
        break;
    } // switch classd

    return obj;
} // print_function

// print_condition
Val print_condition(Val obj, Val stream)
{
    if (nil !=  MiniThread::Get()->GetTlv(TLV_Aprint_escapeA))
    {
        return print_object_default(obj, stream);
    }

    switch (CLASSD_INDEX_OF(obj->Decode<Instance>()->m_classd))
    {
    case CLASSD_(simple_error):
    case CLASSD_(simple_warning):
    case CLASSD_(c6_simple_style_warning):
    case CLASSD_(c6_simple_program_error):
    {
         MiniThread::Get()->mv_value[0] = stream;
         MiniThread::Get()->mv_value[1] =
            obj->Decode<SimpleError>()->m_format_control;

        Val runner = obj->Decode<SimpleError>()->m_format_arguments;
        Int iIndex = 2;
        while (! endp(runner))
        {
             MiniThread::Get()->mv_value[iIndex] = car(runner);
            iIndex += 1;
            runner = cdr(runner);
        } // while
         MiniThread::Get()->m_n = Fixnum::Encode(iIndex);
        C_format(MiniThread::Get());
        break;
    } // simple_error

    case CLASSD_(type_error):
        format(stream,
            L"Expected ~S instead of ~S.",
            obj->Decode<TypeError>()->m_expected_type,
            obj->Decode<TypeError>()->m_datum );
        break;

    case CLASSD_(not_function):
        format(stream,
            L"~S is not function.",
            obj->Decode<NotFunction>()->m_name );
        break;

    case CLASSD_(undefined_function):
        format(stream,
            L"Function ~S is undefined.",
            obj->Decode<UndefinedFunction>()->m_name );
        break;

    case CLASSD_(too_few_arguments):
        format(stream,
            L"Function ~S receives too few arguments: ~S",
            obj->Decode<TooFewArguments>()->m_function,
            obj->Decode<TooFewArguments>()->m_arguments );
        break;

    case CLASSD_(too_many_arguments):
        format(stream,
            L"Function ~S receives too many arguments: ~S",
            obj->Decode<TooManyArguments>()->m_function,
            obj->Decode<TooManyArguments>()->m_arguments );
        break;

    default:
        print_object_default(obj, stream);
        break;
    } // switch classd

    return obj;
} // print_condition

// mapcar_class_name
Val mapcar_class_name(Val classes)
{
    Val names = nil;
    Collector oNames(&names);
    foreach (EnumList, oEnum, classes)
    {
        oNames.Add(oEnum.Get()->Decode<Class>()->m_name);
    } // for each class
    return names;
} // mapcar_class_name


// print record
Val print_record(Val obj, Val stream)
{
    switch (CLASSD_INDEX_OF(obj->Decode<Record>()->m_classd))
    {
    case CLASSD_(bignum):
        print_integer(obj, stream);
        break;

    case CLASSD_(double_float):
        print_double_float(obj, stream);
        break;

    case CLASSD_(hash_table):
        format(stream, L"#<Hash-Table ~A ~D/~D ~X~X>",
            hash_table_test(obj),
            hash_table_count(obj),
            truncate_xx(length(obj->Decode<HashTable>()->m_vector), 2),
            Fixnum::Encode(obj->ToInt() >> 4),
            Fixnum::Encode(obj->ToInt() & 15) );
        break;

    case CLASSD_(package):
        print_package(obj, stream);
        break;

    case CLASSD_(ratio):
        print_object(obj->Decode<Ratio>()->m_num);
        write_char('/', stream);
        print_object(obj->Decode<Ratio>()->m_den);
        break;

    case CLASSD_(simple_string):
        print_simple_string(obj, stream);
        break;

    case CLASSD_(simple_vector):
    {
        if (nil == TLV(Aprint_arrayA))
        {
            goto print_default;
        }
        write_string(L"#(", stream);
        Int len = Fixnum::Decode_(length(obj));
        Int end = len;
        if (fixnump(TLV(Aprint_lengthA)))
        {
            Int limit = Fixnum::Decode_(TLV(Aprint_lengthA));
            if (end > limit)
            {
                end = limit;
            }
        }

        LPCWSTR pwsz = L"";
        for (Int i = 0; i < end; i++)
        {
            write_string(pwsz, stream);
            print_object(svref(obj, i), stream);
            pwsz = L" ";
        }

        if (end != len)
        {
            write_string(pwsz, stream);
            write_string(L" ...", stream);
        }

        write_string(L")", stream);
        break;
    } // simple_vector

    case CLASSD_(single_float):
        print_single_float(obj, stream);
        break;

    case CLASSD_(class_description):
        format(stream, L"#<Class-Description ~S ~X~X>",
            obj->Decode<ClassD>()->m_class->Decode<Class>()->m_name,
            Fixnum::Encode(obj->ToInt() >> 4),
            Fixnum::Encode(obj->ToInt() & 15) );
        break;

    case CLASSD_(double_float_complex):
        print_double_float_complex(obj, stream);
        break;

    case CLASSD_(value_cell):
        format(stream, L"#<Value-Cell ~S ~S>",
            value_cell_type(obj),
            value_cell_name(obj) );
        break;

    case CLASSD_(setf_cell):
        format(stream, L"#<Setf-Cell ~S>",
            setf_cell_name(obj) );
        break;

    case CLASSD_(rational_complex):
        format(stream, L"#c(~S ~S)",
            obj->Decode<RationalComplex>()->m_real,
            obj->Decode<RationalComplex>()->m_imag );
        break;

    case CLASSD_(single_float_complex):
        print_single_float_complex(obj, stream);
        break;

    case CLASSD_(tlv_record):
        format(stream, L"#<Tlv-Record ~S>", tlv_record_name(obj));
        break;

    case CLASSD_(param_info):
        format(stream, L"#<Param-Info ~S ~X~X>",
            obj->Decode<ParamInfo>()->m_lambda_list,
            Fixnum::Encode(obj->ToInt() >> 4),
            Fixnum::Encode(obj->ToInt() & 15) );
        break;

    case CLASSD_(standard_class):
        format(stream, L"#<Standard-Class ~S>",
            obj->Decode<Class>()->m_name );
        break;

    case CLASSD_(built_in_class):
        format(stream, L"#<Built-In-Class ~S>",
            obj->Decode<Class>()->m_name );
        break;

    case CLASSD_(funcallable_standard_class):
        format(stream, L"#<Funcallable-Standard-Class ~S>",
            obj->Decode<Class>()->m_name );
        break;

    case CLASSD_(structure_class):
        format(stream, L"#<Structure-Class ~S>",
            obj->Decode<Class>()->m_name );
        break;

    case CLASSD_(standard_method):
    {
        StandardMethod* p = obj->Decode<StandardMethod>();
        Val gf = p->m_generic_function; 
        format(stream, L"#<Standard-Method ~S ~S ~S ~X~X>",
            subclassp(class_of(gf), CLASS_standard_generic_function) ?
                gf->Decode<StandardGenericFunction>()->m_name :
                gf,
            p->m_qualifiers,
            consp(p->m_specializers) ?
                mapcar_class_name(p->m_specializers) :
                p->m_specializers,
            Fixnum::Encode(obj->ToInt() >> 4),
            Fixnum::Encode(obj->ToInt() & 15) );
        break;
    }

    case CLASSD_(standard_direct_slot_definition):
        format(stream, L"#<Standard-Direct-Slot-Definition ~S ~X~X>",
            obj->Decode<DirectSlotD>()->m_name,
            Fixnum::Encode(obj->ToInt() >> 4),
            Fixnum::Encode(obj->ToInt() & 15) );
        break;

    case CLASSD_(standard_effective_slot_definition):
        format(stream, L"#<Standard-Effective-Slot-Definition ~S ~D ~X~X>",
            obj->Decode<EffectiveSlotD>()->m_name,
            obj->Decode<EffectiveSlotD>()->m_location,
            Fixnum::Encode(obj->ToInt() >> 4),
            Fixnum::Encode(obj->ToInt() & 15) );
        break;

    case CLASSD_(structure_direct_slot_definition):
        format(stream, L"#<Structure-Direct-Slot-Definition ~S ~X~X>",
            obj->Decode<DirectSlotD>()->m_name,
            Fixnum::Encode(obj->ToInt() >> 4),
            Fixnum::Encode(obj->ToInt() & 15) );
        break;

    case CLASSD_(structure_effective_slot_definition):
        format(stream, L"#<Structure-Effective-Slot-Definition ~S ~D ~X~X>",
            obj->Decode<EffectiveSlotD>()->m_name,
            obj->Decode<EffectiveSlotD>()->m_location,
            Fixnum::Encode(obj->ToInt() >> 4),
            Fixnum::Encode(obj->ToInt() & 15) );
        break;

    print_default:
    default:
        if (subclassp(class_of(obj), CLASS_condition))
        {
            print_condition(obj, stream);
        }
        else
        {
            print_object_default(obj, stream);
        }
        break;
    } // switch classd

    return obj;
} // print_record

} // MiniLisp
