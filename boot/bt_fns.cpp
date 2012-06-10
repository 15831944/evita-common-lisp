#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - boot - thread
// boot/bt_builder.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: /proj/evcl3/boot/bt_fns.cpp 28 2006-10-22 01:40:22 yosi $
//
#include "../mini/mini_lisp.h"

namespace CommonLisp
{

Val funcall(Val, Val)
    { return nil; }

} // CommonLisp

namespace MiniLisp
{

Val print_package(Val, Val);
Val print_simple_string(Val, Val);
Val C_format(Kernel::Thread*);


Val print_function(Val obj, Val stream)
{
    format(stream, L"#<~S ~S ~X~X>",
        class_name(class_of(obj)),
        obj->Decode<NativeCodeFunction>()->m_name,
        Fixnum::Encode(obj->ToInt() >> 4),
        Fixnum::Encode(obj->ToInt() & 15) );
    return obj;
} // print_function


// print record
Val print_record(Val obj, Val stream)
{
    if (simple_string_p(obj))
    {
        return print_simple_string(obj, stream);
    }

    if (packagep(obj))
    {
        return print_package(obj, stream);
    }

    if (value_cell_p(obj))
    {
        format(stream, L"#<Value-Cell ~S ~S>",
            value_cell_type(obj),
            value_cell_name(obj) );
        return obj;
    }

    if (setf_cell_p(obj))
    {
        format(stream, L"#<Setf-Cell ~S>",
            setf_cell_name(obj) );
        return obj;
    }

    Val klass = class_of(obj);

    if (CLASS_standard_class == klass)
    {
        format(stream, L"#<Standard-Class ~S>",
            obj->Decode<Class>()->m_name );
        return obj;
    }

    if (CLASS_structure_class == klass)
    {
        format(stream, L"#<Structure-Class ~S>",
            obj->Decode<Class>()->m_name );
        return obj;
    }

    if (CLASS_built_in_class == klass)
    {
        format(stream, L"#<Built-In-Class ~S>",
            obj->Decode<Class>()->m_name );
        return obj;
    }

    if (CLASS_funcallable_standard_class == klass)
    {
        format(stream, L"#<Funcallable-Standard-Class ~S>",
            obj->Decode<Class>()->m_name );
        return obj;
    }

    if (CLASS_standard_effective_slot_definition == klass)
    {
        format(stream, L"#<Standard-Effective-Slot ~S ~D ~X~X>",
            obj->Decode<EffectiveSlotD>()->m_name,
            obj->Decode<EffectiveSlotD>()->m_location,
            Fixnum::Encode(obj->ToInt() >> 4),
            Fixnum::Encode(obj->ToInt() & 15) );
        return obj;
    }

    if (CLASS_simple_error == klass)
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
        return obj;
    }

    format(stream, L"#<~A ~X~X>",
        type_of(obj),
        Fixnum::Encode(obj->ToInt() >> 4),
        Fixnum::Encode(obj->ToInt() & 15) );

    return obj;
} // print_record


// upcase
static void upcase(char16* pwsz)
{
    while (0 != *pwsz)
    {
        if (*pwsz >= 'a' && *pwsz <= 'z')
        {
            *pwsz = static_cast<char16>(*pwsz + 'A' - 'a');
        }
        pwsz++;
    } // while
} // upcase


//////////////////////////////////////////////////////////////////////
//
// ParseSymbol
//
Val parse_symbol(LPCWSTR pwchName, LPCWSTR pwchEnd)
{
    ASSERT(pwchName < pwchEnd);

    LPCWSTR pwchColon = NULL;
    {
        LPCWSTR pwchRunner = pwchName;
        while (pwchRunner < pwchEnd)
        {
            if (':' == *pwchRunner)
            {
                pwchColon = pwchRunner;
                break;
            }
            pwchRunner++;
        } // while
    } // pwchColon

    Val package;

    bool fInternal;

    if (NULL == pwchColon)
    {
        fInternal = true;

        if (0 == TLV_ApackageA)
        {
            package = PACKAGE_si;
        }
        else
        {
            package = TLV(ApackageA);

            if (! packagep(package))
            {
                package = PACKAGE_si;
            }
        }

        if (PACKAGE_cl_user == package)
        {
            package = PACKAGE_si;
        }
    }
    else
    {
        WCHAR wszPackageName[100];

        ::CopyMemory(
            wszPackageName,
            pwchName,
            (pwchColon - pwchName) * sizeof(WCHAR) );

        wszPackageName[pwchColon - pwchName] = 0;

        upcase(wszPackageName);

        if (0 == wszPackageName[0])
        {
            package = PACKAGE_keyword;
        }
        else
        {
            StackString oPackageName(wszPackageName);
            package = find_package(oPackageName.Encode());

            if (! packagep(package))
            {
                error(L"No such package: ~S", oPackageName.Encode());
            }
        } // if

        if (':' == pwchColon[1])
        {
            fInternal = true;
            pwchName = pwchColon + 2;
        }
        else
        {
            fInternal = false;
            pwchName = pwchColon + 1;
        }
    }

    StackString oName(pwchName, pwchEnd);
        upcase(oName);

    Val name = oName;

    Val status;
    Val symbol = find_symbol(name, package, &status);

    if (nil != status)
    {
        return symbol;
    }

    if (fInternal)
    {
        if (length(package->Decode<Package>()->m_internal_table) <
            Fixnum::Encode(100) )
        {
            error(L"Can't make interanl symbol ~S in ~S.", name, package);
        }
        return intern(name, package);
    }
    else
    {
        return export_intern(oName, package);
    }
} // parse_symbol

// schedule_finalization - for stream
Val schedule_finalization(Val, Val) { return 0; }

} // MiniLisp
