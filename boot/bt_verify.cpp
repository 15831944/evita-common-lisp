#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - boot - Image Writer
// boot/bt_writer.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: /proj/evcl3/boot/bt_verify.cpp 29 2006-10-13 22:46:47 yosi $
//
#include "../mini/mini_lisp.h"

namespace Boot
{

using namespace Debugger;

enum { DONT_CARE = - 1 };

static void
verify_cpl(Val name, ...)
{
    Val klass = find_class(name);
    Val cpl = klass->Decode<Class>()->m_class_precedence_list;

    va_list args;
    va_start(args, name);

    Val runner = cpl;
    while (consp(runner))
    {
        if (car(runner) != klass)
        {
            format(t, L"~2%; expect ~S instead of ~S.~%", klass, car(runner));
            format(t, L"cpl=~S~%", cpl);
            error(L"Bad cpl: ~S", cpl);
        }

        runner = cdr(runner);
        if (CLASS_t == klass)
        {
            break;
        }

        name = va_arg(args, Val);
        klass = find_class(name);
    } // while

    va_end(args);

    if (nil != runner)
    {
        error(L"Malformed CPL: ~S", cpl);
    }
} // verify_cpl

#define verify_eq(mp_x, mp_y) \
{ \
    if (mp_x != mp_y) \
    { \
        Fail(L"Must be equal\r\n" \
            L## #mp_x L"(%d)\r\n" \
            L## #mp_y L"(%d)\r\n", mp_x, mp_y ); \
    } \
} // verify_eq

static void verify_subclass(Val klass, bool fSubclass, Val super)
{
    if (fSubclass)
    {
        if (! subclassp(klass, super))
        {
            error(L"~S must be a subclass of ~S.", klass, super);
        }
    }
    else
    {
        if (subclassp(klass, super))
        {
            error(L"~S must be NOT a subclass of ~S.", klass, super);
        }
    }
} // verify_subclass


static void verify_built_in_class(Val klass)
{
    BuiltInClass* pClass = klass->Decode<BuiltInClass>();

    Val instanced = pClass->m_instanced;
    if (nil == instanced)
    {
        ASSERT(nil == pClass->m_slots);
        return;
    }

    ClassD* pInstanceD = instanced->Decode<ClassD>();

    if (ClassD::Format_Fixed == pInstanceD->m_format->ToInt())
    {
        verify_eq(
            pInstanceD->m_format_misc, 
            length(pInstanceD->m_slots) );
    }


    if (CLASS_cons == klass)
    {
        verify_eq(
            pInstanceD->m_format->ToInt(),
            ClassD::Format_Cons );

        verify_eq(
            length(pClass->m_slots),
            Fixnum::Encode(2) );
        return;
    }

    if (CLASS_symbol == klass)
    {
        verify_eq(
            pInstanceD->m_format->ToInt(),
            ClassD::Format_Fixed );

        verify_eq(
            length(pClass->m_slots),
            Fixnum::Encode(sizeof(Symbol) / sizeof(Val)) );
        return;
    }

    if (CLASS_null == klass)
    {
        return;
    }

#if 0
    if (CLASS_character == klass)
    {
        verify_eq(
            pInstanceD->m_format->ToInt(),
            ClassD::Format_Immediate );
        return;
    }
#endif

    if (CLASS_fixnum == klass)
    {
        verify_eq(
            pInstanceD->m_format->ToInt(),
            ClassD::Format_Immediate );
        return;
    }

    if (CLASS_invalid_object == klass)
    {
        return;
    }

    switch (pInstanceD->m_format->ToInt())
    {
    case ClassD::Format_Immediate:
        break;

    case ClassD::Format_Function:
    {
        Val eslotd = second(pClass->m_slots);

        if (eslotd->Decode<EffectiveSlotD>()->m_name != Q("SI::CLASSD") )
        {
            error(L"~S must have classd slot.", klass);
        }
        break;
    } // function

    case ClassD::Format_BinFixed:
    case ClassD::Format_Fixed:
    case ClassD::Format_Instance:
    case ClassD::Format_Mixed:
    {
        Val eslotd = first(pClass->m_slots);

        if (eslotd->Decode<EffectiveSlotD>()->m_name != Q("SI::CLASSD") )
        {
            error(L"~S must have classd slot.", klass);
        }
    } // fixed

    } // switch format
} // verify_built_in_class


// verify_std_class
static void verify_std_class(
    Val             klass,
    ClassD::Format  eFormat,
    int             iTag )
{
    StandardClass* pClass = klass->Decode<StandardClass>();

    if (nil == pClass->m_instanced)
    {
        ASSERT(nil == pClass->m_slots);
        return;
    }

    ClassD* pInstanceD = pClass->m_instanced->Decode<ClassD>();

    if (pInstanceD->m_typespec != pClass->m_name)
    {
        error(L"~S: m_typespec: expect ~S, but ~S.",
            klass,
            pClass->m_name,
            pInstanceD->m_typespec );
    }

    if (pInstanceD->m_format != FromInt<Val_>(eFormat))
    {
        error(L"~S: m_format: expect ~D, but ~D.",
            klass,
            FromInt<Val_>(ClassD::Format_Instance),
            pInstanceD->m_format_misc );
    }

    if (pClass->m_slots != pInstanceD->m_slots)
    {
        error(L"~S: m_slots is wrong.", klass);
    }

    if (length(pInstanceD->m_slots) != pInstanceD->m_format_misc)
    {
        error(L"~S: m_format_misc: expect ~D, but ~D.",
            klass,
            length(pInstanceD->m_slots),
            pInstanceD->m_format_misc );
    }

    if (nil != pClass->m_slots)
    {
        Val eslotd = first(pClass->m_slots);
        ASSERT(eslotd->Decode<EffectiveSlotD>()->m_name != Q("SI::CLASSD"));
    }

    if (Fixnum::Encode(iTag) != pInstanceD->m_tag_code)
    {
        error(L"~S: m_tag_code: expect ~D, but ~D.",
            klass,
            Fixnum::Encode(iTag),
            pInstanceD->m_tag_code );
    }

    switch (iTag)
    {
    case Instance::Tag:
        if (pInstanceD->m_format_param !=
            add_xx(pInstanceD->m_format_misc, 4) )
        {
            error(L"~S: m_format_param: expect ~D, but ~D.",
                klass,
                add_xx(pInstanceD->m_format_misc, 4),
                pInstanceD->m_format_param );
        }
        break;

    case FuncallableInstance::Tag:
        if (pInstanceD->m_format_param != FromInt<Val_>(sizeof(FuncallableInstance)))
        {
            error(L"~S: m_format_param: expect ~D, but ~D.",
                klass,
                FromInt<Val_>(sizeof(FuncallableInstance)),
                pInstanceD->m_format_param );
        }
        break;
    default:
        CAN_NOT_HAPPEN();
    } // switch tag
} // verify_std_class


// verify_fsc
//  o m_instanced.m_format = Format_FuncallableInstance
//  o m_instanced.m_format_param = 0
//  o m_instanced.m_format != 0
//  o m_instanced.m_tag_code == FuncallableInstance::Tag
static void verify_fsc(Val klass)
{
    verify_subclass(klass, 0, CLASS_structure_object);
    verify_subclass(klass, 1, CLASS_standard_object);
    verify_subclass(klass, 1, CLASS_funcallable_standard_object);

    verify_std_class(
        klass,
        ClassD::Format_FuncallableInstance,
        FuncallableInstance::Tag );

    Val instanced = klass->Decode<Class>()->m_instanced;
    if (nil == instanced) return;

    ClassD* pInstanceD = instanced->Decode<ClassD>();

    if (pInstanceD->m_format_misc->ToInt() >=
        sizeof(StandardGenericFunction) )
    {
        error(L"~S: m_format_misc: expect ~D, but ~D.",
            klass,
            FromInt<Val_>(sizeof(StandardGenericFunction)),
            pInstanceD->m_format_misc );
    }
} // verify_fsc

static void verify_standard_class(Val klass)
{
    verify_subclass(klass, 0, CLASS_structure_object);
    verify_subclass(klass, 1, CLASS_standard_object);
    verify_subclass(klass, 0, CLASS_funcallable_standard_object);

    verify_std_class(
        klass,
        ClassD::Format_Instance, 
        Instance::Tag );
} // verify_standard_class

// verify_structure_class
//  o subclass of structure-object.
//  o m_instanced.m_format = Format_FuncallableInstance
//  o m_instanced.m_format_param = 0
//  o m_instanced.m_format != 0
//  o m_instanced.m_tag_code == FuncallableInstance::Tag
static void verify_structure_class(Val klass)
{
    StructureClass* pClass = klass->Decode<StructureClass>();

    verify_subclass(klass, 1, CLASS_structure_object);
    verify_subclass(klass, 0, CLASS_standard_object);
    verify_subclass(klass, 0, CLASS_funcallable_standard_object);

    Int cSlots = Fixnum::Decode_(length(pClass->m_slots));

    {
        Val instanced = pClass->m_instanced;
            ASSERT(nil != instanced);

        ClassD* pClassD = instanced->Decode<ClassD>();

        verify_eq(
            pClassD->m_format->ToInt(),
            ClassD::Format_Structure );

        verify_eq(
            pClassD->m_format_param->ToInt(),
            static_cast<Int>(cSlots * sizeof(Val) + sizeof(Val) ) );

        verify_eq(
            pClassD->m_format_misc, 
            Fixnum::Encode(cSlots) );

        verify_eq(
            pClassD->m_tag_code,
            Fixnum::Encode(StructureObject::Tag) );
    }

    if (nil != pClass->m_slots)
    {
        Val eslotd = first(pClass->m_slots);
        ASSERT(eslotd->Decode<EffectiveSlotD>()->m_name != Q("SI::CLASSD"));
    }
} // verify_structure_class


// verify_class_slots
static void verify_class_slots(Val klass)
{
    foreach (EnumList, oEnum, klass->Decode<Class>()->m_slots)
    {
        Val eslotd1 = oEnum.Get();
        foreach (EnumList, oEnum2, cdr(oEnum.GetList()))
        {
            Val eslotd2 = oEnum2.Get();
            if (eslotd2->Decode<EffectiveSlotD>()->m_name ==
                eslotd1->Decode<EffectiveSlotD>()->m_name )
            {
                error(L"~S has duplicated slot ~S.~%~S",
                    klass,
                    eslotd1->Decode<EffectiveSlotD>()->m_name,
                    klass->Decode<Class>()->m_slots );
            }
        } // for each slot
    } // for each slot
} // verify_class_slots


// verify_class
static void verify_class(Val name, Val klass)
{
    format(t, L"; verify ~S~%", klass);

    ASSERT(name == klass->Decode<Class>()->m_name);

    Val instanced = klass->Decode<Class>()->m_instanced;
    if (nil != instanced)
    {
        ClassD* pInstanceD = instanced->Decode<ClassD>();
        if (pInstanceD->m_class != klass)
        {
            error(L"~S: instanced->m_class=~S~%",
                klass,
                pInstanceD->m_class );
        }
    }

    verify_class_slots(klass);

    Val metaclass = class_of(klass);
    if (CLASS_built_in_class == metaclass)
    {
        verify_built_in_class(klass);
    }
    else if (CLASS_standard_class == metaclass)
    {
        verify_standard_class(klass);
    }
    else if (CLASS_structure_class == metaclass)
    {
        verify_structure_class(klass);
    }
    else if (CLASS_funcallable_standard_class == metaclass)
    {
        verify_fsc(klass);
    }
    else if (CLASS_foreign_class == metaclass)
    {
        // FIXME: NYI: implement verify_foreign_class
    }
    else
    {
        error(L"We should not use metaclass ~S.", metaclass);
    }
} // verify_class

// verify_classes
static void verify_classes()
{
    Val classes =  VAR(Aruntime_environmentA)->
            Decode<Environment>()->m_classes;

    foreach (EnumHashTable, oEnum, classes)
    {
        verify_class(oEnum.GetKey(), oEnum.GetVal());
    } // for each class

    verify_cpl(t);

    // 07 Objects
    verify_cpl(Qstandard_object, t);

    // 10 Symbols
    verify_cpl(Qsymbol, t);

    // 12 Numbers
    verify_cpl(Qnumber, t);
    verify_cpl(Qcomplex, Qnumber, t);
    verify_cpl(Qreal, Qnumber, t);
    verify_cpl(Qfloat, Qreal, Qnumber, t);
    verify_cpl(Qrational, Qreal, Qnumber, t);
    verify_cpl(Qratio, Qrational, Qreal, Qnumber, t);
    verify_cpl(Qinteger, Qrational, Qreal, Qnumber, t);
    verify_cpl(Qfixnum, Qinteger, Qrational, Qreal, Qnumber, t);
    verify_cpl(Qbignum, Qinteger, Qrational, Qreal, Qnumber, t);

    verify_cpl(Qdouble_float, Qfloat, Qreal, Qnumber, t);
    verify_cpl(Qsingle_float, Qfloat, Qreal, Qnumber, t);

    verify_cpl(Qrational_complex, Qcomplex, Qnumber, t);
    verify_cpl(Qdouble_float_complex, Qcomplex, Qnumber, t);
    verify_cpl(Qsingle_float_complex, Qcomplex, Qnumber, t);

    // 13 Characters
    verify_cpl(Qcharacter, t);

    // 14 Conses
    verify_cpl(Qcons, Qlist, Qsequence, t);

    verify_cpl(Qnull, Qsymbol, Qlist, Qsequence, t);

    // 15 Arrays
    verify_cpl(Qvector, Qarray, Qsequence, t);

    verify_cpl(Qsimple_vector, 
        Qdata_vector, Qvector, Qsimple_array, Qarray,
        Qsequence, t );

    // 16 Strings
    verify_cpl(Qstring, Qvector, Qarray, Qsequence, t);

    verify_cpl(Qsimple_string,
        Qstring, Qdata_vector, Qvector, Qsimple_array, Qarray,
        Qsequence, t );
} // verify_classes

// verify_htb
static void verify_htb(LPCWSTR pwsz, Val htb)
{
    Int iCount = 0;
    foreach (EnumHashTable, oEnum, htb)
    {
        Val key = oEnum.Get();
        if (QQremoved_slot_marker == key || QQfree_slot_marker == key)
        {
            error(L"Broken EnumHashTable hash-table ~A", make_string(pwsz));
        }
        iCount += 1;
    } // for each entry

    if (Fixnum::Encode(iCount) !=
        svref(htb->Decode<HashTable>()->m_vector, Fixnum::Encode(0)) )
    {
        error(L"Broken hash-table ~A", make_string(pwsz));
    }
} // verify_htb


// verify_package
//  o Number of external symbols
//  o Number of internal symbols
static void verify_package(LPCWSTR pwsz, Int iExt, Int iInt)
{
    class Internal
    {
        public: static void Verify(
            LPCWSTR pwsz, LPCWSTR pwszWhere, Val vector, Int iExpect )
        {
            Int iCount = 0;

            foreach (PackageImpl::EnumAll, oEnum, vector)
            {
                Val present = oEnum.Get();
                if (symbolp(present))
                {
                    iCount += 1;
                }
                else if (PackageImpl::Free() == present)
                {
                    // free
                }
                else if (PackageImpl::Removed() == present)
                {
                    // removed
                }
                else
                {
                    Fail(L"Package %s's %s vector contains unexpected value.",
                        pwsz, pwszWhere );
                }
            } // for each slot

            if (svref(vector, Fixnum::Encode(0)) != Fixnum::Encode(iCount))
            {
                Fail(L"Package %s's %s count is mismatched.",
                    pwsz, pwszWhere );
            }

            if (DONT_CARE != iExpect)
            {
                if (iCount != iExpect)
                {
                    Fail(L"Expect %d %s symbols in %s, but %d.",
                        iExpect, pwszWhere, pwsz, iCount );
                }
            }
        } // Verify
    }; // Internal

    StackString oName(pwsz);
    Val package = find_package(oName);
    if (! packagep(package)) Fail(L"Package %s isn't created.", pwsz);

    Internal::Verify(
        pwsz, L"external", 
        package->Decode<Package>()->m_external_table, iExt );

    Internal::Verify(
        pwsz, L"internal", 
        package->Decode<Package>()->m_internal_table, iInt );
} // verify_package


// verify_tlvrec
static void verify_tlvrec(Val name, Val tlvrec)
{
    if (! tlv_record_p(tlvrec))
        { error(L"Bad TLV ~S: ~S", name, tlvrec); }

    if (tlvrec->Decode<TlvRecord>()->m_name != name)
        { error(L"Bad TLV Record: ~S", name); }
} // verify_tlvrec


// verify_vars
static void verify_vars()
{
    uint cConsts = 0;
    uint cSpecials = 0;
    uint cTLVs = 0;

    verify_htb(L"*runtime-environment*.variables",
        VAR(Aruntime_environmentA)->Decode<Environment>()->m_variables );

    foreach (
        EnumHashTable,
        oEnum,
        VAR(Aruntime_environmentA)->Decode<Environment>()->m_variables )
    {
        Val name = oEnum.GetKey();
        Val kind_alist = oEnum.GetVal();
        Val kind = first(kind_alist);
        Val alist = rest(kind_alist);

        if (kind == Kconstant)
        {
            Val kons = assq(Kconstant, alist);
            if (! consp(kons)) 
                error(L"Bad constant ~S: ~S", name, kons);

            cConsts += 1;

            Val cell = gethash(name, VAR(Avalue_tableA));

            if (! value_cell_p(cell))
                error(L"Constant ~S in *value-table* is ~S.", name, cell);

            if (value_cell_type(cell) != Kconstant)
                error(L"Constant ~S must not be ~S.", name, cell);
        }
        else if (kind == Kspecial)
        {
            Val kons = assq(Kspecial, alist);
            if (! consp(kons)) error(L"Bad variable ~S: ~S", name, kons);
            if (t != cdr(kons)) error(L"Bad variable ~S: ~S", name, kons);

            Val frob = gethash(name, VAR(Avalue_tableA));

            kons = assq(Q("TLV"), alist);
            if (nil == kons)
            {
                if (! value_cell_p(frob))
                    error(L"Variable ~S in *value-table* is ~S,", name, frob);

                if (value_cell_type(frob) != Kvariable)
                    error(L"Variable ~S must not be ~S.", name, frob);
                    
                cSpecials += 1;
            }
            else if (consp(kons))
            {
                if (! tlv_record_p(frob))
                    error(L"TLV ~S in *value-table* is ~S,", name, frob);

                verify_tlvrec(name, cdr(kons));
                cTLVs += 1;
            }
            else
            {
                CAN_NOT_HAPPEN();
            } // if
        }
        else
        {
            error(L"Bad variable kind ~S for ~S.", name, kind_alist);
        }
    } // for each class

    {
        verify_htb(L"*value-table*", VAR(Avalue_tableA));

        uint cHtbConsts = 0;
        uint cHtbSpecials = 0;
        uint cHtbTLVs  = 0;
        foreach (EnumHashTable, oEnum, VAR(Avalue_tableA))
        {
            Val name = oEnum.GetKey();
            Val frob = oEnum.GetVal();

            if (value_cell_p(frob))
            {
                if (name != value_cell_name(frob))
                    error(L"Broken *value-table* ~S: ~S", name, frob);

                if (value_cell_type(frob) == Kvariable)
                {
                    cHtbSpecials += 1;
                }
                else if (value_cell_type(frob) == Kconstant)
                {
                    cHtbConsts += 1;
                }
                else
                {
                    error(L"Broken value-cell ~S: ~S", name, frob);
                }
            }
            else if (tlv_record_p(frob))
            {
                verify_tlvrec(name, frob);
                cHtbTLVs += 1;
            }
            else
            {
                error(L"Broken *value-table*: ~S", frob);
            }
        } // for each tlvrec

        if (cHtbConsts != cConsts)
            error(L"Environment and *value-table*/const aren't match.");

        if (cHtbSpecials != cSpecials)
            error(L"Environment and *value-table*/var aren't match.");

        if (cHtbTLVs != cTLVs)
            error(L"Environment and *value-table*/tlv aren't match.");
    }
} // verify_vars


//////////////////////////////////////////////////////////////////////
//
// Verify
//
void Verify()
{
    ASSERT(
        Fixnum::Encode(ClassD::Tag) ==
        CLASSD_class_description->Decode<ClassD>()->m_tag_code );

    verify_vars();

    // Array
    //  classd of data-vector must have element-type.
    {
        foreach (
            EnumList,
            oEnum,
            find_class(Qdata_vector)->Decode<Class>()->m_direct_subclasses )
        {
            Val klass = oEnum.Get();
            Val instanced = klass->Decode<Class>()->m_instanced;
            Val elt_ty = instanced->Decode<ClassD>()->m_element_type;
            if (symbolp(elt_ty))
            {
                // may be t, character, single-float, ...
            }
            else if (consp(elt_ty))
            {
                // may be (complex single-float), ...
                // may be (signed-byte n), ...
                // may be (unsigned-byte n), ...
            }
            else
            {
                error(L"~S doesn't have element-type information.", klass);
            }
        } // for each subclass
    }

    // Verify package
    {
        verify_package(L"COMMON-LISP", PACKAGE_cl_externals, 0);
        verify_package(L"CL", PACKAGE_cl_externals, 0);

        verify_package(L"KEYWORD", DONT_CARE, 0);

        verify_package(L"COMMON-LISP-USER", 0, 0);
        verify_package(L"CL-USER", 0, 0);

        verify_package(L"SYSTEM", DONT_CARE, DONT_CARE);
        verify_package(L"SI", DONT_CARE, DONT_CARE);

        verify_package(L"CLOS", 2 + PACKAGE_clos_externals, 0);

        verify_package(L"EXTENSION", DONT_CARE, 0);
        verify_package(L"EXT", DONT_CARE, 0);
    } // package

    verify_classes();
} // Verify

} // Boot
