#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - boot - Boot Image Builder
// boot/bt_builder.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id$
//

#include "../build.h"

#include "../kernel/ke_memory.h"
#include "../kernel/ke_dll_link.h"
#include "../kernel/ke_weak.h"

#include "../mini/mini_lisp.h"

namespace Boot
{

using namespace MiniLisp;

void build_04_Types();
void build_07_Objects();
void build_09_Conditions();
void build_12_Numbers();
void build_13_Characters();
void build_22_Printers();

void finalize_classes();
void init_13_Characters();

//////////////////////////////////////////////////////////////////////
//
//  s_all_packages
// Description:
//  For keeping all packages instead of lisp variable si::*all-packages*.
//  Since we have to keep list of packages before lisp variable available.
//
Val s_all_packages;


static Latch __declspec(align(Kernel_Arch_Record_Align))
    s_oWeakAreaLatch;

static ValueCell __declspec(align(Kernel_Arch_Record_Align))
    s_oWeakAreaCell;


static ValueCell __declspec(align(Kernel_Arch_Record_Align))
    s_oWeakAreaLatchCell;


// 00 Misc
void defclassd(Val*, ClassD::Format, Int, Val);
Int deftlv(const char16*, Val);
void augment_variable(Val, Val, Val);

// 03 Evluation and Compilation
Val defconstant(const char16*, Val);
void defspecial(const char16*);

// 05 Data and Control Flow
Val defvar(const char16*, Val = nil);
Val intern_variable(Val);

// 06 Objects
Val defclass(const char16*, Val, Val, Val);

// 10 Symbols
Val defsym(Val, const char16*);

// 12 Numbers
Val make_single_float(UINT, UINT, uint32);
Val make_double_float(UINT, UINT, uint64);

// 11 Packages
Val defpackage(const char16*, const char16*, int, int);
void package_bulk_exports(Val, const char16* const*, size_t);
void package_add_uses(Val, const char16*);

// 23 Reader
Val make_standard_readtable();

//////////////////////////////////////////////////////////////////////
//
// augment variable information
//
void augment_variable(Val name, Val kind, Val alist)
{
    ASSERT(NULL != VAR_Aruntime_environmentA);

    Environment* pEnv = VAR(Aruntime_environmentA)->Decode<Environment>();

    setf_gethash(cons(kind, alist), name, pEnv->m_variables);
} // augment_variable


// make_class
Val make_class(Val classd, Val name, Val instanced)
{
    Val klass = MiniThread::Get()->AllocInstance(classd);
    Class* pClass = klass->Decode<Class>();
        pClass->m_name                  = name;
        pClass->m_instanced             = instanced;
        pClass->m_plist                 = nil;
        pClass->m_flags                 = Fixnum::Encode(0);
        pClass->m_direct_methods        = nil;
        pClass->m_direct_superclasses   = nil;
        pClass->m_direct_subclasses     = nil;
        pClass->m_class_precedence_list = nil;
        pClass->m_direct_slots          = nil;
        pClass->m_slots                 = nil;
        pClass->m_prototype             = nil;

    // Register to global environment
    setf_gethash(
        klass,
        name,
        VAR(Aruntime_environmentA)->Decode<Environment>()->m_classes );

    return klass;
} // new_class


//////////////////////////////////////////////////////////////////////
//
// defclass
//
//  Initialize instanceD->m_class and m_format_misc (elty).
//
Val defclass(
    const char16*   pwszName,
    Val             instanced,
    Val             classd,
    Val             format_misc )
{
    if (NULL == instanced)
    {
        instanced = nil;
    }

    Val klass = make_class(classd, parse_symbol(pwszName), instanced);

    if (nil != instanced)
    {
        ClassD* pInstanceD = instanced->Decode<ClassD>();

        pInstanceD->m_class = klass;

        instanced->Decode<ClassD>()->m_element_type = nil;

        if (Fixnum::Encode(0) == format_misc)
        {
            // ignore
        }
        else if (fixnump(format_misc))
        {
            instanced->Decode<ClassD>()->m_format_misc = format_misc;
        }
        else
        {
            instanced->Decode<ClassD>()->m_element_type = format_misc;
        }
    } // if

    return klass;
} // defclass

// defclass_super
void defclass_super(Val klass, const char16* pwszSupers)
{
    Class* pClass = klass->Decode<Class>();
    Collector oSupers(&pClass->m_direct_superclasses);

    const char16* pwszRunner = pwszSupers; 
    while (0 != *pwszRunner)
    {
        while (' ' == *pwszRunner) pwszRunner++;
        const char16* pwchName = pwszRunner;
        while (' ' != *pwszRunner && 0 != *pwszRunner) pwszRunner++;
        Val super_name = parse_symbol(pwchName, pwszRunner);
        Val super = find_class(super_name, nil);

        if (nil == super)
        {
            error(L"No such class ~S in ~S.",
                super_name, super_name->Decode<Symbol>()->m_package );
        }

        if (nil != memq(super, pClass->m_direct_superclasses))
        {
            error(L"~S already has a super class ~S.", klass, super);
        }

        oSupers.Add(super);

        Class* pSuper = super->Decode<Class>();
        pSuper->m_direct_subclasses = cons(
            klass,
            pSuper->m_direct_subclasses );

        if (0 != *pwszRunner)
        {
            pwszRunner++;
        }
    } // for
} // defclass_super


//////////////////////////////////////////////////////////////////////
//
//  defclassd
void defclassd(
    Val*            inout_classd,
    ClassD::Format  eFormat,
    Int             iParam,
    Val             misc )
{
    Int nTag = Record::Tag;
    Val format_misc = Fixnum::Encode(0);

    switch (eFormat)
    {
    case ClassD::Format_None:
        ASSERT(0 == iParam);
        return;

    case ClassD::Format_Cons:
        ASSERT(sizeof(Cons) == iParam);
        format_misc = FromInt<Val_>(iParam);
        nTag = Cons::TagX;
        break;

    case ClassD::Format_BinFixed:
    case ClassD::Format_Fixed:
    case ClassD::Format_Mixed:
        ASSERT(0 == iParam % Fixnum::One);
        format_misc = FromInt<Val_>(iParam);
        break;

    case ClassD::Format_Structure:
        ASSERT(0 == iParam % Fixnum::One);
        format_misc = FromInt<Val_>(iParam - sizeof(Val));
        break;

    case ClassD::Format_Instance:
        format_misc = FromInt<Val_>(iParam - offsetof(Storage, mv_element));
        iParam += sizeof(Instance);
        nTag = Instance::Tag;
        break;

    case ClassD::Format_Function:
        iParam = sizeof(NativeCodeFunction);
        nTag = Funcallable::Tag;
        break;

    case ClassD::Format_FuncallableInstance:
        format_misc = FromInt<Val_>(iParam - offsetof(Storage, mv_element));
        iParam = sizeof(FuncallableInstance);
        nTag = FuncallableInstance::Tag;
        break;

    case ClassD::Format_Immediate:
        ASSERT(0 != iParam);
        iParam *= Fixnum::One;
        nTag = Fixnum::Decode_(misc);
        break;

    case ClassD::Format_Array:
        iParam = offsetof(Array, mv_dimension);
        break;

    case ClassD::Format_Storage:
        iParam = offsetof(Storage, mv_element);
        break;

    case ClassD::Format_Vector:
    case ClassD::Format_BinVec:
    case ClassD::Format_Foreign:
    case ClassD::Format_String:
        iParam *= Fixnum::One;
        break;

    default:
        CAN_NOT_HAPPEN();
    } // switch format

    Val classd;

    if (&CLASSD_class_description == inout_classd)
    {
        classd = *inout_classd;
    }
    else
    {
        classd = MiniThread::Get()->AllocRecord(CLASSD_class_description);
    }

    ClassD* pClassD = classd->Decode<ClassD>();
        pClassD->m_format       = FromInt<Val_>(eFormat);
        pClassD->m_format_param = FromInt<Val_>(iParam);
        pClassD->m_format_misc  = format_misc;
        pClassD->m_tag_code     = Fixnum::Encode(nTag);

    *inout_classd = classd;
} // defclassd

//////////////////////////////////////////////////////////////////////
//
// defconstant
//  variable => (:constant . ( (:constant . <value>) ... ))
//
Val defconstant(const char16* pwszName, Val val)
{
    Val cell = defvar(pwszName, val);
        ASSERT(cell->Decode<ValueCell>()->m_type == Kvariable);

    cell->Decode<ValueCell>()->m_type = Kconstant;

    // register constant name into environment.
    augment_variable(
        value_cell_name(cell),
        Kconstant,
        list(cons(Kconstant, val)) );

    return cell;
} // defconstant


//////////////////////////////////////////////////////////////////////
//
// defpackage
//
Val defpackage(
    const char16*         pwszName,
    const char16*         pwszAlias,
    int             iExternalSize,
    int             iInternalSize )
{
    Val names;

    if (0 == *pwszAlias)
    {
        names = make_string(pwszName);
    }
    else
    {
        names = list(make_string(pwszName), make_string(pwszAlias));

    }

    Val package = internal_make_package(
        names,
        Fixnum::Encode(iExternalSize),
        Fixnum::Encode(iInternalSize) );

    s_all_packages = cons(package, s_all_packages);

    return package;
} // defpackage


//////////////////////////////////////////////////////////////////////
//
// defspecial
//  Registers special-operator into global environment.
//
void defspecial(const char16* pwszName)
{
    Val name = parse_symbol(pwszName);

    Environment* pEnv =
        VAR(Aruntime_environmentA)->Decode<Environment>();

    setf_symbol_function(make_not_function_function(name), name);

    setf_gethash(list(Kspecial_operator), name, pEnv->m_functions);
} // defspecial


//////////////////////////////////////////////////////////////////////
//
// defsym
//
Val defsym(Val package, const char16*  pwszName)
{
    if (0 == ::lstrcmp(pwszName, L"NIL"))
    {
        return nil;
    }

    return export_intern(pwszName, package);
} // defsym


//////////////////////////////////////////////////////////////////////
//
// deftlv
//
Int deftlv(const char16* pwsz, Val init)
{
    Val name = parse_symbol(pwsz);
    ASSERT(name->Decode<Symbol>()->m_package != PACKAGE_cl_user);

    Int iIndex = MiniLisp::deftlv(name, init);

    set_tlv(iIndex, init);

    return iIndex;
} // deftlv


//////////////////////////////////////////////////////////////////////
//
// defvar
//
// Note: We don't call augment_variable here.
//
Val defvar(const char16* pwszName, Val init)
{
    Val name = parse_symbol(pwszName);
    Val cell = intern_variable(name);

    if (QQunbound_marker == value_cell_value(cell))
    {
        setf_value_cell_value(init, cell);
    }

    return cell;
} // defvar

// install_latch
Val install_latch(Val cell)
{
    ASSERT(! latch_p(value_cell_value(cell)));
    Val latch = make_latch(value_cell_name(cell));
    return setf_value_cell_value(latch, cell);
} // install_latch


//////////////////////////////////////////////////////////////////////
//
// Boot Package
//
void 
package_add_uses(Val package, const char16* pwszUses)
{
    char16 wszName[100];
    LPWSTR pwszName = wszName;
    while (0 != *pwszUses)
    {
        if (' ' != *pwszUses)
        {
            *pwszName++ = *pwszUses++;
        }
        else
        {
            while (' ' == *pwszUses)
            {
                pwszUses++;
            }

            if (pwszName != wszName)
            {
                *pwszName = 0;

                {
                    StackString oName(wszName);
                    use_package(oName.Encode(), package);
                }

                pwszName = wszName;
            }
        }
    } // while

    if (pwszName != wszName)
    {
        *pwszName = 0;
        StackString oName(wszName);
        use_package(oName.Encode(), package);
    }
} // package_add_uses


//////////////////////////////////////////////////////////////////////
//
// Bulk exports
//
void package_bulk_exports(
    Val             package,
    const char16* const*  prgwszSym,
    size_t          cSyms )
{
    const char16* const* pwszEnd = prgwszSym + cSyms;

    for (
        const char16* const* pwszRunner = prgwszSym; 
        pwszRunner < pwszEnd; 
        pwszRunner++ )
    {
        export_intern(*pwszRunner, package);
    } // for
} // package_bulk_exports


//////////////////////////////////////////////////////////////////////
//
// Intern variable
//
Val intern_variable(Val name)
{
    ASSERT(symbolp(name));

    Val value_table = value_cell_value(VAR_Avalue_tableA);

    Val cell = gethash(name, value_table);

    if (nil != cell)
    {
        return cell;
    }

    cell = make_value_cell(name, QQunbound_marker);

    return setf_gethash(cell, name, value_table);
} // intern_variable


//////////////////////////////////////////////////////////////////////
//
// One time initialization
//
void
initOnce()
{
    Thread::Get()->m_pRecordArea = Kernel::Memory::AllocDataArea(
        Thread::Get(),
        Kernel::Area::ScanType_Record,
        sizeof(Null) );

    // Initialize nil
    // Note: We could allocate nil in read-only area. However, Common Lisp
    // allows function whose name is NIL! A function name is symbol or
    // two elements list (setf <symbol>).
    Null* pNil = reinterpret_cast<Null*>(
        MiniThread::Get()->m_pRecordArea->Alloc(sizeof(Null)) );

    DEBUG_PRINTF(L"Nil = %p\r\n", pNil->Encode());

    ASSERT(pNil->Encode() == nil);

    nil = pNil->Encode();

    setf_car(nil, nil);
    setf_cdr(nil, nil);

    // FIXME 2007-03-21: To make object nil read-only, we split record
    // allocation area.

    Thread::Get()->m_pRecordArea = Kernel::Memory::AllocDataArea(
        Thread::Get(),
        Kernel::Area::ScanType_Record,
        Kernel::Memory::Param_AllocUnit * 4 );

    Thread::Get()->m_pBinObjArea = Kernel::Memory::AllocDataArea(
        Thread::Get(),
        Kernel::Area::ScanType_BinObj,
        Kernel::Memory::Param_AllocUnit * 2 +
            0x10000 * sizeof(Character) );

    Thread::Get()->m_waiting = nil;
    Thread::Get()->m_next_waiter = nil;

    // Create classd of classd.
    {
        __declspec(align(Kernel_Arch_Record_Align)) ClassD oClassD;
            ::ZeroMemory(&oClassD, sizeof(oClassD));
            oClassD.m_classd = oClassD.Encode();
            oClassD.m_format = FromInt<Val_>(ClassD::Format_Fixed);
            oClassD.m_format_param = FromInt<Val_>(sizeof(ClassD));
            oClassD.m_tag_code     = Fixnum::Encode(Record::Tag);

        CLASSD_class_description = MiniThread::Get()->
            AllocRecord(oClassD.Encode());

        ClassD* pClassD = CLASSD_class_description->Decode<ClassD>();
            ::memcpy(pClassD, &oClassD, sizeof(oClassD));
            pClassD->m_classd = CLASSD_class_description;
    }

    s_all_packages = nil;

    // Make class description
    {
        #define DEFCLASS(mp_cname, mp_NAME, mp_super) \
            defclassd(&CLASSD_##mp_cname,

        #define DEFCLASSINFO_(mp_metaclass, mp_format, mp_param, mp_misc) \
                ClassD::Format_##mp_format, mp_param, mp_misc

        #define DEFCLASSINFO(mp_metaclass, mp_format, mp_param) \
            DEFCLASSINFO_(mp_metaclass, mp_format, mp_param, Fixnum::Encode(0))

        #define ENDCLASS() );

        #include "../kernel/ke_layout.inc"

        ASSERT(0 != CLASSD_hash_table);
        ASSERT(0 != CLASSD_package);
        ASSERT(0 != CLASSD_readtable);
    }

    init_13_Characters();

    QQnull_string =
        QQnull_string = MiniThread::Get()->AllocBinVec(
            CLASSD_simple_string,
            Fixnum::Encode(0) );

    QQnull_vector =
        QQnull_vector = MiniThread::Get()->AllocVector(
            CLASSD_simple_vector,
            Fixnum::Encode(0) );

    VAR_Aweak_areaA = s_oWeakAreaCell.Encode();
    {
        s_oWeakAreaCell.m_classd = CLASSD_value_cell;
    }

    VAR_Aweak_area_latchA = s_oWeakAreaLatchCell.Encode();
    {
        s_oWeakAreaLatchCell.m_classd = CLASSD_value_cell;
    }

    VAR(Aweak_area_latchA) = s_oWeakAreaLatch.Encode();
    {
        s_oWeakAreaLatch.m_thread     = nil;
        s_oWeakAreaLatch.m_state      = nil;
        s_oWeakAreaLatch.m_name       = nil;
        s_oWeakAreaLatch.m_lock_count = Fixnum::Encode(0);
        s_oWeakAreaLatch.m_spinlock   = nil;
    }


    // Make package and symbols
    {
        #define DEFKEY(mp_cname, mp_name) \
            K##mp_cname = \
                export_intern(mp_name, PACKAGE_keyword);

        #define DEFPACKAGE(\
                mp_cname, mp_name, mp_alias, mp_ext, mp_int, mp_uses) \
            PACKAGE_##mp_cname = \
                defpackage(mp_name, mp_alias, mp_ext, mp_int);

        #define DEFSYM(mp_pkg, mp_cname, mp_name) \
            Q##mp_cname = defsym(PACKAGE_##mp_pkg, mp_name);

        #include "./bt_object.inc"
    }

    // Realize symbol "nil"
    {
        pNil->m_classd = CLASSD_simple_vector;
        pNil->m_length = Fixnum::Encode(6);

        nil->Decode<Symbol>()->m_classd    = CLASSD_null;
        nil->Decode<Symbol>()->m_name      = make_string(L"NIL");
        nil->Decode<Symbol>()->m_hash_code = hash_string(symbol_name(nil));
        nil->Decode<Symbol>()->m_package   = PACKAGE_cl;
        nil->Decode<Symbol>()->m_function  = nil;
        nil->Decode<Symbol>()->m_plist     = nil;

        package_put(
            PACKAGE_cl->Decode<Package>()->m_external_table,
            nil );
    } // reinit nil

    // Initialize symbols in CL package
    {
        static const char16* const k_rgwsz_CL[] =
        {
            // There are 978 external symbols in common-lisp package.
            #include "./bt_package_cl.inc"
        }; // k_rgwsz_CL

        package_bulk_exports(
            PACKAGE_cl,
            k_rgwsz_CL,
            lengthof(k_rgwsz_CL) );
    } // package cl

    // Initialize symbols in KEYWORD package
    {
        static const char16* const k_rgwsz_KEYWORD[] =
        {
            #include "./bt_package_keyword.inc"
        }; // k_rgwsz_KEYWORD

        package_bulk_exports(
            PACKAGE_keyword,
            k_rgwsz_KEYWORD,
            lengthof(k_rgwsz_KEYWORD));
    } // package keyword

    // Make markers
    //  free-slot-marker and removed-slot-marker must be created before
    //  calling make-hash-table.
    {
        class Marker : public Record_<Layout::C_marker>
        {
            public: static Val Make(const char16* pwsz)
            {
                Val marker = MiniThread::Get()->AllocRecord(CLASSD_marker);
                marker->Decode<Marker>()->m_name = intern(pwsz, PACKAGE_si);
                return marker;
            } // Make
        }; // Marker

        QQforward_cell_marker = Marker::Make(L"FORWARD-CELL-MARKER");
        QQfree_slot_marker    = Marker::Make(L"FREE-SLOT-MARKER");
        QQremoved_slot_marker = Marker::Make(L"REMOVED-SLOT-MARKER");
        QQunbound_marker      = Marker::Make(L"UNBOUND-MARKER");
    }

    // Initialize special variables
    {
        Val name   = intern(L"*VALUE-TABLE*", PACKAGE_si);
        Val valtab = make_hash_table();
        VAR_Avalue_tableA = make_value_cell(name, valtab);
        setf_gethash(VAR_Avalue_tableA, name, valtab);
    }

    // Initialize *all-packages* list.
    {
        VAR_Aall_packagesA = intern_variable(
            intern(L"*ALL-PACKAGES*", PACKAGE_si) );

        VAR(Aall_packagesA) = s_all_packages;

        VAR_Aall_packages_latchA = intern_variable(
            intern(L"*ALL-PACKAGES-LATCH*", PACKAGE_si) );

        install_latch(VAR_Aall_packages_latchA);
    }

    // From here, parse_symbol is ready to use.
    {
        #define DEFVAR(mp_cname, mp_name) \
            VAR_##mp_cname = defvar(mp_name, nil);

        #include "./bt_object.inc"
    }

    // Value Table
    {
        install_latch(VAR_Avalue_table_latchA);
    }

    // Setf Table
    {
        VAR(Asetf_tableA) = make_hash_table();
        install_latch(VAR_Asetf_table_latchA);
    }

    // TLV vector
    {
        VAR(Aweak_areaA) = Fixnum::Encode(0);
        install_latch(VAR_Aweak_area_latchA);
    }

    // Caller Table
    {
        VAR(Acaller_tableA) = make_hash_table();
        install_latch(VAR_Acaller_table_latchA);
    }

    // Global environment
    {
        VAR(Aruntime_environmentA) =
            make_environment(Q("*RUNTIME-ENVIRONMENT*"), t);
    }

    // TLV vector
    {
        VAR(Atlv_vectorA) = make_vector(lengthof(Thread::Get()->mv_tlv));
        setf_svref(Fixnum::Encode(0), VAR(Atlv_vectorA), Fixnum::Encode(0));

        install_latch(VAR_Atlv_vector_latchA);
    }

    // Weak Area
    {
        VAR(Aweak_areaA) = s_oWeakAreaCell.m_value;
        VAR(Aweak_area_latchA) = nil;
        install_latch(VAR_Aweak_area_latchA);
    }

    ASSERT(Fixnum::Encode(0) ==
        svref(PACKAGE_cl_user->Decode<Package>()->m_internal_table,
              Fixnum::Encode(0)) );

    // Initialize package
    // Initialize TLV
    //
    // After:
    //  intern_value
    {
        #define DEFPACKAGE( \
                mp_cname, mp_name, mp_alias, mp_ext, mp_int, mp_uses) \
            package_add_uses(PACKAGE_##mp_cname, mp_uses);

        #define DEFTLV(mp_cname, mp_name, mp_init) \
            TLV_##mp_cname = deftlv(mp_name, mp_init);

        #include "./bt_object.inc"
    }

    ASSERT(Fixnum::Encode(0) ==
        svref(PACKAGE_cl_user->Decode<Package>()->m_internal_table,
              Fixnum::Encode(0)) );

    set_tlv(TLV_ApackageA, PACKAGE_si);

    // Objects
    {
        #define DEFOBJECT(mp_cname, mp_expr) \
            Defobject(mp_cname, mp_expr);

        #include "./bt_object.inc"
    }

    Defobject(ty_complex_double_float, list(Q("COMPLEX"), Q("DOUBLE-FLOAT")));
    Defobject(ty_complex_single_float, list(Q("COMPLEX"), Q("SINGLE-FLOAT")));

    // Initialize class
    {
        #define DEFCLASS(mp_cname, mp_NAME, mp_super) \
            Q##mp_cname = parse_symbol(L##mp_NAME); \
            ty_##mp_cname = Q##mp_cname; \
            CLASS_##mp_cname = \
                defclass(L##mp_NAME, CLASSD_##mp_cname,

        #define DEFCLASSINFO_(mp_metaclass, mp_format, mp_param, mp_misc) \
            CLASSD_##mp_metaclass, mp_misc );

        #define DEFCLASSINFO(mp_metaclass, mp_format, mp_param) \
            DEFCLASSINFO_(mp_metaclass, mp_format, mp_param, 0)

        #include "../kernel/ke_layout.inc"

        ASSERT(
            Qstandard_base_class->Decode<Symbol>()->m_package ==
                PACKAGE_clos );
    }

    ASSERT(Fixnum::Encode(0) ==
        svref(PACKAGE_cl_user->Decode<Package>()->m_internal_table,
              Fixnum::Encode(0)) );

    // Patch weak-vector
    {
        ASSERT(0 != Qpackage);
        foreach (EnumList, oEnum, s_all_packages)
        {
            Val package = oEnum.Get();
            Val vector = package->Decode<Package>()->m_internal_table;
            WeakVectorLeader* p = reinterpret_cast<WeakVectorLeader*>(
                vector->Decode<SimpleVector>() ) - 1;
            p->m_kind = Qpackage;
        } // for each package
    }

    // Reader
    VAR(Adefault_readtableA)  = make_standard_readtable();
    VAR(Astandard_readtableA) = make_standard_readtable();

    // Bulk intern
    {
        static const char16* const k_rgwsz[] =
        {
            #include "./bt_symbols.inc"
        }; // k_rgwsz

        for (int i = 0; i < lengthof(k_rgwsz); i++)
        {
            parse_symbol(k_rgwsz[i]);
        } // for i
    }

    // Set super class
    {
        #define DEFCLASS(mp_cname, mp_NAME, mp_super) \
            defclass_super(CLASS_##mp_cname,

        #define DEFSUPER(mp_supers) \
                L##mp_supers );

        #include "../kernel/ke_layout.inc"
    }

    ASSERT(Fixnum::Encode(0) ==
        svref(PACKAGE_cl_user->Decode<Package>()->m_internal_table,
        Fixnum::Encode(0)) );

    // special operators
    defspecial(L"BLOCK");                   // 1
    defspecial(L"CATCH");                   // 2
    defspecial(L"EVAL-WHEN");               // 3
    defspecial(L"FLET");                    // 4
    defspecial(L"FUNCTION");                // 5
    defspecial(L"GO");                      // 6
    defspecial(L"IF");                      // 7
    defspecial(L"LABELS");                  // 8
    defspecial(L"LET");                     // 9
    defspecial(L"LET*");                    // 10
    defspecial(L"LOAD-TIME-VALUE");         // 11
    defspecial(L"LOCALLY");                 // 12
    defspecial(L"MACROLET");                // 13
    defspecial(L"MULTIPLE-VALUE-CALL");     // 14
    defspecial(L"MULTIPLE-VALUE-PROG1");    // 15
    defspecial(L"PROGN");                   // 16
    defspecial(L"PROGV");                   // 17
    defspecial(L"QUOTE");                   // 18
    defspecial(L"RETURN-FROM");             // 19
    defspecial(L"SETQ");                    // 20
    defspecial(L"SYMBOL-MACROLET");         // 21
    defspecial(L"TAGBODY");                // 22
    defspecial(L"THE");                    // 23
    defspecial(L"THROW");                  // 24
    defspecial(L"UNWIND-PROTECT");         // 25

    // Constants
    defconstant(L"ARRAY-DIMENSION-LIMIT",
        Fixnum::Encode(Array::DimensionLimit) );
    defconstant(L"ARRAY-RANK-LIMIT",
        Fixnum::Encode(Array::RankLimit) );
    defconstant(L"ARRAY-TOTAL-SIZE-LIMIT",
        Fixnum::Encode(Array::TotalSizeLimit) );
    defconstant(L"BOOLE-1",         Fixnum::Encode(0));
    defconstant(L"BOOLE-2",         Fixnum::Encode(1));
    defconstant(L"BOOLE-AND",       Fixnum::Encode(2));
    defconstant(L"BOOLE-ANDC1",     Fixnum::Encode(3));
    defconstant(L"BOOLE-ANDC2",     Fixnum::Encode(4));
    defconstant(L"BOOLE-C1",        Fixnum::Encode(5));
    defconstant(L"BOOLE-C2",        Fixnum::Encode(6));
    defconstant(L"BOOLE-CLR",       Fixnum::Encode(7));
    defconstant(L"BOOLE-EQV",       Fixnum::Encode(8));
    defconstant(L"BOOLE-IOR",       Fixnum::Encode(9));
    defconstant(L"BOOLE-NAND",      Fixnum::Encode(10));
    defconstant(L"BOOLE-NOR",       Fixnum::Encode(11));
    defconstant(L"BOOLE-ORC1",      Fixnum::Encode(12));
    defconstant(L"BOOLE-ORC2",      Fixnum::Encode(13));
    defconstant(L"BOOLE-SET",       Fixnum::Encode(14));
    defconstant(L"BOOLE-XOR",       Fixnum::Encode(15));

    defconstant(L"CALL-ARGUMENTS-LIMIT", Fixnum::Encode(254));
    defconstant(L"CHAR-CODE-LIMIT",      Fixnum::Encode(Character::Max));

    defconstant(L"INTERNAL-TIME-UNITS-PER-SECOND", Fixnum::Encode(100));

    defconstant(L"LAMBDA-LIST-KEYWORDS",
        list(Q("&ALLOW-OTHER-KEYS"),
             Q("&AUX"),
             Q("&BODY"),
             Q("&ENVIRONMENT"),
             Q("&KEY"),
             Q("&OPTIONAL"),
             Q("&REST"),
             Q("&WHOLE") ) );

    defconstant(L"LAMBDA-PARAMETERS-LIMIT", Fixnum::Encode(254));

    defconstant(L"MOST-NEGATIVE-FIXNUM", Fixnum::Encode(Fixnum::MostNegative));
    defconstant(L"MOST-POSITIVE-FIXNUM", Fixnum::Encode(Fixnum::MostPositive));

    defconstant(L"MULTIPLE-VALUES-LIMIT",  Fixnum::Encode(254));
    defconstant(L"NIL", nil);

    defconstant(L"T", t);

    defvar(L"SI:*DEFAULT-FEATURES*", list(
        #if BITS_BIG_ENDIAN
                Q(":BIG-ENDIAN"),
        #else // BITS_BIG_ENDIAN
                Q(":LITTLE-ENDIAN"),
        #endif // BITS_BIG_ENDIAN

        #if SIZEOF_VAL == 4
                Q(":32BIT"),
        #elif SIZEOF_VAL == 8
                Q(":64BIT"),
        #else
                #error Unsupported SIZEOF_VAL
        #endif // SIZEOF_VAL == 8

        parse_symbol(L":ANSI-CL"),
        parse_symbol(L":COMMON-LISP"),
        parse_symbol(L":IEEE-FLOATING-POINT") ) );

    // 10 Packages
    {
        defvar(L"*PACKAGE-DEFAULT-USE-LIST*", list(PACKAGE_cl));
    }

    // 19 Filenames
    {
        defvar(L"*PATHNAME-HOSTS*", nil);
        install_latch(defvar(L"*PATHNAME-HOSTS-LATCH*"));

        defvar(L"*LOGICAL-HOSTS*", nil);
        install_latch(defvar(L"*LOGICAL-HOSTS-LATCH*"));

        defconstant(L"+LOGICAL-RESERVED-CHARS+", make_string(L";"));
    }

    VAR(Aimage_save_timeA) = Fixnum::Encode(0);


    // Setf Cells
    {
        intern_setf_cell(Q("FUNCTION-NAME"));
        intern_setf_cell(Q("REF"));
        intern_setf_cell(Q("SUBSEQ"));
        intern_setf_cell(Q("GETF"));
        intern_setf_cell(Q("ROW-MAJOR-AREF"));
        intern_setf_cell(Q("BIT"));
        intern_setf_cell(Q("AREF"));
        intern_setf_cell(Q("MASK-FIELD"));
        intern_setf_cell(Q("THE"));
        intern_setf_cell(Q("ERROR"));
        intern_setf_cell(Q("APPLY"));
        intern_setf_cell(Q("CHAR"));
        intern_setf_cell(Q("LOGICAL-PATHNAME-TRANSLATIONS"));
        intern_setf_cell(Q("SBIT"));
        intern_setf_cell(Q("FDEFINITION"));
        intern_setf_cell(Q("VALUES"));
        intern_setf_cell(Q("LDB"));
    } // setf cells
} // initOnce


//////////////////////////////////////////////////////////////////////
//
// Initialize lisp runtime
//
void initRuntime()
{
    set_tlv(TLV_AenvironmentA,
        VAR(Aruntime_environmentA) );

    set_tlv(TLV_ApackageA, PACKAGE_si);

    set_tlv(TLV_AreadtableA,
        value_cell_value(VAR_Astandard_readtableA) );

    set_tlv(TLV_AfeaturesA,
        symbol_value(parse_symbol(L"SI:*DEFAULT-FEATURES*")) );

    set_tlv(TLV_Aread_baseA,  Fixnum::Encode(10));

    set_tlv(TLV_Aprint_escapeA, t);
    set_tlv(TLV_Aprint_baseA,   Fixnum::Encode(10));

    bind_standard_streams();

    // common-lisp package must have 978 external symbols.
    ASSERT(0 == cmp_xx(
        svref(PACKAGE_cl->Decode<Package>()->m_external_table, Fixnum::Encode(0)),
        978 ) );
} // initRuntime


static void
initPass_DllLink()
{
    Val htb = make_hash_table(Qequal);

    VAR(Adll_file_tableA) = htb;

    // Populate default dll entry
    {
        QQself_file_info = MiniThread::Get()->AllocRecord(
            CLASSD_dll_file_info );

        defobject(QQself_file_info, QQself_file_info);

        DllFileInfo* pInfo = QQself_file_info->Decode<DllFileInfo>();
            pInfo->m_handle     = Fixnum::Encode(0);
            pInfo->m_filename   = make_string(L".");
            pInfo->m_proc_table = make_hash_table(Qequal);

        setf_gethash(QQself_file_info, pInfo->m_filename, htb);
    }

    install_latch(VAR_Adll_link_latchA);

    VAR(Adll_link_areaA) =
        Fixnum::Encode(Memory::AllocDataArea(
            Thread::Get(),
            Area::ScanType_DllLink,
            sizeof(DllEntry) ) );
} // initPass_DllLink


static void
initPass_ObStack()
{
    Q("SI:.STACK-CONS");
    Q("SI:.STACK-LIST");
    Q("SI:.STACK-LIST*");
    Q("SI:.STACK-VECTOR");
    Q("SI:.STACK-ALLOCATE-BINARY");
    Q("SI:.STACK-ALLOCATE-FUNCTION");
    Q("SI:.STACK-MAKE-STRUCTURE");
    Q("SI:.STACK-MAKE-INSTANCE");
} // initPass_ObStack


// build_50_Extensions
void build_50_Extensions()
{
    defvar(L"*finalizations*", nil);
    install_latch(defvar(L"*finalizations-latch*"));

    deftlv(L"*object-pool*", nil);
    Q("ext:define-pool");
    Q("ext:with-pool");

    // 50 Weak Object
    Q("ext:make-weak-pointer");
    Q("ext:weak-pointer-value");
} // build_50_Extensions

// build_end
void build_end()
{
    // Defvars
    {
        t = Q("T");
        ASSERT(0 != t);

        Val special= cons(Kspecial, t);

        foreach (EnumHashTable, oEnum, VAR(Avalue_tableA))
        {
            Val name = oEnum.GetKey();
            Val frob = oEnum.GetVal();

            if (tlv_record_p(frob))
            {
                Boot::augment_variable(name, Kspecial,
                    list(special, cons(Qtlv, frob)) );
            }
            else if (value_cell_p(frob))
            {
                if (Kvariable == value_cell_type(frob))
                {
                    Boot::augment_variable(name, Kspecial, list(special));
                }
                else if (Kconstant == value_cell_type(frob))
                {
                    // ok
                }
                else
                {
                    error(L"Broken ~S for ~S.", frob, name);
                }
            }
            else
            {
                error(L"*value-table* must not have ~S for ~S.~%",
                    name, frob ); 
            }
        } // for
    }
} // build_end

extern void BuildForCompiler();
extern void BuildForCompilerTarget();
extern void BuildForPlatform();
extern void BuildForTarget();


void Build()
{
    Boot::initOnce();

    BuildForPlatform();

    Boot::initRuntime();

    Boot::initPass_DllLink();
    Boot::initPass_ObStack();

    BuildForTarget();

    // We define system types before compiler for loading function signatures.
    Boot::build_04_Types();

    BuildForCompiler();

    Boot::build_07_Objects();
    Boot::build_09_Conditions();
    Boot::build_12_Numbers();
    Boot::build_13_Characters();
    Boot::build_22_Printers();
    Boot::build_50_Extensions();

    finalize_classes();

    Boot::build_end();
} // Build

} // namespace Boot
