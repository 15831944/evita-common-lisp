#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - boot - Build 07 Objects
// boot/bt_build_07_object.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: /proj/evcl3/boot/bt_build_07_object.cpp 30 2006-10-15 01:54:43 yosi $
//
#include "../mini/mini_lisp.h"

namespace Boot
{

using namespace MiniLisp;

Val defvar(const char16*, Val = nil);
Val install_latch(Val);
static bool find_eslotd(Val, Val);

// StdMc - standard-method-combination
class StdMc : 
    public Instance_<Layout::C_standard_method_combination> {};


Val add_method(Val, Val, Val) { return nil; }


// install_gf
Val install_gf(Val fname, int iReqs, int iOpts, Val keys, Val ll)
{
    if (fboundp(fname)) return fdefinition(fname);

    Val param_info = MiniThread::Get()->AllocRecord(CLASSD_param_info);
    {
        ParamInfo* p = param_info->Decode<ParamInfo>();
            p->m_nreqs = Fixnum::Encode(iReqs);
            p->m_nopts = Fixnum::Encode(iOpts);
            p->m_keys  = keys;
            p->m_lambda_list = ll;
            p->m_order       = nil;
    } // param_info

    Val gf = allocate_funcallable_instance(CLASSD_standard_generic_function);

    StandardGenericFunction* p = gf->Decode<StandardGenericFunction>();
        p->m_plist              = nil;
        p->m_name               = fname;
        p->m_methods            = nil;
        p->m_method_class       = CLASS_standard_method;
        p->m_method_combination = VAR(Astandard_method_combinationA);
        p->m_param_info         = param_info;

    setf_fdefinition(gf, fname);

    return gf;
} // build_gf


// build_gfs
static void build_gfs()
{
    Val gf;
    #include "./_bt_gf.inc"

    // Make prototype of standard-generic-function for avoiding to call
    // allocate-instance <= class-prototype <= defmethod.
    {
        Val proto = MiniThread::Get()->AllocInstance(CLASSD_standard_method);
            StandardMethod* p = proto->Decode<StandardMethod>();
                p->m_plist              = nil;
                p->m_generic_function   = nil;
                p->m_specializers       = nil;
                p->m_qualifiers         = nil;
                p->m_lambda_list        = nil;
                p->m_function           = nil;

        CLASS_standard_method->Decode<StandardClass>()->
            m_prototype = proto;
    }

    // for ensure-generic-function-using-class
    {
        Val proto = allocate_funcallable_instance(
            CLASSD_standard_generic_function );

        StandardGenericFunction* p = proto->Decode<StandardGenericFunction>();
            p->m_plist              = nil;
            p->m_name               = nil;
            p->m_methods            = nil;
            p->m_method_class       = CLASS_standard_method;
            p->m_method_combination = VAR(Astandard_method_combinationA);
            p->m_param_info         = nil;

        CLASS_standard_generic_function->Decode<StandardClass>()->
            m_prototype = proto;
    }
} // build_gfs


//////////////////////////////////////////////////////////////////////
//
// finalize_classes
//
class ComputeCPL
{
    struct Klass
    {
        Val     m_class;
        bool    m_fVisit;
    }; // Klass

    struct Pair
    {
        Klass*  m_pSub;
        Klass*  m_pSuper;
    }; // Pair

    Klass   m_rgoKlass[100];
    UINT    m_cClasses;
    Pair    m_rgoPair[100];
    UINT    m_cPairs;

    public: ComputeCPL() :
        m_cClasses(0),
        m_cPairs(0) {}

    public: Val Run(Val klass)
    {
        walk(mapClassToKlass(klass));

        Val cpl = nil;

        UINT cPairs = m_cPairs;
        while (cPairs >= 1)
        {
            Klass* pKlass = findLeastClass();
                ASSERT(NULL != pKlass);

            for (
                Pair* pRunner = &m_rgoPair[0];
                pRunner < &m_rgoPair[m_cPairs];
                pRunner++ )
            {
                if (pRunner->m_pSuper == pKlass)
                {
                    pRunner->m_pSuper = NULL;
                    pRunner->m_pSub = NULL;
                    cPairs -= 1;
                }
            } // for

            cpl = cons(pKlass->m_class, cpl);
        } // cPairs

        return cons(klass, cpl);
    } // Run

    Klass* findLeastClass()
    {
        for (
            Pair* pRunner = &m_rgoPair[m_cPairs - 1];
            pRunner >= &m_rgoPair[0];
            pRunner-- )
        {
            if (NULL != pRunner->m_pSuper)
            {
                if (NULL == findSubClass(pRunner->m_pSuper))
                {
                    return pRunner->m_pSuper;
                }
            }
        } // for
        CAN_NOT_HAPPEN();
    } // findLeastClass

    Klass* findSubClass(Klass* pKlass)
    {
        for (
            Pair* pRunner = &m_rgoPair[0];
            pRunner < &m_rgoPair[m_cPairs];
            pRunner++ )
        {
            if (pRunner->m_pSub == pKlass)
            {
                return pRunner->m_pSub;
            }
        } // for
        return NULL;
    } // findSubClass

    // mapClassToKlass
    Klass* mapClassToKlass(Val klass)
    {
        for (
            Klass* pRunner = &m_rgoKlass[0];
            pRunner < &m_rgoKlass[m_cClasses];
            pRunner++ )
        {
            if (pRunner->m_class == klass)
            {
                return pRunner;
            }
        } // for
        ASSERT(m_cClasses < lengthof(m_rgoKlass));

        Klass* pKlass = &m_rgoKlass[m_cClasses];
        pKlass->m_class = klass;
        pKlass->m_fVisit = false;
        m_cClasses += 1;
        return pKlass;
    } // mapClassToKlass

    // walk
    /// Construct Rc and R.
    //      Rc = { (C, C1) (C1, C2) ... (Cn-1, Cn) }
    //      R  = union Rc where c = all of S
    //          where S is set of C and its superclases
    //
    void walk(Klass* pKlass)
    {
        if (pKlass->m_fVisit)
        {
            return;
        }

        pKlass->m_fVisit = true;

        for (
            Val runner = pKlass->m_class->Decode<Class>()->
                m_direct_superclasses;
            ! endp(runner);
            runner = cdr(runner) )
        {
            Klass* pSuper = mapClassToKlass(car(runner));

            m_rgoPair[m_cPairs].m_pSub   = pKlass;
            m_rgoPair[m_cPairs].m_pSuper = pSuper;
                m_cPairs += 1;
            walk(pSuper);
            pKlass = pSuper;
        } // for each direct super class
    } // walk
}; // ComputeCPL


// compute_class_precedence_list
Val compute_class_precedence_list(Val klass)
{
    ComputeCPL oComputeCPL;
    return oComputeCPL.Run(klass);
} // compute_class_precedence_list

// sizeof_ty
//  Returns number of byte to store value of ty.
static int sizeof_ty(Val ty)
{
    if (ty == ty_int32 || ty == ty_uint32) return 4;

    if (ty == ty_float32) return 4;
    if (ty == ty_float64) return 8;

    if (ty == ty_int16  || ty == ty_uint16) return 2;
    if (ty == ty_int8   || ty == ty_uint8)  return 1;

    if (ty == ty_int64  || ty == ty_uint64) return 8;

    return sizeof(Val);
} // sizeof_ty


// compute_slots
static Val compute_slots(Val klass)
{
    Class* pClass = klass->Decode<Class>();
        ASSERT(nil != pClass->m_instanced);

    Val metaclass = class_of(klass);
    Val slot_classd;
    {
        if (metaclass == CLASS_structure_class)
        {
            slot_classd = CLASSD_structure_effective_slot_definition;
        }
        else
        {
            slot_classd = CLASSD_standard_effective_slot_definition;
        }
    } // slot_classd

    Val slots = nil;
    Collector oSlotds(&slots);

    Val cpl = nreverse_list(pClass->m_class_precedence_list);

    enum
    {
        Location_Foreign,
        Location_Index,
        Location_Offset,
    } eLocation = Location_Offset;

    if (metaclass == CLASS_standard_class)
    {
        eLocation = Location_Index;
    }
    else if (metaclass == CLASS_funcallable_standard_class)
    {
        eLocation = Location_Index;
    }
    else if (metaclass == CLASS_structure_class)
    {
        eLocation = Location_Index;
    }
    else if (metaclass == CLASS_foreign_class)
    {
        eLocation = Location_Foreign;
    }

    int ofs = 0;
    foreach (EnumList, oEnum, cpl)
    {
        Val runner = oEnum.Get();

        if (runner != klass)
        {
            Val super_name = runner->Decode<Class>()->m_name;
            if (Qarray == super_name) continue;
            if (Qsimple_array == super_name) continue;
            if (Qvector == super_name) continue;
            if (Qstring == super_name) continue;
            if (Qbit_vector == super_name) continue;
            if (Qdata_vector == super_name) continue;

            // Instances
            if (Qinstance == super_name) continue;

            if (Qnative_code_object == super_name) continue;
            if (Qfuncallable_instance == super_name) continue;
        } // runner != klass

        foreach (EnumList, oEnum, runner->Decode<Class>()->m_direct_slots)
        {
            Val dslotd = oEnum.Get();
            DirectSlotD* pDSlotD = dslotd->Decode<DirectSlotD>();

            if (find_eslotd(pDSlotD->m_name, slots))
            {
                continue;
            }

            int sz;
            {
                switch (eLocation)
                {
                case Location_Foreign:
                    sz = sizeof_ty(pDSlotD->m_type);
                    break;

                case Location_Index:
                    sz = 1;
                    break;

                case Location_Offset:
                    sz = max(sizeof_ty(pDSlotD->m_type), sizeof(Val));
                    break;

                default:
                    CAN_NOT_HAPPEN();
                } // sz
            } // sz

            // align ofs
            {
                int mod = ofs %sz;
                if (0 != mod)
                {
                    ofs += sz - mod;
                }
            }

            {
                Val eslotd = MiniThread::Get()->AllocInstance(slot_classd);

                #define copy_e2d(mp_slot) \
                    eslotd->Decode<EffectiveSlotD>()-> \
                        mp_slot = dslotd->Decode<DirectSlotD>()->mp_slot

                copy_e2d(m_name);
                copy_e2d(m_allocation);
                copy_e2d(m_type);
                copy_e2d(m_initargs);
                copy_e2d(m_initform);
                copy_e2d(m_initfunction);

                eslotd->Decode<EffectiveSlotD>()->
                    m_location = Fixnum::Encode(ofs);

                oSlotds.Add(eslotd);
            }

            ofs += sz;
        } // for each direct sltod
    } // for each class

    nreverse_list(cpl);

    return slots;
} // compute_slots


// init_instanced
static Val init_instanced(Val klass)
{
    Class* pClass = klass->Decode<Class>();

    ClassD* pInstanceD = pClass->m_instanced->Decode<ClassD>();
        pInstanceD->m_class = klass;
        pInstanceD->m_slots = pClass->m_slots;
        pInstanceD->m_hash_code =
            pClass->m_name->Decode<Symbol>()->m_hash_code;

    Val n = length(pClass->m_slots);

    switch (pInstanceD->m_format->ToInt())
    {
    case ClassD::Format_Fixed:
    case ClassD::Format_Mixed:
        if (pInstanceD->m_format_param != n)
        {
            error(L"~S: expect ~D but ~D.",
                klass,
                pInstanceD->m_format_param,
                n );
        }

        pInstanceD->m_format_misc = n;
        break;

    case ClassD::Format_Instance:
        ASSERT(CLASS_standard_class == class_of(klass));

        if (Fixnum::Encode(0) == pInstanceD->m_format_param)
        {
            pInstanceD->m_format_param = add_xx(n, 4);
        }
        else if (pInstanceD->m_format_param != add_xx(n, 4))
        {
            error(L"~S: expect ~D slots but ~D.",
                klass,
                pInstanceD->m_format_param,
                add_xx(n, 4) );
        }

        pInstanceD->m_format_misc = n;
        break;

    case ClassD::Format_FuncallableInstance:
    {
        Int cbGF = sizeof(StandardGenericFunction) - sizeof(Storage);
        unless (n->ToInt() >= cbGF)
        {
            error(L"~S: expect ~D, but ~D,",
                klass,
                FromInt<Val_>(cbGF),
                n );
        }

        pInstanceD->m_format_misc = n;
        break;
    } // funcallable-instance

    case ClassD::Format_Structure:
        if (pInstanceD->m_format_param != add_xx(n, 1))
        {
            error(L"~S: expect ~D but ~D.",
                klass,
                pInstanceD->m_format_param,
                add_xx(n, 1) );
        }

        pInstanceD->m_format_misc = n;
        break;

    default:
        ASSERT(CLASS_standard_class != class_of(klass));
    } // switch format

    switch (pInstanceD->m_format->ToInt())
    {
    case ClassD::Format_BinVec:
        if (pInstanceD->m_element_type == Q("BIT"))
        {
            pInstanceD->m_typespec = pClass->m_name;
        }
        else if (pClass->m_name == Q("BIGNUM"))
        {
            pInstanceD->m_typespec = pClass->m_name;
        }
        else
        {
            pInstanceD->m_typespec =list(
                Qsimple_array,
                pInstanceD->m_element_type,
                list(Q("*")) );
        }
        break;

    default:
        if (pInstanceD->m_class == CLASS_rational_complex)
        {
            pInstanceD->m_typespec = list(Qcomplex, Qrational);
        }
        else if (pInstanceD->m_class == CLASS_single_float_complex)
        {
            pInstanceD->m_typespec = list(Qcomplex, Qsingle_float);
        }
        else if (pInstanceD->m_class == CLASS_double_float_complex)
        {
            pInstanceD->m_typespec = list(Qcomplex, Qdouble_float);
        }
        else
        {
            pInstanceD->m_typespec = pClass->m_name;
        }
        break;
    } // switch format

    return klass;
} // init_instanced


// finalize_class
static Val finalize_class(Val klass)
{
    format(t, L"finalize-class ~S~%", klass);

    if (nil != klass->Decode<Class>()->m_instanced)
    {
        klass->Decode<Class>()->m_class_precedence_list =
            compute_class_precedence_list(klass);

        klass->Decode<Class>()->m_slots =
            compute_slots(klass);

        init_instanced(klass);
    }
    else if (CLASS_built_in_class == class_of(klass))
    {
        klass->Decode<Class>()->m_class_precedence_list =
            compute_class_precedence_list(klass);
    }

    return klass;
} // finalize_class


static void finalize_class_walk(Val klass)
{
    finalize_class(klass);
    foreach (EnumList, oEnum, klass->Decode<Class>()->m_direct_subclasses)
    {
        finalize_class_walk(oEnum.Get());
    } // for each subclass
} // finalize_class_walk


// finaliz_classes
void finalize_classes()
{
    finalize_class_walk(find_class(t));

    // For upgraded-array-element-type
    // direct-subclasses of data-vector must be ordred by subtypep.
    {
        CLASS_data_vector->Decode<Class>()->m_direct_subclasses =
            nreverse_list(
                CLASS_data_vector->Decode<Class>()->m_direct_subclasses );
    }
} // finalize_classes


// find_eslotd
static bool find_eslotd(Val name, Val slots)
{
    foreach (EnumList, oEnum, slots)
    {
        Val eslotd = oEnum.Get();
        if (eslotd->Decode<EffectiveSlotD>()->m_name == name)
        {
            return true;
        }
    } // for each slotd
    return false;
} // find_eslotd


// set_direct_slots
static void set_direct_slots(Val klass, ...)
{
    va_list args;
    va_start(args, klass);

    Val metaclass = class_of(klass);

    Val slot_classd;
    {
        if (metaclass == CLASS_structure_class)
        {
            slot_classd = CLASSD_structure_direct_slot_definition;
        }
        else
        {
            slot_classd = CLASSD_standard_direct_slot_definition;
        }
    } // slot_classd

    Class* pClass = klass->Decode<Class>();

    Collector oSlotds(&pClass->m_direct_slots);
    for (;;)
    {
        const char16* pwszName = va_arg(args, const char16*);
            if (NULL == pwszName || 0 == *pwszName) break;

        Val ty = va_arg(args, Val);

        Val slotd = MiniThread::Get()->AllocInstance(slot_classd);

        DirectSlotD* pSlotD = slotd->Decode<DirectSlotD>();
            pSlotD->m_name = parse_symbol(pwszName);
            pSlotD->m_allocation   = Q(":INSTANCE");
            pSlotD->m_type         = ty;
            pSlotD->m_initform     = nil;
            pSlotD->m_initfunction = nil;
            pSlotD->m_readers      = nil;
            pSlotD->m_writers      = nil;
            pSlotD->m_initargs     = nil;

        // We populate initargs for condition here for error,
        // other classes are populated from defclass.
        if (subclassp(klass, CLASS_condition))
        {
            Val key = intern(
                symbol_name(pSlotD->m_name),
                PACKAGE_keyword );

            pSlotD->m_initargs = list(key);
        } // if


        oSlotds.Add(slotd);
    } // for

    va_end(args);
} // set_direct_slots


// Build_07_Objects
void build_07_Objects()
{
    // Compute direct slots
    {
        // [A]
            //Val ty_array_bit  = list(Qarray, Qbit);
            Val ty_array_rank = Q("ARRAY-RANK");
        // [F]
            Val ty_format_control   = Q("FORMAT-CONTROL");
            Val ty_function_name    = Q("FUNCTION-NAME");
        // [K]
            Val ty_keyword = Q("KEYWORD");
        // [M]
            Val ty_member_Kinstance_Kclass = list(
                Q("MEMBER"), Q(":INSTANCE"), Q(":CLASS") );

            Val ty_member_Klocked_nil = list(
                Q("MEMBER"), Q(":LOCKED"), nil );

        // [O]
            Val ty_or_class_description_null =
                list(Qor, Qclass_description, Qnull);

            Val ty_or_environment_null =
                list(Qor, Qenvironment, Qnull);

            Val ty_or_function_null =
                list(Qor, Qfunction, Qnull);

            Val ty_or_generic_function_null =
                list(Qor, Qgeneric_function, Qnull);

            Val ty_or_hash_table_null =
                list(Qor, Qhash_table, Qnull);

            Val ty_or_list_eql_t =
                list(Qor, Qlist, ty_eql_t);

            Val ty_or_package_null =
                list(Qor, Qpackage, Qnull);

            Val ty_or_param_info_null =
                list(Qor, Qparam_info, Qnull);

            Val ty_or_sequence_index_cons =
                list(Qor, Q("SEQUENCE-INDEX"), Qnull);

            Val ty_or_symbol_cons =
                list(Qor, Qsymbol, Qcons);

            Val ty_or_thread_null =
                list(Qor, Qthread, Qnull);
        // [S]
            Val ty_sequence_index = Q("SEQUENCE-INDEX");
        // [T]
            Val ty_Qt = t;
            Val ty_type_specifier = Q("TYPE-SPECIFIER");
        // [U]
            #define ty_UInt ty_uint

        #if SIZEOF_VAL == 8
            Val ty_uint = ty_uint64;
        #else
            Val ty_uint = ty_uint32;
        #endif

            ASSERT(NULL != ty_int8); ASSERT(NULL != ty_int16); 
            ASSERT(NULL != ty_int32); ASSERT(NULL != ty_int64);

            ASSERT(NULL != ty_uint8); ASSERT(NULL != ty_uint16); 
            ASSERT(NULL != ty_uint32); ASSERT(NULL != ty_uint64);

        #define DEFCLASS(mp_cname, mp_NAME, mp_super) \
            set_direct_slots(CLASS_##mp_cname,

        #define DEFSLOT_(mp_n, mp_cname, mp_STR, mp_ctype, mp_ty) \
            L## mp_STR, ty_##mp_ty,

        #define DEFSLOT_OVERRIDE(mp_NAME) \
            L## mp_NAME, t,

        #define ENDCLASS() NULL );

        #include "../kernel/ke_layout.inc"
    } // direct slots

    defvar( L"*EQL-SPECIALIZERS*", make_hash_table(Qeql));
    install_latch(defvar(L"*EQL-SPECIALIZERS-LATCH*", nil));

    // BUGBUG: Make *discriminator-constructors* bigger.
    defvar(L"*DISCRIMINATOR-CONSTRUCTORS*", make_hash_table(Qequal));
    install_latch(defvar(L"*DISCRIMINATOR-CONSTRUCTORS-LATCH*"));

    // *standard-method-combination*
    {
        Val mc = MiniThread::Get()->
            AllocInstance(CLASSD_standard_method_combination);

        VAR(Astandard_method_combinationA) = mc;

        mc->Decode<StdMc>()->m_type    = Q("STANDARD");
        mc->Decode<StdMc>()->m_options = nil;
    }

    build_gfs();
} // build_07_Objects

} // Boot
