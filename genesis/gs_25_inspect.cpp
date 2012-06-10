#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - 25 Environment - Inspect
// genesis/gs_25_inspect.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/genesis/gs_25_inspect.cpp#11 $
//
// Description:
//  This file contains C implementation of lisp functions.
//      inspect
//      time
#include "./gs_lisp.h"

#include "../kernel/ke_memory.h"

namespace CommonLisp
{

static Val get_val(uint8* pb, const EffectiveSlotD* pSlotD)
{
    Int ofs = Fixnum::Decode_(pSlotD->m_location);
    Val ty = pSlotD->m_type;

    #if SIZEOF_VAL == 4
    if (ty == ty_int32 || ty == ty_int)
    {
        return make_int(*reinterpret_cast<int32*>(pb + ofs));
    } // int32

    if (ty == ty_uint32 || ty == ty_uint)
    {
        return make_uint(*reinterpret_cast<uint32*>(pb + ofs));
    } // uint32
    #endif // SIZEOF_VAL == 4

    #if SIZEOF_VAL == 8
    if (ty == ty_int64 || ty == ty_int)
    {
        return make_int(*reinterpret_cast<int64*>(pb + ofs));
    } // int64

    if (ty == ty_uint64 || ty == ty_uint)
    {
        return make_uint(*reinterpret_cast<uint64*>(pb + ofs));
    } // uint64
    #endif // SIZEOF_VAL == 8

    if (ty == ty_float32)
    {
        Val x = MiniThread::Get()->AllocBinObj(CLASSD_single_float);
        x->Decode<SingleFloat>()->m_flt = *reinterpret_cast<float32*>(
            pb + ofs );
        return x;
    } // float32

    if (ty == ty_float64)
    {
        Val x = MiniThread::Get()->AllocBinObj(CLASSD_double_float);
        x->Decode<DoubleFloat>()->m_dbl = *reinterpret_cast<float64*>(
            pb + ofs );
        return x;
    } // float64

    return *reinterpret_cast<Val*>(pb + ofs);
} // get_val


// inspect_binobj
static void inspect_binobj(Val obj)
{
    Record* p = obj->Decode<Record>();
    ClassD* pClassD = p->m_classd->Decode<ClassD>();

    foreach (EnumList, oEnum, pClassD->m_slots)
    {
        EffectiveSlotD* pSlotD = oEnum.Get()->Decode<EffectiveSlotD>();

        format(t, L";  [~D] ~S = ~S~%",
            pSlotD->m_location,
            pSlotD->m_name,
            get_val(reinterpret_cast<uint8*>(p), pSlotD) );
    } // for each slot
} // inspect_binobj


// inspect_record
static void inspect_record(Val obj)
{
    Record* p = obj->Decode<Record>();
    ClassD* pClassD = p->m_classd->Decode<ClassD>();

    Val* pval = reinterpret_cast<Val*>(p);

    int i = 0;
    foreach (EnumList, oEnum, pClassD->m_slots)
    {
        EffectiveSlotD* pSlotD = oEnum.Get()->Decode<EffectiveSlotD>();

        format(t, L";  [~D] ~S = ~S~%",
            Fixnum::Encode(i),
            pSlotD->m_name,
            *pval );
        pval++;
        i += 1;
    } // for each slot
} // inspect_record


// inspect_structure
static void inspect_structure(Val obj)
{
    Record* p = obj->Decode<Record>();
    ClassD* pClassD = p->m_classd->Decode<ClassD>();

    Val* pval = reinterpret_cast<Val*>(p);
    pval++;

    int i = 0;
    foreach (EnumList, oEnum, pClassD->m_slots)
    {
        EffectiveSlotD* pSlotD = oEnum.Get()->Decode<EffectiveSlotD>();

        format(t, L";  [~D] ~S = ~S~%",
            Fixnum::Encode(i),
            pSlotD->m_name,
            *pval );
        pval++;
        i += 1;
    } // for each slot
} // inspect_structure


// inspect_storage
static void inspect_storage(Val storage)
{
    format(t, L"; Storage:~%");
    ClassD* pClassD = storage->Decode<Storage>()->m_storaged->Decode<ClassD>();

    Int i = 0;
    foreach (EnumList, oEnum, pClassD->m_slots)
    {
        EffectiveSlotD* pSlotD = oEnum.Get()->Decode<EffectiveSlotD>();

        format(t, L";  [~D] ~S = ~S~%",
            Fixnum::Encode(i),
            pSlotD->m_name,
            storage->Decode<Storage>()->mv_element[i] );
        i += 1;
    } // for each slot
} // inspect_storage

// inspect
Val inspect(Val obj)
{
    format(t, L"; Type ~S: ~S~%", type_of(obj), obj);

    switch (obj->GetTag4())
    {
    case_Tag_Fixnum:
        format(t, L";  #x~X~%", obj);
        format(t, L";  #o~O~%", obj);
        format(t, L";  #b~B~%", obj);
        break;

    case_Tag_Record:
    {
        format(t, L"; #x~X~X~%",
            Fixnum::Encode(obj->ToInt() >> 4),
            Fixnum::Encode(obj->ToInt() & 15) );

        ClassD* pClassD = obj->GetClassD()->Decode<ClassD>();

        switch (pClassD->m_format->ToInt())
        {
        case ClassD::Format_BinVec:
        case ClassD::Format_Fixed:
        case ClassD::Format_Mixed:
            inspect_record(obj);
            break;

        case ClassD::Format_Structure:
            inspect_structure(obj);
            break;

        case ClassD::Format_Array:
        case ClassD::Format_Vector:
        case ClassD::Format_String:
            inspect_record(obj);
            break;

        case ClassD::Format_Storage:
            inspect_storage(obj);
            break;

        case ClassD::Format_BinFixed:
            inspect_binobj(obj);
            break;

        case ClassD::Format_Instance:
            inspect_storage(obj->Decode<Instance>()->m_storage);
            break;

        default:
            format(t, L"Unknown format ~D~%", pClassD->m_format);
            break;
        } // switch format
        break;
    } // Tag_Record

    case Val_::Tag_Function:
    {
        Funcallable* p = obj->Decode<Funcallable>();
        format(t, L";  classd = ~S~%",   p->m_classd);
        format(t, L";  cb     = ~S~%",   Fixnum::Encode(p->m_cbFunction));
        format(t, L";  cookie = #x~X~X~%",
            Fixnum::Encode(p->m_nCookie >> 4),
            Fixnum::Encode(p->m_nCookie & 15) );

        ClassD* pClassD = obj->Decode<Funcallable>()->
            m_classd->Decode<ClassD>();

        switch (pClassD->m_format->ToInt())
        {
        case ClassD::Format_Function:
            format(t, L";  name = ~S~%", obj->Decode<NativeCodeFunction>()->
                m_name );
            break;

        case ClassD::Format_FuncallableInstance:
            inspect_storage(obj->Decode<FuncallableInstance>()->m_storage);
            break;

        default:
            format(t, L";  broken function~%");
            break;
        } // switch format
        break;
    } // Tag_Function

    case_Tag_Cons:
    {
        Cons* p = obj->Decode<Cons>();
        format(t, L"; at #x~X~X~%",
            Fixnum::Encode(p->ToInt() >> 4),
            Fixnum::Encode(p->ToInt() & 15) );
        format(t, L";  car = ~S~%", p->m_car);
        format(t, L";  cdr = ~S~%", p->m_cdr);
        break;
    } // Tag_Cons

    default:
        format(t, L"; #x~X~X~%",
            Fixnum::Encode(obj->ToInt() >> 4),
            Fixnum::Encode(obj->ToInt() & 15) );
        break;
    } // switch tag

    format(t, L";~%");
    return obj;
} // inspect

} // CommonLisp
