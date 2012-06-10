#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - Architecture Abstraction
// kernel/ke_arch.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/kernel/ke_arch.cpp#7 $
//
#include ke_arch_cpp

#include "./ke_layout.h"
#include "./ke_memory.h"

namespace Kernel
{

//////////////////////////////////////////////////////////////////////
//
// Val::GetClassD
//
Val Val_::GetClassD() const
{
    // 16-way branch
    switch (GetTag4())
    {
    case_Tag_Fixnum:
        return CLASSD_fixnum;

    case Val_::Tag_Null:
        return CLASSD_null;

    case_Tag_Record:
        return Decode<Record>()->m_classd;

    case_Tag_Cons:
        return CLASSD_cons;

    case Val_::Tag_Function:
        return Decode<Funcallable>()->m_classd;

    #if USE_SINGLE_FLOAT_TAG
        case Val_::Tag_SingleFloat:
            return CLASSD_single_float;
    #endif//  USE_SINGLE_FLOAT_TAG
    } // tag

    return CLASSD_invalid_object;
} // Val_::GetClassD


//////////////////////////////////////////////////////////////////////
//
// Val_::GetPtr
//
void* Val_::GetPtr() const
{
    switch (GetTag4())
    {
    case_Tag_Record:
        return Decode<Record>();

    case Val_::Tag_Function:
        return Decode<Funcallable>();

    case_Tag_Cons:
        return Decode<Cons>();

    case Val_::Tag_Null:
        return Decode<Null>();

    default:
        CAN_NOT_HAPPEN();
    } // switch tag
} // Val_::GetPtr


//////////////////////////////////////////////////////////////////////
//
// Val_::GetSize
//
size_t Val_::GetSize() const
{
    switch (GetTag4())
    {
    case_Tag_Immediate:
        return sizeof(Val);

    case_Tag_Record:
        return GetClassD()->Decode<ClassD>()->ComputeSize(Decode<Record>());

    case_Tag_Cons:
        return sizeof(Cons);

    case Val_::Tag_Function:
        return Decode<Funcallable>()->m_cbFunction;
    } // tag

    return sizeof(Val);
} // Val_::GetSize


//////////////////////////////////////////////////////////////////////
//
// Memory::MapToArea
//
Area* Memory::MapToArea(Val x)
{
    switch (x->GetTag4())
    {
    case_Tag_Immediate:
        return NULL;

    case_Tag_Record:
        return Memory::MapToArea(x->Decode<Record>());

    case_Tag_Cons:
        return Memory::MapToArea(x->Decode<Cons>());

    case Val_::Tag_Function:
        return Memory::MapToArea(x->Decode<Funcallable>());
    } // tag
    CAN_NOT_HAPPEN();
} // Memory::MapToArea

} // Kernel
