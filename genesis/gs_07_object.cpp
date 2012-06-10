#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - 7 Objects
// genesis/gs_07_object.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/genesis/gs_07_object.cpp#3 $
//
#include "./gs_lisp.h"

namespace Genesis
{

// applicable_method_p
static bool applicable_method_p(MiniThread* p, Val method)
{
    Int i = 0;
    foreach (
        EnumList,
        oEnum,
        method->Decode<StandardMethod>()->m_specializers )
    {
        Val specializer = oEnum.Get();
        if (! classp(specializer)) return false;
        if (! subclassp(class_of(p->mv_value[i]), specializer)) return false;
        i += 1;
    } // for each method

    return true;
} // applicable_method_p

// genesis_discriminator
Val genesis_discriminator(MiniThread* p, Val gf)
{
    StandardGenericFunction* pGF = gf->Decode<StandardGenericFunction>();

    foreach (EnumList, oEnum, pGF->m_methods)
    {
        Val method = oEnum.Get();
        if (applicable_method_p(p, method))
        {
            p->m_fn = method->Decode<StandardMethod>()->m_function;
            return CallLisp(p);
        }
    } // for each method

    Val args = multiple_value_list(p);
    error(L"There is no applicable method for ~S with ~S", gf, args);
} // genesis_discriminator

} // Genesis
