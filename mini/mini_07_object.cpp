#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - mii - 07 Objects
// mini/mini_07_object.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/mini/mini_07_object.cpp#3 $
//
#include "./mini_lisp.h"

namespace MiniLisp
{

// classp
bool classp(Val obj)
{
    return subclassp(class_of(obj), CLASS_class);
} // classp

// funcallable_standard_object_p
bool funcallable_standard_object_p(Val x)
{
    if (! functionp(x)) return false;

    Val klass = x->Decode<FuncallableInstance>()->m_classd->
        Decode<ClassD>()->m_class;

    return subclassp(klass, CLASS_funcallable_standard_object);
} // funcallable_standard_object_p


// subclassp
bool subclassp(Val class1, Val class2)
{
    if (class1 == class2) return true;

    Class* pClass1 = class1->Decode<Class>();
    Val cpl1 = pClass1->m_class_precedence_list;
    if (nil != cpl1) return nil != memq(class2, cpl1);

    foreach (EnumList, oEnum, pClass1->m_direct_superclasses)
    {
        if (subclassp(oEnum.Get(), class2)) return true;
    } // for each super

    return false;
} // subclassp

} // MiniLisp

namespace CommonLisp
{

// class_of
Val class_of(Val x)
{
    return x->GetClassD()->Decode<ClassD>()->m_class;
} // class_of

// find_class
Val find_class(Val name, Val errorp, Val env)
{
    if (nil == env)
    {
        env = VAR(Aruntime_environmentA);
    }

    for (;;)
    {
        Val klass = gethash(name, env->Decode<Environment>()->m_classes);
        if (nil != klass)
        {
            return klass;
        }

        env = env->Decode<Environment>()->m_outer;
        if (nil == env)
        {
            break;
        }
    } // for

    if (nil != errorp)
    {
        error(Qclass_not_found, Kname, name);
    }

    return nil;
} // find_class

} // CommonLisp
