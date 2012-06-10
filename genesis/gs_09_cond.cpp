#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - 9 Conditions
// genesis/gs_09_cond.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/genesis/gs_09_cond.cpp#5 $
//
#include "./gs_lisp.h"

namespace Genesis
{

//////////////////////////////////////////////////////////////////////
//
//  c_error
//
void __declspec(noreturn)
C_error(MiniThread* p)
{
  try_again:
    if (Fixnum::Encode(0) == p->m_n)
    {
        values(
            Qtoo_few_arguments,
            Kfunction,
            symbol_function(Qerror),
            Karguments,
            nil );
        goto try_again;
    } // if

    Val args = multiple_value_list(p);
    Val datum = car(args);
        args = cdr(args);


    format(t, L"c_error: datum=~S args=~S~%", datum, args);

    Val cond;

    if (simple_stringp(datum))
    {
        LPCWSTR pwszControl = datum->Decode<SimpleString>()->GetElements();
        cond = make_simple_error(pwszControl, args);
    }
#if 0
    else if (Qnot_function == datum)
    {
        cond = p->AllocInstance(CLASSD_not_function);

        NotFunction* pCond = cond->Decode<NotFunction>();
            pCond->m_name = cadr(memq(Kname, args));
    }
    else if (Qtoo_few_arguments == datum)
    {
        cond = p->AllocInstance(CLASSD_too_few_arguments);

        TooFewArguments* pCond = cond->Decode<TooFewArguments>();
            pCond->m_function  = cadr(memq(Kfunction, args));
            pCond->m_arguments = copy_list(cadr(memq(Karguments, args)));
    }
    else if (Qtoo_many_arguments == datum)
    {
        cond = p->AllocInstance(CLASSD_too_many_arguments);

        TooManyArguments* pCond = cond->Decode<TooManyArguments>();
            pCond->m_function  = cadr(memq(Kfunction, args));
            pCond->m_arguments = copy_list(cadr(memq(Karguments, args)));
    }
    else if (Qtype_error == datum)
    {
        cond = make_type_error(
            cadr(memq(Kdatum, args)),
            cadr(memq(Kexpected_type, args)) );
    }
    else if (Qundefined_function == datum)
    {
        cond = p->AllocInstance(CLASSD_undefined_function);

        UndefinedFunction* pCond = cond->Decode<UndefinedFunction>();
            pCond->m_name = cadr(memq(Kname, args));
    }
    else
    {
        error(make_type_error(
            datum,
            list(Qor, Qstring, Qsymbol, Qcondition) ) );
    }
#else
    else if (subclassp(class_of(datum), CLASS_condition))
    {
        cond = datum;
    }
    else
    {
        class MakeCond
        {
            public: static Val Run(Val datum, Val args)
            {
                Val klass = datum;
                if (! classp(klass))
                    klass = find_class(datum);

                Val cond = MiniThread::Get()->AllocInstance(
                    klass->Decode<Class>()->m_instanced );

                foreach (EnumList, oEnum, args)
                {
                    Val key = oEnum.Get();
                        oEnum.Next();
                    Val val = oEnum.Get();

                    Val slotd = find_slotd(klass, key);
                    if (nil != slotd)
                    {
                        EffectiveSlotD* pSlotD =
                            slotd->Decode<EffectiveSlotD>();

                        Val storage = cond->Decode<Instance>()->m_storage;

                        storage->Decode<Storage>()->
                            mv_element[Fixnum::Decode_(pSlotD->m_location)] = val;
                    }
                } // for each initarg

                return cond;
            } // Run

            static Val find_slotd(Val klass, Val key)
            {
                foreach (EnumList, oEnum, klass->Decode<Class>()->m_slots)
                {
                    Val eslotd = oEnum.Get();
                    EffectiveSlotD* pESlotD = eslotd->Decode<EffectiveSlotD>();
                    if (nil != memq(key, pESlotD->m_initargs))
                    {
                        return eslotd;
                    }
                } // for each slotd
                return nil;
            } // find_slotd
        }; // MakeCond

        cond = MakeCond::Run(datum, args);
    } // if
#endif

    invoke_debugger(cond);
} // c_error

// expand_assert - support simple form only
Val expand_assert(Val form, Val)
{
    CHECK_SYNTAX(form, 2, 5, "(assert test [(place*) datum form*])");
    Val test = second(form);
    return list(Qunless, test,
        list(QZassert, list(Qquote, test), nil, nil, nil) );
} // expand_assert


// expand_check_type
Val expand_check_type(Val form, Val)
{
    CHECK_SYNTAX(form, 3, 4, "(check-type place type [string])");
    Val place = second(form);
    Val type  = third(form);
    return list(Qunless, list(Qtypep, place, list(Qquote, type)),
        list(Qerror, list(Qquote, Qtype_error),
                Kdatum, place,
                Kexpected_type, list(Qquote, type) ));
} // expand_check_type

} // Genesis


namespace CommonLisp
{

void __declspec(noreturn)
error(Val cond)
{
    funcall(Qerror, cond);
} // error

// error
void __declspec(noreturn)
error(Val cond, Val key1, Val a)
{
    apply(Qerror, list(cond, key1, a));
} // error

// error
void __declspec(noreturn)
error(Val cond, Val key1, Val a, Val key2, Val b)
{
    apply(Qerror, list(cond, key1, a, key2, b));
} // error

void __declspec(noreturn)
error(Val cond, Val key1, Val a, Val key2, Val b, Val key3, Val c)
{
    apply(Qerror, list(cond, key1, a, key2, b, key3, c));
} // error

} // CommonLisp
