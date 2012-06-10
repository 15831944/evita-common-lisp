#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - minilisp - type system
// compiler/ty/ty_00_fns.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/ty/ty_00_fns.cpp#40 $
//
#include "./ty_defs.h"

#include "../cm/cm_fns.h"
#include "../ir/ir_defs.h"

namespace Compiler
{

extern Ty ty_and_values_values(Ty, Ty);

namespace
{

//////////////////////////////////////////////////////////////////////
//
// TyOr
//
class TyOr
{
    Val m_ty;
    
    public: TyOr() : m_ty(nil) {}

    public: void Parse(Ty ty)
    {
        if (! (consp(ty) && first(ty) == Qor)) ty = list(Qor, ty);
        m_ty = ty;
    } // Parse

    public: static Ty Union(const TyOr* p, const TyOr* q)
    {
        Val result = list(Qor);
        Val last = result;

        foreach (EnumList, oEnum, rest(p->m_ty))
        {
            Val ty = oEnum.Get();
            if (! isSubtypep(ty, result)) last = setf_cdr(list(ty), last);
        } // for each elty

        foreach (EnumList, oEnum, rest(q->m_ty))
        {
            Val ty = oEnum.Get();
            if (! isSubtypep(ty, result)) last = setf_cdr(list(ty), last);
        } // for each elty

        if (nil == cdr(result)) return ty_t;
        if (nil == cddr(result)) return second(result);
        return result;
    } // Union

    static bool isSubtypep(Ty ty1, Val tys)
    {
        foreach (EnumList, oEnum, rest(tys))
        {
            Ty ty2 = oEnum.Get();
            if (Subtypep_Yes == ty_subtypep(ty1, ty2)) return true;
        } // for each ty
        return false;
    } // isSubtypep
}; // TyOr

} // namespace


// find_type
Val find_type(Val name)
{
    Val env = TLV(AenvironmentA);

    for (;;)
    {
        Environment* pEnv = env->Decode<Environment>();

        {
            with_shared_latch(pEnv->m_latch);

            Val datum = gethash(name, pEnv->m_types);
            if (nil != datum) return datum;

            Val klass = gethash(name, pEnv->m_classes);
            if (nil != klass) return name;

            env = pEnv->m_outer;
        }

        if (nil == env) return nil;
    } // for
} // find_type


//////////////////////////////////////////////////////////////////////
//
// ty_and
//
Ty ty_and(Ty ty1, Ty ty2)
{
    if (ty_unspecified == ty1) return ty1;
    if (ty_unspecified == ty2) return ty2;

    ty1 = ty_canonical(ty1);
    ty2 = ty_canonical(ty2);

    if (ty_void == ty1)
    {
        warn(L"Unexpected use of ~S.", ty1);
        return nil;
    }

    if (ty_void == ty2)
    {
        warn(L"Unexpected use of ~S.", ty2);
        return nil;
    }

    if (nil == ty1 || nil == ty2) return nil;

    if (ty_is_values(ty1) && ty_is_values(ty2))
    {
        return ty_and_values_values(ty1, ty2);
    }

    if (ty_is_values(ty1)) return ty_and(ty_get_primary(ty1), ty2);
    if (ty_is_values(ty2)) return ty_and(ty1, ty_get_primary(ty2));

    {
        TyInteger oTy1 = TyInteger::Parse(ty1);
        TyInteger oTy2 = TyInteger::Parse(ty2);
        if (oTy1.IsValid() && oTy2.IsValid())
            { return (oTy1 && oTy2).Unparse(); }
    }

    // ty1 < ty2
    Subtypep sub12 = ty_subtypep(ty1, ty2);
    if (Subtypep_Yes == sub12) return ty1;

    // ty2 < ty1
    Subtypep sub21 = ty_subtypep(ty2, ty1);
    if (Subtypep_Yes == sub21) return ty2;

    if (Subtypep_No == sub12 && Subtypep_No == sub21)
    {
        return nil;
    }

    {
        Val class1 = ty1;
            if (symbolp(ty1))
            {
                class1 = find_class(ty1, nil, TLV(AenvironmentA));
            }

        Val class2 = ty2;
            if (symbolp(ty2))
            {
                class2 = find_class(ty2, nil, TLV(AenvironmentA));
            }

        if (classp(class1) && classp(class2)) return nil;
    }

    if (consp(ty1) && Qor == first(ty1))
    {
        Ty ty = nil;
        foreach (EnumList, oEnum, rest(ty1))
        {
            ty = ty_or(ty, ty_and(oEnum.Get(), ty2));
        } // for each elt
        return ty;
    } // or

    if (consp(ty2) && Qor == first(ty2))
    {
        Ty ty = nil;
        foreach (EnumList, oEnum, rest(ty2))
        {
            ty = ty_or(ty, ty_and(ty1, oEnum.Get()));
        } // for each elt
        return ty;
    } // or

    //return list(Qand, ty1, ty2);
    return ty1;
} // ty_and


// ty_canonical
Ty ty_canonical(Ty ty)
{
    ty = ty_expand(ty);
    if (ty == Qfixnum) return ty_fixnum_;
    if (ty == CLASS_fixnum) return ty_fixnum_;
    if (! consp(ty)) return ty;

    if (equal(ty, ty_simple_array_A_1A)) return ty_data_vector;

    Val op = first(ty);
    if (op == Qfunction)        return op;
    if (op == Qreal)            return op;
    if (op == Qrational)        return op;
    if (op == Qfloat)           return op;
    if (op == Qdouble_float)    return op;
    if (op == Qsingle_float)    return op;

#if 0
    if (op == Qmod)
      { return list(Qinteger, Fixnum::Encode(0), sub(second(ty), 1)); }
#endif

    if (op == Qsimple_array)
    {
        Val runner = cdr(ty);
        if (nil == runner) return op;
        Val elty = pop(runner);
        if (nil == runner) return op;
        Val dims = pop(runner);
        if (fixnump(dims))  return op;   // rank
        if (QA == dims)     return op;   // *
        if (! consp(dims))  return op;
        if (nil == cdr(dims))
        {
            if (Qcharacter == elty) return ty_simple_string;
            return ty_vector;
        }
        return op;
    } // if simple_array

    return ty;
} // ty_canonical


bool is_length_3_list(Val x)
{
    if (! consp(x)) return false;
    x = cdr(x);
    if (! consp(x)) return false;
    x = cdr(x);
    if (! consp(x)) return false;
    return cdr(x) == nil;
} // is_length_3_list


//////////////////////////////////////////////////////////////////////
//
// ty_diff
//
Ty ty_diff(Ty ty1, Ty ty2)
{
    if (ty1 == ty2) return nil;
    if (ty_list == ty1) return ty_cons;
    if (is_length_3_list(ty1) && first(ty1) == Qor)
    {
        if (ty_equal(second(ty1), ty2)) return third(ty1);
        if (ty_equal(third(ty1),  ty2)) return second(ty1);
    } // or
    return Qnot;
} // ty_diff


// ty_equal
bool ty_equal(Ty ty1, Ty ty2)
{
    ty1 = ty_expand(ty1);
    ty2 = ty_expand(ty2);
    return equal(ty1, ty2);
} // ty_equal


// ty_expand
Ty ty_expand(Ty ty)
{
    if (symbolp(ty))
    {
        if (nil == ty) return ty;

        // Native Types
        if (ty_void == ty) return ty;
        if (ty_bool== ty) return ty;

        if (ty_int == ty) return ty;
        if (ty_uint == ty) return ty;
        if (ty_int8 == ty) return ty;
        if (ty_uint8 == ty) return ty;
        if (ty_int16 == ty) return ty;
        if (ty_uint16 == ty) return ty;
        if (ty_int32 == ty) return ty;
        if (ty_uint32 == ty) return ty;
        if (ty_int64 == ty) return ty;
        if (ty_uint64 == ty) return ty;

        if (ty_float32 == ty) return ty;
        if (ty_float64 == ty) return ty;

        // Compiler types
        if (ty_c6_stack_cell == ty) return ty;
        if (ty_c6_literal_cell == ty) return ty;

        if (Qunspecified == ty) return ty;

        if (Qvalues == ty)
        {
            warn(L"Invalid type specifier: ~S", ty);
            return ty_t;
        }

        Val datum = find_type(ty);
        if (nil == datum)
        {
            style_warn(L"Type ~S is undefined.", ty);
            return t;
        }

        if (datum == ty)
        {
            return ty;
        }

        if (functionp(datum))
        {
            ty = funcall(datum, list(ty), TLV(AenvironmentA));
        }
        else
        {
            ty = datum;
        }

        return ty_expand(ty);
    }
    else if (consp(ty))
    {
        Val op = first(ty);

        if (Qand == op) return ty;
        if (Qeql == op) return ty;
        if (Qmember == op) return ty;
        if (Qnot == op) return ty;
        if (Qor == op) return ty;
        if (Qsatisfies == op) return ty;
        if (Qvalues == op) return ty;

        // Native Type
        if (Qptr == op) return ty;

        // For (typep x '(complex rational))
        if (Qcomplex == op)
        {
            Val party = second(ty);

            if (Subtypep_Yes == ty_subtypep(party, ty_rational))
                { return ty_rational_complex; }

            if (Subtypep_Yes == ty_subtypep(party, ty_single_float))
                { return ty_single_float_complex; }

            if (Subtypep_Yes == ty_subtypep(party, ty_double_float))
                { return ty_double_float_complex; }

            return Qcomplex;
        }

        Val datum = find_type(op);
        if (nil == datum)
        {
            style_warn(L"Type ~S is undefined.", ty);
            return t;
        }

        if (! functionp(datum))
        {
            return ty;
        }

        return ty_expand(funcall(datum, ty, TLV(AenvironmentA)));
    } // if
    return ty;
} // ty_expand


// ty_expand_tylist
Val ty_expand_tylist(Val tylist)
{
    Val anchor = list(Fixnum::Encode(0));
    Val last = anchor;
    bool fKey = false;
    foreach (EnumList, oEnum, tylist)
    {
        Ty paramty = oEnum.Get();

        if (fKey)
        {
            if (paramty == QAallow_other_keys)
            {
                // skip &allow-other-keys
            }
            else if (consp(paramty))
            {
                paramty = list(first(paramty), ty_expand(second(paramty)));
            }
            else
            {
                warn(L"Invalid &key type ~S.", paramty);
                return Qfunction;
            }
        }
        else if (paramty == QAoptional ||
            paramty == QArest )
        {
            // skip lambda-list-keyword
        }
        else if (paramty == QAkey)
        {
            fKey = true;
        }
        else
        {
            paramty = ty_expand(paramty);
        }
        last = setf_cdr(list(paramty), last);
    } // for each elt

    return rest(anchor);
} // ty_expand_tylist


// ty_expand_funty
Ty ty_expand_funty(Ty funty)
{
    if (symbolp(funty)) funty = ty_expand(funty);
    if (Qfunction == funty) return funty;

    if (! consp(funty))
    {
        warn(L"Invalid function type ~S.", funty);
        return Qfunction;
    }

    Val ll = ty_expand_tylist(second(funty));

    Val valty = ty_expand(third(funty));
    if (consp(valty) && Qvalues == first(valty))
    {
        valty = cons(Qvalues, ty_expand_tylist(rest(valty)));
    }

    return list(Qfunction, ll, valty);
} // ty_expand_funty


// ty_get_function_param
Ty ty_get_function_param(Ty fn_ty)
{
    fn_ty = ty_expand(fn_ty);
    if (consp(fn_ty) && Qfunction == first(fn_ty))
    {
        // (function (ty*) ty)
        return cons(Qvalues, second(fn_ty));
    }
    else
    {
        return ty_values_rest_t;
    }
} // ty_get_function_param


// ty_get_function_value
Ty ty_get_function_value(Ty fn_ty)
{
    fn_ty = ty_expand(fn_ty);
    if (consp(fn_ty) && Qfunction == first(fn_ty))
    {
        // (function (ty*) ty)
        return third(fn_ty);
    }
    else
    {
        return ty_values_rest_t;
    }
} // ty_get_function_value


// ty_get_pointee
Ty ty_get_pointee(Ty ty)
{
    ASSERT(ty_is_ptr(ty));
    return second(ty);
} // ty_get_pointee


//////////////////////////////////////////////////////////////////////
//
// primaty type
//
Ty ty_get_primary(Ty ty)
{
    ty = ty_expand(ty);
    if (consp(ty) && Qvalues == first(ty))
    {
        Ty ty2 = nil == cdr(ty) ? Qnull : second(ty);
        if (QAoptional == ty2 || QArest == ty2)
        {
            ty2 = ty_or(Qnull, third(ty));
        }
        return ty2;
    } // if

    return ty;
} // ty_get_primary


// ty_is_ptr
bool ty_is_ptr(Ty ty)
{
    ty = ty_expand(ty);
    return consp(ty) && Qptr == first(ty);
} // ty_is_ptr


// ty_is_values
bool ty_is_values(Ty ty)
{
    ty = ty_expand(ty);
    return consp(ty) && Qvalues == first(ty);
} // ty_is_values


//////////////////////////////////////////////////////////////////////
//
// ty_make_ptr
//
Ty ty_make_ptr(Ty ty)
{
    //ty = ty_expand(ty);
    if (t == ty) return ty_ptr_t;
    return list(Qptr, ty);
} // ty_make_ptr


//////////////////////////////////////////////////////////////////////
//
// ty_nth
//
Ty ty_nth(Ty ty, uint n)
{
    foreach (TyValues::Enum, oEnum, ty)
    {
        if (0 == n) return oEnum.Get();
        n -= 1;
    } // for each elty
    return Qnull;
} // ty_nth


///////////////////////////////////////////////////////////////////////
//
// ty_or_values
//
Ty ty_or_values(Ty ty1, Ty ty2)
{
    if (ty_void == ty1)
    {
        warn(L"Unexpected use of ~S.", ty1);
        return nil;
    }

    if (ty_void == ty2)
    {
        warn(L"Unexpected use of ~S.", ty2);
        return nil;
    }

    if (ty_unspecified == ty1) return ty_unspecified;
    if (ty_unspecified == ty2) return ty_unspecified;

    if (ty1 == nil) return ty2;
    if (ty2 == nil) return ty1;
    if (ty_equal(ty1, ty2)) return ty1;

    Arity oArity1;
        ir_get_ty_arity(ty1, &oArity1);

    Arity oArity2;
        ir_get_ty_arity(ty2, &oArity2);

    if (oArity1.GetMin()  == oArity2.GetMin() &&
        oArity1.GetMax()  == oArity2.GetMax() &&
        oArity1.IsFixed() == oArity2.IsFixed() )
    {
        Val resulty = list(Qvalues);
        Val last = resulty;

        enum
        {
            State_Required,
            State_Optional,
            State_Rest,
        } eState = State_Required;

        TyValues::Enum oEnum1(ty1);
        foreach (TyValues::Enum, oEnum2, ty2)
        {
            switch (eState)
            {
            case State_Required:
                if (oEnum2.IsOptional())
                {
                    last = setf_cdr(list(QAoptional), last);
                    eState = State_Optional;
                }
                else if (oEnum2.IsRest())
                {
                    last = setf_cdr(list(QArest), last);
                    eState = State_Rest;
                }
                break;

            case State_Optional:
                if (oEnum2.IsRest())
                {
                    last = setf_cdr(list(QArest), last);
                    eState = State_Rest;
                }
                break;
            } // switch state

            Ty ty = ty_or(oEnum1.Get(), oEnum2.Get());
            last = setf_cdr(list(ty), last);
        } // for each elty

        if (nil != cdr(resulty) && nil == cddr(resulty))
            { return second(resulty); }

        return resulty;
    } // if

    return ty_values_rest_t;
    //return list(Qor, ty1, ty2);
} // ty_or_values


//////////////////////////////////////////////////////////////////////
//
// ty_or
//
Ty ty_or(Ty ty1, Ty ty2)
{
    if (ty_unspecified == ty1) return ty1;
    if (ty_unspecified == ty2) return ty2;

    if (nil == ty1) return ty2;
    if (nil == ty2) return ty1;

    ty1 = ty_canonical(ty1);
    ty2 = ty_canonical(ty2);

    // Note: We don't call ty_subtypep here. Since, subtypep returns
    // yes for (subtypep '(values) 'null).

    if (ty_is_values(ty1) || ty_is_values(ty2))
    {
#if 1
        return ty_or_values(ty1, ty2);
#else
        Val resulty = list(Qvalues);
        Val last = resulty;
        TyValues::Enum oEnum1(ty1);
        TyValues::Enum oEnum2(ty2);
        enum
        {
            State_Required,
            State_Optional,
            State_Rest,
        } eState = State_Required;
        for (;;)
        {
            if (oEnum1.AtEnd() && oEnum2.AtEnd()) break;

            switch (eState)
            {
            case State_Required:
                if (oEnum1.IsOptional() || oEnum2.IsOptional())
                {
                    last = setf_cdr(list(QAoptional), last);
                    eState = State_Optional;
                }
                else if (oEnum1.IsRest())
                {
                    if (oEnum2.IsRest() || oEnum2.AtEnd())
                    {
                        last = setf_cdr(list(QArest), last);
                        eState = State_Rest;
                    }
                    else
                    {
                        last = setf_cdr(list(QAoptional), last);
                        eState = State_Optional;
                    }
                }
                else if (oEnum1.AtEnd())
                {
                    if (oEnum2.IsRequired())
                    {
                        last = setf_cdr(list(QAoptional), last);
                        eState = State_Optional;
                    }
                    else
                    {
                        last = setf_cdr(list(QArest), last);
                        eState = State_Rest;
                    }
                }
                break;

            case State_Optional:
                if (oEnum1.IsRest())
                {
                    if (oEnum2.IsRest() || oEnum2.AtEnd())
                    {
                        last = setf_cdr(list(QArest), last);
                        eState = State_Rest;
                    }
                }
                else if (oEnum2.IsRest())
                {
                    if (oEnum1.AtEnd())
                    {
                        last = setf_cdr(list(QArest), last);
                        eState = State_Rest;
                    }
                }
                break;
            } // switch eState

            Ty ty = ty_or(oEnum1.Get(), oEnum2.Get());
            last = setf_cdr(list(ty), last);

            if (State_Rest == eState) break;

            oEnum1.Next();
            oEnum2.Next();
        } // for

        return resulty;
#endif
    } // if

    if (Subtypep_Yes == ty_subtypep(ty1, ty2)) return ty2;
    if (Subtypep_Yes == ty_subtypep(ty2, ty1)) return ty1;

    if (ty1 == Qcons && ty2 == Qnull) return Qlist;
    if (ty2 == Qcons && ty1 == Qnull) return Qlist;

    {
        TyInteger oTy1 = TyInteger::Parse(ty1);
        TyInteger oTy2 = TyInteger::Parse(ty2);
        if (oTy1.IsValid() && oTy2.IsValid())
            { return (oTy1 || oTy2).Unparse(); }
    }

    {
        TyOr oTy1, oTy2;
        oTy1.Parse(ty1);
        oTy2.Parse(ty2);
        return TyOr::Union(&oTy1, &oTy2);
    }
} // ty_or


static Val classify(Ty ty)
{
    ASSERT(ty != Qfixnum);
    ASSERT(ty != CLASS_fixnum);

    if (symbolp(ty)) return find_class(ty, nil, TLV(AenvironmentA));
    if (classp(ty)) return ty;
    if (consp(ty)) return find_class(first(ty), nil, TLV(AenvironmentA));
    warn(L"Invalid type specifier: ~S", ty);
    return nil;
} // classify


//////////////////////////////////////////////////////////////////////
//
// ty_subtypep
//
Subtypep
ty_subtypep(Ty ty1, Ty ty2)
{
    if (ty_unspecified == ty1) return Subtypep_Yes;
    if (ty_unspecified == ty2) return Subtypep_Yes;

    //format(t, L"ty_subtypep: ~S ~S~%", ty1, ty2);

    ty1 = ty_canonical(ty1);
    ty2 = ty_canonical(ty2);

    if (ty_equal(ty1, ty2)) return Subtypep_Yes;
    if (ty1 == nil) return Subtypep_Yes;
    if (ty2 == nil) return Subtypep_No;

    if (ty_is_values(ty1) || ty_is_values(ty2))
    {
        TyValues::Enum oEnum1(ty1);
        foreach (TyValues::Enum, oEnum2, ty2)
        {
            Subtypep eSubtypep = ty_subtypep(oEnum1.Get(), oEnum2.Get());
            if (Subtypep_Yes != eSubtypep) return eSubtypep;
            if (oEnum2.IsRest()) break;
            oEnum1.Next();
        } // for each elty
        return oEnum1.AtEnd() ? Subtypep_Yes : Subtypep_No;
    } // if

    if (ty1 == t)   return Subtypep_No;
    if (ty2 == t)   return Subtypep_Yes;

    Val class1 = classify(ty1);
    Val class2 = classify(ty2);

    if (classp(class1) && classp(class2))
    {
        if (! subclassp(class1, class2)) return Subtypep_No;
        if (! consp(ty1) && ! consp(ty2)) return Subtypep_Yes;
    }

    if (classp(ty1)) ty1 = ty1->Decode<Class>()->m_name;
    if (classp(ty2)) ty2 = ty2->Decode<Class>()->m_name;

    {
        TyInteger oTy1 = TyInteger::Parse(ty1);
        if (oTy1.IsValid())
        {
            TyInteger oTy2 = TyInteger::Parse(ty2);
            if (oTy2.IsValid())
            {
                return IsSubtype(oTy1, oTy2) ? 
                    Subtypep_Yes : Subtypep_No;
            }

            if (IsSubtype(oTy1, ty2))
            {
                return Subtypep_Yes;
            }

            if (consp(ty2))
            {
                Val op2 = first(ty2);

                if (op2 == Qsatisfies)
                {
                    return Subtypep_Unknown;
                }

                if (op2 == Qeql)
                {
                    if (oTy1.m_lower != oTy1.m_upper) return Subtypep_No;
                    return oTy1.m_lower == second(ty2) ?
                        Subtypep_Yes : Subtypep_No;
                } // eql
            } // if
        } // if
    }

    {
        TyInteger oTy2 = TyInteger::Parse(ty2);
        if (oTy2.IsValid())
        {
            if (IsSubtype(ty1, oTy2)) return Subtypep_Yes;
            if (! consp(ty1)) return Subtypep_No;

            Val op1 = first(ty1);

            if (op1 == Qeql)
            {
                return IsTypeOf(second(ty1), oTy2) ?
                    Subtypep_Yes : Subtypep_No;
            } // eql

            if (op1 == Qmember)
            {
                foreach (EnumList, oEnum, rest(ty1))
                {
                    if (! IsTypeOf(oEnum.Get(), oTy2)) return Subtypep_No;
                } // for each elt

                return Subtypep_Yes;
            } // member
        } // if
    }

    if (consp(ty1))
    {
        Val op1 = first(ty1);

        if (Qsatisfies == op1) return Subtypep_Unknown;

        if (Qeql == op1)
        {
            return ty_typep(second(ty1), ty2) ? Subtypep_Yes : Subtypep_No;
        }

        if (Qmember == op1)
        {
            foreach (EnumList, oEnum, rest(ty1))
            {
                if (! ty_typep(oEnum.Get(), ty2)) return Subtypep_No;
            } // for each elt

            return Subtypep_Yes;
        }
    } // if consp

    if (consp(ty2))
    {
        Val op2 = first(ty2);

        if (Qsatisfies == op2) return Subtypep_Unknown;

        if (Qeql == op2)
        {
            // (subtypep 'null '(eql nil))
            return CLASS_null == ty1 && second(ty2) == nil ?
                Subtypep_Yes : Subtypep_No;
        }

        if (Qor == op2)
        {
            Subtypep eSubtypep = Subtypep_No;

            foreach (EnumList, oEnum, rest(ty2))
            {
                Ty ty = oEnum.Get();

                switch (ty_subtypep(ty1, ty))
                {
                case Subtypep_Yes:     return Subtypep_Yes;
                case Subtypep_Unknown: eSubtypep = Subtypep_Unknown; break;
                } // switch
            } // for each elt

            return eSubtypep;
        } // or
    } // if consp

    return Subtypep_Unknown;
} // ty_subtypep


// typep
bool
ty_typep(Val obj, Val ty)
{
    if (ty_void == ty)
    {
        warn(L"Unexpected use of ~S.", ty);
        return nil;
    }

    if (ty_unspecified == ty) return t;

    ty = ty_canonical(ty);

    {
        Val klass = classp(ty) ? ty : find_class(ty, nil, TLV(AenvironmentA));
        if (nil != klass) return subclassp(class_of(obj), klass);
    }

    if (consp(ty))
    {
        Val op = first(ty);

        if (Qand == op)
        {
            foreach (EnumList, oEnum, rest(ty))
            {
                if (! ty_typep(obj, oEnum.Get())) return false;
            } // for
            return true;
        } // if and

        if (Qeql == op) return eql(second(ty), obj);

        if (Qfunction == op) return functionp(obj);

        if (Qmember == op)
        {
            foreach (EnumList, oEnum, rest(ty))
            {
                if (eql(oEnum.Get(), obj)) return true;
            } // for
            return false;
        } // if member

        if (Qnot== op)
        {
            return ! ty_typep(obj, second(ty));
        } // if not

        if (Qor == op)
        {
            foreach (EnumList, oEnum, rest(ty))
            {
                if (ty_typep(obj, oEnum.Get())) return true;
            } // for
            return false;
        } // if or

        {
            TyInteger oTy = TyInteger::Parse(ty);
            if (oTy.IsValid()) return IsTypeOf(obj, oTy);
        }

        if (Qvalues == op)
        {
            foreach (TyValues::Enum, oEnum, ty)
            {
                if (! ty_typep(obj, oEnum.Get())) return false;
                obj = nil;
                if (! oEnum.IsRequired()) break;
            } // for each elty
            return true;
        }

        {
            Val klass = find_class(op, nil, TLV(AenvironmentA));
            if (nil != klass) return ty_typep(obj, klass);
        }
    } // if

    warn(L"Undefined type: ~S", ty);
    return false;
} // ty_typep

} // Compiler
