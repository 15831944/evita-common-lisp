//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - ir - Types
// ty/ty_defs.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/ty/ty_defs.h#14 $
//
#if !defined(INCLUDE_compiler_ty_defs_h)
#define INCLUDE_compiler_ty_defs_h

namespace Compiler
{

typedef Val Ty;
enum Subtypep { Subtypep_No, Subtypep_Yes, Subtypep_Unknown };
struct Arity;

Ty          ty_and(Ty, Ty);
Ty          ty_canonical(Ty);
Ty          ty_diff(Ty, Ty);
bool        ty_equal(Ty, Ty);
Ty          ty_expand(Ty);
Ty          ty_expand_funty(Ty);
Ty          ty_expand_tylist(Ty);
Ty          ty_get_function_param(Ty);
Ty          ty_get_function_value(Ty);
Ty          ty_get_pointee(Ty);
Ty          ty_get_primary(Ty);
bool        ty_is_ptr(Ty);
bool        ty_is_values(Ty);
Ty          ty_make_ptr(Ty);
Ty          ty_nth(Ty, uint);
Ty          ty_or(Ty, Ty);
Ty          ty_or_values(Ty, Ty);
Subtypep    ty_subtypep(Ty, Ty);
bool        ty_typep(Val, Ty);
Ty          ty_type_of(Val);

// ty_set_function_value
inline Ty ty_set_function_value(Ty funty, Ty ty)
{
    ASSERT(consp(funty) && Qfunction == car(funty));
    return setf_car(ty, cddr(funty));
} // ty_set_function_value


// ty_type_of
inline Ty ty_type_of(Val obj)
    { return type_of(obj); }

// ty_typep
//////////////////////////////////////////////////////////////////////
//
// EnumTy
//
class EnumTy
{
    public: enum State
    {
        State_Atom,
        State_AtomEnd,

        State_Start,
        State_Req,
        State_Opt,
        State_Rest,
        State_KeyName,
        State_KeyVal,
        State_End,
    }; // State

    protected: Val m_runner;
    protected: State m_eState;

    public: EnumTy(Val ty)
    {
        ty = ty_expand(ty);
        if (consp(ty) && Qvalues == first(ty))
        {
            m_eState = State_Start;
            m_runner = cdr(ty);
            Next();
        }
        else
        {
            m_eState = State_Atom;
            m_runner = ty;
        }
    } // tyIterator

    bool AtEnd() const { return State_End == m_eState; }

    public: Ty Get() const
    {
        switch (m_eState)
        {
        case State_Atom:
            return m_runner;

        case State_AtomEnd:
        case State_End:
            return ty_null;

        case State_Req:
        case State_Opt:
        case State_Rest:
            return nil == m_runner ? Qnull : first(m_runner);

        case State_KeyName:
            return ty_symbol;

        case State_KeyVal:
            // BUGBUG: NYI: We should get type from keyword list.
            return ty_t;

        default:
            CAN_NOT_HAPPEN();
        } // switch state
    } // Get

    public: State GetState() const
        { return m_eState; }

    public: State Next()
    {
        switch (m_eState)
        {
        case State_Atom:
            return m_eState = State_AtomEnd;

        case State_AtomEnd:
            return m_eState;

        case State_Req:
            m_runner = cdr(m_runner);
            // FALLTHROUGH

        case State_Start:
            if (nil == m_runner)
            {
                return m_eState = State_End;
            }

            if (QAoptional == first(m_runner))
            {
                m_runner = cdr(m_runner);
                return m_eState = State_Opt;
            }

            if (QArest == first(m_runner))
            {
                m_runner = cdr(m_runner);
                return m_eState = State_Rest;
            }

            if (QAkey == first(m_runner))
            {
                m_runner = cdr(m_runner);
                return m_eState = State_KeyName;
            }

            return m_eState = State_Req;

        case State_Opt:
            m_runner = cdr(m_runner);

            if (nil == m_runner)
            {
                return m_eState = State_End;
            }

            if (QArest == first(m_runner))
            {
                m_runner = cdr(m_runner);
                return m_eState = State_Rest;
            }

            if (QAkey == first(m_runner))
            {
                m_runner = cdr(m_runner);
                return m_eState = State_KeyName;
            }

            return m_eState;

        case State_Rest:
        case State_End:
            return m_eState;

        case State_KeyName:
            return m_eState = State_KeyVal;

        case State_KeyVal:
            return m_eState = State_KeyName;

        default:
            CAN_NOT_HAPPEN();
        } // switch state
    } // Next
}; // EnumTy


//////////////////////////////////////////////////////////////////////
//
// tyIterator
//
class tyIterator
{
    public: enum State
    {
        State_AtomStart,
        State_Atom,
        State_AtomEnd,

        State_Start,
        State_Req,
        State_Opt,
        State_Rest,
        State_KeyName,
        State_KeyVal,
        State_End,
    }; // State

    protected: Val m_ty;
    protected: Val m_runner;
    protected: State m_eState;

    protected: tyIterator() {}

    public: tyIterator(Val ty)
    {
        ty = ty_expand(ty);
        if (consp(ty) && Qvalues == first(ty))
        {
            m_eState = State_Start;
            m_runner = cdr(ty);
        }
        else
        {
            m_eState = State_AtomStart;
            m_runner = ty;
        }

        m_ty = m_runner;
    } // tyIterator

    // GetKeyTy - returns type of keyworad.
    public: Ty GetKeyTy(Val key) const
    {
        if (! consp(m_ty)) return ty_t;

        Val keys = cdr(memq(QAkey, m_ty));
        Val ty = second(assq(key, keys));
        if (nil == ty)
        {
            if (nil != memq(QAallow_other_keys, keys))
            {
                ty = t;
            }
        }
        return ty;
    } // GetKeyTy

    public: Ty GetTy() const
    {
        switch (m_eState)
        {
        case State_Atom:
            return m_runner;

        case State_AtomEnd:
        case State_End:
            return ty_null;

        case State_Req:
        case State_Opt:
        case State_Rest:
            return nil == m_runner ? Qnull : first(m_runner);

        case State_KeyName:
            return ty_symbol;

        case State_KeyVal:
            // BUGBUG: NYI: We should get type from keyword list.
            return ty_t;

        default:
            CAN_NOT_HAPPEN();
        } // switch state
    } // GetTy

    public: State GetState() const
        { return m_eState; }

    public: State Next()
    {
        switch (m_eState)
        {
        case State_AtomStart:
            return m_eState = State_Atom;

        case State_Atom:
            return m_eState = State_AtomEnd;

        case State_AtomEnd:
            return m_eState;

        case State_Req:
            m_runner = cdr(m_runner);
            // FALLTHROUGH

        case State_Start:
            return start();

        case State_Opt:
            m_runner = cdr(m_runner);

            if (nil == m_runner)
            {
                return m_eState = State_End;
            }

            if (QArest == first(m_runner))
            {
                m_runner = cdr(m_runner);
                return m_eState = State_Rest;
            }

            if (QAkey == first(m_runner))
            {
                m_runner = cdr(m_runner);
                return m_eState = State_KeyName;
            }

            return m_eState;

        case State_Rest:
        case State_End:
            return m_eState;

        case State_KeyName:
            return m_eState = State_KeyVal;

        case State_KeyVal:
            return m_eState = State_KeyName;

        default:
            CAN_NOT_HAPPEN();
        } // switch state
    } // Next


    protected: State start()
    {
        if (nil == m_runner)
        {
            return m_eState = State_End;
        }

        if (QAoptional == first(m_runner))
        {
            m_runner = cdr(m_runner);
            return m_eState = State_Opt;
        }

        if (QArest == first(m_runner))
        {
            m_runner = cdr(m_runner);
            return m_eState = State_Rest;
        }

        if (QAkey == first(m_runner))
        {
            m_runner = cdr(m_runner);
            return m_eState = State_KeyName;
        }

        return m_eState = State_Req;
    } // start
}; // tyIterator


//////////////////////////////////////////////////////////////////////
//
// tyArgsIterator
//
class tyArgsIterator : public tyIterator
{
    public: tyArgsIterator(Val ty)
    {
        ty = ty_expand(ty);
        if (consp(ty) && Qfunction == first(ty))
        {
            m_eState = State_Start;
            m_runner = second(ty);
        }
        else
        {
            // function, symbol, si::predicate(?), etc.
            m_eState = State_Start;
            m_runner = cdr(ty_values_rest_t);
        }

        m_ty = m_runner;

        start();
    } // tyArgsIterator
}; // tyArgIterator


////////////////////////////////////////////////////////////
//
// TyValues
//
class TyValues
{
    public: class Enum
    {
        public: enum State
        {
            State_End,
            State_KeyName,
            State_KeyValue,
            State_Optional,
            State_Required,
            State_Rest,
            State_Single,
            State_Start,
        }; // State

        Val m_runner;
        State m_eState;

        protected: Enum() {}

        // start
        protected: void start()
        {
            if (first(m_runner) == QAoptional)
            {
                m_eState = State_Optional;
                m_runner = rest(m_runner);  // skip &optional
            }
            else if (first(m_runner) == QArest)
            {
                m_eState = State_Rest;
                m_runner = rest(m_runner);  // skip &rest
            }
            else
            {
                m_eState = State_Required;
            }
        } // start

        public: Enum(Ty ty)
        {
            ty = ty_expand(ty);

            if (consp(ty) && first(ty) == Qvalues)
            {
                m_runner = rest(ty);
                start();
            }
            else
            {
                m_eState = State_Single;
                m_runner = ty;
            }
        } // TyValues

        public: bool AtEnd() const { return State_End == m_eState; }

        public: Ty Get() const
        {
            switch (m_eState)
            {
            case State_End:      return Qnull;
            case State_KeyName:  return Qsymbol;
            case State_KeyValue: return t;
            case State_Single:   return m_runner;
            default: return first(m_runner);
            } // switch state
        } // Get

        public: bool IsOptional() const
            { return State_Optional == m_eState; }

        public: bool IsRequired() const
        {
            switch (m_eState)
            {
            case State_Required:
            case State_Single:
                return true;
            default:
                return false;
            } // switch state
        } // IsRequired

        public: bool IsRest() const
        {
            switch (m_eState)
            {
            case State_KeyName:
            case State_KeyValue:
            case State_Rest:
                return true;
            default:
                return false;
            } // switch state
        } // IsRest

        public: void Next()
        {
            switch (m_eState)
            {
            case State_End:
                break;

            case State_KeyName:
                m_eState = State_KeyValue;
                break;

            case State_KeyValue:
                m_eState = State_KeyName;
                break;

            case State_Optional:
                m_runner = rest(m_runner);
                if (nil == m_runner)
                {
                    m_eState = State_End;
                }
                else if (first(m_runner) == QArest)
                {
                    m_eState = State_Rest;
                    m_runner = rest(m_runner);  // skip &optional
                }
                else if (first(m_runner) == QAkey)
                {
                    m_eState = State_KeyName;
                }
                break;

            case State_Required:
                m_runner = rest(m_runner);
                if (nil == m_runner)
                {
                    m_eState = State_End;
                }
                else if (first(m_runner) == QAoptional)
                {
                    m_eState = State_Optional;
                    m_runner = rest(m_runner);  // skip &optional
                }
                else if (first(m_runner) == QArest)
                {
                    m_eState = State_Rest;
                    m_runner = rest(m_runner);  // skip &rest
                }
                else if (first(m_runner) == QAkey)
                {
                    m_eState = State_KeyName;
                }
                break;

            case State_Rest:
                break;

            case State_Single:
                m_eState = State_End;
                break;

            default:
                CAN_NOT_HAPPEN();
            } // switch state
        } // Next
    }; // Enum
}; // TyValues


// TyLambdaList
class TyLambdaList : public TyValues
{
    public: class Enum : public TyValues::Enum
    {
        public: Enum(Val ll)
        {
            m_runner = ll;
            start();
        } // Enum
    }; // Enum
}; // TyLambdaList

} // Compiler

#include "./ty_12_number.h"

#endif //!defined(INCLUDE_compiler_ty_defs_h)
