#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - mini - 5 Data and Control Flow
// genesis/geneis_lisp.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/mini/mini_05_ctrl.cpp#6 $
//
#include "./mini_lisp.h"

namespace MiniLisp
{

// function-name-p
bool function_name_p(Val fname)
{
    if (symbolp(fname))
    {
        return true;
    }

    return consp(fname) &&
           Qsetf == car(fname) &&
           consp(cdr(fname)) &&
           symbolp(cadr(fname));
} // function_name_p


// make_undefined_function
Val make_undefined_function(Val name)
{
    Val error = MiniThread::Get()->AllocInstance(CLASSD_undefined_function);
    UndefinedFunction* pError = error->Decode<UndefinedFunction>();
        pError->m_name = name;
    return error;
} // make_undefined_function

} // MiniLisp


namespace CommonLisp
{

// eql
//  Note: (eql -0.0 +0.0) = nil
bool eql(Val x, Val y)
{
    if (x == y)
    {
        return true;
    }

    if (fixnump(x) || fixnump(y))
    {
        return false;
    }

    if (! numberp(y) || ! numberp(x))
    {
        return false;
    }

    if (x->Decode<Record>()->m_classd != y->Decode<Record>()->m_classd)
    {
        return false;
    }

#if defined(EVCL_BOOT)
    return false;
#else
    switch (CLASSD_INDEX_OF(x->Decode<Record>()->m_classd))
    {
    case CLASSD_(bignum):
        if (x->Decode<Bignum>()->m_length != y->Decode<Bignum>()->m_length)
        {
            return false;
        }
        else
        {
            return 0 == ::memcmp(
                x->Decode<Bignum>()->m_rgBigit,
                y->Decode<Bignum>()->m_rgBigit,
                Fixnum::Decode_(x->Decode<Bignum>()->m_length) *
                    sizeof(Bignum::Bigit) );
        } // if

    case CLASSD_(double_float):
        return 0 == ::memcmp(
            &x->Decode<DoubleFloat>()->m_dbl,
            &y->Decode<DoubleFloat>()->m_dbl,
            sizeof(double) );

    case CLASSD_(single_float):
    {
        const uint32* px = reinterpret_cast<uint32*>(
            &x->Decode<SingleFloat>()->m_flt );

        const uint32* py = reinterpret_cast<uint32*>(
            &y->Decode<SingleFloat>()->m_flt );

        return *px == *py;
    } // single_float

    case CLASSD_(ratio):
        return
            eql(x->Decode<Ratio>()->m_num, y->Decode<Ratio>()->m_num) &&
            eql(x->Decode<Ratio>()->m_den, y->Decode<Ratio>()->m_den);

    case CLASSD_(rational_complex):
        return
            eql(x->Decode<RationalComplex>()->m_real,
                y->Decode<RationalComplex>()->m_real ) &&
            eql(x->Decode<RationalComplex>()->m_imag,
                y->Decode<RationalComplex>()->m_imag );

    case CLASSD_(double_float_complex):
        return 0 == ::memcmp(
            &x->Decode<DoubleFloatComplex>()->m_dblReal,
            &y->Decode<DoubleFloatComplex>()->m_dblReal,
            sizeof(double) * 2 );

    case CLASSD_(single_float_complex):
        return 0 == ::memcmp(
            &x->Decode<SingleFloatComplex>()->m_fltReal,
            &y->Decode<SingleFloatComplex>()->m_fltReal,
            sizeof(float) * 2);

    default:
        CAN_NOT_HAPPEN();
    } // switch classd_index
#endif // ! defined(EVCL_BOOT)
} // eql

// equal
bool equal(Val x, Val y)
{
  loop:
    if (eql(x, y))
    {
        return true;
    } // if

    if (consp(x))
    {
        if (consp(y) && equal(car(x), car(y)))
        {
            x = cdr(x);
            y = cdr(y);
            goto loop;
        }
        else
        {
            return false;
        }
    } // if

    if (symbolp(x))
    {
        return eq(x, y);
    }

    if (characterp(x))
    {
        return eq(x, y);
    }

    if (stringp(x))
    {
        return stringp(y) && 0 == string_cs_cmp(x, y);
    }

    if (bit_vector_p(x))
    {
        if (! bit_vector_p(y))
        {
            return false;
        }

        Val n = x->Decode<DataVector>()->m_length;
        if (n != y->Decode<DataVector>()->m_length)
        {
            return false;
        }

        for (Val i = Fixnum::Encode(0); cmp_xx(i, n) < 0; i = add_xx(i, 1))
        {
            if (bit(x, i) != bit(y, i))
            {
                return false;
            }
        } // for i

        return true;
    } // if

    // BUGBUG: NYI: pathname

    return false;
} // equal


//     Characters
//      True if they are char-equal.
//     Numbers
//      True if they are =.
//     Conses
//      True if cars are equalp and cdrs are equalp.
//     Arrays
//      True if they are same number of dimensions and all elements are equalp
//     Structures
//      True if they are same class and all slots are equalp.
//     Hash Tables
//      True if count, test and values are equalp.
//
bool equalp(Val x, Val y)
{
    if (characterp(x))
    {
        if (! characterp(y)) return false;
        return char_upcase(x) == char_upcase(y);
    }

    if (stringp(x))
    {
        if (! stringp(y)) return false;
        return 0 == string_ci_cmp(x, y);
    }

    return equal(x, y);
} // equalp


// fboundp
bool fboundp(Val fname)
{
    if (symbolp(fname))
    {
        return functionp(fname->Decode<Symbol>()->m_function);
    }
    else if (consp(fname) &&
             Qsetf == car(fname) &&
             consp(cdr(fname)) &&
             symbolp(cadr(fname)) )
    {
        Val cell = find_setf_cell(cadr(fname));
        if (nil == cell)
        {
            return false;
        }
        return functionp(setf_cell_function(cell));
    }

    error(make_type_error(fname, Qfunction_name));
} // fboundp

// fdefinition
Val fdefinition(Val fname)
{
    Val fun = nil;

    if (symbolp(fname))
    {
        fun = symbol_function(fname);
    }
    else if (function_name_p(fname))
    {
        Val cell = find_setf_cell(cadr(fname));
        if (nil != cell) fun = setf_cell_function(cell);
    }

    if (! functionp(fun))
    {
        error(make_undefined_function(fname));
    }

    return fun;
} // fdefintion

// multiple_value_list
Val multiple_value_list(Thread* p)
{
    Val args = nil;
    {
        Int iNth = Fixnum::Decode_(p->m_n);
        while (iNth >= 1)
        {
            iNth -= 1;
            args = cons(p->mv_value[iNth], args);
        } // while
    } // args

    return args;
} // multiple_value_list

// values
Val values()
{
    return  MiniThread::Get()->SetValues();
} // values

// values
Val values(Val a)
  { return  MiniThread::Get()->SetValues(a); }

// values
Val values(Val a, Val b)
  { return  MiniThread::Get()->SetValues(a, b); }

// values
Val values(Val a, Val b, Val c)
  { return  MiniThread::Get()->SetValues(a, b, c); }

// values
Val values(Val a, Val b, Val c, Val d)
  { return  MiniThread::Get()->SetValues(a, b, c, d); }

// values
Val values(Val a, Val b, Val c, Val d, Val e)
  { return  MiniThread::Get()->SetValues(a, b, c, d, e); }

// values
Val values(Val a, Val b, Val c, Val d, Val e, Val f)
  { return  MiniThread::Get()->SetValues(a, b, c, d, e, f); }

// values
Val values(Val a, Val b, Val c, Val d, Val e, Val f, Val g)
  { return  MiniThread::Get()->SetValues(a, b, c, d, e, f, g); }

// values
Val values(Val a, Val b, Val c, Val d, Val e, Val f, Val g, Val h)
  { return  MiniThread::Get()->SetValues(a, b, c, d, e, f, g, h); }

// values
Val values(Val a, Val b, Val c, Val d, Val e, Val f, Val g, Val h, Val i)
  { return  MiniThread::Get()->SetValues(a, b, c, d, e, f, g, h, i); }

Val values(Val a, Val b, Val c, Val d, Val e, Val f, Val g, Val h, Val i,
    Val j )
  { return  MiniThread::Get()->SetValues(a, b, c, d, e, f, g, h, i, j); }

} // CommonLisp
