#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - big- 12 Numbers - Integer
// big/big_12_integer.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/big/big_12_integer.cpp#5 $
//
//      ash
//      gcd
//      logand
//      logior
//      logxor
//
#include "./big_lisp.h"

#include "./big_12_bignum.h"
#include "./big_12_float.h"
#include "./big_12_ratio.h"

namespace MiniLisp
{

//////////////////////////////////////////////////////////////////////
//
// ash
//
Val C_ash(Val x, Val n)
{
    if (fixnump(x))
    {
        if (fixnump(n) && Fixnum::Decode_(n) <= 0)
        {
            return Fixnum::Encode(
                Fixnum::Decode_(x) >> -Fixnum::Decode_(n) );
        }

        BignumInt oX(Fixnum::Decode_(x));
        return ash(oX, n);
    }
    else if (bignump(x))
    {
        if (fixnump(n))
        {
            Int nN = Fixnum::Decode_(n);
            if (nN > 0)
            {
                return BignumImpl::ShiftLeft(x, nN);
            }
            else if (nN < 0)
            {
                return BignumImpl::ShiftRight(x, -nN);
            }
            else
            {
                return x;
            }
        }
        else if (bignump(n))
        {
            error(L"Too large shift count: ~D", n);
        }
        else
        {
            error(make_type_error(n, Qinteger));
        }
    }
    else
    {
        error(make_type_error(x, Qinteger));
    }
} // C_ash


//////////////////////////////////////////////////////////////////////
//
// gcd
//
Val C_gcd(Val x, Val y)
{
    if (fixnump(x))
    {
        Int iX = Fixnum::Decode_(x);
        if (iX < 0)
        {
            BignumInt oX(-iX);
            return gcd(oX, y);
        }
    }
    else if (bignump(x))
    {
        if (x->Decode<BignumImpl>()->IsMinus())
        {
            x = sub(0, x);
        }
    }
    else
    {
        error(make_type_error(x, Qinteger));
    }

    if (fixnump(y))
    {
        Int iY = Fixnum::Decode_(y);
        if (iY < 0)
        {
            BignumInt oY(-iY);
            return gcd(x, oY);
        }
    }
    else if (bignump(y))
    {
        if (y->Decode<BignumImpl>()->IsMinus())
        {
            y = sub(0, y);
        }
    }
    else
    {
        error(make_type_error(y, Qinteger));
    }

    for (;;)
    {
        int rgfType = 0;
            if (fixnump(x)) rgfType |= 1;
            if (fixnump(y)) rgfType |= 2;

        switch (rgfType)
        {
        case 3: // fixnum fixnum
        {
            Int m = Fixnum::Decode_(x);
            Int n = Fixnum::Decode_(y);
            while (0 != n)
            {
                Int temp = m;
                m = n;
                n = temp % n;
            } // while
            return Fixnum::Encode(m);
        } // fixnum fixnum

        case 1: // fixnum bignum
            swap(x, y);
            // FALLTHROUGH

        case 2: // bignum fixnum
            if (Fixnum::Encode(0) == y)
            {
                return x;
            }
            // FALLTHROUGH

        case 0: // bignum bignum
        {
            Val temp = x;
            x = y;
            y = rem(temp, y);
            break;
        } // bignum bignum

        default:
            CAN_NOT_HAPPEN();
        } // switch rgfType
    } // for
} // gcd


template<class Op_>
class Arith
{
    public: static Val Run(Val x, Val y)
    {
        Int iTypes = 0;

        if (fixnump(x))
        {
            iTypes |= CLASSD_(fixnum);
        }
        else if (bignump(x))
        {
            iTypes |= CLASSD_(bignum);
        }
        else
        {
            goto type_error;
        }

        swap(x, y);
        if (fixnump(x))
        {
            iTypes |= CLASSD_(fixnum) << 16;
        }
        else if (bignump(x))
        {
            iTypes |= CLASSD_(bignum) << 16;
        }
        else
        {
            goto type_error;
        }

        switch (iTypes)
        {
        case CLASSD2_(fixnum, fixnum):
            return Fixnum::Encode(
                Op_::Compute(Fixnum::Decode_(x), Fixnum::Decode_(y)) );

        case CLASSD2_(fixnum, bignum):
        {
            BignumInt oX(Fixnum::Decode_(x));
            return Op_::Compute(oX, y);
        } // fixnum bignum

        case CLASSD2_(bignum, bignum):
            return Op_::Compute(x, y);

        case CLASSD2_(bignum, fixnum):
        {
            BignumInt oY(Fixnum::Decode_(y));
            return Op_::Compute(x, oY);
        } // fixnum bignum

        default:
            CAN_NOT_HAPPEN();
        } // switch

      type_error:
        error(make_type_error(x, Qinteger));
    } // Run
}; // Arith


#define defclass_logarith(mp_Name, mp_op) \
    class Op##mp_Name \
    { \
        public: static Int Compute(Int x, Int y) \
            { return x mp_op y; } \
        public: static Val Compute(Val x, Val y) \
            { return BignumImpl::Log##mp_Name(x, y); } \
    };


#define defun_logarith(mp_name, mp_Name, mp_op) \
    namespace { defclass_logarith(mp_Name, mp_op) } \
    Val log##mp_name##_2(Val x, Val y) \
        { return Arith<Op##mp_Name>::Run(x, y); }

defun_logarith(and, And, &)
defun_logarith(ior, Ior, |)
defun_logarith(xor, Xor, ^)

// expt10
//  For printer and reader.
Val expt10(Int iPower)
{
    ASSERT(iPower >= 0);

    Val result = Fixnum::Encode(1);
    Val baze = Fixnum::Encode(10);

    Int k = iPower;
    while (k > 0)
    {
        if (k & 1)
        {
            result = mul(result, baze);
            k -= 1;
        }
        else
        {
            baze = mul(baze, baze);
            k >>= 1;
        }
    } // while

    return result;
} // expt10

} // MiniLisp


namespace CommonLisp
{

// logbitp
//  (logbitp k n) == (ldb-test (byte 1 k) n)
//  For: array-has-fille-pointer <= vector-push-extend <= write-char
//       <= string-output-stream
bool logbitp(Val k, Val n)
{
    Int nTypes = get_type_2(k, n, CLASSD_bignum, Qinteger);
    if (minusp(k))
    {
        error(make_type_error(k, Qunsigned_byte));
    }

    switch (nTypes)
    {
    case CLASSD2_(fixnum, fixnum):
    {
        if (cmp_xx(k, Fixnum::Bits) >= 0)
            return Fixnum::Decode_(n) < 0;

        Int iVal  = Fixnum::Decode_(n);
        Int iMask = static_cast<Int>(1) << Fixnum::Decode_(k);

        return 0 != (iVal & iMask);
    } // fixnum fixnum

    case CLASSD2_(fixnum, bignum):
        return BignumImpl::Logbitp(Fixnum::Decode_(k), n);

    case CLASSD2_(bignum, fixnum):
    case CLASSD2_(bignum, bignum):
        return minusp(n);

    default:
        CAN_NOT_HAPPEN();
    } // switch nTypes
} // logbitp


// lognot
//  For IntvEdge::operator ~.
Val lognot(Val n)
{
    return logxor_2(n, Fixnum::Encode(-1));
} // lognot

} // CommonLisp
