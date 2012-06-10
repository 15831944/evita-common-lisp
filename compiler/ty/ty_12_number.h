//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - ir - Types - 12 Numbers
// compiler/ty/ty_12_number.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/ty/ty_12_number.h#10 $
//
#if !defined(INCLUDE_compiler_ty_12_number_h)
#define INCLUDE_compiler_ty_12_number_h

#include "../../big/big_lisp.h"
#include "../util/interval.h"

namespace Compiler
{

using namespace Utility;

//////////////////////////////////////////////////////////////////////
//
// TyInteger
//      bit
//      fixnum
//      integer
//      signed-byte
//      unsigned-byte
//      (eql x)
//      (mod n) = (integer 0 (1- n))
//      (integer lower upper)
//      (signed-byte n) = (integer (1+ (ash -1 (1- n))) (1- (ash 1 (1- n))))
//      (unsigned-byte n) (integer 0 (1- (ash 1 n)))
//
//  Method:
//      Lt
//      IsSubtype
//      IsTypeOf
//      Parse
//      Union
//      Unparse
//
class TyInteger : public Interval
{
    public: static TyInteger Fixnum;
    public: static TyInteger Nil;
    public: static TyInteger SignedByte32;

    public: TyInteger() {}
    public: TyInteger(Interval iv) : Interval(iv) {}
    public: TyInteger(Val, Val);
    public: TyInteger(Int l, Int u) : Interval(l, u) {}
    public: TyInteger(IntvEdge l, IntvEdge u) : Interval(l, u) {}
    public: TyInteger(IntvEdge l, Val u) : Interval(l, u) {}
    public: TyInteger(Val l, IntvEdge u) : Interval(l, u) {}
    public: TyInteger(Val lu) : Interval(lu, lu) {}

    public: static TyInteger Parse(Ty);

    public: bool IsValid() const { return ! IsEmpty(); }
    public: Ty Unparse() const;
}; // TyInteger

TyInteger operator &&(const TyInteger& x, const TyInteger& y);
TyInteger operator ||(const TyInteger& x, const TyInteger& y);

bool IsSubtype(const TyInteger&, const TyInteger&);
bool IsSubtype(Ty, const TyInteger&);
bool IsSubtype(const TyInteger&, Ty);
bool IsTypeOf(Ty, const TyInteger&);

TyInteger GreaterThan(const TyInteger&, const TyInteger&);
TyInteger GreaterThanOrEqual(const TyInteger&, const TyInteger&);
TyInteger LessThan(const TyInteger&, const TyInteger&);
TyInteger LessThanOrEqual(const TyInteger&, const TyInteger&);

} // Compiler

#endif //!defined(INCLUDE_compiler_ty_12_number_h)
