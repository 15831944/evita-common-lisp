//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - ir - Types - 15 Arrays
// compiler/ty/ty_15_array.h
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/ty/ty_15_array.h#8 $
//
#if !defined(INCLUDE_compiler_ty_15_array_h)
#define INCLUDE_compiler_ty_15_array_h

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// TyArray
//
class TyArray
{
    Ty  m_elty;
    Val m_dims;
    Val m_rank;
    Val m_kind;

    public: TyArray(Val);
}; // tyArra


} // Compiler

#endif //!defined(INCLUDE_compiler_ty_15_array_h)
