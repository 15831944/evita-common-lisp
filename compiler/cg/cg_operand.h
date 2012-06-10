//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - cg - operand
// cg/cg_operand.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cg/cg_operand.h#3 $
//
#if !defined(INCLUDE_compiler_cg_operand_h)
#define INCLUDE_compiler_cg_operand_h

#include "../ir/ir_operand.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// Memory Cell
//
class MemoryCell : public Operand
{
    protected: int m_iLoc;
        public: int GetLocation() const { return m_iLoc; }

    public: MemoryCell(Kind eKind, int iLoc) :
        Operand(eKind), m_iLoc(iLoc) {}
}; // MemoryCell


//////////////////////////////////////////////////////////////////////
//
// Pseudo Register
//
class Pseudo : public Register
{
    public: Pseudo() :
            Register()
        { SetStorage(Storage_Pseudo); }
}; // Pseudo


//////////////////////////////////////////////////////////////////////
//
// Physical Register
//
class Physical : public Register
{
    public: Physical(int iLoc, Variable* pVar = NULL) :
            Register(pVar, Class_GPR)
        { SetStorage(Storage_Physical, iLoc); }

    public: Physical(Class eClass, int iLoc, Variable* pVar = NULL) :
            Register(pVar, eClass)
        { SetStorage(Storage_Physical, iLoc); }
}; // Physical


//////////////////////////////////////////////////////////////////////
//
// TlvName
//
class TlvName : public Operand
{
    public: static Kind GetKind_() { return Kind_TlvName; }
    public: virtual void HtmlPrint(Val, bool) const;

    public: virtual bool Equal(const Operand* pSx) const
    {
        if (this == pSx) return true;

        if (! pSx->Is<TlvName>())
        {
            return false;
        }

        return pSx->StaticCast<TlvName>()->GetDatum() == m_datum;
    } // Match

    public: virtual Ty GetTy() const
        { return ty_fixnum; }

    protected: Val m_datum;

    public: TlvName(Val datum) :
        Operand(Kind_TlvName), m_datum(datum) {}

    public: Val GetDatum() const { return m_datum; }
    public: Val SetDatum(Val datum) { return m_datum = datum; }
}; // TlvName

} // Compiler

#endif // !defined(INCLUDE_compiler_cg_operand_h)
