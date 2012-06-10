//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - cg - operand
// cg/x86x64_cg_operand.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86x64/compiler/x86x64_cg_operand.h#3 $
//
#if !defined(INCLUDE_compiler_cg_x86x64_operand_h)
#define INCLUDE_compiler_cg_x86x64_operand_h

#include "./x86x64_cg_defs.h"
#include "../../../compiler/cg/cg_operand.h"


#if MACH == MACH_x86

    #define x86x64Operand       x86Operand
    #define x86x64Flags        x86EFlags
    #define x86x64Reloc         x86Reloc
    #define x86x64ThreadData    x86ThreadData
    #define x86x64UpVar         x86UpVar

#elif MACH == MACH_x64

    #define x86x64Operand       x64Operand
    #define x86x64Flags        x64EFlags
    #define x86x64Reloc         x64Reloc
    #define x86x64ThreadData    x64ThreadData
    #define x86x64UpVar         x64UpVar

#endif // MACH == MACH_x86

namespace Compiler
{

// For providing Kind_xxx enumeration.
class x86x64Operand : public Operand
{
    public: enum Kind
    {
        Kind_EFlags     = Kind_TargetOutput1,
        Kind_Reloc      = Kind_TargetInput1,
        Kind_ThreadData = Kind_TargetInput2,
        Kind_UpVar      = Kind_TargetInput3,
    }; // Kind
}; // x86x64Operand


//////////////////////////////////////////////////////////////////////
//
//  x86x64Flags
//
//  Represents x86x64 EFLAGS register. This output is set by x86x64-CMP and
//  used by x86x64-SETCC.
//
class x86x64Flags : public Output
{
    public: static Kind GetKind_() { return Kind_TargetOutput1; }
    public: virtual void HtmlPrint(Val, bool) const;

    public: x86x64Flags();
}; // x86x64Flags


#if 0
//////////////////////////////////////////////////////////////////////
//
// x86x64Reloc Data
//
class x86x64Reloc : public Operand
{
    public: static Kind GetKind_() { return Kind_TargetInput1; }
    public: virtual void HtmlPrint(Val, bool) const;

    public: virtual Ty GetTy() const { return ty_ptr_t; }

    protected: AnnonType m_eType;
        public: AnnonType GetType() const { return m_eType; }

    protected: Literal* m_pDatum;
        public: Literal* GetDatum() const { return m_pDatum; }

    public: x86x64Reloc(AnnonType eType, Literal* pDatum) :
        Operand(GetKind_()),
        m_eType(eType),
        m_pDatum(pDatum) {}
}; // x86x64Reloc
#endif


#if 0
//////////////////////////////////////////////////////////////////////
//
// x86x64Thread Data
//
class x86x64ThreadData : public MemoryCell
{
    public: static Kind GetKind_() { return Kind_TargetInput2; }
    public: virtual void HtmlPrint(Val, bool) const;
    public: virtual Ty GetTy() const;

    public: x86x64ThreadData(int ofs) :
        MemoryCell(GetKind_(), ofs) {}
}; // x86x64ThreadData
#endif


//////////////////////////////////////////////////////////////////////
//
// x86x64UpVar Data
//
class x86x64UpVar : public Operand
{
    public: static Kind GetKind_() { return Kind_TargetInput3; }
    public: virtual void HtmlPrint(Val, bool) const;

    protected: Int m_ofs;
        public: Int GetOffset() const  { return m_ofs; }
        public: Int SetOffset(Int ofs) { return m_ofs = ofs; }

    protected: Variable* m_pVar;
        public: Variable* GetVar() const { return m_pVar; }

    public: x86x64UpVar(Variable* pVar) :
        Operand(GetKind_()),
        m_ofs(0),
        m_pVar(pVar) {}
}; // x86x64UpVar

} // Compiler

#endif // !defined(INCLUDE_compiler_cg_x86x64_operand_h)
