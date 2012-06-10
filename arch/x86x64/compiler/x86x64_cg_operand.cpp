#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - cg - x86x64 - operand
// cg/x86x64/x86x64_cg_operand.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86x64/compiler/x86x64_cg_operand.cpp#4 $
//
#include "./x86x64_cg_operand.h"

#include "../../../compiler/cm/cm_session.h"


namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// x86x64Flags constructor
//
x86x64Flags::x86x64Flags() :
    Output(GetKind_())
{
    m_nName = Session::NewName();
} // x86x64Flags::x86x64Flags


//////////////////////////////////////////////////////////////////////
//
//  x86x64Flags::HtmlPrint
//
void x86x64Flags::HtmlPrint(Val stream, bool fDef) const
{
    HtmlA(stream, fDef);
    format(stream, L"%ef~D", GetName());
    write_string(L"</a>", stream);
} // x86x64Flags::HtmlPrint


#if 0
//////////////////////////////////////////////////////////////////////
//
//  x86x64Reloc::HtmlPrint
//
void x86x64Reloc::HtmlPrint(Val stream, bool) const
{
    switch (m_eType)
    {
    case Annon_ClosedLit:
        format(stream, L"(Reloc clit ~S)", m_pDatum->GetDatum());
        break;

    case Annon_ClosedVar:
        format(stream, L"(Reloc cvar ~S)", m_pDatum->GetDatum());
        break;

    case Annon_SymVal:
        format(stream, L"(Reloc symval ~S)", m_pDatum->GetDatum());
        break;

    case Annon_SymFun:
        format(stream, L"(Reloc symfun ~S)", m_pDatum->GetDatum());
        break;

    case Annon_SymSetf:
        format(stream, L"(Reloc symsetf ~S)", m_pDatum->GetDatum());
        break;

    case Annon_TlvOffset:
        format(stream, L"(Reloc tlvofs ~S)", m_pDatum->GetDatum());
        break;

    default:
        CAN_NOT_HAPPEN();
    } // m_eType
} // x86x64Reloc::HtmlPrint


// x86x64ThreadData::GetTy
Ty x86x64ThreadData::GetTy() const
{
    switch (GetLocation())
    {
    case offsetof(Thread, m_n):
        return ty_ptr_fixnum;

    case offsetof(Thread, m_fn):
        return ty_ptr_function;

    default:
        return ty_ptr_t;
    } // switch location
} // x86x64ThreadData::GetTy


//////////////////////////////////////////////////////////////////////
//
//  x86x64ThreadData::HtmlPrint
//
void x86x64ThreadData::HtmlPrint(Val stream, bool) const
{
    format(stream, L"(TCB ~D)", Fixnum::Encode(m_iLoc));
} // x86x64ThreadData::HtmlPrint
#endif


//////////////////////////////////////////////////////////////////////
//
//  x86x64UpVar::HtmlPrint
//
void x86x64UpVar::HtmlPrint(Val stream, bool) const
{
    html_format(stream, L"(UpVar ~W ~D)",
        m_pVar->GetName(),
        Fixnum::Encode(m_ofs) );
} // x86x64UpVar::HtmlPrint

} // Compiler
