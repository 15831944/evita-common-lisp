#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - compiler - cg - assembler
// cg/cg_operand.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cg/cg_operand.cpp#4 $
//
#include "./cg_operand.h"

#include "../cm/cm_fns.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
//  TlvName::HtmlPrint
//
void TlvName::HtmlPrint(Val stream, bool) const
{
    cm_format(stream, L"{TLV ~S}",
        m_datum->Decode<TlvRecord>()->m_name );
} // TlvName::HtmlPrint

} // Compiler
