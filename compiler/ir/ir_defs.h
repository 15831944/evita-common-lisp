//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - compiler - IR - Data Flow Analysis
// ir/ir_defs.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/ir/ir_defs.h#2 $
//
#if !defined(INCLUDE_compiler_ir_defs_h)
#define INCLUDE_compiler_ir_defs_h

namespace Compiler
{
    class DataFlowBB;

    class PhiInsn;
    class VarDefInsn;

    struct Arity
    {
        UINT    m_nMin;
        UINT    m_nMax;
        bool    m_fRest;

        UINT GetMax() const { return m_nMax; }
        UINT GetMin() const { return m_nMin; }

        bool IsExtra(UINT nNth) const
        {
            return nNth >= m_nMax && ! m_fRest;
        } // IsExtra

        bool IsFixed() const
            { return ! m_fRest && m_nMin == m_nMax; }

        bool IsSingle() const
            { return IsFixed() && 1 == m_nMin; }
    }; // Arity

} // Compiler

#include "./ir_fns.h"
#include "./ir_type.h"

#endif // !defined(INCLUDE_compiler_ir_defs_h)
