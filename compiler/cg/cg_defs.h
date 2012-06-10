//////////////////////////////////////////////////////////////////////////////
//
// evcl - compiler - Code Generator
// cg_defs.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cg/cg_defs.h#5 $
//
#if !defined(INCLUDE_compiler_cg_defs_h)
#define INCLUDE_compiler_cg_defs_h

#include "../cm/cm_base.h"

#include "../ir/ir_defs.h"
#include "../ir/ir_fns.h"

//#include "../../kernel/ke_mach.h"

namespace Compiler
{

//typedef Kernel::RegSet RegSet;

////////////////////////////////////////////////////////////
//
// CopyTask
//  For parallel copy
//
class CopyTask :
    public Atom,
    public WorkListItem_<CopyTask>
{
    public: Register* m_pRd;
    public: Register* m_pRx;
    public: Ty  m_ty;
    public: CopyTask(Register* pRd, Register* pRx) :
        m_pRd(pRd), m_pRx(pRx), m_ty(ty_t) {}

    public: CopyTask(Ty ty, Register* pRd, Register* pRx) :
        m_pRd(pRd), m_pRx(pRx), m_ty(ty) {}
}; // CopyTask


//////////////////////////////////////////////////////////////////////
//
// Utility Functions
//
BBlock* get_next_bblock(BBlock*);
bool is_useless_JUMP(Instruction*);

} // Compiler

#endif //!defined(INCLUDE_compiler_cg_defs_h)
