#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - compiler - cg - x64 expand TRAP-IF
// cg/x64/x64_cg_expand_trap.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x64/compiler/x64_cg_expand_trap.cpp#4 $
//
//
// BUGBUG: We should expand OPENxxx and CLOSE in selector instead of
// assembler. Post RA is another place for expanding OPENxxx and CLOSE.
//
#include "./x64_cg_defs.h"

#include "./x64_cg_instruction.h"

#include "../kernel/x64_ke_thread.h"

#include "../../x86x64/compiler/x86x64_cg_expand_trap.h"

namespace Compiler
{
using namespace X64;

namespace
{

class X64ExpandTrapPass : public X86X64ExpandTrapPass
{
    public: X64ExpandTrapPass() : X86X64ExpandTrapPass(L"X64-TRAP") {}

    protected: virtual void
        insert_service(Literal*, Values*, Instruction*);

    protected: virtual void process_instruction(Instruction* pInsn)
    {
        InsnProcT pfn = k_rgpInsnProc[pInsn->GetOpcode()];
        (this->*pfn)(pInsn);
    } // process_instruction

    typedef void (X64ExpandTrapPass::*InsnProcT)(Instruction*);
    static const InsnProcT k_rgpInsnProc[IrOp_MAX_1 + 1];
}; // X64ExpandTrapPass


const X64ExpandTrapPass::InsnProcT
X64ExpandTrapPass::k_rgpInsnProc[IrOp_MAX_1 + 1] =
{
    #define DEFIROP(mp_name) &X64ExpandTrapPass::process_##mp_name,
    #include "./x64_cg_opcode.inc"
}; // k_rgpInsnProc


//////////////////////////////////////////////////////////////////////
//
// find_service
//
static int
find_service(Val name)
{
    struct ServiceEntry
    {
        Val m_name;
        int m_iService;
    }; // ServiceEntry

    static const ServiceEntry k_rgoServiceMap[] =
    {
        { Qtype_error,          SVC_type_error },
        { Qnot_function,        SVC_not_function },
        { Qunbound_variable,    SVC_unbound_variable },
        { Qundefined_function,  SVC_undefined_function },
    }; // rgoServiceMap

    for (
        const ServiceEntry* pRunner = &k_rgoServiceMap[0];
        pRunner < &k_rgoServiceMap[lengthof(k_rgoServiceMap)];
        pRunner++ )
    {
        if (pRunner->m_name == name)
        {
            return pRunner->m_iService;
        }
    } // for each entry

    return -1;
} // find_service


// X64ExpandTrapPass::insert_service
void X64ExpandTrapPass::insert_service(
    Literal*        pName,
    Values*         pArgs,
    Instruction*    pUnreachable )
{
    int iService = find_service(pName->GetDatum());

    if (-1 == iService)
    {
        ir_insert_insn(
            new CallInsn(ty_void, Obj_Void, pName, pArgs),
            pUnreachable );
    }
    else
    {
        ir_insert_insn(
            new x86x64ServiceInsn(static_cast<X64::SVC>(iService), pArgs),
            pUnreachable );
    }
} // X64ExpandTrapPass::process_TRAPIF

} // namespace

//////////////////////////////////////////////////////////////////////
//
// x64_pass_expand_trap
//
void x64_pass_expand_trap()
{
    X64ExpandTrapPass oPass;
    oPass.Run();
} // x64_pass_expand_trap

} // Compiler
