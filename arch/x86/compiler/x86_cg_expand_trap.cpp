#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - compiler - cg - x86 expand TRAP-IF
// cg/x86/x86_cg_expand_trap.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x86/compiler/x86_cg_expand_trap.cpp#5 $
//
//
// BUGBUG: We should expand OPENxxx and CLOSE in selector instead of
// assembler. Post RA is another place for expanding OPENxxx and CLOSE.
//
#include "./x86_cg_defs.h"

#include "./x86_cg_instruction.h"

#include "../kernel/x86_ke_thread.h"

#include "../../x86x64/compiler/x86x64_cg_expand_trap.h"


namespace Compiler
{
using namespace X86;

namespace
{

class X86ExpandTrapPass : public X86X64ExpandTrapPass
{
    public: X86ExpandTrapPass() : X86X64ExpandTrapPass(L"X86-TRAP") {}

    virtual void insert_service(Literal*, Values*, Instruction*);
}; // X86ExpandTrapPass


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


// X86ExpandTrapPass::insert_service
void X86ExpandTrapPass::insert_service(
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
            new x86x64ServiceInsn(static_cast<X86::SVC>(iService), pArgs),
            pUnreachable );
    }
} // X86ExpandTrapPass::process_TRAPIF

} // namespace

//////////////////////////////////////////////////////////////////////
//
// x86_pass_expand_trap
//
void x86_pass_expand_trap()
{
    X86ExpandTrapPass oPass;
    oPass.Run();
} // x86_pass_expand_trap

} // Compiler
