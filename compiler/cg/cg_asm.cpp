#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - compiler - cg - assembler
// compiler/cg/cg_asm.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cg/cg_asm.cpp#8 $
//
#include "./cg_asm.h"

#include "../cm/cm_module.h"
#include "../cm/cm_target.h"

#include "./cg_instruction.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// Annotate
//
void CgBaseAsmPass::annotate(int eType, Operand* pOperand)
{
    m_oContext.GetFunction()->GetExtension<AsmFunction>()->Append_(
        new AsmAnnon(eType, pOperand, m_oContext.GetAddress()) );
} // CgBaseAsmPass::annotate


//////////////////////////////////////////////////////////////////////
//
// CgBaseAsmPass::Context constuctor
//
CgBaseAsmPass::Context::Context() :
    m_cbBuffer(Default_BufSize),
    m_pFunction(NULL)
{
    m_prgbBuffer = new uint8[Default_BufSize];
    Reset();
} // Phase


//////////////////////////////////////////////////////////////////////
//
// CgBaseAsmPass::Context::Emit32
//
void CgBaseAsmPass::Context::Advance(uint cbAddr, uint cbOfs)
{
    if (m_pbBuffer + cbOfs >= m_prgbBuffer + m_cbBuffer)
    {
        enlarge_buffer();
    }

    m_nAddress += cbAddr;
    m_pbBuffer += cbOfs;
} // CgBaseAsmPass::Cotext::Advance


//////////////////////////////////////////////////////////////////////
//
// CgBaseAsmPass::Context::EmitU16
//
void CgBaseAsmPass::Context::EmitU16(uint16 nDatum)
{
    if (m_pbBuffer + 2 >= m_prgbBuffer + m_cbBuffer)
    {
        enlarge_buffer();
    }

    *m_pbBuffer++ = static_cast<uint8>(nDatum >>  0);
    *m_pbBuffer++ = static_cast<uint8>(nDatum >>  8);
    m_nAddress += 2;
} // CgBaseAsmPass::Cotext::EmitU16


//////////////////////////////////////////////////////////////////////
//
// CgBaseAsmPass::Context::EmitU32
//
void CgBaseAsmPass::Context::EmitU32(uint32 nDatum)
{
    if (m_pbBuffer + 4 >= m_prgbBuffer + m_cbBuffer)
    {
        enlarge_buffer();
    }

    *m_pbBuffer++ = static_cast<uint8>(nDatum >>  0);
    *m_pbBuffer++ = static_cast<uint8>(nDatum >>  8);
    *m_pbBuffer++ = static_cast<uint8>(nDatum >> 16);
    *m_pbBuffer++ = static_cast<uint8>(nDatum >> 24);
    m_nAddress += 4;
} // CgBaseAsmPass::Cotext::EmitU32


//////////////////////////////////////////////////////////////////////
//
// CgBaseAsmPass::Context::EmitU64
//
void CgBaseAsmPass::Context::EmitU64(uint64 nDatum)
{
    if (m_pbBuffer + 8 >= m_prgbBuffer + m_cbBuffer)
    {
        enlarge_buffer();
    }

    *m_pbBuffer++ = static_cast<uint8>(nDatum >>  0);
    *m_pbBuffer++ = static_cast<uint8>(nDatum >>  8);
    *m_pbBuffer++ = static_cast<uint8>(nDatum >> 16);
    *m_pbBuffer++ = static_cast<uint8>(nDatum >> 24);
    *m_pbBuffer++ = static_cast<uint8>(nDatum >> 32);
    *m_pbBuffer++ = static_cast<uint8>(nDatum >> 40);
    *m_pbBuffer++ = static_cast<uint8>(nDatum >> 48);
    *m_pbBuffer++ = static_cast<uint8>(nDatum >> 56);
    m_nAddress += 8;
} // CgBaseAsmPass::Cotext::EmitU64


//////////////////////////////////////////////////////////////////////
//
// CgBaseAsmPass::Context::EmitU8
//
void CgBaseAsmPass::Context::EmitU8(uint8 nDatum)
{
    if (m_pbBuffer + 1 >= m_prgbBuffer + m_cbBuffer)
    {
        enlarge_buffer();
    }

    *m_pbBuffer++ = nDatum;
    m_nAddress += 1;
} // CgBaseAsmPass::Context::EmitU8


//////////////////////////////////////////////////////////////////////
//
// CgBaseAsmPass::Context::PatchU32
//
void CgBaseAsmPass::Context::PatchU32(UINT ofs, uint32 nDatum)
{
    m_prgbBuffer[ofs + 0] = static_cast<uint8>(nDatum >>  0);
    m_prgbBuffer[ofs + 1] = static_cast<uint8>(nDatum >>  8);
    m_prgbBuffer[ofs + 2] = static_cast<uint8>(nDatum >> 16);
    m_prgbBuffer[ofs + 3] = static_cast<uint8>(nDatum >> 24);
} // CgBaseAsmPass::Context::PatchU32


//////////////////////////////////////////////////////////////////////
//
// CgBaseAsmPass::Context::PatchU64
//
void CgBaseAsmPass::Context::PatchU64(UINT ofs, uint64 nDatum)
{
    m_prgbBuffer[ofs + 0] = static_cast<uint8>(nDatum >>  0);
    m_prgbBuffer[ofs + 1] = static_cast<uint8>(nDatum >>  8);
    m_prgbBuffer[ofs + 2] = static_cast<uint8>(nDatum >> 16);
    m_prgbBuffer[ofs + 3] = static_cast<uint8>(nDatum >> 24);
    m_prgbBuffer[ofs + 4] = static_cast<uint8>(nDatum >> 32);
    m_prgbBuffer[ofs + 5] = static_cast<uint8>(nDatum >> 40);
    m_prgbBuffer[ofs + 6] = static_cast<uint8>(nDatum >> 48);
    m_prgbBuffer[ofs + 7] = static_cast<uint8>(nDatum >> 56);
} // CgBaseAsmPass::Context::PatchU64


//////////////////////////////////////////////////////////////////////
//
// CgBaseAsmPass::Context::PatchU8
//
void CgBaseAsmPass::Context::PatchU8(UINT ofs, uint8 nDatum)
{
    m_prgbBuffer[ofs + 0] = nDatum;
} // CgBaseAsmPass::Context::PatchU8


// enlarge_buffer
void CgBaseAsmPass::Context::enlarge_buffer()
{
    uint8* prgbBuffer = new uint8[m_cbBuffer + Default_Extent];
    ::memcpy(prgbBuffer, m_prgbBuffer, m_cbBuffer);
    m_pbBuffer = prgbBuffer + (m_pbBuffer - m_prgbBuffer);
    m_prgbBuffer = prgbBuffer;
    m_cbBuffer += Default_Extent;
} // CgBaseAsmPass::Context::enlarge_buffer


//////////////////////////////////////////////////////////////////////
//
// Process BBlock
//
void
CgBaseAsmPass::process_bblock(BBlock* pBBlock)
{
    ASSERT(NULL != pBBlock);

    pBBlock->SetExtension(new AsmBBlock(getAddress(), getOffset()));

    foreach (BBlock::EnumInsn, oEnum, pBBlock)
    {
        m_pInsn = oEnum.Get();
        process_instruction(m_pInsn);
    } // for each instruction

    pBBlock->GetExtension<AsmBBlock>()->SetEndOffset(getOffset());
} // CgBaseAsmPass::process_bblock


//////////////////////////////////////////////////////////////////////
//
// Process Function
//
void
CgBaseAsmPass::process_function(Function* pFun)
{
    ASSERT(NULL != pFun);

    html_log_format(2, L"<h2>Process ~S</h2>~%", pFun);

    m_oContext.SetFunction(pFun);

    foreach (Function::EnumBBlock, oEnum, pFun)
    {
        process_bblock(oEnum.Get());
    } // for each bblock

    pFun->SetFunObj(make_function(pFun));
} // CgBaseAsmPass::process_function


//////////////////////////////////////////////////////////////////////
//
// CgBaseAsmPass::process_module
//
void CgBaseAsmPass::process_module(Module* pModule)
{
    m_pModule = pModule;

    foreach (Module::EnumFunction, oEnum, pModule)
    {
        process_function(oEnum.Get());
    } // for each function

    foreach (Module::EnumFunction, oEnum, pModule)
    {
        realize_function(oEnum.Get());
    } // for each function
} // CgBaseAsmPass::process_module


// compute_path_size
uint CgBaseAsmPass::compute_path_size(Function* pCallee, Function* pOwner)
{
    uint ofs = 0;
    for (;;)
    {
        Function::EnumCaller oEnum(pCallee);
        Function* pCaller = oEnum.GetNode();

        ofs += pCallee->GetFrameSize();

        if (pCaller == pOwner || pCaller->IsClosure())
        {
            break;
        }

        pCallee = pCaller;
    } // for

    return ofs;
} // CgBaseAsmPass::compute_path_size


// CgBaseAsmPass::get_tag
int CgBaseAsmPass::get_tag(Val klass)
{
    if (CLASS_instance == klass)
        return Instance::Tag;

    if (CLASS_funcallable_instance == klass)
        return FuncallableInstance::Tag;

    if (CLASS_native_code_object == klass)
        return NativeCodeFunction::Tag;

    return Record::Tag;
} // CgBaseAsmPass::get_tag


//////////////////////////////////////////////////////////////////////
//
// get_upvar_holder_aux
//  Returns holder of upvar pVar. pVar isn't accessed by VecRef.
//
static Function* get_upvar_holder_aux(Function* pCallee, Variable* pVar)
{
    if (pCallee->GetFlag())
    {
        return NULL;
    }

    pCallee->SetFlag(1);

    Function* pOwner = pVar->GetOwner();

    foreach (Function::EnumCaller, oEnum, pCallee)
    {
        Function* pCaller = oEnum.GetNode();
        if (pCaller == pOwner || pCaller->IsClosure())
        {
            return pCaller;
        }

        Function* pHolder = get_upvar_holder_aux(pCaller, pVar);
        if (NULL != pHolder)
        {
            return pHolder;
        }
    } // for each caller

    return NULL;
} // get_upvar_holder_aux


//////////////////////////////////////////////////////////////////////
//
// get_upvar_holder
//  Returns holder of upvar pVar. pVar isn't accessed by VecRef.
//
Function*
CgBaseAsmPass::get_upvar_holder(Function* pCallee, Variable* pVar)
{
    Function* pHolder = get_upvar_holder_aux(pCallee, pVar);
        ASSERT(NULL != pHolder);

    // Reset visited flag
    foreach (Module::EnumFunction, oEnum, m_pModule)
    {
        oEnum.Get()->SetFlag(0);
    } // for each fun

    return pHolder;
} // CgBaseAsmPass::get_upvar_holder


//////////////////////////////////////////////////////////////////////
//
// X86CgAsmPass::get_upvar_offset
//
// Description:
//  Returns offset of upvar in base. If base is ESP, we compute size
//  of callee to variable home.
//
int CgBaseAsmPass::get_upvar_offset(Register* pBase, Register* pQd)
{
    ASSERT(pQd->IsPseudo());
    ASSERT(pQd->GetDfn()->Is<UpVarDefInsn>());

    Variable* pVar    = pQd->GetDfn()->GetSx()->StaticCast<Variable>();
    Function* pCallee = pQd->GetDfn()->GetBBlock()->GetFunction();
    Function* pHolder = get_upvar_holder(pCallee, pVar);

    int ofs;
    {
        Register* pRd = NULL;

        if (pVar->GetOwner() == pHolder)
        {
            VarDefInsn* pVarDef = pVar->GetDfn()->
                StaticCast<VarDefInsn>();

            pRd = pVarDef->GetRd();
        }
        else
        {
            UpVarDefInsn* pUpVarDef = get_UPVARDEF(pHolder, pVar);
            foreach (Register::EnumUseSite, oEnum, pUpVarDef->GetRd())
            {
                Instruction* pInsn = oEnum.Get()->GetInstruction();
                if (pInsn->Is<StackDefInsn>())
                {
                    pRd = pInsn->GetRd();
                    break;
                }
            } // for each use site of UPVARDEF
        } // if

        if (NULL == pRd)
        {
            html_log_format(1, L"<h1>Failed: ~S, holder=~S</h1>",
                pQd->GetDfn(), pHolder );

            warn(L"Can't locate upvar ~S.", pVar->GetName());
            return 0;
        }

        unless (pRd->IsStackSlot())
        {
            html_log_format(1, L"<h1>Failed: ~S, holder=~S</h1>",
                pQd->GetDfn(), pHolder );

            warn(L"Invalid upvar ~S.", pVar->GetName());
            return 0;
        }

        ofs = static_cast<int>(pRd->GetLocation());
    } // ofs

    // Callee can reach holder in fixed path.
    if (static_cast<uint>(pBase->GetLocation()) == cm_get_target()->GetSP())
    {
        ofs += compute_path_size(pCallee, pHolder);
    } // if

    html_log_format(4, L"get_upvar_offset: ~S ~S: ~S ~D~:%",
        pBase, pQd, pVar, ofs );

    return ofs;
} // CgBaseAsmPass::get_upvar_offset


// CgBaseAsmPass::get_UPVARDEF
UpVarDefInsn* CgBaseAsmPass::get_UPVARDEF(Function* pHolder, Variable* pVar)
{
    foreach (Function::EnumUpVarSite, oEnum, pHolder)
    {
        OperandBox* pBox = oEnum.Get();
        if (pBox->GetOperand() == pVar)
        {
            return pBox->GetInstruction()->StaticCast<UpVarDefInsn>();
        }
    } // for each upvar
    CAN_NOT_HAPPEN();
} // CgBaseAsmPass::get_UPVARDE

} // Compiler
