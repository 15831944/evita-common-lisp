#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - compiler - IR - bblock
// ir_bblock.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/ir/ir_pass.cpp#3 $
//
#include "./ir_defs.h"
#include "./ir_pass.h"

#include "../cm/cm_module.h"
#include "../cm/cm_session.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// IrPass constructor
//
IrPass::IrPass(LPCWSTR pwszName) :
    Pass(pwszName) {}


//////////////////////////////////////////////////////////////////////
//
// IrPass destructor
//
IrPass::~IrPass()
{
    foreach (Module::EnumFunction, oEnum, Session::Get()->GetModule())
    {
        Function* pFun = oEnum.Get();
        ASSERT(NULL == pFun->GetExtension<void*>());
        ASSERT(0 == pFun->GetFlag());

        foreach (Function::EnumBBlock, oEnum, pFun)
        {
            BBlock* pBBlock = oEnum.Get();
            ASSERT(NULL == pFun->GetExtension<void>());
            ASSERT(0 == pBBlock->GetFlag());
            ASSERT(! pBBlock->IsPushed());
        } // for each bblock
    } // for each fun
} // IrPass::~IrPass


//////////////////////////////////////////////////////////////////////
//
// uninit
//
void IrPass::uninit_function(Function* pFun)
{
    pFun->SetExtension<void*>(NULL);

    foreach (Function::EnumVar, oEnum, pFun)
    {
        Variable* pVar = oEnum.Get();
        pVar->Reset();
    } // for each var

    foreach (Function::EnumBBlock, oEnum, pFun)
    {
        BBlock* pBBlock = oEnum.Get();
        pBBlock->Reset();

        foreach (BBlock::EnumInsn, oEnum, pBBlock)
        {
            Instruction* pInsn = oEnum.Get();
            pInsn->Reset();

            {
                Output* pSd = pInsn->GetOutput();
                if (NULL != pSd)
                {
                    pSd->Reset();
                }
            }
        } // for each instruction
    } // for each bblock
} // IrPass::uninit_function


//////////////////////////////////////////////////////////////////////
//
// FunctionPass constructor
//
FunctionPass::FunctionPass(LPCWSTR pwsz) :
    IrPass(pwsz) {}

//////////////////////////////////////////////////////////////////////
//
// FunctionPass::Run
//
void FunctionPass::Run()
{
    foreach (Module::EnumFunction, oEnum, Session::Get()->GetModule())
    {
        m_nLogChapter += 1;
        m_nLogSection = 0;
        process_function(oEnum.Get());
        uninit_function(oEnum.Get());
    } // for each function
} // FunctionPass::FunctionPass


//////////////////////////////////////////////////////////////////////
//
// ModulePass constructor
//
ModulePass::ModulePass(LPCWSTR pwsz) :
    IrPass(pwsz) {}

//////////////////////////////////////////////////////////////////////
//
// ModulePass::Run
//
void ModulePass::Run()
{
    process_module(Session::Get()->GetModule());

    foreach (Module::EnumFunction, oEnum, Session::Get()->GetModule())
    {
        uninit_function(oEnum.Get());
    } // for each function
} // ModulePass::ModulePass



} // Compiler
