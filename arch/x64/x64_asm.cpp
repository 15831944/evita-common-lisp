#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - boot - x64 Assembler
// arch/x64/x64_asm.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x64/x64_asm.cpp#4 $
//
// Description:
//  This file contains builder for x64 specific objects, such as native
//  code function.
//
#include "./x64_asm.h"
#include "./kernel/x64_ke_thread.h"

namespace MiniLisp
{
    Val allocate_funobj(Val, uint, uint, uint, FunObj::FrameType, uint);
} // MiniLisp

namespace X64
{

//////////////////////////////////////////////////////////////////////
//
// X64Assembler::asm_Jb
//
void X64Assembler::asm_Jb(uint nOpcode, Label& rLabel)
{
    if (op_JMP_Jb == nOpcode)
    {
        m_ofsCode += 5;
    }
    else
    {
        m_ofsCode += 6;
    }

    m_nAddr += 2;

    // Is this long form?
    if (static_cast<uint>(-1) != rLabel.m_nAddr)
    {
        int iRel = rLabel.m_nAddr - m_nAddr;
        if (iRel < -128 || iRel > 127)
        {
            if (op_JMP_Jb == nOpcode)
            {
                m_nAddr += 3;
                nOpcode = op_JMP_Jv;
            }
            else
            {
                m_nAddr += 4;
                nOpcode = op_Jcc_Jv - op_Jcc_Jb + nOpcode;
            }
        }
    }

    m_rgoJump[m_cJumps].m_pLabel  = &rLabel;
    m_rgoJump[m_cJumps].m_nAddr   = m_nAddr;
    m_rgoJump[m_cJumps].m_ofsCode = m_ofsCode;
    m_rgoJump[m_cJumps].m_nOpcode = nOpcode;
    m_cJumps += 1;
} // X64Assembler::asm_Jb


//////////////////////////////////////////////////////////////////////
//
//  X64Assembler::installFunObj
//
Val
X64Assembler::installFunObj(const Defun* pDefun)
{
    Val funobj = makeFunObj(pDefun);
    return setf_fdefinition(funobj, pDefun->m_name);
} // X64Assembler::installFunObj


//////////////////////////////////////////////////////////////////////
//
//  X64Assembler::makeFunObj
//
Val X64Assembler::makeFunObj(const Defun* pDefun)
{
    resolve_jumps();

    uint cbCodeVec = m_nAddr;
    uint cbAnnon = sizeof(FunObj::Annon) * m_cAnnons;
    uint cbGcMap = 4;

    Val funobj = allocate_funobj(
        pDefun->m_classd,
        cbCodeVec,
        cbAnnon,
        cbGcMap,
        pDefun->m_eFrame,
        pDefun->m_cbFrame );

    FunObj* pFunObj = funobj->Decode<FunObj>();
        pFunObj->m_name = pDefun->m_name;

    // Copy codevec
    {
        uint nAddr = 0;
        uint ofsCode = 0;
        uint8* pbCodeVec = reinterpret_cast<uint8*>(pFunObj + 1);
        for (uint i = 0; i < m_cJumps; i++)
        {
            Jump* pJump = &m_rgoJump[i];

            ::CopyMemory(
                pbCodeVec + nAddr,
                m_rgbCode + ofsCode,
                pJump->m_nAddr - nAddr );

            nAddr   = pJump->m_nAddr;
            ofsCode = pJump->m_ofsCode;
        } // for i

        ::CopyMemory(
            pbCodeVec + nAddr,
            m_rgbCode + ofsCode,
            m_nAddr - nAddr );

        nAddr = m_nAddr;

        while (0 != nAddr % 4)
        {
            pbCodeVec[nAddr] = op_NOP;
            nAddr += 1;
        }
    } // copy cod vector

    // Embed lisp object
    {
        for (uint i = 0; i < m_cAnnons; i++)
        {
            Annon* pAnnon = &m_rgoAnnon[i];

            uint nAddr = pAnnon->m_nAddr;

            switch (pAnnon->m_eType)
            {
            case FunObj::Annon::Type_DllLink:
            {
                DllLink* p = pAnnon->m_value->StaticCast<DllLink>();
                Val entry = intern_dll_entry(p->m_file, p->m_proc);
                pFunObj->PatchDllEntry(
                    nAddr,
                    reinterpret_cast<DllEntry*>(entry->ToInt()) );
                break;
            } // FunObj::Annon::Type_DllLink

            case FunObj::Annon::Type_Label:
            {
                Label* pLabel = pAnnon->m_value->StaticCast<Label>();

                pFunObj->PatchUn(
                        nAddr,
                        pLabel->m_nAddr + pFunObj->GetCodeVec() );
                break;
            } // Type_Label

            case FunObj::Annon::Type_LispVal:
                pFunObj->PatchVal(nAddr, pAnnon->m_value);
                break;

            case FunObj::Annon::Type_NamedCallee:
            {
                Val callee = register_caller(pAnnon->m_value, funobj);
                pFunObj->PatchCallee(nAddr, callee);
                break;
            } // named_callee

            default:
                CAN_NOT_HAPPEN();
            } // switch eType
        } // for i
    }

    // Popuplate annotations
    {
        FunObj::Annon* pnAnnon = pFunObj->GetAnnon();

        for (uint i = 0; i < m_cAnnons; i++)
        {
            Annon* pAnnon = &m_rgoAnnon[i];
            pnAnnon->m_eType = pAnnon->m_eType;
            pnAnnon->m_ofs   = pAnnon->m_nAddr;
            pnAnnon++;
        } // for i
    }

    // Populate GC Map
    pFunObj->GetGcMap()[0] = 0;

    // Make FunObj GC-able.
    pFunObj->m_nCookie = FunObj::Cookie;

    return funobj;
} // X64Assembler::makeFunObj


//////////////////////////////////////////////////////////////////////
//
// emit_prologue
//      min     max
//      0       -1      nothing to emit
//      n       n       cmp $rn, n; jne arity_error
//      n       -1      cmp $rn, n; jlt arity_error
//      n       m       cmp $rn, n; jlt arity_erorr; cmp $rn, m; jgt
//
void
X64Assembler::emit_prologue(Defun* pDefun)
{
    if (0 == pDefun->m_iMin && -1 == pDefun->m_iMax)
    {
        pDefun->m_fCheckArity = false;
    }
    else
    {
        ASSERT(pDefun->m_iMin <= pDefun->m_iMax);


        if (pDefun->m_iMin == pDefun->m_iMax)
        {
            if (0 != pDefun->m_iMin)
            {
                cmp($rn, Fixnum::Encode(pDefun->m_iMin));
            }
            else
            {
                test($rn, $rn);
            }
            jne(pDefun->m_oArityError);
        }
        else
        {
            if (0 != pDefun->m_iMin)
            {
                cmp($rn, Fixnum::Encode(pDefun->m_iMin));
                jl(pDefun->m_oArityError);
            }

            if (-1 != pDefun->m_iMax)
            {
                cmp($rn, Fixnum::Encode(pDefun->m_iMax));
                jg(pDefun->m_oArityError);
            }
        }
    } // if
} // X64Assembler::emit_prologue


//////////////////////////////////////////////////////////////////////
//
// emit_epilogue
//
void
X64Assembler::emit_epilogue(Defun* pDefun)
{
    if (pDefun->m_fCheckArity)
    {
        label(pDefun->m_oArityError);
        call(ea($rtcb, SVC_arity_error));
    }
} // X64Assembler::emit_epilogue


//////////////////////////////////////////////////////////////////////
//
//  X64Assembler::resolve_jumps
//
void
X64Assembler::resolve_jumps()
{
  try_again:
    for (uint i = 0; i < m_cJumps; i++)
    {

        Jump* pJump = &m_rgoJump[i];

        int iRel = pJump->m_pLabel->m_nAddr - pJump->m_nAddr;
        if (op_NOP == pJump->m_nOpcode)
        {
            m_rgbCode[pJump->m_ofsCode - 4] = static_cast<uint8>(iRel);
            m_rgbCode[pJump->m_ofsCode - 3] = static_cast<uint8>(iRel >> 8);
            m_rgbCode[pJump->m_ofsCode - 2] = static_cast<uint8>(iRel >> 16);
            m_rgbCode[pJump->m_ofsCode - 1] = static_cast<uint8>(iRel >> 24);
        }
        else if (iRel >= -128 && iRel <= 127)
        {
            // Short form
            uint nLen = op_JMP_Jb == pJump->m_nOpcode ? 5 : 6;

            m_rgbCode[pJump->m_ofsCode - nLen] =
                static_cast<uint8>(pJump->m_nOpcode);

            m_rgbCode[pJump->m_ofsCode - nLen + 1] =
                static_cast<uint8>(iRel);
        }
        else
        {
            // Convert to long form
            switch (pJump->m_nOpcode)
            {
            case op_JMP_Jv:
                m_rgbCode[pJump->m_ofsCode - 5] = op_JMP_Jv;
                break;

            case op_JMP_Jb:
                pJump->m_nOpcode = op_JMP_Jv;

                m_rgbCode[pJump->m_ofsCode - 5] =
                    static_cast<uint8>(pJump->m_nOpcode);

                shift_labels(pJump->m_nAddr, 3);
                goto try_again;

            default:
                m_rgbCode[pJump->m_ofsCode - 6] =
                    static_cast<uint8>(pJump->m_nOpcode >> 8);

                m_rgbCode[pJump->m_ofsCode - 5] =
                    static_cast<uint8>(pJump->m_nOpcode);

                if (pJump->m_nOpcode >= op_Jcc_Jv)
                {
                    break;
                }

                pJump->m_nOpcode = op_Jcc_Jv - op_Jcc_Jb + pJump->m_nOpcode;

                shift_labels(pJump->m_nAddr, 4);
                goto try_again;
            } // switch opcode

            m_rgbCode[pJump->m_ofsCode - 4] = static_cast<uint8>(iRel);
            m_rgbCode[pJump->m_ofsCode - 3] = static_cast<uint8>(iRel >> 8);
            m_rgbCode[pJump->m_ofsCode - 2] = static_cast<uint8>(iRel >> 16);
            m_rgbCode[pJump->m_ofsCode - 1] = static_cast<uint8>(iRel >> 24);
        }
    } // for i
} // X64Assembler::resolve_jumps


//////////////////////////////////////////////////////////////////////
//
// X64Assembler::shift_labels
//
void X64Assembler::shift_labels(uint nAddr, uint nInc)
{
    m_nAddr += nInc;

    {
        uint i = m_cJumps;
        while (i >= 1)
        {
            i -= 1;

            Jump* pJump = &m_rgoJump[i];
            if (pJump->m_nAddr < nAddr)
            {
                break;
            }

            pJump->m_nAddr += nInc;
        } // while
    }

    for (
        Label* pLabel = m_pLastLabel;
        NULL != pLabel;
        pLabel = pLabel->m_pPrev )
    {
        if (pLabel->m_nAddr < nAddr)
        {
            break;
        }

        pLabel->m_nAddr += nInc;
    } // for each label

    {
        uint i = m_cAnnons;
        while (i >= 1)
        {
            i -= 1;

            Annon* pAnnon = &m_rgoAnnon[i];
            if (pAnnon->m_nAddr < nAddr)
            {
                break;
            }

            pAnnon->m_nAddr += nInc;
        } // while
    }

    {
        uint i = m_cDllLinks;
        while (i >= 1)
        {
            i -= 1;

            DllLink* pDllLink = &m_rgoDllLink[i];
            if (pDllLink->m_nAddr < nAddr)
            {
                break;
            }

            pDllLink->m_nAddr += nInc;
        } // while
    }
} // X64Assembler::shift_labels

} // X64
