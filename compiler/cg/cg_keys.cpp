#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - compiler - Expand CHECKKYES and PARSEKEYS
// compiler/cg/cg_keys.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cg/cg_keys.cpp#3 $
//
// Description:
// Simple layout method:
//    Classifies basic blocks by last instruction and successors into
//    following four classes:
//      o normal(0)        - block ends with JUMP or BRANCH.
//      o return(1)        - block ends with RET.
//      o nonlocal exit point (2) - block has only nonlocal in-edge.
//      o terminate(3)     - block ends with UNREACHABLE.
// 
//    When a block ends with JUMP, its class is a class of successor.
//    When a block ends with BRANCH, its class is determined by successors.
//
//
#include "./cg_defs.h"

#include "../ir/ir_defs.h"
#include "../ir/ir_function.h"
#include "../ir/ir_pass.h"

namespace Compiler
{

class Expander : public FunctionPass
{
    public: Expander() : FunctionPass(L"CG-EXPAND-KEYS") {}

    virtual void process_function(Function*);

    void process_CHECKKEYS(Instruction*);
    void process_PARSEKEYS(Instruction*);

    typedef void (Expander::*InsnProcT)(Instruction*);
    static const InsnProcT k_rgpInsnProc[IrOp_MAX_1 + 1];
}; // Expander

const Expander::InsnProcT
Expander::k_rgpInsnProc[IrOp_MAX_1 + 1] =
{
    #define DEFIROP(mp_name) &Expander::process_##mp_name,
    #include "./cg_opcode.inc"
}; // k_rgpInsnProc


//////////////////////////////////////////////////////////////////////
//
// Expander::process_function
//
void
Expander::process_function(Function* pFun)
{
    html_log_format(2, L"<h2>Process ~S</h2>~%", pFun);

    foreach (Function::EnumBBlock, oEnumBB, pFun)
    {
        BBlock* pbbCurr = oEnumBB.Get();

        BBlock::EnumInsn oEnum(pbbCurr);
        while (! oEnum.AtEnd())
        {
            Instruction* pInsn = oEnum.Get();
                oEnum.Next();

            if (pInsn->GetBBlock() != pbbCurr)
            {
                break;
            }

            InsnProcT pfn = k_rgpInsnProc[pInsn->GetOpcode()];
            (this->*pfn)(pInsn);
        } // for each insn
    } // for each bblock
} // Expander::process_function

//////////////////////////////////////////////////////////////////////
//
// insert_parse_keys
//
//    curr:
//      COPY t %runner <= %rest
//      COPY fixnum %flags <= 0
//      JUMP loop_1
//
//    loop_1:
//      CONSP bool %bool <= %runner
//      BRANCH %bool loop_2 loop_end
//
//    loop_2:
//      CAR %car <= %runner
//      CDR %runner <= %runner
//      CONSP bool %bool <= %runner
//      BRANCH %bool loop_key1_1 odd
//
//    loop_key1_1:
//      EQ bool %bool <= %car key_1
//      BRANCH %bool loop_key1_2 loop_key2
//
//    loop_key1_2:
//      LOGIOR fixnum %flags <= %flags 1
//      CAR t %car <= %runner
//      COPY t %keyval1 <= %car
//      JUMP loop_2
//
//      ... key_i ...
//
//    other:
//      EQ bool %bool <= %allow_other_keys nil
//      TRAPIFNOT %bool si::unrecognized-keywoard-argument %car
//      JUMP loop_3
//
//    loop_3:
//      CDR t %runner <= %runner
//      JUMP loop_1
//
//    odd:
//      EQ bool %bool <= %allow_other_keys nil
//      TRAPIFNOT %bool si::odd-numbers-of-keywoard-arguments %rest
//      JUMP end
//
//    loop_end:
//      EQ bool %bool <= %runner nil
//      BRANCH %bool end not_cons
//
//    not_cons:
//      EQ bool %bool <= %allow_other_keys nil
//      TRAPIFNOT %bool type-error %runner cons
//      JUMP end
//
//    end:
//
static void
insert_parse_keys(
    BBlock*     pbbEnd,
    Register*   prKeys,
    Register*   prRest,
    Register*   prAllowOtherKeys )
{
    ASSERT(NULL != pbbCurr);

    Register* prRunner = new Register();
    Register* prFlags  = new Register();


    BBlock* pbbLoop1 = ir_insert_bblock(NewBBlock(), pbbEnd);
    BBlock* pbbLoop2 = ir_insert_bblock(NewBBlock(), pbbEnd);
    BBlock* pbbLoop3 = ir_insert_bblock(NewBBlock(), pbbEnd);

    BBlock* pbbOther;
    BBlock* pbbOdd;
    BBlock* pbbLoopEnd
    BBlock* pbbNotCons;
    {
        if (NULL == prAllowOtherKeys)
        {
            pbbOther   = pbbLoop3;
            pbbOdd     = pbbEnd;
            pbbLoopEnd = pbbEnd;
            pbbNotCons = pbbEnd;
        }
        else
        {
            pbbOther   = ir_insert_bblock(NewBBlock(), pbbLoop3);
            pbbOdd     = ir_insert_bblock(NewBBlock(), pbbEnd);
            pbbLoopEnd = ir_insert_bblock(NewBBlock(), pbbEnd);
            pbbNotCons = ir_insert_bblock(NewBBlock(), pbbEnd);
        }
    } // pbbOther

    // curr
    {
        pbbCurr->AppbbEndInsn(new CopyInsn(prRunner, prRest));
        pbbCurr->AppbbEndInsn(new CopyInsn(prRunner, Fixnum::Encode(0)));
        pbbCurr->AppbbEndInsn(new JumpInsn(pbbLoop1);
    }

    // loop_1
    {
        Bool* pBool = insert_consp(pbbLoop1, prRunner);
        pbbLoop1->AppbbEndInsn(new BranchInsn(pBool, pbbLoop2, pbbLoopEnd);
    }

    Register::EnumUseSite oEnumKey(pKeys);

    BBlock* pbbKey1 = if (oEnumKey.AtEnd() ?
        pbbKey1 = pbbOther :
        ir_insert_bblock(NewBBlock(), pbbOther);

    // loop_2
    Register* prCar = insert_car(pbbLoop2, prRunner);
    {
        insert_cdr(pbbLoop2, prRunner);
        Bool* pBool = insert_consp(pbbLoop2, prRunner);
        pbbLoop2->AppbbEndInsn(new BranchInsn(pBool, pbbKey1, pbbOd);
    }

    UINT nPosn = 1;
    while (! oEnumKey.AtEnd())
    {
        KeyInsn* pinKey = oEnumKey.Get()->StaticCast<KeyInsn>();
            oEnumKey.Next();

        BBlock* pbbKey2 = ir_insert_bblock(NewBBlock(), pbbOther);

        BBlock* pbbKeyNext = oEnumKey.AtEnd() ?
            pbbOther :
            ir_insert_bblock(NewBBlock(), pbbOther);

        Bool* pBool = insert_eq(pbbKey1, prCar, pinKey->GetSx());
        pbbKey1->AppendInsn(new BranchInsn(pBool, pbbKey2, pbbKeyNext);


        // Rewrite KEYVAL and KEYSUPPLIEDP instructions
        {
            Register::EnumUseSite oEnum(pinKey->GetRd());
            while (! oEnum.AtEnd())
            {
                Instruction* pInsn = oEnum.Get()->GetInstruction();
                    oEnum.Next();

                if (pInsn->Is<KeyValInsn>())
                {
                    ir_replace_insn(
                        new CopyInsn(pInsn->GetRd(), prCar),
                        pInsn );
                }
                else if (pInsn-><KeySuppliedInsn>())
                {
                    pbbKey2->AppendInsn(
                        new LogIorInsn(ty_fixnum, prFlags,
                            prFlags, NewLiteral(1 << nPosn) ) );

                    ir_replace_insn(
                        new Logbitp(pInsn->GetBd(), nPosn, prFlags),
                        pInsn );

                    nPosn += 1;
                }
                else
                {
                    CAN_NOT_HAPPEN();
                }
            } // for each use site
        }

        ir_remove_insn(pinKey);

        pbbKey1 = pbbKeyNext;
    } // for each key

    // other
    if (pbbOther != pbbEnd)
    {
        Bool* pBool = insert_eq(pbbOther, prAllowOtherKeys, nil);

        pbbOther->AppendInsn(
            new TrapIfNotInsn(pBool,
                Qunrecognized_keyword_argument, prCar ) );

        pbbOther->AppendInsn(new JumpInsn(pbbLoop3));
    } // other

    // loop3
    {
        insert_cdr(pbbLoop3, prRunner);
        pbbLoop3->AppendInsn(new JumpInsn(pbbLoop1));
    } // loop3

    if (NULL != prAllowOtherKeys)
    {
        // odd
        {
            Bool* pBool = insert_eq(pbbOdd, prAllowOddKeys, nil);

            pbbOdd->AppendInsn(new TrapIfNotInsn(pBool,
                    Qodd_numbers_of_keyword_arguments, prCar ) );

            pbbOdd->AppendInsn(new JumpInsn(pbbLoop3));
        } // odd

        // loop_end
        {
            Bool* pBool = insert_eq(pbbLoopEnd, prRunner, nil);
            pbbLoopEnd->AppendInsn(new BranchInsn(pBool, pbbEnd, pbbNotCons);
        } // loop_end

        // not_cons
        {
            Bool* pBool = insert_eq(pbbOdd, prAllowOddKeys, nil);
            pbbNotCons->AppendInsn(new TrapIfNotInsn(pBool,
                    Qtype_error, prRunner, Qcons ) );
        }
    } // allow-other-keys
} // insert_parse_keys


//////////////////////////////////////////////////////////////////////
//
// Expander::process_CHECKKEYS
//
//    curr:
//      COPY boolean %allow_other_keys <= nil
//      COPY t %runner <= %rest
//      JUMP loop_1
//
//    loop_1:
//      EQ bool %bool %runner 'nil
//      BRANCH %bool loop_end loop_2
//
//    loop_2:
//      CONSP bool %bool <= %runner
//      BRANCH %bool loop_3 loop_end
//
//    loop_3
//      CAR t %car <= %runner
//      CDR t %runner <= %runner
//      EQ bool %bool <= %car ':allow-other-keys
//      BRANCH %bool got loop_4
//
//    loop_4:
//      CONSP bool %bool <= %runner
//      BRANCH %bool loop_5 loop_end
//
//    loop_5:
//      CDR t %runner <= %runner
//      JUMP loop_1
//
//    loop_got:
//      CAR t %car <= %runner
//      COPY t %allow_other_keys <= %car
//      JUMP loop_end
//
//    loop_end:
void
Expander::process_CHECKKEYS(Instruction* pCheckKeys)
{
    BBlock* pbbLoopEnd = ir_split_after(pCheckKeys);
    BBlock* pbbLoop1   = ir_insert_(NewBBlock(), pSucc);
    BBlock* pbbLoop2   = ir_insert_(NewBBlock(), pSucc);
    BBlock* pbbLoop3   = ir_insert_(NewBBlock(), pSucc);
    BBlock* pbbLoopGot = ir_insert_(NewBBlock(), pSucc);

    Register* prAllowOtherKeys = new Register();
        prAllowOtherKeys->MarkNotSSA();

    Register* prRunner = new Register();
        prRunner->MarkNotSSA();

    // curr
    {
        ir_insert_insn(
            new CopyInsn(prAllowOtherKeys, NewLiteral(nil)),
            pChekKeys );

        ir_insert_insn(
            new CopyInsn(prRunner, pCheckKeys->GetRx()),
            pCheckKeys );

        ir_replace_insn(
            new JumpInsn(pbbLoop1),
            pCheckKeys );
    }

    // loop_1
    {
        Bool* pBd = insert_eq(pbbLoop1, prRunner, nil);
        pbbLoop1->AppbbEndInsn(new BranchInsn(pBd, pbbLoopEnd, pbbLoop2));
    }

    // loop_2
    {
        Register* pR_car = insert_car(pbbLoop2, prRunner);
        insert_cdr(pbbLoop2, prRunner);
        Bool* pBd = insert_eq(pbbLoop2, pR_car, Kallow_other_keys);
        pbbLoop2->AppbbEndINsn(new BranchInsn(pBd, pbbLoopGot, pbbLoop3));
    }

    // loop_3
    {
        insert_cdr(pbbLoop3, prRunner);
        pbbLoop3->AppbbEndInsn(new JumpInsn(pbbLoop1));
    }

    // loop_got
    {
        Register* pR_car = insert_car(pbbLoopGot, prRunner);
        pbbLoopGot->AppbbEndInsn(new CopyInsn(prAllowOtherKeys, pR_car));
        pbbLoopGot->AppbbEndInsn(new JumpInsn(pbbLoopEnd));
    }

    insert_parse_keys(
        pbbLoopEnd,
        pCheckKeys->GetRd(),
        pCheckkeys->GetRx(),
        prAllowOtherKeys );
} // Expander::process_CHECKKEYS


//////////////////////////////////////////////////////////////////////
//
// Expander::process_PARSEKEYS
//
void Expander::process_PARSEKEYS(Instruction* pParseKeys)
{
    BBlock* pLoopbbEnd = ir_split_after(pParseKeys);
    insert_parse_keys(
        pbbLoopEnd,
        pParseKeys->GetRd(),
        pParseKeys->GetRx(),
        NULL );
} // Expander::process_PARSEKEYS

//////////////////////////////////////////////////////////////////////
//
// x86_pass_expand_keys
//
void x86_pass_expand_keys()
{
    Expander oPass;
    oPass.Run();
} // x86_pass_expand_trap

} // Compiler
