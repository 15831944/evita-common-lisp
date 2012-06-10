//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - compiler - IR - Functions
// compiler/ir/ir_fns.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/ir/ir_fns.h#12 $
//
#if !defined(INCLUDE_compiler_ir_fns_h)
#define INCLUDE_compiler_ir_fns_h

#include "../cm/cm_fns.h"

#include "./ir_cfg.h"       // CfgEdge::Kind
#include "./ir_function.h"  // Function::Kind
#include "./ir_type.h"

namespace Compiler
{

    ////////////////////////////////////////////////////////////
    //
    // BBlock related
    //
    BBlock* InsertBBlock(BBlock*, BBlock*);
    BBlock* NewBBlock();
    BBlock* ir_remove_bblock(BBlock*);
    void    ReplacePhiOperands(BBlock*, BBlock*, BBlock*);
    void    ir_eliminate_unreachable_bblocks(Function*);
    BBlock* ir_split_bblock_after(Instruction*);

    ////////////////////////////////////////////////////////////
    //
    // Call Graph related
    //
    void    ir_eliminate_unreachable_functions();
    CgEdge* ir_find_cg_edge(Function*, Function*);
    void    ir_remove_function(Function*);

    ////////////////////////////////////////////////////////////
    //
    // CFG related
    //
    CfgEdge*    FindCfgEdge(BBlock*, BBlock*);
    bool        ir_eliminate_infinite_loop(Function*);
    CfgEdge*    ir_replace_cfg_edge_from(CfgEdge*, BBlock*);
    CfgEdge*    ir_replace_cfg_edge_to(CfgEdge*, BBlock*);
    CfgEdge*    RemoveCfgEdge(CfgEdge*);
    bool        ir_remove_useless_instructions(Function*);
    BBlock*     SplitCfgEdge(CfgEdge*);

    inline CfgEdge* ir_get_cfg_edge(BBlock* pFrom, BBlock* pTo)
    {
        CfgEdge* pEdge = FindCfgEdge(pFrom, pTo);
            ASSERT(NULL != pEdge);
        return pEdge;
    } // ir_get_cfg_edge

    CfgEdge* AddCfgEdge(
        BBlock*         pFrom,
        BBlock*         pTo ,
        CfgEdge::Kind   eKind = CfgEdge::Kind_Normal );

    CfgEdge* RemoveCfgEdge(BBlock* pCurr, BBlock* pSucc);

    ////////////////////////////////////////////////////////////
    //
    // DFA related
    //
    void ComputeLiveness(Function*);
    void SolveBackward(Function*);

    //////////////////////////////////////////////////
    //
    // Dominance Tree
    bool ComputeDominance(Function*);
    bool ComputePostDominance(Function*);

    ////////////////////////////////////////////////////////////
    //
    // Instruction related
    //
    Instruction* ir_insert_insn(Instruction*, Instruction*);
    Instruction* ir_move_insn(Instruction*, Instruction*);
    Instruction* ir_remove_insn(Instruction*);
    Instruction* ir_replace_insn(Instruction*, Instruction*);

    bool ir_dominate_p(const Instruction*, const Instruction*);
    void ir_number_instructions(Function*);
    bool ir_post_dominate_p(const Instruction*, const Instruction*);

    ////////////////////////////////////////////////////////////
    //
    // Operands
    //
    Function* NewFunction(Val, Function::Kind = Function::Kind_Normal);
    Integer*  NewInteger(Int);
    Literal*  NewLiteral(Val);
    Output*   NewOutput(Val);
    void      ir_replace_all_users(Operand*, Output*);
    bool      ir_can_nil(Operand*);
    Val       ir_get_callee_name(Operand*);


    inline Literal* NewLiteral(Int iVal)
        { return NewLiteral(Fixnum::Encode(iVal)); }

    ////////////////////////////////////////////////////////////
    //
    // Closure
    //
    Register* ir_add_extra_param(Function*, Ty, Register*);
    Function* ir_split_closure(Function*);

    ////////////////////////////////////////////////////////////
    //
    // UpVar
    //
    Register* ir_find_variable(Function*, Variable*);
} // Compiler

#endif // !defined(INCLUDE_compiler_ir_fns_h)
