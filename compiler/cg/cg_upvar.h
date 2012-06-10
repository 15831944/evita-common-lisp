//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - cg - upvar
// comcg/cg_upvar.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cg/cg_upvar.h#4 $
//
#if !defined(INCLUDE_compiler_cg_upvar_h)
#define INCLUDE_compiler_cg_upvar_h

#include "../cm/cm_defs.h"
#include "../cm/cm_base.h"
#include "../cm/cm_fns.h"

#include "../ir/ir_defs.h"
#include "../ir/ir_operand.h"
#include "../ir/ir_pass.h"

#include "./cg_defs.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// UpVarPass
//
class UpVarPass : public ModulePass
{
    public: UpVarPass(
        LPCWSTR     pwsz,
        Operand**   prgpExtra,
        Operand**   prgpVector ) :
            m_prgpExtra(prgpExtra),
            m_prgpVector(prgpVector),
            ModulePass(pwsz) {}

    public: virtual void process_module(Module*);

    protected: enum Access
    {
        Access_Base,        // Accessed by base+offset
        Access_Closed,      // Accessed as closed variable
        Access_Register,    // Acccessed register
        Access_Sp,          // same as base, but base is SP.
        Access_Vector,      // Accessed by VECREF
    }; // Access

    protected: enum Path
    {
        Path_None,
        Path_ClosedFixed,
        Path_ClosedVariable,
        Path_Closure,       // Closure
        Path_Fixed,         // Single path
        Path_Mixed,         // Multiple path including closure
        Path_Variable,      // Single path with variable stack frame
        Path_Visited,
    }; // Path

    protected: class Home;
    protected: class UpVar;

    // UpVar
    protected: class UpVar :
        public WorkListItem_<UpVar>
    {
        public: Home*           m_pHome;
        public: Variable*       m_pVar;
        public: UpVarDefInsn*   m_pUpVarDef;
        public: Register*       m_pRx;
        public: uint            m_nVecIdx;
        public: Access          m_eAccess;
        public: bool            m_fReadOnly;

        public: UpVar(
            Home*           pHome,
            Variable*       pVar,
            UpVarDefInsn*   pUpVarDef,
            Access          eAccess,
            bool            fReadOnly ) :
                m_pHome(pHome),
                m_pVar(pVar),
                m_pUpVarDef(pUpVarDef),
                m_eAccess(eAccess),
                m_fReadOnly(fReadOnly),
                m_pRx(NULL),
                m_nVecIdx(static_cast<uint>(-1)) {}
    }; // UpVar

    protected: typedef WorkList_<UpVar> UpVarList;

    // Home
    //  m_pFun      -- owner of variable
    //  m_pHolder   -- holder of upvar
    //  m_pBase     -- base register to access upvars of holder in m_pFun
    //  m_oUpVars   -- m_pHolder's upvars access by m_pFun
    //  m_cUpVars   -- number of upvars in m_oUpVars
    //  m_ePath     -- path from m_pFun to m_pHolder
    protected: class Home :
        public WorkListItem_<Home>
    {
        public: Function*       m_pOwner;
        public: Function*       m_pHolder;
        public: Register*       m_pBase;
        public: UpVarList       m_oUpVars;
        public: uint            m_cUpVars;
        public: Path            m_ePath;

        public: Home(
            Function*   pOwner,
            Function*   pHolder,
            Path        ePath ) :
                m_pOwner(pOwner),
                m_pHolder(pHolder),
                m_pBase(NULL),
                m_ePath(ePath),
                m_cUpVars(0) {}
    }; // Home

    protected: typedef WorkList_<Home> HomeList;

    // FunExt
    protected: class FunExt
    {
        public: HomeList        m_oHomes;
        public: Register*       m_pSp;
        public: uint            m_cExtras;
        public: uint            m_cVecElts;

        public: FunExt() :
            m_pSp(NULL),
            m_cExtras(0),
            m_cVecElts(0) {}
    }; // FunExt

    protected: Module*   m_pModule;
    protected: Operand** m_prgpExtra;
    protected: Operand** m_prgpVector;

    protected: virtual void analyze_by_target(Module*) = 0;

    protected: void analyze_in_local(Module*);
    protected: void analyze_in_global(Module*);

    protected: Path analyze_path(Function*, Function*, Function**);
    protected: Path analyze_path_aux(Function*, Function*, Function**);

    protected: Home*  find_home(Function*, Function*);
    protected: UpVar* find_upvar(Function*, Variable*);

    protected: Register* get_base(Function*, Variable*, Instruction*);
    protected: UpVar*   get_upvar(Function*, Variable*);

    protected: Register* insert_cvector(Operand**, uint, Instruction*);
    protected: Operand* insert_cell(Function*, Variable*, Instruction*);

    protected: Register* intern_stack_slot(Function*, Variable*);
    protected: Register* intern_cell_param(Function*, UpVar*);

    protected: void patch_optional(Function*);

    protected: void update_upvar_in_closure(Function*);
    protected: void update_upvar_in_inner(Function*);
    protected: void update_upvar_in_inner(UpVar*, SlotInsn*);
    protected: void update_call_site(Instruction*);

    protected: void update_restify(Function*);

    protected: void dump_analysis(Module*);
}; // UpVarPass

} // Compiler

#endif //!defined(INCLUDE_compiler_cg_upvar_h)
