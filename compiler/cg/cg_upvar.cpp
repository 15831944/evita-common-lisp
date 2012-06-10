#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - compiler - cg - Closure Finalizer
// compiler/cg/cg_upvar.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cg/cg_upvar.cpp#12 $
//
#include "./cg_upvar.h"

#include "./cg_instruction.h"

#include "../cm/cm_module.h"
#include "../cm/cm_target.h"

namespace Compiler
{

namespace
{

enum Limits
{
    MaxClosureInsns = 10,
}; // Limits

static const char16* const k_rgwszAccess[] =
{
    L"Base",
    L"Closed",
    L"Register",
    L"Sp",
    L"Vector",
}; // k_rgwszAccess

static const char16* const k_rgwszPath[] =
{
    L"None",
    L"ClosedFixed",
    L"ClosedVariable",
    L"Closure",
    L"Fixed",
    L"Mixed",
    L"Variable",
    L"Visited",
}; // k_rgwszPath

//////////////////////////////////////////////////////////////////////
//
// is_read_only
//  Returns true if specified cell is read-only in all functions.
//
static bool
is_read_only(Register* pCell)
{
    foreach (Register::EnumUseSite, oEnum, pCell)
    {
        SlotInsn* pSlot = oEnum.Get()->GetInstruction()->
            StaticCast<SlotInsn>();

        foreach (Register::EnumUseSite, oEnum, pSlot->GetRd())
        {
            Instruction* pInsn= oEnum.Get()->GetInstruction();

            if (pInsn->Is<StoreInsn>())
            {
                return false;
            }

            ASSERT(pInsn->Is<LoadInsn>());
        } // for each use site
    } // for each use site

    return true;
} // is_read_only

} // namespace

//////////////////////////////////////////////////////////////////////
//
// UpVarPass::analyze_in_global
//
//  o Propagates read-write from callee to caller.
//  o pUpVar->m_fReadOnly => false if callee modifies pUpVar.
//
void UpVarPass::analyze_in_global(Module* pModule)
{
    bool fChanged = true;
    while (fChanged)
    {
        fChanged = false;

        html_log_format(3, L"<ol>");

        // Process callee before caller.
        foreach (Module::EnumFunction_Postorder, oEnum, pModule)
        {
            Function* pFun = oEnum.Get();

            html_log_format(3, L"<li>~S</li>~%", pFun);

            FunExt* pFunExt = pFun->GetExtension<FunExt>();

            foreach (HomeList::Enum, oEnum, &pFunExt->m_oHomes)
            {
                Home* pHome = oEnum.Get();
                foreach (UpVarList::Enum, oEnum, &pHome->m_oUpVars)
                {
                    UpVar* pUpVar = oEnum.Get();

                    if (! pUpVar->m_fReadOnly)
                    {
                        continue;
                    }

                    // Do callees modify upvars in pFun?
                    Variable* pVar = pUpVar->m_pVar;
                    foreach (Function::EnumCallee, oEnum, pFun)
                    {
                        Function* pCallee = oEnum.GetNode();

                        UpVar* pUpVarE = find_upvar(pCallee, pVar);

                        if (NULL != pUpVarE && ! pUpVarE->m_fReadOnly)
                        {
                            pUpVar->m_fReadOnly = false;
                            fChanged = true;
                            break;
                        }
                    } // for each callee
                } // for each upvar
            } // for each home
        } // for each fun
        html_log_format(3, L"</ol>~%");
    } // while
} // UpVarPass::analyze_in_global


//////////////////////////////////////////////////////////////////////
//
// UpVarPass::analyze_in_local
//
void UpVarPass::analyze_in_local(Module* pModule)
{
    html_log_format(3, L"<ol>");
    // Split closure if closure is called and used.
    foreach (Module::EnumFunction, oEnum, pModule)
    {
        Function* pFun = oEnum.Get();

        html_log_format(3, L"<li>~S<ul>~%", pFun);

        FunExt* pFunExt = new FunExt();
        pFun->SetExtension<FunExt>(pFunExt);

        foreach (Function::EnumUpVarSite, oEnum, pFun)
        {
            UpVarDefInsn* pUpVarDef = oEnum.Get()->GetInstruction()->
                StaticCast<UpVarDefInsn>();

            Variable* pVar   = pUpVarDef->GetSx()->StaticCast<Variable>();
            Function* pOwner = pVar->GetOwner();

            Home* pHome = find_home(pFun, pOwner);
            if (NULL == pHome)
            {
                if (pFun->IsClosure())
                {
                    pHome = new Home(pOwner, pFun, Path_Closure);
                }
                else
                {
                    Function* pHolder = pOwner;
                    Path ePath = analyze_path(pFun, pOwner, &pHolder);
                    switch (ePath)
                    {
                    case Path_ClosedFixed:
                        ePath = Path_Fixed;
                        break;

                    case Path_ClosedVariable:
                        ePath = Path_Variable;
                        break;
                    } // switch ePath

                    pHome = new Home(pOwner, pHolder, ePath);
                }

                pFunExt->m_oHomes.Push(pHome);
            } // if

            Access eAccess;
            {
                switch (pHome->m_ePath)
                {
                case Path_Closure:
                    eAccess = Access_Closed;
                    break;

                case Path_Fixed:
                    eAccess = Access_Sp;
                    break;

                case Path_Mixed:
                    eAccess = Access_Vector;
                    break;

                case Path_Variable:
                    eAccess = Access_Base;
                    break;

                default:
                    CAN_NOT_HAPPEN();
                } // switch path
            } // eAccess

            UpVar* pUpVar = new UpVar(
                pHome,
                pVar,
                pUpVarDef,
                eAccess,
                is_read_only(pUpVarDef->GetRd()) );

            pHome->m_oUpVars.Push(pUpVar);

            pHome->m_cUpVars += 1;
        } // for each upvar

        // Assign closed variable index.
        // This order must be as same as processing order in update_call_site.
        {
            uint nCldIdx = 0;
            uint nVecIdx = 0;
            foreach (
                HomeList::Enum,
                oEnum,
                &pFun->GetExtension<FunExt>()->m_oHomes )
            {
                Home* pHome = oEnum.Get();
                foreach (UpVarList::Enum, oEnum, &pHome->m_oUpVars)
                {
                    UpVar* pUpVar = oEnum.Get();


                    switch (pUpVar->m_eAccess)
                    {
                    case Access_Closed:
                        pUpVar->m_pUpVarDef->GetRd()->SetStorage(
                            Register::Storage_Closed,
                            nCldIdx );

                        nCldIdx += 1;
                        break;

                    case Access_Vector:
                        pUpVar->m_nVecIdx = nVecIdx;
                        nVecIdx += 1;
                        break;
                    } // switch access
                } // for each upvar
            } // for each home
        } // assign

        html_log_format(3, L"</ul></li>~%");
    } // for each fun
    html_log_format(3, L"</ol>~%");
} // UpVarPass::analyze_in_local


//////////////////////////////////////////////////////////////////////
//
// UpVarPass::analyze_path
//
UpVarPass::Path
UpVarPass::analyze_path(
    Function*   pCallee,
    Function*   pOwner,
    Function**  out_pHolder )
{
    if (pCallee->GetFunKind() == Function::Kind_Finally)
    {
        return Path_Variable;
    }

    Path ePath = analyze_path_aux(pCallee, pOwner, out_pHolder);

    // Reset visit flag
    foreach (Module::EnumFunction, oEnum, m_pModule)
    {
        oEnum.Get()->SetFlag(0);
    } // for each fun

    ASSERT(Path_None    != ePath);
    ASSERT(Path_Visited != ePath);
    return ePath;
} // UpVarPass::analyze_path

//////////////////////////////////////////////////////////////////////
//
// UpVarPass::analyze_path
//
UpVarPass::Path
UpVarPass::analyze_path_aux(
    Function*   pCallee,
    Function*   pOwner,
    Function**  out_pHolder )
{
    // Closure is home of all upvars.
    ASSERT(! pCallee->IsClosure());

    Path ePath = static_cast<Path>(pCallee->GetFlag());
    if (Path_None != ePath)
    {
        return ePath;
    }

    pCallee->SetFlag(Path_Visited);

    html_log_format(3, L"<li>analyze_path: ~S => ~S~%", pCallee, pOwner);
    html_log_format(4, L"<ul>");

    foreach (Function::EnumCaller, oEnum, pCallee)
    {
        Function* pCaller = oEnum.GetNode();

        html_log_format(4, L"<li>caller ~S</li>~%", pCaller);

        if (pCaller == pOwner)
        {
            switch (ePath)
            {
            case Path_ClosedFixed:
            case Path_ClosedVariable:
                ePath = Path_Mixed;
                break;

            case Path_Fixed:
            case Path_Visited:
                // multiple path
                ePath = Path_Variable;
                break;

            case Path_None:
                ePath = Path_Fixed;
                break;

            case Path_Variable:
                break;

            default:
                CAN_NOT_HAPPEN();
            } // switch ePath
        }
        else if (pCaller->IsClosure())
        {
            *out_pHolder = pCaller;
            switch (ePath)
            {
            case Path_ClosedFixed:
            case Path_ClosedVariable:
            case Path_Fixed:
            case Path_Variable:
            case Path_Visited:
                ePath = Path_Mixed;
                break;

            case Path_None:
                ePath = Path_ClosedFixed;
                break;

            default:
                CAN_NOT_HAPPEN();
            } // switch ePath
        }
        else
        {
            switch (ePath)
            {
            case Path_ClosedFixed:
            case Path_ClosedVariable:
                ePath = Path_Mixed;
                break;

            case Path_Fixed:
            case Path_Variable:
            case Path_Visited:
                switch (analyze_path_aux(pCaller, pOwner, out_pHolder))
                {
                case Path_Mixed:
                    ePath = Path_Mixed;
                    break;
                default:
                    ePath = Path_Variable;
                    break;
                } // switch path
                break;

            case Path_None:
                ePath = analyze_path_aux(pCaller, pOwner, out_pHolder);
                break;

            default:
                CAN_NOT_HAPPEN();
            } // switch Path
        } // if

        pCaller->SetFlag(ePath);
        if (Path_Mixed == ePath)
        {
            break;
        }
    } // for each caller

    if (pCallee->IsStackRestify())
    {
        switch (ePath)
        {
        case Path_ClosedFixed:
            ePath = Path_ClosedVariable;
            break;

        case Path_Fixed:
            ePath = Path_Variable;
            break;
        } // switch
    }

    html_log_format(4, L"<li>~A</li>~%", k_rgwszPath[ePath]);

    html_log_format(4, L"</ul></li>~%");

    ASSERT(Path_None != ePath);
    return ePath;
} // UpVarPass::analyze_path_aux


//////////////////////////////////////////////////////////////////////
//
// UpVarPass::dump_analysis
//
void
UpVarPass::dump_analysis(Module* pModule)
{
    static const char16* rgwszStorage[Variable::Storage_MAX_1];
        rgwszStorage[Variable::Storage_Heap]     = L"heap";
        rgwszStorage[Variable::Storage_Literal]  = L"literal";
        rgwszStorage[Variable::Storage_Register] = L"regiser";
        rgwszStorage[Variable::Storage_Stack]    = L"stack";


    html_log_format(2, L"<table border='1'>~%");
    foreach (Module::EnumFunction, oEnum, pModule)
    {
        Function* pFun = oEnum.Get();
        FunExt* pFunExt = pFun->GetExtension<FunExt>();

        if (pFunExt->m_oHomes.IsEmpty())
        {
            continue;
        }

        html_log_format(2, L"<tr><td>~S</td>~%", pFun);
        html_log_format(2, L"<td><table border='1'>~%");
        html_log_format(2, L"<tr>");
            html_log_format(2, L"<th>Holder</th>");
            html_log_format(2, L"<th>Path</th>");
            html_log_format(2, L"<th>UpVars</th>");
        html_log_format(2, L"</tr>~%");

        foreach (HomeList::Enum, oEnum, &pFunExt->m_oHomes)
        {
            Home* pHome = oEnum.Get();

            html_log_format(2, L"<tr>");
            html_log_format(2, L"<td>~S</td>", pHome->m_pHolder);
            html_log_format(2, L"<td>~A</td>", k_rgwszPath[pHome->m_ePath]);

            html_log_format(2, L"<td><table border='1'>~%");
            html_log_format(2, L"<tr>");
                html_log_format(2, L"<th>Name</th>");
                html_log_format(2, L"<th>Storage</th>");
                html_log_format(2, L"<th>Access</th>");
                html_log_format(2, L"<th>R/W</th>");
            html_log_format(2, L"</tr>~%");


            foreach (UpVarList::Enum, oEnum, &pHome->m_oUpVars)
            {
                UpVar* pUpVar = oEnum.Get();

                html_log_format(2, L"<tr>");
                html_log_format(2, L"<td>~W</td>", 
                    pUpVar->m_pVar->GetName());
                html_log_format(2, L"<td>~A</td>", 
                    rgwszStorage[pUpVar->m_pVar->GetStorage()] );
                html_log_format(2, L"<td>~A</td>",
                    k_rgwszAccess[pUpVar->m_eAccess] );
                html_log_format(2, L"<td>~A</td>", 
                    pUpVar->m_fReadOnly ? L"R/O" : L"R/W" );
                html_log_format(2, L"</tr>~%");
            } // for each upvar

            html_log_format(2, L"</table></td>~%");

            html_log_format(2, L"</tr>~%");
        } // for each home

        html_log_format(2, L"</table></td></tr>~%");
    } // for each fun
    html_log_format(2, L"</table>~%");
} // UpVarPass::dump_analysis


//////////////////////////////////////////////////////////////////////
//
// UpVarPass::find_home
//
UpVarPass::Home*
UpVarPass::find_home(Function* pFun, Function* pOwner)
{
    foreach (HomeList::Enum, oEnum, &pFun->GetExtension<FunExt>()->m_oHomes)
    {
        Home* pHome = oEnum.Get();
        if (pHome->m_pOwner == pOwner)
        {
            return pHome;
        }
    } // for each home

    return NULL;
} // UpVarPass::find_home


//////////////////////////////////////////////////////////////////////
//
// UpVarPass::find_upvar
//
UpVarPass::UpVar*
UpVarPass::find_upvar(Function* pCallee, Variable* pVar)
{
    foreach (
        HomeList::Enum,
        oEnum,
        &pCallee->GetExtension<FunExt>()->m_oHomes )
    {
        Home* pHome = oEnum.Get();
        foreach (UpVarList::Enum, oEnum, &pHome->m_oUpVars)
        {
            UpVar* pUpVar = oEnum.Get();
            if (pUpVar->m_pVar == pVar)
            {
                return pUpVar;
            }
        } // for each upvar
    } // for each home
    return NULL;
} // UpVarPass::find_upvar


//////////////////////////////////////////////////////////////////////
//
// UpVarPass::get_base
//
Register*
UpVarPass::get_base(
    Function*       pFun,
    Variable*       pVar,
    Instruction*    pInsn )
{
    if (pVar->GetOwner() == pFun || pFun->IsClosure())
    {
        FunExt* pFunExt = pFun->GetExtension<FunExt>();
        if (NULL == pFunExt->m_pSp)
        {
            pFunExt->m_pSp = new Physical(cm_get_target()->GetSP());

            Instruction* pPrologue = pFun->GetPrologueInsn();

            ir_insert_insn(
                new FrameRefInsn(pFunExt->m_pSp, pFun),
                pPrologue->GetNext() );
        } // if

        return pFunExt->m_pSp;
    }
    else
    {
        UpVar* pUpVar = get_upvar(pFun, pVar);

        switch (pUpVar->m_eAccess)
        {
        case Access_Base:
        case Access_Vector:
            ASSERT(NULL != pUpVar->m_pHome->m_pBase);
            return pUpVar->m_pHome->m_pBase;

        case Access_Sp:
        {
            Register* pBase = new Register();
            ir_insert_insn(
                new FrameRefInsn(pBase, pUpVar->m_pHome->m_pHolder),
                pInsn );
            return pBase;
        } // Access_Sp

        case Access_Closed:
        case Access_Register:
        default:
            CAN_NOT_HAPPEN();
        } // switch access
    } // if
} // get_base


//////////////////////////////////////////////////////////////////////
//
// UpVarPass::get_upvar
//
UpVarPass::UpVar*
UpVarPass::get_upvar(Function* pCallee, Variable* pVar)
{
    UpVar* pUpVar = find_upvar(pCallee, pVar);
    ASSERT(NULL != pUpVar);
    return pUpVar;
} // UpVarPass::get_upvar


//////////////////////////////////////////////////////////////////////
//
// UpVarPass::insert_cell
//
//  Called by:
//      UpVarPass::update_call_site
//
//  Description:
//   Inserts load cell into register instructions at pInsn.
//
Operand*
UpVarPass::insert_cell(
    Function*       pCaller,
    Variable*       pVar,
    Instruction*    pInsn )
{
    if (pVar->GetOwner() == pCaller)
    {
        // Load value from cell.
        Instruction* pVarDef = pVar->GetDfn();

        switch (pVar->GetStorage())
        {
        case Variable::Storage_Literal:
        case Variable::Storage_Heap:
            // VARDEF returns cell.
            return pVarDef->GetRd();

        case Variable::Storage_Stack:
        {
            // Address of stack-cell.
            // SLOT (ptr t) %ptr <= stack-cell value %cell
            Register* pPtr = new Register();
            ir_insert_insn(
                new SlotInsn(ty_ptr_t, pPtr,
                    NewLiteral(Qc6_stack_cell), NewLiteral(Qvalue),
                    pVarDef->GetRd() ),
                pInsn );
            return pPtr;
        } // Storage_Stack

        default:
            CAN_NOT_HAPPEN();
        } // switch storage
    }
    else if (pCaller->IsClosure())
    {
        UpVar* pUpVar = get_upvar(pCaller, pVar);

        html_log_format(4, L"~S: ~S:~S~:%",
            pCaller,
            pUpVar->m_pUpVarDef->GetBBlock(),
            pUpVar->m_pUpVarDef );

        switch (pVar->GetStorage())
        {
        case Variable::Storage_Heap:
            return pUpVar->m_pUpVarDef->GetRd();

        case Variable::Storage_Literal:
        {
            // Load content of literal-cell.
            Register* pPtr = new Register();
            ir_insert_insn(
                new SlotInsn(ty_ptr_t, pPtr,
                    NewLiteral(pUpVar->m_pUpVarDef->GetTy()),
                    NewLiteral(Qvalue),
                    pUpVar->m_pUpVarDef->GetRd() ),
                pInsn );

            Register* pRd = new Register();
            ir_insert_insn(new LoadInsn(pRd, pPtr), pInsn);

            return pRd;
        } // Variable::Storage_Literal

        default:
            CAN_NOT_HAPPEN();
        } // switch storage
    }
    else
    {
        UpVar* pUpVar = get_upvar(pCaller, pVar);
        Val cell_ty = pUpVar->m_pUpVarDef->GetTy();
        Register* pPtr = NULL;

        if (ty_c6_literal_cell == cell_ty)
        {
            cell_ty = ty_t;
        }

        switch (pUpVar->m_eAccess)
        {
        case Access_Base:
        case Access_Sp:
            // [base+n] contains cell.
            pPtr = new Register();
            ir_insert_insn(
                new VarRefInsn(ty_make_ptr(cell_ty), pPtr,
                    pUpVar->m_pHome->m_pBase,
                    pUpVar->m_pUpVarDef->GetRd() ),
                pInsn );
            break;

        case Access_Register:
            // Parameter register contains cell.
            return intern_cell_param(pCaller, pUpVar);

        case Access_Vector:
            #if 0
                // [vector+n] contains cell.
                pPtr = new Register();
                ir_insert_insn(
                    new VecRefInsn(ty_make_ptr(cell_ty), pPtr, 
                        pUpVar->m_pHome->m_pBase,
                        pUpVar->m_nVecIdx ),
                    pInsn );
                break;
            #else
                return intern_cell_param(pCaller, pUpVar);
            #endif

        default:
            warn(L"Failed to analyze free variable ~S.",
                pUpVar->m_pVar->GetName() );
            return NewLiteral(nil);
        } // switch path

        Register* pCell = new Register();
        ir_insert_insn(new LoadInsn(pCell, pPtr), pInsn);

        return pCell;
    } // if
} // UpVarPass::insert_cell


//////////////////////////////////////////////////////////////////////
//
// UpVarPass::intern_stack_slot
//
//  Description:
//   Inserts STACKDEF ty %rd <= %cell after PROJECT.
//
Register*
UpVarPass::intern_stack_slot(Function* pCaller, Variable* pVar)
{
    ASSERT(pCaller->IsClosure());

    UpVar* pUpVar = get_upvar(pCaller, pVar);

    if (NULL != pUpVar->m_pRx)
    {
        return pUpVar->m_pRx;
    }

    Register* pRx = new Register();

    pUpVar->m_pRx = pRx;

    pRx->SetStorage(Register::Storage_Stack);
    pRx->SetVar(pVar);

    Instruction* pInsn = pCaller->GetPrologueInsn()->GetNext();
    while (pInsn->Is<ProjectInsn>() || pInsn->Is<CountInsn>())
    {
        pInsn = pInsn->GetNext();
    } // while

    ir_insert_insn(
        new StackDefInsn(
            pUpVar->m_pUpVarDef->GetTy(),
            pRx,
            pUpVar->m_pUpVarDef->GetRd(),
            pUpVar->m_pUpVarDef->GetRd() ), // BUGBUG: Load upvar
        pInsn );

    return pRx;
} // UpVarPass::intern_stack_slot


//////////////////////////////////////////////////////////////////////
//
// UpVarPass::intern_cell_param
//
//  Description:
//    Makes pFun takes value or cell as parameter.
//
Register*
UpVarPass::intern_cell_param(Function* pFun, UpVar* pUpVar)
{
    if (NULL == pUpVar->m_pRx)
    {
        Register* pRd = new Register(pUpVar->m_pVar);
        pUpVar->m_pRx = pRd;
        Ty ty = ty_ptr_t;
        switch (pUpVar->m_eAccess)
        {
        case Access_Vector:
            ty = pUpVar->m_pVar->GetTy();
            break;
        } // switch access
        ir_add_extra_param(pFun, ty, pUpVar->m_pRx);
    }

    return pUpVar->m_pRx;
} // UpVarPass::intern_cell_param


//////////////////////////////////////////////////////////////////////
//
// UpVarPass::patch_optional
//
void
UpVarPass::patch_optional(Function* pFun)
{
    ASSERT(NULL != pFun);
    if (0 == pFun->m_cExtraParams) return;
    if (pFun->GetArityMin() == pFun->GetArityMax()) return;

    Register* pRegN = NULL;
    {
        PrologueInsn* pPrologue = pFun->GetPrologueInsn();
        foreach (Values::Enum, oEnum, pPrologue->GetVd())
        {
            CountInsn* pCount = oEnum.Get()->GetInstruction()->
                DynamicCast<CountInsn>();
            if (NULL != pCount)
            {
                pRegN = pCount->GetRd();
                break;
            }
        } // for each use
    } // pRegN
    if (NULL == pRegN) return;

    html_log_format(3, L"Patch &amp;optional: ~S with ~S~:%", pFun, pRegN);

    foreach (Register::Enum, oEnum, pRegN)
    {
        Instruction* pCmp = oEnum.Get()->GetInstruction();
        switch (pCmp->GetOpcode())
        {
        case IrOp_EQ: case IrOp_NE:
        case IrOp_LT: case IrOp_LE:
        case IrOp_GT: case IrOp_GE:
            html_log_format(4, L"patch ~S ~S => ", pCmp->GetBBlock(), pCmp);
            pCmp->GetOperandBox(1)->Replace(
                NewLiteral(add_xx(pCmp->GetLy(), pFun->m_cExtraParams)) );
            html_log_format(4, L" ~S~:%", pCmp);
            break;

        default:
            CAN_NOT_HAPPEN();
        } // switch opcode
    } // for each use
} // UpVarPass::patch_optional


// need_split_p
//  Returns true if
//    o Closuer is called.
//    o Closure has OPENBLOCK, OPENCATCH or OPENTAGBODY.
static bool need_split_p(Function* pFun)
{
    unless (pFun->HasUpVar()) return false;

    // Note: Since finally procedure can't be closure, we do
    // need to check use site is OPENFINALLY,.
    bool fHasUseSite = false;
    foreach (Function::EnumUseSite, oEnum, pFun)
    {
        if (! oEnum.Get()->GetInstruction()->Is<OpenFinallyInsn>())
        {
            fHasUseSite = true;
            break;
        }
    } // for each use site

    unless (fHasUseSite) return false;
    if (pFun->HasCallSite()) return true;

    uint cInsns = 0;
    foreach (Function::EnumBBlock, oEnum, pFun)
    {
        BBlock* pBBlock = oEnum.Get();
        foreach (BBlock::EnumInsn, oEnum, pBBlock)
        {
            Instruction* pInsn = oEnum.Get();
            cInsns += 1;

            when (cInsns > MaxClosureInsns) return true;

            switch (pInsn->GetOpcode())
            {
            case IrOp_OPENBLOCK:
            case IrOp_OPENCATCH:
            case IrOp_OPENTAGBODY:
                return true;
            } // switch opcode
        } // for each insn
    } // for each bblock

    return false;
} // need_split_p


//////////////////////////////////////////////////////////////////////
//
// UpVarPass::process_module
//
void
UpVarPass::process_module(Module* pModule)
{
    html_log_format(2, L"<h2>Analysis</h2>~%");

    m_pModule = pModule;

    {
        WorkList_<Function> oFuns;

        foreach (Module::EnumFunction, oEnum, pModule)
        {
            Function* pFun = oEnum.Get();
            oFuns.Push(pFun);
        } // for each fun

        while (! oFuns.IsEmpty())
        {
            Function* pFun = oFuns.Pop();

            update_restify(pFun);

            if (need_split_p(pFun))
            {
                html_log_format(4, L"split ~S~:%", pFun);
                ir_split_closure(pFun);
            } // if
        } // for each fun
    }

    html_log_format(2, L"<h3>Local Analysis</h3>~%");
    analyze_in_local(pModule);
    dump_analysis(pModule);

    html_log_format(2, L"<h3>Global Analysis</h3>~%");
    analyze_in_global(pModule);
    dump_analysis(pModule);

    html_log_format(2, L"<h3>Target Analysis</h3>~%");
    analyze_by_target(pModule);
    dump_analysis(pModule);

    html_log_format(2, L"<h2>Update</h2>~%");

    foreach (Module::EnumFunction, oEnum, pModule)
    {
        Function* pFun = oEnum.Get();

        if (pFun->IsClosure())
        {
            html_log_format(2, L"<h3>Update reference in closure ~S</h3>~%",
                pFun );

            update_upvar_in_closure(pFun);
        }
        else if (pFun->HasUpVar())
        {
            html_log_format(2, L"<h3>Update reference in inner ~S</h3>~%",
                pFun );

            update_upvar_in_inner(pFun);
        } // if
    } // for each fun

    foreach (Module::EnumFunction, oEnum, pModule)
    {
        Function* pFun = oEnum.Get();

        html_log_format(2, L"<h3>Update call site to ~S</h3>~%", pFun);

        foreach (Function::EnumCallSite, oEnum, pFun)
        {
            update_call_site(oEnum.Get()->GetInstruction());
        } // for each call site

        foreach (Function::EnumUseSite, oEnum, pFun)
        {
            update_call_site(oEnum.Get()->GetInstruction());
        } // for each call site
    } // for each fun

    // Remove useless instructions
    foreach (Module::EnumFunction, oEnum, pModule)
    {
        Function* pFun = oEnum.Get();
        html_log_format(2, L"<h3>Clean ~S</h3>~%", pFun);

        patch_optional(pFun);
        ir_remove_useless_instructions(pFun);
        pFun->Reset();
    } // for each fun
} // UpVarPass::process_module


//////////////////////////////////////////////////////////////////////
//
// UpVarPass::insert_cvector
//
//  For Access_Vector
//
Register*
UpVarPass::insert_cvector(
    Operand**       prgpCell,
    uint            cCells,
    Instruction*    pInsn)
{
    Register* pRd = new Register();

    ir_insert_insn(
        new StackVecInsn(pRd, prgpCell, cCells),
        pInsn );

    return pRd;
} // UpVarPass::insert_cvector


//////////////////////////////////////////////////////////////////////
//
// UpVarPass::update_call_site
//
// Description:
//  o Inserts extra arguments to VALUES instructions.
//  o Processes upvars in provider's point of view.
//
//      VALUES ty %vx <= %sx %sy
//      CALL %rd <= #'foo %vx
//     ==>
//      SLOT (ptr t) %ptr <= si:closed-cell si:value %cell
//      LOAD extra1 <= %ptr
//      VALUES ty %vx <= extra1 extar2... %sx %sy
//      CALL %rd <= #'foo %vx
//      USE %r1     -- for base and sp
//
void
UpVarPass::update_call_site(Instruction* pInsn)
{
    switch (pInsn->GetOpcode())
    {
    case IrOp_CALL:
    case IrOp_CLOSURE:
    case IrOp_OPENFINALLY:
        break;
    default:
        return;
    } // switch opcode

    Function* pCallee = pInsn->GetSx()->StaticCast<Function>();

    unless (pCallee->HasUpVar())
    {
        return;
    }

    Function* pCaller = pInsn->GetBBlock()->GetFunction();

    html_log_format(3, L"<h4>Update ~S: ~S:~S</h4>~%",
        pCaller, pInsn->GetBBlock(), pInsn );

    ValuesInsn* pValuesI;
    {
        Values* pVy = pInsn->GetSy()->StaticCast<Values>();
        Instruction* pDefI = pVy->GetDfn();

        if (pDefI->Is<ValuesInsn>())
        {
            pValuesI = pDefI->StaticCast<ValuesInsn>();
        }
        else if (pDefI->Is<ValuesAInsn>())
        {
            pValuesI = pDefI->StaticCast<ValuesAInsn>();
        }
        else if (pDefI->Is<PrologueInsn>())
        {
            // The function must be inner function splitted closure.
            pValuesI = NULL;
        }
        else
        {
            error(L"Invalid CALL instruction.");
            return;
        }
    } // pValuesI

    uint cExtras = 0;
    //uint cElts = 0;

    html_log_format(3, L"<ol>~%");

    foreach (
        HomeList::Enum,
        oEnum,
        &pCallee->GetExtension<FunExt>()->m_oHomes )
    {
        Home* pHome = oEnum.Get();
        bool fInsertBase = false;
        foreach (UpVarList::Enum, oEnum, &pHome->m_oUpVars)
        {
            UpVar* pUpVar = oEnum.Get();
            Variable* pVar = pUpVar->m_pVar;

            html_log_format(4, L"<li>process ~S ~A~:%",
                pVar, k_rgwszAccess[pUpVar->m_eAccess] );

            // How does callee access upvar?
            switch (pUpVar->m_eAccess)
            {
            case Access_Base:
                if (NULL == pValuesI)
                {
                    error(L"Can't use base access path in ~S.",
                        pCaller->GetName() );
                    break;
                }

                if (! fInsertBase)
                {
                    Register* pBase = get_base(pCaller, pVar, pValuesI);
                    fInsertBase = true;
                    pValuesI->InsertBefore(pBase, cExtras);
                    cExtras += 1;
                }
                goto insert_use;

            case Access_Sp:
                goto insert_use;

            insert_use:
                if (pCaller == pVar->GetOwner())
                {
                    Register* pRd = pVar->GetDfn()->GetRd();
                    pRd->SetStorage(Register::Storage_Stack);
                    ir_insert_insn(new UseInsn(pRd), pInsn->GetNext());
                }
                else if (pCaller->IsClosure())
                {
                    Register* pRx = intern_stack_slot(pCaller, pVar);
                    ir_insert_insn(new UseInsn(pRx), pInsn->GetNext());
                } // if
                break;

            case Access_Closed:
            case Access_Register:
                if (NULL == pValuesI)
                {
                    error(L"Can't use closed/register access path in ~S.",
                        pCaller->GetName() );
                    break;
                }

                pValuesI->InsertBefore(
                    insert_cell(pCaller, pVar, pValuesI),
                    cExtras );
                cExtras += 1;
                break;

            case Access_Vector:
                if (NULL == pValuesI)
                {
                    error(L"Can't use vector access path in ~S.",
                        pCaller->GetName() );
                    break;
                }

                #if 0
                    m_prgpVector[cElts] = insert_cell(pCaller, pVar, pValuesI);
                    cElts += 1;
                #else
                {
                    Operand* pSx = insert_cell(pCaller, pVar, pValuesI);
                    if (NULL == pSx)
                    {
                        html_log_format(1,
                            L"<div style='color:red'>"
                            L"Failed to locate ~S."
                            L"</div>~%",
                            pVar );

                        warn(L"Failed to locate free variable ~S.",
                            pVar->GetName() );

                        pSx = NewLiteral(nil);
                    }
                    pValuesI->InsertBefore(pSx, cExtras);
                    cExtras += 1;
                }
                #endif
                break;

            default:
                CAN_NOT_HAPPEN();
            } // switch access

            html_log_format(4, L"</li>~%");
        } // for each upvar
    } // for each owner

    html_log_format(3, L"</ol>~%");
    html_log_format(3, L"Updated call site: ~S~:%", pValuesI);

    #if 0
        if (cElts >= 1)
        {
            // BUGBUG: NYI: share vector among call sites of same callee. We
            // can put vector on least common dominator block.
            pValuesI->InsertBefore(
                insert_cvector(m_prgpVector, cElts, pValuesI),
                cExtras );
            cExtras += 1;
        } // if
    #endif

    // calle must have upvar.
} // UpVarPass::update_call_site

//////////////////////////////////////////////////////////////////////
//
// UpVarPass::update_restify
//
// Description:
//  Set nil to restify operand of PROLOGUE instruction if function doesn't
//  use rest parameter.
//
void UpVarPass::update_restify(Function* pFun)
{
    if (! pFun->HasRestParam())
    {
        return;
    }

    PrologueInsn* pPrologue = pFun->GetPrologueInsn();

    Val max_param = Fixnum::Encode(pFun->GetArityMax());

    Val restify = Knone;

    if (NULL != pPrologue->GetVd())
    {
        foreach (Values::EnumUseSite, oEnum, pPrologue->GetVd())
        {
            Instruction* pProject = oEnum.Get()->GetInstruction()->
                DynamicCast<ProjectInsn>();

            if (NULL != pProject)
            {
                if (pProject->GetLy() == max_param)
                {
                    restify = pPrologue->GetLy();
                    break;
                }
            }
        } // for each use site
    } // if

    pPrologue->GetOperandBox(1)->Replace(NewLiteral(restify));
} // UpVarPass::update_restify


//////////////////////////////////////////////////////////////////////
//
// UpVarPass::update_upvar_in_closure
//
//  Note: instruction selection pass replaces closed variable references
//  with target specific operand.
void UpVarPass::update_upvar_in_closure(Function*)
{
    // nothing to do
} // UpVarPass::update_upvar_in_closure


//////////////////////////////////////////////////////////////////////
//
// UpVarPass::update_upvar_in_inner
//
// Description:
//  o Replace upvar references based on upvar's access method.
//  o Assign register for upvar passed by parameter.
//
void UpVarPass::update_upvar_in_inner(Function* pFun)
{
    FunExt* pFunExt = pFun->GetExtension<FunExt>();

    foreach (HomeList::Enum, oEnum, &pFunExt->m_oHomes)
    {
        Home* pHome = oEnum.Get();

        ASSERT(NULL == pHome->m_pBase);

        switch (pHome->m_ePath)
        {
        case Path_Closure:
            break;

        case Path_Fixed:
        {
            // Insert FRAMEREF after PROLOGUE
            pHome->m_pBase = new Physical(cm_get_target()->GetSP());

            Instruction* pPrologue = pFun->GetPrologueInsn();

            ir_insert_insn(
                new FrameRefInsn(pHome->m_pBase, pHome->m_pHolder),
                pPrologue->GetNext() );
            break;
        }

        case Path_Mixed:
            break;

        case Path_Variable:
        {
            // Intern base param for upvar
            pHome->m_pBase = new Register();
            ir_add_extra_param(pFun, ty_ptr_t, pHome->m_pBase);

            html_log_format(3, L"insert base ~S for ~S~:%",
                pHome->m_pBase,
                pHome->m_pHolder );
            break;
        } // Path_Mixed, Path_Variable

        default:
            CAN_NOT_HAPPEN();
        } // switch path
    } // for each home

    foreach (HomeList::Enum, oEnum, &pFunExt->m_oHomes)
    {
        Home* pHome = oEnum.Get();
        foreach (UpVarList::Enum, oEnumHome, &pHome->m_oUpVars)
        {
            UpVar* pUpVar = oEnumHome.Get();

            html_log_format(3, L"process upvar ~S~:%", pUpVar->m_pVar);

            // For passing mixed upvar as parameter.
            if (pHome->m_ePath == Path_Mixed)
            {
                intern_cell_param(pFun, pUpVar);
            }

            Register::EnumUseSite oEnum(pUpVar->m_pUpVarDef->GetRd());
            while (! oEnum.AtEnd())
            {
                OperandBox* pBox = oEnum.Get();
                    oEnum.Next();

                Instruction* pInsn = pBox->GetInstruction();

                switch (pInsn->GetOpcode())
                {
                case IrOp_SLOT:
                    update_upvar_in_inner(
                        pUpVar,
                        pInsn->StaticCast<SlotInsn>() );
                    break;

                case IrOp_VARREF:
                    break;

                default:
                    CAN_NOT_HAPPEN();
                } // switch opcode
            } // for each use site of UPVAR
        } // for each upvar
    } // for each home
} // UpVarPass::update_upvar_in_inner


//////////////////////////////////////////////////////////////////////
//
// UpVarPass::update_upvar_in_inner
//
// Description:
//  o Processes upvar in consumer's point of view.
//  o Inserts VARREF/VECREF before SLOT.
//
void
UpVarPass::update_upvar_in_inner(
    UpVar*      pUpVar,
    SlotInsn*   pSlot )
{
    Val cell_ty     = pUpVar->m_pUpVarDef->GetTy();
    Val ptr_cell_ty = ty_make_ptr(cell_ty);

    Register* pRy  = pUpVar->m_pUpVarDef->GetRd();
    Function* pFun = pSlot->GetBBlock()->GetFunction();

    pRy->SetStorage(Register::Storage_Pseudo);

    Register* pCell = new Register();
    Register* pPtr  = NULL;
    Register* pBase = pUpVar->m_pHome->m_pBase;

    switch (pUpVar->m_eAccess)
    {
    case Access_Base:
        // [base+n] contains cell.
        pPtr = new Register();
        ir_insert_insn(
            new VarRefInsn(ptr_cell_ty, pPtr, pBase, pRy),
            pSlot );
        break;

    case Access_Sp:
        // [sp+n] contains cell.
        pPtr = new Register();
        ir_insert_insn(
            new VarRefInsn(ptr_cell_ty, pPtr, pBase, pRy),
            pSlot );
        break;

    case Access_Register:
        // Get cell from parameter register.
        pCell = intern_cell_param(pFun, pUpVar);
        break;

    case Access_Vector:
        #if 0
            // Vector element contains cell
            pPtr = new Register();
            ir_insert_insn(
                new VecRefInsn(ptr_cell_ty, pPtr,
                    pBase, pUpVar->m_nVecIdx ),
                pSlot );
        #else
            pCell = intern_cell_param(pFun, pUpVar);
        #endif
        break;

    default:
        CAN_NOT_HAPPEN();
    } // switch eAccess

    if (NULL != pPtr)
    {
        ir_insert_insn(new LoadInsn(pCell, pPtr), pSlot);
    }

    pSlot->GetOperandBox(2)->Replace(pCell);
} // UpVarPass::update_upvar_in_inner

} // Compiler
