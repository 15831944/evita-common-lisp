

#if 0
//////////////////////////////////////////////////////////////////////
//
// get_nth_param
//
static Register* get_nth_param(Function* pFun, int iNth)
{
    Values* pVx = pFun->GetPrologueInsn()->GetVd();
    if (NULL == pVx)
    {
        // All parameters aren't used.
        return NULL;
    }

    foreach (Values::EnumUseSite, oEnum, pVx)
    {
        ProjectInsn* pProject = oEnum.Get()->GetInstruction()->
            DynamicCast<ProjectInsn>();
        if (NULL == pProject) continue;
        if (pProject->GetLy() == Fixnum::Encode(iNth))
        {
            return pProject->GetRd();
        }
    } // for each use
    return NULL;
} // get_nth_param


//////////////////////////////////////////////////////////////////////
//
// update_nth_arg_type
//
static void update_nth_arg_type(
    Function*   pFun,
    int         iNth,
    Ty          newty )
{
    foreach (Function::EnumCallSite, oEnum, pFun)
    {
        CallInsn* pCall = oEnum.Get()->GetInstruction()->
            StaticCast<CallInsn>();

        ValuesInsn* pArgs = pCall->GetVy()->GetDfn()->
            DynamicCast<ValuesInsn>();
        if (NULL == pArgs) continue;

        OperandBox* pBox = pArgs->GetOperandBox(iNth);
        Register* pR1 = pBox->GetOperand()->DynamicCast<Register>();
        if (NULL == pR1) continue;
        if (Subtypep_Yes == ty_subtypep(pR1->GetTy(), newty))
        {
            continue;
        }

        Instruction* pDfn = pR1->GetDfn();
        switch (pDfn->GetOpcode())
        {
        // FIXME 2007-04-20: If Dfn is CALL, we should do backward type
        // inference.
        case IrOp_RUNTIMECAST:
        case IrOp_SIGMA:
            pDfn->SetTy(newty);
            break;

        default:
        {
            Register* pR2 = new Register;
            Instruction* pArgCast = new RuntimeCastInsn(
                newty, pR2, pR1,
                pCall->GetFrame(),
                RuntimeCastInsn::Attr_Nop );

            ir_insert_insn(pArgCast, pArgs);
            pArgCast->SetIndex(pArgs->GetIndex());
            pBox->Replace(pR2);
            break;
        } // default
        } // switch opcode
        pArgs->UpdateTy();
        pCall->UpdateTy();
    } // for each call site
} // update_nth_arg_type

//////////////////////////////////////////////////////////////////////
//
//  update_lambda_list_type
//
static bool update_lambda_list_type(Function* pFun)
{
    int iMin = pFun->GetArityMin();
    if (0 == iMin) return false;

    bool fChanged = false;
    Val llrunner = second(pFun->GetTy());
    for (int iNth = 0; iNth < iMin; iNth++)
    {
        Register* pRx = get_nth_param(pFun, iNth);
        if (NULL == pRx)
        {
            // iNth parameter isn't used.
            continue;
        }

        foreach (Register::EnumUseSite, oEnum, pRx)
        {
            Instruction* pCast = oEnum.Get()->GetInstruction()->
                DynamicCast<RuntimeCastInsn>();

            if (NULL == pCast) continue;

            if (ir_dominate_p(pRx->GetDfn(), pCast))
            {
                Ty newty = pCast->GetTy();
                unless (ty_equal(first(llrunner), newty))
                {
                    update_nth_arg_type(pFun, iNth, newty);
                    setf_car(newty, llrunner);
                    fChanged = true;
                }
                break;
            }
        } // for each user

        llrunner = cdr(llrunner);
    } // for iNth

    return fChanged;
} // update_lambda_list_type
#endif
