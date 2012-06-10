#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - compiler - 3 Evaluation and Compilation
// cl_03_lambda.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cl/cl_03_lambda.cpp#26 $
//
#include "./cl_defs.h"
#include "../cm/cm_base.h"
#include "../ir/ir_instruction.h"

namespace Compiler
{

// elaborate_value_type
static Val elaborate_value_type(Function* pFun, Val value_ty)
{
    if (nil == value_ty) return value_ty;
    if (ty_void == value_ty) return value_ty;

    value_ty = nil;
    foreach (BBlock::EnumInEdge, oEnum, pFun->GetExitBB())
    {
        Instruction* pRet = oEnum.GetNode()->GetLastInsn();
        if (! pRet->Is<RetInsn>()) continue;

        Val ret_ty = pRet->GetSx()->GetTy();
        value_ty = ty_or_values(value_ty, ret_ty);
    } // for each edge

    return value_ty;
} // elaborate_value_type


// ClParser::computeFunctionType
Ty ClParser::computeFunctionType(const LambdaList* pLambdaList, Ty value_ty)
{
    Val anchor = list(nil);
    Val last = anchor;

    foreach (ParamList::Enum, oEnum, &pLambdaList->m_oReqs)
    {
        ParamDef* pParamDef = oEnum.Get();
        last = setf_cdr(list(pParamDef->GetTy()), last);
    } // for reqs

    if (! pLambdaList->m_oOpts.IsEmpty())
    {
        last = setf_cdr(list(QAoptional), last);
    }

    foreach (ParamList::Enum, oEnum, &pLambdaList->m_oOpts)
    {
        ParamDef* pParamDef = oEnum.Get();
        last = setf_cdr(list(pParamDef->GetTy()), last);
    } // for opts

    if (NULL != pLambdaList->m_pRest)
    {
        last = setf_cdr(list(QArest), last);
        last = setf_cdr(list(pLambdaList->m_pRest->GetTy()), last);
    }

    if (nil != pLambdaList->m_key)
    {
        last = setf_cdr(list(QAkey), last);
    }

    foreach (ParamList::Enum, oEnum, &pLambdaList->m_oKeys)
    {
        ParamDef* pParamDef = oEnum.Get();
        last = setf_cdr(
            list(pParamDef->m_key, pParamDef->GetTy()),
            last );
    } // for keys

    if (QAallow_other_keys == pLambdaList->m_key)
    {
        last = setf_cdr(list(QAallow_other_keys), last);
    }

    if (ty_void == value_ty) value_ty = ty_unspecified;

    return list(Qfunction, cdr(anchor), value_ty);
} // ClParser::computeFunctionType


//////////////////////////////////////////////////////////////////////
//
// ClParser::makeParamDef
//
ClParser::ParamDef*
ClParser::makeParamDef(Val name, Val initform)
{
    if (! symbolp(name))
    {
        warn(L"Parameter name must be a symbol instead of ~S.", name);
        name = make_symbol(L"err");
    }

    if (isSpecialVariable(name))
    {
        return new ParamDef(ParamDef::Kind_Special, name, NULL, initform);
    }
    else
    {
        Variable* pVar = newVariable(name);
        return new ParamDef(ParamDef::Kind_Lexical, name, pVar, initform);
    }
} // ClParser::makeParamDef


//////////////////////////////////////////////////////////////////////
//
// ClParser::makeVarDef
//
ClParser::VarDef*
ClParser::makeVarDef(Val name, Val initform)
{
    if (! symbolp(name))
    {
        warn(L"Vareter name must be a symbol instead of ~S.", name);
        name = make_symbol(L"err");
    }

    VarDef* pVarDef;

    if (isSpecialVariable(name))
    {
        pVarDef = new VarDef(
            VarDef::Kind_Special,
            name,
            NULL,
            initform );
    }
    else
    {
        pVarDef = new VarDef(
            VarDef::Kind_Lexical,
            name,
            newVariable(name),
            initform );
    } // if

    return pVarDef;
} // ClParser::makeVarDef


//////////////////////////////////////////////////////////////////////
//
// ClParser::parse_lambda
//
Function*
ClParser::parse_lambda(const Expect* pExpect, Val form)
{
    Function* pFun = NewFunction(val_anonymous);

    LexEnv oLexEnv(LexEnv::Kind_lambda, pFun, pExpect->Type);

    LambdaList oLambdaList;
    Val forms;
    {
        Context oContext(pFun, &oLexEnv);
        ContextScope oScope(this, &oContext);
        forms = parseLambdaList(&oLexEnv, form, &oLambdaList);
    } // forms

    processLambda(val_anonymous, pFun, &oLexEnv, &oLambdaList, forms);

    return pFun;
} // ClParser::parse_lambda


//////////////////////////////////////////////////////////////////////
//
//  ClParser::parseLambdaList
//
// Description:
//  Parsers lambda-list and declarations and returns the first form.
//
Val
ClParser::parseLambdaList(
    LexEnv*,//     pLexEnv,
    Val         form,
    LambdaList* pLambdaList )
{
    ASSERT(NULL != pLambdaList);

    ASSERT(GetLexEnv()->GetKind() == LexEnv::Kind_lambda);

    Session::Get()->RememberSource(form);

    int iMax = 0;
    int iMin = 0;

    {
        Val lambda_list = cadr(form);

        if (! listp(lambda_list))
        {
            warn(L"Invalid lambda list: ~S", lambda_list);
        }
        else
        {
            Val runner = lambda_list;

            enum
            {
                State_Required,
                State_Optional,
                State_Rest,
                State_RestAfter,
                State_Key,
                State_AllowOtherKeys,
                State_Aux
            } eState = State_Required;

            while (consp(runner))
            {
                Session::Get()->RememberSource(runner);

                Val token = car(runner);
                    runner = cdr(runner);

                switch (eState)
                {
                case State_Required:
                    if (QAoptional == token)
                    {
                        eState = State_Optional;
                    }
                    else if (QArest == token)
                    {
                        eState = State_Rest;
                    }
                    else if (QAkey == token)
                    {
                        pLambdaList->m_key = token;
                        eState = State_Key;
                    }
                    else if (QAaux == token)
                    {
                        eState = State_Aux;
                    }
                    else
                    {
                        ParamDef* pParamDef = makeParamDef(token);
                        pLambdaList->m_oReqs.Append(pParamDef);
                        addVarDcl(pParamDef);
                        iMin += 1;
                        iMax += 1;
                    }
                    break;

                case State_Optional:
                    if (QArest == token)
                    {
                        eState = State_Rest;
                    }
                    else if (QAkey == token)
                    {
                        pLambdaList->m_key = token;
                        eState = State_Key;
                    }
                    else if (QAaux == token)
                    {
                        eState = State_Aux;
                    }
                    else
                    {
                        Val name = Fixnum::Encode(0);
                        Val svar = nil;
                        Val initform = nil;

                        if (symbolp(token))
                        {
                            name = token;
                        }
                        else if (consp(token) && nil == cdr(token))
                        {
                            name = first(token);
                        }
                        else if (consp(token) && consp(cdr(token)) &&
                                 nil == cddr(token) )
                        {
                            name = first(token);
                            initform = second(token);
                        }
                        else if (consp(token) && consp(cdr(token)) &&
                                 consp(cddr(token)) &&
                                 nil == cdddr(token) )
                        {
                            name = first(token);
                            initform = second(token);
                            svar = third(token);

                            if (nil == svar)
                            {
                                warn(L"Can't use nil as variable.");
                            }
                        } // if

                        if (! symbolp(name) || ! symbolp(svar))
                        {
                            warn(L"Invalid &optional parameter: ~S", token);
                        }

                        ParamDef* pParamDef = makeParamDef(name, initform);
                        pLambdaList->m_oOpts.Append(pParamDef);
                        addVarDcl(pParamDef);
                        iMax += 1;

                        if (nil != svar)
                        {
                            VarDef* pSVarDef = makeVarDef(svar);
                            pParamDef->m_pSupVar = pSVarDef;
                            addVarDcl(pSVarDef);
                        }
                    }
                    break;

                case State_Rest:
                {
                    ParamDef* pParamDef = makeParamDef(token);
                    pLambdaList->m_pRest = pParamDef;
                    addVarDcl(pParamDef);

                    eState = State_RestAfter;
                    break;
                } // State_Rest

                case State_RestAfter:
                    if (QAkey == token)
                    {
                        pLambdaList->m_key = token;
                        eState = State_Key;
                    }
                    else if (QAaux == token)
                    {
                        eState = State_Aux;
                    }
                    else
                    {
                        warn(L"More than one symbol after &rest.");
                        runner = nil;
                    }
                    break;

                case State_Key:
                    if (QAallow_other_keys == token)
                    {
                        pLambdaList->m_key = token;
                        eState = State_AllowOtherKeys;
                    }
                    else if (QAaux == token)
                    {
                        eState = State_Aux;
                    }
                    else
                    {
                        Val name = Fixnum::Encode(0);
                        Val svar = nil;
                        Val initform = nil;

                        if (symbolp(token))
                        {
                            name = token;
                        }
                        else if (consp(token) && nil == cdr(token))
                        {
                            name = first(token);
                        }
                        else if (consp(token) && consp(cdr(token)) &&
                                 nil == cddr(token) )
                        {
                            name = first(token);
                            initform = second(token);
                        }
                        else if (consp(token) && consp(cdr(token)) &&
                                 consp(cddr(token)) &&
                                 nil == cdddr(token) )
                        {
                            name = first(token);
                            initform = second(token);
                            svar = third(token);

                            if (nil == svar)
                            {
                                warn(L"Can't use nil as variable.");
                            }
                        } // if

                        Val key;
                        if (symbolp(name))
                        {
                            key = intern(symbol_name(name), PACKAGE_keyword);
                        }
                        else if (consp(name) &&
                                 consp(cdr(name)) &&
                                 nil == cddr(name) )
                        {
                            key  = first(name);
                            name = second(name);
                        }
                        else
                        {
                            key = Fixnum::Encode(0);
                        } // if

                        if (! symbolp(name) ||
                            ! symbolp(key) ||
                            ! symbolp(svar) )
                        {
                            warn(L"Invalied keyword parameter ~S.", token);
                            name = make_symbol(L"err");
                            key  = name;
                        }

                        ParamDef* pParamDef = makeParamDef(name, initform);
                        pParamDef->m_key = key;
                        pLambdaList->m_oKeys.Append(pParamDef);
                        addVarDcl(pParamDef);

                        if (nil != svar)
                        {
                            VarDef* pSVarDef = makeVarDef(svar);
                            pParamDef->m_pSupVar = pSVarDef;
                            addVarDcl(pSVarDef);
                        }
                    }
                    break;

                case State_AllowOtherKeys:
                    if (QAaux == token)
                    {
                        eState = State_Aux;
                    }
                    else
                    {
                        warn(L"Extra name after &allow-other-keys: ~S", token);
                    }
                    break;

                case State_Aux:
                {
                    Val name = token;
                    Val initform = nil;

                    if (symbolp(token))
                    {
                        // nothing to do
                    }
                    else if (consp(token) && nil == cdr(token))
                    {
                        name = first(token);
                    }
                    else if (consp(token) && consp(cdr(token)) &&
                             nil == cddr(token) )
                    {
                        name = first(token);
                        initform = second(token);
                    }
                    else
                    {
                        warn(L"Invalid &aux parameter: ~S", token);
                    }

                    ParamDef* pParamDef = makeParamDef(name, initform);
                    pLambdaList->m_oAuxs.Append(pParamDef);
                    addVarDcl(pParamDef);
                    break;
                } // State_Aux

                default:
                    CAN_NOT_HAPPEN();
                    // NOTREACHED
                } // switch eState
            } // while

            if (State_Rest == eState)
            {
                warn(L"Missing symbol after &rest.");
            }
            else if (nil != runner)
            {
                warn(L"Malformed lambda list: ~S", lambda_list);
            }
        } // if
    }

    if (NULL == pLambdaList->m_pRest && nil != pLambdaList->m_key)
    {
        ParamDef* pParamDef = makeParamDef(make_symbol(L"rest"));
        pLambdaList->m_pRest = pParamDef;
        pParamDef->SetFlags(ParamDef::Flag_DynamicExtent);
        addVarDcl(pParamDef);
    }

    Function* pOwner = GetLexEnv()->GetOwner();

    pOwner->SetArity(iMin, iMax);
    pOwner->SetRestParam(NULL != pLambdaList->m_pRest);

    Val forms = parse_declarations(
        cddr(form),
        pOwner );

    pOwner->SetTy(computeFunctionType(pLambdaList, GetLexEnv()->GetTy()));

    deactivateLexEnv();

    return forms;
} // ClParser::parseLambdaList


//////////////////////////////////////////////////////////////////////
//
// ClParser::processLambda
//
void
ClParser::processLambda(
    Val                 block_name,
    Function*           pFun,
    LexEnv*             pLexEnv,
    const LambdaList*   pLambdaList,
    Val                 forms )
{
    ASSERT(NULL != pFun);
    ASSERT(NULL != pLexEnv);
    ASSERT(NULL != pLambdaList);

    Context oContext(pFun, pLexEnv);

    {
        ContextScope oScope(this, &oContext);

        if (processLambdaList(pFun, pLambdaList))
        {
            activateFreeDcls();

            if (val_anonymous == block_name)
            {
                Expect oExpect(pLexEnv->GetTy());
                parseForms(&oExpect, forms);
            }
            else
            {
                if (consp(pFun->GetName()) && function_name_p(pFun->GetName()))
                {
                    if (pLambdaList->m_oReqs.IsEmpty())
                    {
                        warn(L"~S must take at least one required parameter.",
                           pFun->GetName() );
                    }
                    else
                    {
                        pLexEnv->SetTy(
                            pLambdaList->m_oReqs.GetHead()->GetTy() );
                    }
                } // if

                Expect oExpect(ExpectContext_Value, pLexEnv->GetTy());
                parseBlockAux(&oExpect, block_name, forms);
            }
        } // if

        closeLexEnv();
    }

    if (! option_check_parameter_count(pFun))
    {
        pFun->GetEntryInsn()->GetOperandBox(0)->
            Replace(NewLiteral(Knone));
    }

    // Optmize SSA variable
    {
        Function::EnumVar oEnum(pFun);
        while (! oEnum.AtEnd())
        {
            Variable* pVar = oEnum.Get();
                oEnum.Next();
            optimize_VARDEF(pVar);
        } // for each var
    }

    ir_remove_useless_instructions(pFun);

    {
        Val value_ty = elaborate_value_type(pFun, pLexEnv->GetTy());
            ASSERT(consp(pFun->GetTy()));
            ASSERT(Fixnum::Encode(3) == length(pFun->GetTy()));

        //if (Qunspecified != value_ty)
        {
            ty_set_function_value(pFun->GetTy(), value_ty);
        }
    }
} // ClParser::processLambda


//////////////////////////////////////////////////////////////////////
//
// ClParser::processLambdaList
//
bool
ClParser::processLambdaList(
    Function*           pFun,
    const LambdaList*   pLambdaList )
{
    ASSERT(NULL != pFun);
    ASSERT(NULL != pLambdaList);

    BBlock* pSucc = GetContext()->GetSucc();

    Values* pVd = new Values();

    // PROLOGUE ty %vd <= %vx restify
    {
        Val ty = list(Qvalues);

        {
            Val last = ty;
            for (int i = 0; i < pFun->GetArityMin(); i++)
            {
                last = setf_cdr(list(t), last);
            } // for

            if (pFun->GetArityMin() != pFun->GetArityMax())
            {
                last = setf_cdr(list(QAoptional), last);
                for (int i = pFun->GetArityMin(); i < pFun->GetArityMax(); i++)
                {
                    last = setf_cdr(list(t), last);
                } // for
            } // if

            if (pFun->HasRestParam())
            {
                last = setf_cdr(list(QArest), last);
                last = setf_cdr(list(t), last);
            }
        }

        EntryInsn* pEntry = pFun->GetEntryInsn();

        // Enable check arity
        pEntry->GetOperandBox(0)->Replace(NewLiteral(t));

        Values* pVx = pEntry->GetVd();

        {
            Val restify = Knone;
            ParamDef* pRest = pLambdaList->m_pRest;
            if (NULL != pRest)
            {
                if (pRest->GetFlag(NameDcl::Flag_DynamicExtent))
                {
                    restify = Kstack;
                }
                else
                {
                    restify = Kheap;
                }
            } // if

            emitInsn(
                new PrologueInsn(ty, pVd, pVx, NewLiteral(restify)) );
        }
    }

    Register* pRn = NULL;
    {
        UINT nNth = 0;

        foreach (ParamList::Enum, oEnum, &pLambdaList->m_oReqs)
        {
            ParamDef* pParamDef = oEnum.Get();
            Register* pRx = new Register();
                pRx->SetVar(pParamDef->GetVar());

            pParamDef->m_pRx = pRx;
            emitInsn(new ProjectInsn(ty_t, pRx, pVd, nNth));

            nNth += 1;
        } // for each req

        foreach (ParamList::Enum, oEnum, &pLambdaList->m_oOpts)
        {
            ParamDef* pParamDef = oEnum.Get();
            Register* pRx = new Register();
                pRx->SetVar(pParamDef->GetVar());

            pParamDef->m_pRx = pRx;
            emitInsn(new ProjectInsn(ty_t, pRx, pVd, nNth));
            nNth += 1;
        } // for each optional

        if (! pLambdaList->m_oOpts.IsEmpty())
        {
            pRn = new Register();
            emitInsn(new CountInsn(pRn, pVd));
        } // if optional

        if (NULL != pLambdaList->m_pRest)
        {
            Register* pRx = new Register();
                pRx->SetVar(pLambdaList->m_pRest->GetVar());
            pLambdaList->m_pRest->m_pRx = pRx;
            emitInsn(new ProjectInsn(ty_list, pRx, pVd, nNth));
        }
    } // prepare


    // We'll share bind frame between req and rest.
    OpenBindInsn* pOpenBind = NULL;
    UINT nNth = 0;

    // Required parameters
    foreach (ParamList::Enum, oEnum, &pLambdaList->m_oReqs)
    {
        ParamDef* pParamDef = oEnum.Get();
        Register* pRx = emitRuntimeCast(pParamDef->GetTy(), pParamDef->m_pRx);
        pOpenBind = emitBind(pParamDef, pRx, pOpenBind);
        nNth += 1;
    } // for each required

    // Optional parameter
    if (! pLambdaList->m_oOpts.IsEmpty())
    {
        foreach (ParamList::Enum, oEnum, &pLambdaList->m_oOpts)
        {
            ParamDef* pParamDef = oEnum.Get();

            Bool* pBx = new Bool();
            emitInsn(new GtInsn(pBx, pRn, NewLiteral(nNth)));
                nNth += 1;

            pOpenBind = processLambdaListOptional(pSucc, pParamDef, pBx);
        } // for each param
    } // if optional

    // Rest parameter
    if (NULL != pLambdaList->m_pRest)
    {
        emitBind(
            pLambdaList->m_pRest,
            pLambdaList->m_pRest->m_pRx,
            pOpenBind );
    } // if rest

    // Keyword parameters
    if (nil != pLambdaList->m_key)
    {
        Val keys;
        {
            UINT cKeys = 0;
            foreach (ParamList::Enum, oEnum, &pLambdaList->m_oKeys)
            {
                cKeys += 1;
            } // for each key

            keys = make_vector(cKeys);
            UINT nNth = 0;
            foreach (ParamList::Enum, oEnum, &pLambdaList->m_oKeys)
            {
                ParamDef* pParamDef = oEnum.Get();
                setf_svref(pParamDef->m_key, keys, nNth);
                nNth += 1;
            } // for each key
        } // keys

        Values* pVkeys = new Values();
        emitInsn(new ParseKeysInsn(
            pVkeys,
            pLambdaList->m_pRest->m_pRx,
            pLambdaList->m_key,
            keys ) );

        Register* pRflags = new Register();
        emitInsn(new ProjectInsn(ty_fixnum, pRflags, pVkeys, 0));

        foreach (ParamList::Enum, oEnum, &pLambdaList->m_oKeys)
        {
            ParamDef* pParamDef = oEnum.Get();

            pParamDef->m_pRx = new Register();

            emitInsn(new KeyValInsn(
                pParamDef->m_pRx, pVkeys, pParamDef->m_key ) );
        } // for each key

        foreach (ParamList::Enum, oEnum, &pLambdaList->m_oKeys)
        {
            ParamDef* pParamDef = oEnum.Get();

            Bool* pBx = new Bool();
            emitInsn(new KeySuppliedInsn(pBx, pRflags, pParamDef->m_key));

            pOpenBind = processLambdaListOptional(pSucc, pParamDef, pBx);
        } // for each key
    } // if keyword

    // Auxilliary
    foreach (ParamList::Enum, oEnum, &pLambdaList->m_oAuxs)
    {
        VarDef* pVarDef = oEnum.Get();

        Expect oExpect(pVarDef);
        Operand* pSx = parseForm1(&oExpect, pVarDef->GetInitForm());

        if (Obj_Unreachable == pSx)
        {
            return false;
        }

        emitBind(pVarDef, pSx);
    } // for each var

    return true;
} // ClParser::processLambdaList


//////////////////////////////////////////////////////////////////////
//
// ClParser::processLambdaListOptional
//
OpenBindInsn*
ClParser::processLambdaListOptional(
    BBlock*     pSucc,
    ParamDef*   pParamDef,
    Bool*       pBx )
{
    ASSERT(NULL != pSucc);
    ASSERT(NULL != pParamDef);
    ASSERT(NULL != pBx);

    Register* pRd = new Register();
        pRd->SetVar(pParamDef->GetVar());

    Register* pRsup = NULL;

    Ty ty = ty_expand(pParamDef->GetTy());

    if (ty_t == ty && constantp(pParamDef->GetInitForm()))
    {
        Expect oExpect(pParamDef);
        Operand* pSz = parseForm1(&oExpect, pParamDef->GetInitForm());

        Register* pRy = pParamDef->m_pRx;
            emitInsn(new SelectInsn(pRd, pBx, pRy, pSz));

        if (NULL != pParamDef->m_pSupVar) pRsup = emitBool(pBx);
    }
    else
    {
        BBlock* pCmp  = GetContext()->GetCurr();
        BBlock* pNot  = newBBlock();
        BBlock* pJoin = newBBlock();
        emitInsn(new BranchInsn(pBx, pJoin, pNot));

        GetContext()->SetCurrSucc(pNot, pNot);

        Expect oExpect(pParamDef);
        Operand* pSnot = parseForm1(&oExpect, pParamDef->GetInitForm());

        if (Obj_Unreachable == pSnot)
        {
            GetContext()->SetCurrSucc(pJoin, pSucc);
        }
        else
        {
            BBlock* pNot = GetContext()->GetCurr();

            Register* pRgot = pParamDef->m_pRx;

            BBlock* pGot;
            {
                if (ty == ty_t)
                {
                    pGot = pCmp;
                    emitInsn(new JumpInsn(pJoin));
                }
                else
                {
                    pGot = pJoin;

                    pJoin = newBBlock();
                    emitInsn(new JumpInsn(pJoin));

                    GetContext()->SetCurrSucc(pGot, pSucc);
                    pRgot = emitRuntimeCast(ty, pRgot);
                    emitInsn(new JumpInsn(pJoin));
                } // if
            }

            GetContext()->SetCurrSucc(pJoin, pSucc);

            {
                PhiInsn* pPhi = new PhiInsn(pParamDef->GetTy(), pRd);
                emitInsn(pPhi);

                pPhi->AddInput(pGot, pRgot);
                pPhi->AddInput(pNot, pSnot);

                //pPhi->UpdateTy();
            }

            if (NULL != pParamDef->m_pSupVar)
            {
                pRsup = new Register();
                PhiInsn* pPhi = new PhiInsn(ty_boolean, pRsup);
                emitInsn(pPhi);

                pPhi->AddInput(pGot, Obj_True);
                pPhi->AddInput(pNot, Obj_Nil);

                //pPhi->UpdateTy();
            }
        }
    } // if initform

    OpenBindInsn* pOpenBind = emitBind(pParamDef, pRd);

    if (NULL != pParamDef->m_pSupVar)
    {
        VarDef* pSVar = pParamDef->m_pSupVar;

        // BUGBUG: NYI: recover linenumber for optional/key parameter.
        Expect oExpect(pSVar);
        if (! ty_typep(nil, pSVar->GetTy()))
        {
            unexpected_type(&oExpect, Qnull, nil);
        }
        else if (! ty_typep(t, pSVar->GetTy()))
        {
            unexpected_type(&oExpect, ty_eql_t, t);
        }

        pOpenBind = emitBind(pSVar, pRsup, pOpenBind);
    } // if

    return pOpenBind;
} // ClParser::processLambdaListOptional

} // Compiler
