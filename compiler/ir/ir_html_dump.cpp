#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - compiler - Dump IR
// cm_dump.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/ir/ir_html_dump.cpp#10 $
//

#include "../cm/cm_session.h"
#include "../cm/cm_fns.h"

namespace Compiler
{

// dump - BBlock
void html_dump_bblock(Val s, BBlock* pBBlock)
{
    if (! streamp(s)) return;

    html_format(s, L"<h2>~:S</h2>~%", pBBlock);

    cm_format(s, L"preorder=~D postorder=~D<br/>~%",
        Fixnum::Encode(pBBlock->GetPreorder()),
        Fixnum::Encode(pBBlock->GetPostorder()) );

    cm_format(s, L"<table border='1'>~%");
    cm_format(s, L"<tr><th>Predecessors</th><th>Successors</th></tr>~%");
    cm_format(s, L"<tr><td><ol>~%");
    foreach (BBlock::EnumInEdge, oEnum, pBBlock)
    {
        html_format(s, L"<li>~S</li>~%", oEnum.Get());
    } // for each pred
    cm_format(s, L"</ol></td>~%");

    cm_format(s, L"<td><ol>~%");
    foreach (BBlock::EnumOutEdge, oEnum, pBBlock)
    {
        html_format(s, L"<li>~S</li>~%", oEnum.Get());
    } // for each succ
    cm_format(s, L"</ol></td></tr>~%");
    cm_format(s, L"</table>~%");

    // Instructions
    cm_format(s, L"<table>~%");
    foreach (BBlock::EnumInsn, oEnum, pBBlock)
    {
        Instruction* pInsn = oEnum.Get();

        cm_format(s, L"<tr><td>");

        html_format(s, L"~D: ~:S",
            pInsn->GetIndex(),
            pInsn );


        cm_format(s, L"</td><td with=50></td><td>");

        if (NULL != pInsn->GetVar())
        {
            html_format(s, L"&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"
                L"; ~S~%", pInsn->GetVar() );
        }

        cm_format(s, L"</td></tr>~%");
    } // for each bblock
    cm_format(s, L"</table>~%");

    cm_format(s, L"<a href='#top'>&#x25B2;</a>");

    pBBlock->HtmlA(s);
    cm_format(s, L"BBlock Top</a>~%");

    pBBlock->GetFunction()->HtmlA(s);
    cm_format(s, L"Function Top</a>~%");
} // html_dump_bblock


// html_dump_bblocks
void html_dump_bblocks(Val s, Function* pFun)
{
    if (! streamp(s)) return;

    foreach (Function::EnumBBlock, oEnum, pFun)
    {
        html_dump_bblock(s, oEnum.Get());
    } // for each bblock
} // html_dump_bblocks


// dump - Function
void html_dump_fun(Val s, Function* pFun)
{
    if (! streamp(s)) return;

    html_format(s, L"<hr/>~%");
    html_format(s, L"<h1>~:S</h1>preorder=~D postorder=~D~:%",
        pFun,
        pFun->GetPreorder(),
        pFun->GetPostorder() );

    html_format(s, L"<ul>");
    html_format(s, L"<li>type  = ~W</li>~%", pFun->GetTy());
    html_format(s, L"<li>arity = ~D, ~D, ~D</li>~%",
        pFun->GetArityMin(),
        pFun->GetArityMax(),
        pFun->HasRestParam() );
    html_format(s, L"</ul>");

    if (pFun->HasCaller())
    {
        cm_format(s, L"<h2>Callers</h2>~%");
        cm_format(s, L"<ol>~%");
        foreach (Function::EnumCaller, oEnum, pFun)
        {
            Function* pCaller = oEnum.Get()->GetFrom();
            html_format(s, L"<li>~S</li>~%", pCaller);

            cm_format(s, L"<ol>~%");
            foreach (Function::EnumCallSite, oEnum, pFun)
            {
                Instruction* pInsn = oEnum.Get()->GetInstruction();
                if (pInsn->GetBBlock()->GetFunction() == pCaller)
                {
                    html_format(s, L"<li>~S</li>~%", pInsn);
                }
            } // for each call site
            cm_format(s, L"</ol>~%");
        } // for each caller
        cm_format(s, L"</ol>~%");
    } // caller

    if (pFun->HasCallee())
    {
        cm_format(s, L"<h2>Callees</h2>~%");
        cm_format(s, L"<ol>~%");
        foreach (Function::EnumCallee, oEnum, pFun)
        {
            html_format(s, L"<li>~S</li>~%", oEnum.Get()->GetTo());
        } // for each Callee
        cm_format(s, L"</ol>~%");
    } // Callee

    if (pFun->HasVar())
    {
        cm_format(s, L"<h2>Variables</h2>~%");
        cm_format(s, L"<table border='1'>");
        cm_format(s, L"<tr>");
            cm_format(s, L"<th>Name</th>");
            cm_format(s, L"<th>UpVar</th>");
            cm_format(s, L"<th>Storage</th>");
            cm_format(s, L"<th>Type</th>");
            cm_format(s, L"<th>Dfn</th>");
        cm_format(s, L"</tr>");

        foreach (Function::EnumVar, oEnum, pFun)
        {
            Variable* pVar = oEnum.Get();

            cm_format(s, L"<tr>");

            cm_format(s, L"<td>~S</td>", pVar->GetName());

            cm_format(s, L"<td>~D</td>",
                Fixnum::Encode(pVar->GetUpVarCount()) );


            html_format(s, L"<td>~A</td>",
                Variable::Storage_Heap == pVar->GetStorage() ?
                    L"Heap" :
                Variable::Storage_Literal == pVar->GetStorage() ?
                    L"Literal" :
                Variable::Storage_Register == pVar->GetStorage() ?
                    L"Register" :
                Variable::Storage_Stack == pVar->GetStorage() ?
                    L"Stack" : L"Unknown" );

            cm_format(s, L"<td>~S</td>", pVar->GetTy());

            if (NULL == pVar->GetDfn())
            {
                cm_format(s, L"<td><i>removed</i></td>");
            }
            else
            {
                html_format(s, L"<td>~S</td>", pVar->GetDfn());
            }

            cm_format(s, L"</tr>");
        } // for each variable

        cm_format(s, L"</table>~%");
    } // variable

    if (pFun->HasUpVar())
    {
        cm_format(s, L"<h2>UpVar</h2>~%");
        cm_format(s, L"<table border='1'>");
        cm_format(s, L"<tr>");
            cm_format(s, L"<th>Name</th>");
            cm_format(s, L"<th>Owner</th>");
            cm_format(s, L"<th>Def Site</th>");
        cm_format(s, L"</tr>");

        foreach (Function::EnumUpVarSite, oEnum, pFun)
        {
            OperandBox* pBox = oEnum.Get();

            cm_format(s, L"<tr>");

            html_format(s, L"<td>~S</td>",
                pBox->GetOperand() );

            html_format(s, L"<td>~S</td>",
                pBox->GetOperand()->StaticCast<Variable>()->GetOwner() );

            html_format(s, L"<td>~S</td>",
                pBox->GetInstruction() );

            cm_format(s, L"</tr>");
        } // for each variable
        cm_format(s, L"</table>~%");
    } // upvar

    cm_format(s, L"<h2>Registers</h2>~%");
    cm_format(s, L"<table border='1'>");
    cm_format(s, L"<thead>");
    cm_format(s, L"<th>Name</th>");
    cm_format(s, L"<th>Variable</th>");
    cm_format(s, L"<th>DfnBB</th>");
    cm_format(s, L"<th>Dfn</th>");
    cm_format(s, L"</thead>~%");
    foreach (Function::EnumReg, oEnum, pFun)
    {
        Register* pRd = oEnum.Get();
        cm_format(s, L"<tr>");
        html_format(s, L"<td>~S</td>", pRd);
        if (NULL != pRd->GetVar())
        {
            html_format(s, L"<td>~S</td>", pRd->GetVar());
        }
        else
        {
            html_format(s, L"<td><i>none</i></td>");
        }
        html_format(s, L"<td>~S</td>", pRd->GetDfn()->GetBBlock());
        html_format(s, L"<td>~S</td>", pRd->GetDfn());
        cm_format(s, L"</tr>~%");
    } // for each reg
    cm_format(s, L"</table>~%");

    html_dump_bblocks(s, pFun);
} // html_dump_fun


// log_html_start
void
log_html_start(Val s, const char16* pwszName, const char16* pwszSuffix)
{
    Val title;
        if (NULL == pwszSuffix)
        {
            title = make_string(pwszName);
        }
        else
        {
            char16 wsz[100];
                ::wsprintf(wsz, L"%ls %ls", pwszName, pwszSuffix);
            title = make_string(wsz);
        }

    cm_format(s, L"<!DOCTYPE html PUBLIC '-//W3C//DTD XHTML 1.0 Strict//EN' 'http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd'>~%");
    cm_format(s, L"<html><head>~%");
    cm_format(s, L"<meta http-equiv='Content-Type' content='text/html; charset=utf-8'/>~%");

    cm_format(s, L"<title>~A</title>~%", title);
    cm_format(s, L"<style type='text/css'>"
        L"body  { font-size: 9pt; font-family: Verdana; }"
        L"h1 { font-size: 140%; }"
        L"h1 { margin-bottom: 5pt}"
        L"h2 { font-size: 120%; }"
        L"h2 { margin-bottom: 2pt}"
        L"h3 { font-size: 110%; }"
        L"h3 { margin-bottom: 2pt}"
        L"h4 { font-size: 100%; }"
        L"h4 { margin-bottom: 2pt}"
        L"ol { margin-top: 2pt; margin-bottom: 2pt;}"
        L"table { font-size: 9pt; }"
        L"table { border-collapse: collapse; }"
        L"table { margin-top: 0px; }"
        L"th { align: left; background: #9999cc; color: #ffffff; }"
        L"ul { margin-top: 0pt; margin-bottom: 2pt;}"
        L"~%"

        // For logging
        L"div.r { color: #990000; font-weight: bold; }" // red
        L"~%"

        // For instruction
        L"span.IrOp_CALL { color: #CC0000; font-width: bold; }"
        L"~%"

        // For HTML IR dump
        L"span.l { color: #660000; }" // literal
        L"a.r { color: #006666; }" // register
        L"i.t { color: #F5A225; }" // type
        L"a.v { color: #660000; }" // variable
        L"~%</style>~%" );

    cm_format(s, L"</head>~%<body>~%");

    cm_format(s, L"<h1><a name='top'>~A</a></h1>~%", title);
} // log_html_start


//////////////////////////////////////////////////////////////////////
//
// Dump IR
//
void html_dump(Val s)
{
    if (! streamp(s)) return;

    cm_format(s, L"<h1><a name='funlist'>Functions</a></h1>~%");
    cm_format(s, L"<ol>~%");
    foreach (Module::EnumFunction, oEnum, Session::Get()->GetModule())
    {
        Function* pFun = oEnum.Get();
        html_format(s, L"<li>~S preorder=~D postorder=~D</li>~%", 
            pFun,
            pFun->GetPreorder(),
            pFun->GetPostorder() );
    } // for each function
    cm_format(s, L"</ol>~%");

    foreach (Module::EnumFunction, oEnum, Session::Get()->GetModule())
    {
        html_dump_fun(s, oEnum.Get());
    } // for each function
} // html_dump

} // Compiler
