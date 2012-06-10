#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - compiler - Common Lisp Parser Main
// cl_main.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cl/cl_main.cpp#7 $
//
#include "./cl_defs.h"

#include "../cm/cm_base.h"
#include "../cm/cm_pass.h"
#include "../ir/ir_instruction.h"

namespace Compiler
{

ClParser::ParserTable ClParser::s_oParserTable;

//////////////////////////////////////////////////////////////////////
//
// ClParser constructor
//
ClParser::ClParser() :
    IrPass(L"CL-PARSE"),
    m_ltv_counter(Fixnum::Encode(0)),
    m_pFrame(NULL),
    m_fToplevelForm(true) {}


//////////////////////////////////////////////////////////////////////
//
// Run
//
void ClParser::Run()
{
    if (0 == s_oParserTable.GetCount())
    {
        install_parsers();
    }

    run(Ktoplevel, ty_values_rest_t, Session::Get()->GetForm());
} // ClParser::Run

Function* ClParser::run(Val name, Ty valty, Val form)
{
    m_funtab = make_hash_table();
    m_vartab = make_hash_table();

    Function* pFun = NewFunction(
        name,
        Function::Kind_Toplevel );

    pFun->SetTy(list(Qfunction, nil, valty));

    LexEnv oLexEnv(LexEnv::Kind_toplevel, pFun);

    LambdaList oLambdaList;

    processLambda(
        val_anonymous,
        pFun,
        &oLexEnv,
        &oLambdaList,
        list(form) );

    return pFun;
} // ClParser::run


//////////////////////////////////////////////////////////////////////
//
// ClParser::install_parsers
//
void
ClParser::install_parsers()
{
    #define declare_parser(mp_name) \
        s_oParserTable.Set(Q##mp_name, &ClParser::parse_##mp_name);

    #define declare_parser_setf(mp_name) \
        s_oParserTable.Set(SETF_##mp_name, &ClParser::parse_setf_##mp_name);

    #define parse_null parse_not
    #include "./cl_parser.inc"
    #undef parse_null
} // ClParser::install_parsers


ClParser::ParserT
ClParser::ParserTable::Get(Val name) const
{
    UINT iStart = static_cast<UINT>(name->ToInt() % lengthof(m_rgoEntry));
    UINT iEnd = lengthof(m_rgoEntry);

    const Entry* pStart  = &m_rgoEntry[iStart];
    const Entry* pEnd    = &m_rgoEntry[iEnd];
    const Entry* pRunner = pStart;
    for (;;)
    {
        if (pRunner->m_name == name)
        {
            return pRunner->m_pfn;
        }

        if (NULL == pRunner->m_name)
        {
            return NULL;
        }

        pRunner++;

        if (pRunner >= pEnd)
        {
            pRunner = &m_rgoEntry[0];
        }

        ASSERT(pRunner != pStart);
    } // for
} // ClParser::ParserTable::Get


void
ClParser::ParserTable::Set(Val name, ParserT pfn)
{
    UINT iStart = static_cast<UINT>(name->ToInt() % lengthof(m_rgoEntry));
    UINT iEnd = lengthof(m_rgoEntry);

    Entry* pStart  = &m_rgoEntry[iStart];
    Entry* pEnd    = &m_rgoEntry[iEnd];
    Entry* pRunner = pStart;
    for (;;)
    {
        if (pRunner->m_name == name)
        {
            pRunner->m_pfn = pfn;
            return;
        }

        if (NULL == pRunner->m_name)
        {
            m_cParsers += 1;
            pRunner->m_name = name;
            pRunner->m_pfn = pfn;
            return;
        }

        pRunner++;

        if (pRunner >= pEnd)
        {
            pRunner = &m_rgoEntry[0];
        }

        ASSERT(pRunner != pStart);
    } // for
} // ClParser::ParserTable::Set


//////////////////////////////////////////////////////////////////////
//
// Parser Entry Point
//
void
parse()
{
    ClParser oParser;
    oParser.Run();
} // Parse

} // Compiler
