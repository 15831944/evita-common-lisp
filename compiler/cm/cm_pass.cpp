#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - compiler - Pass
// compiler/cm/cm_pass.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cm/cm_pass.cpp#7 $
//
#include "./cm_pass.h"

#include "./cm_fns.h"
#include "./cm_session.h"

namespace Compiler
{

void html_dump_bblocks(Val, Function*);

//////////////////////////////////////////////////////////////////////
//
// Pass constructor
//
Pass::Pass(const char16* pwszName) :
    Timee(pwszName),
    m_stream(nil),
    m_nLogChapter(0),
    m_nLogSection(0)
{
    Val key = intern(pwszName, PACKAGE_c6);
    Val settings = cdr(assq(key, TLV(c6_AsettingsA)));
    Val debug = cdr(assq(Kdebug, settings));

    if (nil == debug)
    {
        settings = cdr(assq(QA, TLV(c6_AsettingsA)));
        debug = cdr(assq(Kdebug, settings));
    }

    m_iDebug = fixnump(debug) ? static_cast<int>(Fixnum::Decode_(debug)) : 0;
    m_nNth = Session::Get()->StartPass(this);

    StartLog(L"");
} // Pass::Pass


//////////////////////////////////////////////////////////////////////
//
// Pass destructor
//
Pass::~Pass()
{
    // If compilation error was occured. IR may be incorrect.
    bool fVerify = Session::Get()->CanContinue() ?
                cm_verify(Session::Get()->GetModule()) :
                true;

    if (! fVerify)
    {
        #if _DEBUG
        {
            if (! streamp(m_stream))
            {
                m_nLogChapter = 0;
                start_log(L"");
                cm_verify(Session::Get()->GetModule());
            }
        }
        #endif // _DEBUG

        warn(L"Verification failed after ~D ~S pass.",
            Fixnum::Encode(m_nNth),
            make_string(GetName()) );
    } // if ! fVerify

    if (streamp(m_stream))
    {
        if (! fVerify)
        {
            cm_format(m_stream,
                L"<b style='color:red'>Verification failed!</b>~%" );
        }

        html_dump(m_stream);
    } // if

    EndLog();

    Session::Get()->EndPass();
} // Pass::~Pass


// Pass::StartLog
void Pass::StartLog(const char16* pwszSuffix)
{
    if (0 == m_iDebug) return;

    m_nLogSection += 1;

    start_log(pwszSuffix);
} // Pass::StartLog

    char16 wszLogFile[MAX_PATH];


// Pass::start_log
void Pass::start_log(const char16* pwszSuffix)
{
    if (0 == m_nLogChapter)
    {
        ::wsprintf(wszLogFile, L"%s_%02u_%s%s.htm",
            Session::Get()->GetLogName(),
            m_nNth,
            GetName(),
            pwszSuffix );
    }
    else
    {
        ::wsprintf(wszLogFile, L"%s_%02u_%s_%02u_%02u_%s.htm",
            Session::Get()->GetLogName(),
            m_nNth,
            GetName(),
            m_nLogChapter,
            m_nLogSection,
            pwszSuffix );
    }

    Val logname = make_string(wszLogFile);

    m_stream = open(logname, Kdirection, Koutput, Kif_exists, Ksupersede);

    log_html_start(m_stream, GetName(), pwszSuffix);

    if (0 == m_nLogChapter)
    {
        cm_format(m_stream, L"<p><a href='#funlist'>Functions</a></p>~%");
    }
} // Pass::start_log


// Pass::EndLog
void Pass::EndLog()
{
    if (0 == m_iDebug) return;

    if (streamp(m_stream))
    {
        cm_format(m_stream, L"~%</body></html>~%");
        close(m_stream);
    } // if stream
} // Pass::EndLog


// SubPass::SubPass
SubPass::SubPass(Pass* pPass, const char16* pwsz, Function* pFun) :
    Timee(pwsz),
    m_pPass(pPass),
    m_pFun(pFun),
    m_stream(nil)
{
    if (nil != m_pPass->m_stream)
    {
        if ('*' == *pwsz)
        {
            pwsz++;
            html_format(m_pPass->m_stream, 
                L"<h2><a href='#pass_~A'>~A</a></h2>~%",
                pwsz, pwsz );
        }
        else
        {
            m_stream = m_pPass->m_stream;
            m_pPass->StartLog(pwsz);
        }
    } // if
} // SubPass::SubPass

// SubPass dtor
SubPass::~SubPass()
{
    if (nil != m_stream)
    {
        if (NULL != m_pFun) 
        {
            html_dump_bblocks(m_pPass->m_stream, m_pFun);
        }

        m_pPass->EndLog();
        m_pPass->m_stream = m_stream;
    } // if
} // SubPass::~SubPass


Timee::~Timee()
{
    uint nInterval = ::GetTickCount() - m_nStart;

    Val stream = Session::Get()->GetLogStream();
    if (nil == stream) return;

    cm_format(stream, L"<tr><td>~A</td><td>~:D</td></tr>~%",
        make_string(GetName()),
        Fixnum::Encode(nInterval) );
} // Timee::~Timee

} // Compiler
