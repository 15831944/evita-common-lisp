#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - Compiler Session
// compiler/cm/cm_session.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cm/cm_session.cpp#9 $
//

#include "./cm_session.h"

#include "./cm_fns.h"
#include "./cm_pass.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// Comtext constructor
//
Session::Session(Target* pTarget, Val form) :
    m_fVerify(false),
    m_cBBlockNames(0),
    m_cNames(0),
    m_cPasses(0),
    m_cErrors(0),
    m_cWarns(0),
    m_cStyleWarns(0),
    m_pTarget(pTarget),
    m_pPass(NULL),
    m_form(form),
    m_linenum(nil),
    m_load_time_values(nil),
    m_stream(nil)
{
    if (nil == TLV(c6_AsessionA))
    {
        m_nLevel = 0;
    }
    else
    {
        m_nLevel = TLV(c6_AsessionA)->StaticCast<Session>()->m_nLevel + 1;
    }

    set_tlv(TLV_c6_AsessionA, Fixnum::Encode(this));

    m_oOptimizeQualities.Load();

    Val Ktime   = intern(L"TIME", PACKAGE_keyword);
    Val Kverify = intern(L"VERIFY", PACKAGE_keyword);

    ::GetLocalTime(&m_stStart);

    Val settings = TLV(c6_AsettingsA);

    Val time = cdr(assq(Ktime, settings));

    m_fVerify = nil != cdr(assq(Kverify, settings));

    #if 0
    {
        ::wsprintf(m_wszLogName,
            L"__toplevel_%u-%02u-%02u_%02u-%02u-%02u",
            m_stStart.wYear,
            m_stStart.wMonth,
            m_stStart.wDay,
            m_stStart.wHour,
            m_stStart.wMinute,
            m_stStart.wSecond );
    }
    #else
        if (0 == m_nLevel)
        {
            ::wsprintf(m_wszLogName, L"__toplevel");
        }
        else
        {
            ::wsprintf(m_wszLogName, L"__l%02u", m_nLevel);
        }
    #endif

    if (nil != time)
    {
        char16 wszLogFile[MAX_PATH];
            ::wsprintf(wszLogFile, L"%s.htm", m_wszLogName);

        Val logname = make_string(wszLogFile);
        m_stream = open(logname, Kdirection, Koutput, Kif_exists, Ksupersede);

        log_html_start(m_stream, m_wszLogName);

        cm_format(m_stream, L"<p>Start at ~A.</p>~%",
            time_string(&m_stStart) );

        cm_format(m_stream, L"<table border='1' cellpadding='3' cellspacing='3'>~%");
    } // if
} // Session


//////////////////////////////////////////////////////////////////////
//
// Session destructor
//
Session::~Session()
{
    ::GetLocalTime(&m_stEnd);

    if (streamp(m_stream))
    {
        cm_format(m_stream, L"</table>~%");

        cm_format(m_stream, L"<p>End at ~A.</p>~%",
            time_string(&m_stEnd) );

        cm_format(m_stream, L"<hr>~%");

        cm_format(m_stream, L"</body></html>~%");

        close(m_stream);
    }
} // Session::~Session


//////////////////////////////////////////////////////////////////////
//
// Session::AddLoadTimeValue
//
Val
Session::AddLoadTimeValue(Val cookie, Val form)
{
    push(cons(cookie, form), m_load_time_values);
    return cookie;
} // Session::LoadTimeValue

//////////////////////////////////////////////////////////////////////
//
// Session::EndPass
//
void
Session::EndPass()
{
    m_pPass = NULL;
} // EndPass


// Session::StartPass
uint Session::StartPass(Pass* pPass)
{
    ASSERT(NULL != pPass);

    m_pPass  = pPass;
    m_cPasses += 1;

    return m_cPasses;
} // Session::StartPass


// Session::RememberSource
//  Remember source line number of specified form. If line number of form
//  isn't found, we use the largest line number in *read-line-number*. That
//  form is generated form, such as macro expansion.
void Session::RememberSource(Val form)
{
    Val htb = TLV(Aread_line_number_tableA);
    if (! hash_table_p(htb)) return;

    Val linenum = gethash(form, htb);
    if (nil != linenum)
    {
        m_linenum = linenum;
        return;
    }

    if (nil == m_linenum)
    {
        // Pick the largest linenum up.
        m_linenum = Fixnum::Encode(0);
        foreach (EnumHashTable, oEnum, htb)
        {
            Val x = oEnum.GetVal();
            if (m_linenum < x) m_linenum = x;
        } // for each entry
    } // if
} // Session::RememberSource


//////////////////////////////////////////////////////////////////////
//
// Get Target
//
Target* cm_get_target()
{
    return Session::Get()->GetTarget();
} // cm_get_target


// OptimizeQualities::Load
//  Loads compilation qualities from environment.
void OptimizeQualities::Load()
{
    class Getter
    {
        public: static Int Get(Val name, Int iDefault)
        {
            Val val = cdr(assq(name, TLV(c6_AoptimizationA)));
            if (! fixnump(val)) val = Fixnum::Encode(iDefault);
            Int iVal = Fixnum::Decode_(val);
            return iVal >= 0 && iVal <= 3 ? iVal : iDefault;
        } // Get
    }; // Getter

    m_iCompilationSpeed = Getter::Get(Qcompilation_speed, 3);
    m_iDebug            = Getter::Get(Qdebug, 3);
    m_iSafety           = Getter::Get(Qsafety, 3);
    m_iSpace            = Getter::Get(Qspace, 0);
    m_iSpeed            = Getter::Get(Qspeed, 0);
} // OptimizeQualities::get_value

} // Compiler
