#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - compiler - pre-compiled header
// compiler_cl_defs.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cm/cm_fns.cpp#5 $
//
#include "./cm_fns.h"

#include "./cm_pass.h"
#include "./cm_session.h"

namespace CommonLisp
{

Val make_string(const char* pchStart, size_t cch)
{
    Val str = make_string(Fixnum::Encode(cch));

    char16* p = str->Decode<SimpleString>()->GetElements();

    const char* pchEnd = pchStart + cch;
    for (const char* pch = pchStart; pch < pchEnd; pch++)
    {
        *p++ = *pch;
    } // for
    return str;
} // make_string

} // CommonLisp

namespace Compiler
{

void CompilerInternalError(const char* pszFile, int iLine)
{
    warn(L"Compiler Interanl error at ~A(~D).",
        make_string(pszFile),
        Fixnum::Encode(iLine) );
} // CompilerInternalError


// time_string
Val time_string(const SYSTEMTIME* pst)
{
    WCHAR wsz[100];
        ::wsprintf(wsz, L"%4u-%02u-%02u %02u:%02u:%02u.%03u",
            pst->wYear,
            pst->wMonth,
            pst->wDay,
            pst->wHour,
            pst->wMinute,
            pst->wSecond,
            pst->wMilliseconds );

    return make_string(wsz);
} // time_string

void write_warn_prefix(LPCWSTR pwszClass)
{
    write_string(L"; ");
    write_string(pwszClass);
    write_string(L": ");

    Val linenum = Session::Get()->GetLineNum();
    if (nil != linenum)
    {
        Val truename;
        {
            Val situation = TLV(c6_AsituationA);
            if (Qcompile_file == situation)
            {
                truename = TLV(Acompile_file_truenameA);
            }
            else if (Qload == situation)
            {
                truename = TLV(Aload_truenameA);
            }
            else
            {
                truename = nil;
            }
        } // truename

        if (nil != truename)
        {
            format(t, L"~A(~D): ", truename, add_xx(linenum, 1));
        }
    } // if
} // write_warn_prefix


// style_warn
void style_warn(LPCWSTR pwszControl)
{
    Session::Get()->m_cStyleWarns += 1;
    write_warn_prefix(L"Style-warn");
    format(t, pwszControl);
    format(t, L"~%");
} // style_warn

// style_warn
void style_warn(LPCWSTR pwszControl, Val a)
{
    Session::Get()->m_cStyleWarns += 1;
    write_warn_prefix(L"Style-warn");
    format(t, pwszControl, a);
    format(t, L"~%");
} // style_warn

// style_warn
void style_warn(LPCWSTR pwszControl, Val a, Val b)
{
    Session::Get()->m_cStyleWarns += 1;
    write_warn_prefix(L"Style-warn");
    format(t, pwszControl, a, b);
    format(t, L"~%");
} // style_warn

// warn
void warn(LPCWSTR pwszControl)
{
    Session::Get()->m_cWarns += 1;
    write_warn_prefix(L"Warn");
    format(t, pwszControl);
    format(t, L"~%");
} // warn

// warn
void warn(LPCWSTR pwszControl, Val a)
{
    Session::Get()->m_cWarns += 1;
    write_warn_prefix(L"Warn");
    format(t, pwszControl, a);
    format(t, L"~%");
} // warn

// warn
void warn(LPCWSTR pwszControl, Val a, Val b)
{
    Session::Get()->m_cWarns += 1;
    write_warn_prefix(L"Warn");
    format(t, pwszControl, a, b);
    format(t, L"~%");
} // warn


// warn
void warn(LPCWSTR pwszControl, Val a, Val b, Val c)
{
    Session::Get()->m_cWarns += 1;
    write_warn_prefix(L"Warn");
    format(t, pwszControl, a, b, c);
    format(t, L"~%");
} // warn


// warn
void warn(LPCWSTR pwszControl, Val a, Val b, Val c, Val d)
{
    Session::Get()->m_cWarns += 1;
    write_warn_prefix(L"Warn");
    format(t, pwszControl, a, b, c, d);
    format(t, L"~%");
} // warn


Val log_format(int iLevel, LPCWSTR pwsz)
{
    Val stream = Session::GetPass()->GetLogStream(iLevel);
    if (! streamp(stream)) return nil;
    format(stream, pwsz);
    force_output(stream);
    return nil;
} // log_format

Val log_format(int iLevel, LPCWSTR pwsz, Val a)
{
    Val stream = Session::GetPass()->GetLogStream(iLevel);
    if (! streamp(stream)) return nil;
    format(stream, pwsz, a);
    force_output(stream);
    return nil;
} // log_format

Val log_format(int iLevel, LPCWSTR pwsz, Val a, Val b)
{
    Val stream = Session::GetPass()->GetLogStream(iLevel);
    if (! streamp(stream)) return nil;
    format(stream, pwsz, a, b);
    force_output(stream);
    return nil;
} // log_format

Val log_format(int iLevel, LPCWSTR pwsz, Val a, Val b, Val c)
{
    Val stream = Session::GetPass()->GetLogStream(iLevel);
    if (! streamp(stream)) return nil;
    format(stream, pwsz, a, b, c);
    force_output(stream);
    return nil;
} // log_format

Val log_format(int iLevel, LPCWSTR pwsz, Val a, Val b, Val c, Val d)
{
    Val stream = Session::GetPass()->GetLogStream(iLevel);
    if (! streamp(stream)) return nil;
    format(stream, pwsz, a, b, c, d);
    force_output(stream);
    return nil;
} // log_format

Val log_format(int iLevel, LPCWSTR pwsz, Val a, Val b, Val c, Val d, Val e)
{
    Val stream = Session::GetPass()->GetLogStream(iLevel);
    if (! streamp(stream)) return nil;
    format(stream, pwsz, a, b, c, d, e);
    force_output(stream);
    return nil;
} // log_format

Val log_format(int iLevel, 
    LPCWSTR pwsz, Val a, Val b, Val c, Val d, Val e, Val f)
{
    Val stream = Session::GetPass()->GetLogStream(iLevel);
    if (! streamp(stream)) return nil;
    format(stream, pwsz, a, b, c, d, e, f);
    force_output(stream);
    return nil;
} // log_format

Val log_format(int iLevel, 
    LPCWSTR pwsz, Val a, Val b, Val c, Val d, Val e, Val f, Val g)
{
    Val stream = Session::GetPass()->GetLogStream(iLevel);
    if (! streamp(stream)) return nil;
    format(stream, pwsz, a, b, c, d, e, f, g);
    force_output(stream);
    return nil;
} // log_format


void html_log_format(int iLevel, LPCWSTR pwszFormat, ...)
{
    Val stream = Session::GetPass()->GetLogStream(iLevel);
    if (! streamp(stream)) return;

    if ('<' != *pwszFormat && ' ' != *pwszFormat)
    {
        while (--iLevel > 0)
        {
            format(stream, L"&nbsp;");
        } // while
    } // if

    va_list args;
    va_start(args, pwszFormat);
    html_formatV(stream, pwszFormat, args);
    va_end(args);
    force_output(stream);
} // html_log_format

} // Compiler
