#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - boot - Image Writer
// boot/bt_writer.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/platform/win/boot/win_bt_builder.cpp#1 $
//
#include "../../../mini/mini_lisp.h"

namespace Boot
{

Val defvar(const char16*, Val);

static Val make_charset(uint);

struct CsEntry
{
    uint            m_nCodePage;
    const char16*   m_pwsz;
}; // CsEntry

//////////////////////////////////////////////////////////////////////
//
// CsEntry
//  List of supported charset.
//
//  This list is created from IANA CHARSET[1].
//
//  FIXME: 2007-03-03: We should consult HKEY_CLASSES_ROOT\MIME\Database\Charset.
//
// References:
//  [1] http://www.iana.org/assignments/character-sets
//
static const CsEntry
k_rgoCsEntry[] =
{
    { 376,  L"US-ASCII" },
    //{ 874, L"Tahi"),
    { 932, L"SHIFT_JIS" },
    { 936, L"GB2312" },
    { 949, L"KS_C_5601 KSC_5601 KSC5601" },
    { 950, L"BIG5" },
    //{ 1361, L"Korean"),

    { 1250, L"WINDOWS-1250" },
    { 1251, L"WINDOWS-1251" },
    { 1252, L"WINDOWS-1252" },
    { 1253, L"WINDOWS-1253" },
    { 1254, L"WINDOWS-1254" },
    { 1255, L"WINDOWS-1255" },
    { 1256, L"WINDOWS-1256" },
    { 1257, L"WINDOWS-1257" },
    { 1258, L"WINDOWS-1258" },

    { 28951, L"ISO-8859-1" },   // latin1
    { 28952, L"ISO-8859-2" },   // central european
    { 28953, L"ISO-8859-3" },   // turkish
    { 28954, L"ISO-8859-4" },   // baltic
    { 28955, L"ISO-8859-5" },   // cyrillic
    { 28956, L"ISO-8859-6" },   // arabic
    { 28957, L"ISO-8859-7" },   // greek
    { 28958, L"ISO-8859-8" },   // hebrew
    { 28959, L"ISO-8859-9" },   // latin5
    { 28269, L"ISO-6937" },     // latin1

    { 65000, L"UTF-7" },
    { 65001, L"UTF-8" },
    // Other FE
    { 50220, L"ISO2022-JP" }, // no HK

#if 0
    { 50221, IDS_LANG_JAPANESE,             IDS_CS_ISO2022_JP_I }, // ESC(I
    { 50222, IDS_LANG_JAPANESE,             IDS_CS_ISO2022_JP_J }, // ESC(J
    { 50225, IDS_LANG_KOREAN,               IDS_CS_ISO2022_KR },
    { 50227, IDS_LANG_TRADITIONAL_CHINESE,  IDS_CS_ISO2022_CN },
    { 50229, IDS_LANG_SIMPLIFIED_CHINESE,   IDS_CS_ISO2022_CN },
    { 50932, IDS_LANG_JAPANESE,             IDS_CS_AUTO_DETECT },
    { 50949, IDS_LANG_KOREAN,               IDS_CS_AUTO_DETECT },
#endif

    // Unicode
    {  1200, L"UTF-16-LE" },
    {  1201, L"UTF-16-BE" },

    { 51932, L"EUC-JP" },
    { 51949, L"EUC-KR" },
}; // k_rgoCsEntry


// make_charset
static Val make_charset(uint nCodePage)
{
    Val code_page = Fixnum::Encode(nCodePage);
    Val charset = MiniThread::Get()->AllocRecord(CLASSD_charset);
    Charset* p = charset->Decode<Charset>();
    p->m_name = nil;
    p->m_code_page = code_page;
    p->m_aliases = nil;
    setf_gethash(charset, code_page, VAR(Acharset_tableA));
    return charset;
} // make_charset


// codePageEnumProc
static BOOL CALLBACK codePageEnumProc(char16* pwsz)
{
    uint nCodePage = 0;
    while (0 != *pwsz)
    {
        nCodePage *= 10;
        nCodePage += *pwsz - '0';
        pwsz++;
    } // while

    Val code_page = Fixnum::Encode(nCodePage);

    Val charset = gethash(code_page, VAR(Acharset_tableA));

    if (nil == charset)
    {
        Val charset = make_charset(nCodePage);
        setf_gethash(charset, code_page, VAR(Acharset_tableA));
    }

    return TRUE;
} // codePageEnumProc


// BuildForPlatform
void BuildForPlatform()
{
    VAR(Acharset_tableA) = make_hash_table();

    for (
        const CsEntry* p = &k_rgoCsEntry[0];
        p < &k_rgoCsEntry[lengthof(k_rgoCsEntry)];
        p++ )
    {
        Val charset = make_charset(p->m_nCodePage);

        const char16* pwsz = ::lstrchrW(p->m_pwsz, ' ');
        if (NULL == pwsz)
        {
            pwsz = p->m_pwsz + ::lstrlenW(p->m_pwsz);
        }

        Val name = intern(
            make_string(p->m_pwsz, pwsz - p->m_pwsz),
            PACKAGE_keyword );

        charset->Decode<Charset>()->m_name = name;

        setf_gethash(charset, name, VAR(Acharset_tableA));

        while (0 != *pwsz)
        {
            const char16* pwszE = ::lstrchrW(pwsz, ' ');
            if (NULL == pwszE)
            {
                pwszE = pwsz + ::lstrlenW(pwsz);
            }

            Val name = intern(
                make_string(pwsz, pwszE - pwszE),
                PACKAGE_keyword );

            charset->Decode<Charset>()->m_aliases = cons(
                name, 
                charset->Decode<Charset>()->m_aliases );

            setf_gethash(charset, name, VAR(Acharset_tableA));

            pwsz = pwszE;
            while (' ' == *pwsz) pwsz++;
        } // while
    } // for each p

    ::EnumSystemCodePagesW(codePageEnumProc, CP_SUPPORTED);

    foreach (EnumHashTable, oEnum, VAR(Acharset_tableA))
    {
        Val charset = oEnum.GetVal();
        Charset* p = charset->Decode<Charset>();

        char16 wsz[20];
            ::wsprintf(wsz, L"CP%u", Fixnum::Decode_(p->m_code_page));
        Val cpname = intern(wsz, PACKAGE_keyword);
        if (p->m_name == nil)
        {
            p->m_name = cpname;
        }
        else if (nil == memq(cpname, p->m_aliases))
        {
            p->m_aliases = cons(cpname, p->m_aliases);
        }
    } // for each charset
} // BuildForPlatform

} // Boot
