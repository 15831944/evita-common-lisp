#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - 24 System Construction
// genesis/gs_24_syscon.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/genesis/gs_24_syscon.cpp#4 $
//
// Description:
//  This file contains C implementation of lisp functions.
//      inspect
//      time
//
#include "./gs_lisp.h"
#include "../kernel/ke_finalize.h"

namespace Genesis
{

using namespace CommonLisp;


namespace
{

class FinallyClose
{
    Val m_stream;
    bool m_fAbort; 
    public: FinallyClose(Val stream) : m_stream(stream), m_fAbort(true) {}
    public: void SetNormal() { m_fAbort = false; }
    public: ~FinallyClose()
    {
        if (m_fAbort)
        {
            format(t, L"; Abort loading ~S.~%",
                m_stream->Decode<FileStream>()->m_pathname );
        }
        close(m_stream);
    } // ~FinallyClose
}; // FinallyClose

} // namespace

// C_load
//   NYI: load: keyworad arguments verbose, print, if-does-not-exist,
//   external-format
Val C_load(MiniThread* p)
{
    Val filespec = p->mv_value[0];

    check_type(filespec, simple_string);

    Val pathname = filespec;

    Val stream = open(pathname, Kelement_type,  Qcharacter);

    foreach (EnumList, oEnum, VAR(AfinalizationsA))
    {
        Finalization* p = oEnum.Get()->Decode<Finalization>();
        if (p->m_object == stream)
        {
            p->m_function = Qidentity;
        }
    } // for each cons

    bool fPrint   = nil != TLV(Aload_printA);
    bool fVerbose = nil != TLV(Aload_verboseA);

    if (fVerbose)
    {
        format(t, L"; Loading ~S~%", pathname);
    }

    Val eof = list(nil);
    Val lntab = make_hash_table(Qeq);

    BindFrameScope oLet(6);
        oLet.Bind(TLV_Aload_pathnameA, pathname);
        oLet.Bind(TLV_Aload_truenameA, pathname);

        oLet.Bind(TLV_ApackageA);
        oLet.Bind(TLV_AreadtableA);

        oLet.Bind(TLV_Aread_line_number_tableA, lntab);
        oLet.Bind(TLV_c6_AsituationA, Qload);

    FinallyClose oFinallyClose(stream);

    for (;;)
    {
        clrhash(lntab);
        Val form = read(stream, nil, eof);
        if (form == eof)
        {
            break;
        }

        Val val = eval(form);

        if (fPrint)
        {
            if (Fixnum::Encode(1) == p->m_n)
            {
                format(t, L"~S~%", val);
            }
            else if (Fixnum::Encode(0) == p->m_n)
            {
                // nothing to print
            }
            else
            {
                Val vals = nil;
                {
                    Int iIndex = Fixnum::Decode_(p->m_n);
                    while (iIndex >= 1)
                    {
                        iIndex -= 1;
                        vals = cons(p->mv_value[iIndex], vals);
                    } // while
                } // vals

                Val nth = Fixnum::Encode(0);
                for (Val x = vals; ! endp(x); x = cdr(x))
                {
                    format(t, L"; [~D] ~S: ~S~%",
                        nth,
                        type_of(car(x)),
                        car(x) );

                    nth = add_xx(nth, 1);
                } // for
            } // if
        } // if fPrint
    } // for

    oFinallyClose.SetNormal();

    if (fVerbose)
    {
        format(t, L"; End loading ~S~%", pathname);
    }

    return t;
} // C_load

} // Genesis
