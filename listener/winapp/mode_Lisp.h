//////////////////////////////////////////////////////////////////////////////
//
// Editor - Edit Mode - Plain Text
// listener/winapp/mode_Lisp.h
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/listener/winapp/mode_Lisp.h#3 $
//
#if !defined(INCLUDE_mode_Lisp_h)
#define INCLUDE_mode_Lisp_h

#include "./ed_Mode.h"

namespace Edit
{

/// <remark>
///   Lisp mode facotry
/// </remark>
class LispModeFactory : public ModeFactory
{
    // ctor
    public: LispModeFactory();

    // [C]
    public: override Mode* Create(Buffer*);

    // [G]
    protected: override const char16* getExtensions() const
        { return L"cl l lisp lsp scm el"; }

    public: override const char16* GetName() const
        { return L"Lisp"; }
}; // LispModeFactory

} // Edit

#endif //!defined(INCLUDE_mode_Lisp_h)
