//////////////////////////////////////////////////////////////////////////////
//
// Editor - Edit Mode - Haskell Mode
// listener/winapp/mode_Haskell.h
//
// Copyright (C) 1996-2009 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/listener/winapp/mode_Haskell.h#1 $
//
#if !defined(INCLUDE_mode_Haskell_h)
#define INCLUDE_mode_Haskell_h

#include "./ed_Mode.h"

#include "./ed_Buffer.h"

namespace Edit
{

/// <summary>
///  Haskell mode factory
/// </summary>
class HaskellModeFactory : public ModeFactory
{
    // ctor
    public: HaskellModeFactory();

    // [C]
    public: override Mode* Create(Buffer*);

    // [G]
    protected: override const char16* getExtensions() const
        { return L"hs hsc"; }

    public: override const char16* GetName() const
        { return L"Haskell"; }
}; // HaskellModeFactory

} // Edit

#endif //!defined(INCLUDE_mode_Haskell_h)
