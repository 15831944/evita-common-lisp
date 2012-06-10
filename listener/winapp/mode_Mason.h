//////////////////////////////////////////////////////////////////////////////
//
// Editor - Edit Mode - C++ Mode
// listener/winapp/mode_Mason.h
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/listener/winapp/mode_Mason.h#3 $
//
#if !defined(INCLUDE_mode_Mason_h)
#define INCLUDE_mode_Mason_h

#include "./mode_Perl.h"

namespace Edit
{

/// <summary>
///   Mason mode factory.
/// </summary>
class MasonModeFactory : public ModeFactory
{
    // ctor
    public: MasonModeFactory();

    // [C]
    public: override Mode* Create(Buffer*);

    // [G]
    protected: override const char16* getExtensions() const
        { return L"m mi"; }

    public: override const char16* GetName() const
        { return L"Mason"; }

    // [I]
    public: override bool IsSupported(const char16*) const;
}; // MasonModeFactory

} // Edit

#endif //!defined(INCLUDE_mode_Mason_h)
