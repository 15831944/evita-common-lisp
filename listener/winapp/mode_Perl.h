//////////////////////////////////////////////////////////////////////////////
//
// Editor - Edit Mode - C++
// listener/winapp/mode_Perl.h
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/listener/winapp/mode_Perl.h#4 $
//
#if !defined(INCLUDE_mode_Perl_h)
#define INCLUDE_mode_Perl_h

#include "./ed_Mode.h"

#include "./ed_Buffer.h"

namespace Edit
{

/// <summary>
///   PerlMode factory
/// </summary>
class PerlModeFactory : public ModeFactory
{
    // ctor
    public: PerlModeFactory();

    // [C]
    public: override Mode* Create(Buffer*);

    // [G]
    protected: override const char16* getExtensions() const
        { return L"pl pm t"; }

    public: override const char16* GetName() const
        { return L"Perl"; }
}; // PerlModeFactory

} // Edit

#endif //!defined(INCLUDE_mode_Perl_h)
