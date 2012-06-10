//////////////////////////////////////////////////////////////////////////////
//
// Editor - Edit Mode - C++ Mode
// listener/winapp/mode_Config.h
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/listener/winapp/mode_Config.h#3 $
//
#if !defined(INCLUDE_mode_Config_h)
#define INCLUDE_mode_Config_h

#include "./ed_Mode.h"

#include "./ed_Buffer.h"

namespace Edit
{

/// <summary>
///   Configuration file mode
/// </summary>
class ConfigModeFactory : public ModeFactory
{
    // ctor
    public: ConfigModeFactory();

    // [C]
    public: override Mode* Create(Buffer*);

    // [G]
    protected: override const char16* getExtensions() const
        { return L"cfg mk stanza"; }

    public: override const char16* GetName() const
        { return L"Config"; }

    // [I]
    public: override bool IsSupported(const char16*) const;
}; // ConfigModeFactory

} // Edit

#endif //!defined(INCLUDE_mode_Config_h)
