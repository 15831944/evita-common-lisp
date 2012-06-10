//////////////////////////////////////////////////////////////////////////////
//
// Editor - Edit Mode - C++ Mode
// listener/winapp/mode_Cxx.h
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/listener/winapp/mode_Cxx.h#7 $
//
#if !defined(INCLUDE_mode_Cxx_h)
#define INCLUDE_mode_Cxx_h

#include "./ed_Mode.h"

#include "./ed_Buffer.h"

namespace Edit
{

/// <summary>
///  C++ mode factory
/// </summary>
class CxxModeFactory : public ModeFactory
{
    // ctor
    public: CxxModeFactory();

    // [C]
    public: override Mode* Create(Buffer*);

    // [G]
    protected: override const char16* getExtensions() const
        { return L"cc cpp cxx c hpp hxx h css cs ev"; }

    public: override const char16* GetName() const
        { return L"C++"; }
}; // CxxModeFactory

/// <summary>
///  Java mode factory
/// </summary>
class JavaModeFactory : public ModeFactory
{
    // ctor
    public: JavaModeFactory();

    // [C]
    public: override Mode* Create(Buffer*);

    // [G]
    protected: override const char16* getExtensions() const
        { return L"java js"; }

    public: override const char16* GetName() const
        { return L"Java"; }
}; // JavaModeFactory

} // Edit

#endif //!defined(INCLUDE_mode_Cxx_h)
