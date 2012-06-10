//////////////////////////////////////////////////////////////////////////////
//
// Editor - Edit Mode - Plain Text
// listener/winapp/mode_PlainText.h
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/listener/winapp/mode_PlainText.h#2 $
//
#if !defined(INCLUDE_mode_PlainText_h)
#define INCLUDE_mode_PlainText_h

#include "./ed_Mode.h"

namespace Edit
{

//////////////////////////////////////////////////////////////////////
//
// PlainTextMode
//
class PlainTextMode : public Mode
{
    // ctor
    public: PlainTextMode(ModeFactory*, Buffer*);

    // [D]
    public: override bool DoColor(Count) { return false; }
}; // PlainTextMode


//////////////////////////////////////////////////////////////////////
//
// PlainTextModeFactory
//
class PlainTextModeFactory : public ModeFactory
{
    // ctor
    public: PlainTextModeFactory();

    // [C]
    public: override Mode* Create(Buffer* pBuffer)
        { return new PlainTextMode(this, pBuffer); }

    // [G]
    protected: override const char16* getExtensions() const
        { return L"txt"; }

    public: override const char16* GetName() const
        { return L"Plain"; }
}; // PlainTextModeFactory

} // Edit

#endif //!defined(INCLUDE_mode_PlainText_h)
