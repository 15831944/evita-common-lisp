//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - pre-compiled header
// kernel_defs.h
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/platform/win/ILispEngine.h#1 $
//
#if !defined(INCLUDE_platform_win_ILispEngine_h)
#define INCLUDE_platform_win_ILispEngine_h

//////////////////////////////////////////////////////////////////////
//
// ILispEngine interface
//
interface ILispEngine
{
    virtual void* Bless(size_t, void*) = 0;
    virtual int   Init(size_t) = 0;
    virtual int   Interrupt(void*) = 0;
    virtual int   LoadImage(const char16*) = 0;
    virtual int   Start(void*) = 0;
}; // ILispEngine

typedef ILispEngine* (*GetEngineFn)(void);


#endif //!defined(INCLUDE_platform_win_ILispEngine_h)
