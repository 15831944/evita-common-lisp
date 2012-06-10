//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - compiler - common mm
// compiler/cm/cm_mm.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cm/cm_mm.h#2 $
//
#if !defined(INCLUDE_compiler_cm_mm_h)
#define INCLUDE_compiler_cm_mm_h

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// Memory Manager
//
class Mm
{
    HANDLE m_hHeap;

    protected: Mm();
    protected: ~Mm();

    public: void* Alloc(size_t);
}; // Mm

} // Compiler

#endif //!defined(INCLUDE_compiler_cm_mm_h)
