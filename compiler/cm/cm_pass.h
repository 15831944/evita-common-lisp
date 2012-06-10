//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - compiler - common - pass
// compiler/cm/cm_pass.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cm/cm_pass.h#4 $
//
#if !defined(INCLUDE_compiler_cm_pass_h)
#define INCLUDE_compiler_cm_pass_h

#include "./cm_defs.h"

namespace Compiler
{

class Pass;
class SubPass;

//////////////////////////////////////////////////////////////////////
//
// Pass
//
class Pass : public Timee
{
    friend class Session;
    friend class SubPass;

    public: uint m_nStartTick;
    public: uint m_nEndTick;

    public: Pass(const char16*);
    public: ~Pass();

    protected: Val m_stream;
        public: Val GetLogStream(int iLevel)
            { return m_iDebug >= iLevel ? m_stream : nil; }

    protected: int m_iDebug;
        public: int GetDebug() const { return m_iDebug; }

    protected: uint m_nLogChapter;
    protected: uint m_nLogSection;
    protected: uint m_nNth;

    public: void StartLog(LPCWSTR);
    public: void EndLog();
    void start_log(LPCWSTR);
}; // Pass


//////////////////////////////////////////////////////////////////////
//
// SubPass
//
//  If sub pass name doesn't start with '*', sub pass emits logs into
//  separate file.
//
class SubPass : public Timee
{
    protected: Pass*        m_pPass;
    protected: Function*    m_pFun;
    protected: Val          m_stream;

    protected: uint             m_nStartTick;

    public: SubPass(Pass*, const char16*, Function* = NULL);
    public: ~SubPass();
}; // SubPass

} // Compiler

#endif //!defined(INCLUDE_compiler_cm_pass_h)
