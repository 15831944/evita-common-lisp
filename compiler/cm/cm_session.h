//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - compiler - Compilation Session
// compiler/cm/cm_session.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cm/cm_session.h#5 $
//
#if !defined(INCLUDE_compiler_cm_session_h)
#define INCLUDE_compiler_cm_session_h

#include "./cm_fns.h"
#include "./cm_mm.h"
#include "./cm_module.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// OptimizeQualities
//
class OptimizeQualities
{
    protected: Int m_iCompilationSpeed;
    protected: Int m_iDebug;
    protected: Int m_iSafety;
    protected: Int m_iSpace;
    protected: Int m_iSpeed;

    public: Int GetCompilationSpeed() const { return m_iCompilationSpeed; }
    public: Int GetDebug() const { return m_iDebug; }
    public: Int GetSafety() const { return m_iSafety; }
    public: Int GetSpace()  const { return m_iSpace; }
    public: Int GetSpeed()  const { return m_iSpeed; }

    public: Int SetCompilationSpeed(Int v)
        { ASSERT(v >=0 && v <= 3); return m_iCompilationSpeed = v; }

    public: Int SetDebug(Int v)
        { ASSERT(v >=0 && v <= 3); return m_iDebug = v; }

    public: Int SetSafety(Int v)
        { ASSERT(v >=0 && v <= 3); return m_iSafety = v; }

    public: Int SetSpace(Int v)
        { ASSERT(v >=0 && v <= 3); return m_iSpace = v; }

    public: Int SetSpeed(Int v)
        { ASSERT(v >=0 && v <= 3); return m_iSpeed = v; }

    public: OptimizeQualities() :
        m_iCompilationSpeed(3),
        m_iDebug(3),
        m_iSafety(3),
        m_iSpace(0),
        m_iSpeed(0) {}

    public: void Load();
}; // OptimizeQualities


//////////////////////////////////////////////////////////////////////
//
// Session
//
class Session :
    public Mm
{
    private: uint m_cBBlockNames;
    private: uint m_cNames;
    private: bool           m_fVerify;
    private: SYSTEMTIME     m_stStart;
    private: SYSTEMTIME     m_stEnd;
    private: char16         m_wszLogName[MAX_PATH];
    private: uint           m_nLevel;

    public:    Val m_littab;
    private: Val m_stream;

    public: uint m_cErrors;
    public: uint m_cWarns;
    public: uint m_cStyleWarns;


    // ctor
    public: Session(Target*, Val);
    public: ~Session();

    // [C]
    public: bool CanContinue() const
        { return 0 == m_cErrors && 0 == m_cWarns; }

    // [G]
    public: static Session* Get()
        { return reinterpret_cast<Session*>(TLV(c6_AsessionA)); }

    // [I]
    public: bool IsVerify() const
        { return m_fVerify; }

    // [N]
    public: static int NewBBlockName()
        { return ++Session::Get()->m_cBBlockNames; }

    public: static int NewName()
        { return ++Session::Get()->m_cNames; }

    ////////////////////////////////////////////////////////////
    //
    // Properties
    //
    private: Val m_form;
        public: Val GetForm() const { return m_form; }

    private: Module m_oModule;
        public: Module* GetModule() { return &m_oModule; }

    private: Target* m_pTarget;
        public: Target* GetTarget() const { return m_pTarget; }

    private: Val m_load_time_values;
        public: Val GetLoadTimeValues() const { return m_load_time_values; }
        public: Val AddLoadTimeValue(Val, Val);

    ////////////////////////////////////////////////////////////
    //
    // Pass related
    //
    private: uint  m_cPasses;
    private: Pass* m_pPass;
    public: void EndPass();
    public: uint StartPass(Pass*);

    public: static Pass* GetPass()
        { return Session::Get()->m_pPass; }

    ////////////////////////////////////////////////////////////
    //
    // Source code tracking
    //
    private: Val m_linenum;
        public: Val GetLineNum() const { return m_linenum; }
        public: void RememberSource(Val);

    ////////////////////////////////////////////////////////////
    //
    // Optimize Quality
    //
    public: OptimizeQualities m_oOptimizeQualities;

    ////////////////////////////////////////////////////////////
    //
    // Log related
    //
    public: LPCWSTR GetLogName()   const { return m_wszLogName; }
    public: Val     GetLogStream() const { return m_stream; }
}; // Session

} // Compiler

#endif //!defined(INCLUDE_compiler_cm_session_h)
