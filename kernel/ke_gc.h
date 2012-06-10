//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - pre-compiled header
// kernel/ke_thread.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/kernel/ke_gc.h#8 $
//
#if !defined(INCLUDE_kernel_gc_h)
#define INCLUDE_kernel_gc_h

namespace Kernel
{

class Area;
class Frame;
class Thread;

///////////////////////////////////////////////////////////////////////////////
//
// Gc
//
class Gc
{
    private: Gc() {}

    public: static uint sm_nAgeScanMax;
    public: static Thread* sm_pThread;

    public: static bool IsGarbage(Val);
    public: static bool IsOldToYoung(uint, Val);
    public: static bool Run(uint, size_t*, size_t*);

    public: static void UpdateRS();
        static void updateRS_area(Area*);
        static void updateRS_funobj(uint, Val);
        static void updateRS_record(uint, Val);

    static size_t getObjectSize(Val);


    static void phaseScanGcAnchor();
    static void phaseScanRS();
    static void phaseScanTospace();
    static void phaseUpdateOld();
    static void fixObjTab();

    public: static Val resolveForward(Val);

    static void scanArea(Area*);
    static void scanObject(uint, Val);

    public: static Val  moveObject(Val);

    static Val  updateCell(uint, Val*);
    static void updateRange(Val*, Val*);
    static void updateRange(uint, Val*, Val*);
    static void updateThread(Thread*);

    // updateRange
    static void updateRange(uint nAge, void* pvStart, void* pvEnd)
    {
        updateRange(
            nAge,
            reinterpret_cast<Val*>(pvStart),
            reinterpret_cast<Val*>(pvEnd) );
    } // updateRange

    // Remembered Set
    static void remember(Val*);
    static void remember(uint, Val*);
    static void rememberFunObj(uint, Val);

    // Machine dependent methods
    static void prepareFunObj(Val, Val);
    static void scanFunObj(uint, Val);
    static void beforeUpdateThread(Thread*);
    static bool updateFrame(Thread*, Frame*);
    static void afterUpdateThread(Thread*);

    // Finalization
    static void phaseInvokeFinalization();

    // Weak Objectes
    static Val copyWeakObject(void*, size_t);
    static void scanWeakArea(Area*);

    // Utility functions
    static int  age_of(void*);
    static bool from_space_p(Val);

    ////////////////////////////////////////////////////////////
    //
    // ForwardCell
    //
    class ForwardCell : public AsInt
    {
        Val m_cookie;
        Val m_val;

        public: ForwardCell(Val);
        public: Val Get() const { return m_val; }

        public: static ForwardCell* DynamicCast(Val);
        public: static ForwardCell* StaticCast(Val);
    }; // ForwardCell

    static void Format(const char16*, ...);
    static void Printf(const char16*, ...);
}; // Gc

#if ! defined(GC_DEBUG)
    #define GC_DEBUG _DEBUG
#endif // ! defined(GC_DEBUG)

#if GC_DEBUG
    #define GC_FORMAT \
        ::OutputDebugStringA(__FUNCTION__ ": "); \
        Gc::Format

    #define GC_PRINTF \
        ::OutputDebugStringA(__FUNCTION__ ": "); \
        Gc::Printf
#else // ! defined(GC_DEBUG)
    #define GC_FORMAT   __noop
    #define GC_PRINTF   __noop
#endif // ! defined(GC_DEBUG)

} // Kernel

#endif //!defined(INCLUDE_kernel_gc_h)
