//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - pre-compiled header
// kernel/ke_thread.h
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/kernel/ke_thread.h#7 $
//
#if !defined(INCLUDE_kernel_thread_h)
#define INCLUDE_kernel_thread_h

#include "./ke_frame.h"
#include "./ke_host.h"
#include "./ke_layout.h"
#include "./ke_memory.h"

namespace Kernel
{

class Area;
    class ObStackArea;

class Frame;
    class BindFrame;
    // class BlockFrame;
    // class CatchFrame;
    // class FinallyFrame;
    class FunctionFrame;
    class ObStackFrame;
    // class TagbodyFrame;

//////////////////////////////////////////////////////////////////////
//
// Kernel Thread
//
//
// Note: We should not add constructor with arguments of this class.
// Since, we define classes derived this class.
//
class Thread : public Record
{
    public: void* operator new(size_t, void* pv) { return pv; }

    public: template<class T> T* Extend()
        { return reinterpret_cast<T*>(this); }

    //public: Val           m_classd;                       //  [0] +0
    public: Val             m_name;                         //  [1] +4

    public: Val             m_waiting;                      //  [2]
    public: Val             m_next_waiter;                  //  [3]

    public: Area*           m_pConsArea;                    //  [4]
    public: Area*           m_pRecordArea;                  //  [5]
    public: Area*           m_pBinObjArea;                  //  [6]
    public: Area*           m_pFunObjArea;                  //  [7]

    public: ObStackArea*    m_pObStackArea;                 //  [8]

    public: Thread*  m_pNextThread;                         //  [9]
    public: Thread*  m_pPrevThread;                         // [10]

    public: Val    m_fp;                                    // [11]
    public: Val    m_fn;                                    // [12]
    public: Val    m_n;                                     // [13]
    public: Val    mv_value[Host::Multiple_Values_Limit];   // [14]

    public: static Thread* sm_pFirstThread;
    public: static Thread* sm_pLastThread;

    public: struct ObjectStat
    {
        Val m_count;
        Val m_size;

        enum { TypeCode_MAX_1 = 64 };
    }; // ObjectStart

    protected: ObStackArea m_oEmptyObStackArea;

    public: HANDLE   m_hThread;

    public: ObjectStat  mv_stat[ObjectStat::TypeCode_MAX_1];

    // mv_tlv must be the last member variable of Thread.
    public: Val       mv_tlv[Host::TLV_Limit];

    public: Thread(HANDLE hThread = NULL);

    public: void Init(size_t);
    public: void MachInit();
    public: void ResetAlloc();
    public: __declspec(noreturn) void Start();

    ////////////////////////////////////////////////////////////
    //
    // For C
    //
    #if USE_DECLSPEC_THREAD
        protected: static __declspec(thread) Thread* sm_pTlsThread;
            public: static Thread* Get() { return sm_pTlsThread; }
            public: void init_tls() { sm_pTlsThread = this; }

    #else // USE_DECLSPEC_THREAD
        protected: static DWORD sm_dwTlsThread;

            public: static Thread* Get()
            {
                return reinterpret_cast<Thread*>(
                    ::TlsGetValue(sm_dwTlsThread) );
            } // Thread::Get

            public: void init_tls()
                { ::TlsSetValue(sm_dwTlsThread, this); }
    #endif // USE_DECLSPEC_THREAD

    ////////////////////////////////////////////////////////////
    //
    // Services
    //
    Val RegisterCaller(Val name, Val caller);
    Val SxHash(Val);

    ////////////////////////////////////////////////////////////
    //
    // Lock
    //
    void Resume()  { ::ResumeThread(m_hThread); }
    void Suspend() { ::SuspendThread(m_hThread); }

    Val WakeUp(Val x)
    {
        ASSERT(m_waiting == x);

        Val next_waiter = m_next_waiter;
        m_next_waiter = nil;
        m_waiting = nil;
        return next_waiter;
    } // WakeUp

    ////////////////////////////////////////////////////////////
    //
    // Frame
    //
    Frame* GetFP() const { return reinterpret_cast<Frame*>(m_fp); }
    Frame* SetFP(Frame* p) { m_fp = Fixnum::Encode(p); return p; }

    public: class EnumFrame
    {
        Frame*          m_pRunner;
        FunctionFrame*  m_pFunFrame;
        FunctionFrame   m_oFunFrame;

        public: EnumFrame(Thread* p) :
            m_pRunner(p->GetFP()),
            m_pFunFrame(NULL) {}

        public: bool AtEnd() const
            { return NULL == m_pRunner; }

        public: Frame* Get() const;
        public: void Next();
    }; // EnumFrame

    ////////////////////////////////////////////////////////////
    //
    // Tlv
    //
    Val GetTlv(Int ofsTlv) const
    {
        return *Tlv(ofsTlv);
    } // GetTlv

    Val SetTlv(Int ofsTlv, Val val)
    {
        return *Tlv(ofsTlv) = val;
    } // SetTlv

    Val* Tlv(Int ofsTlv) const
    {
        ASSERT(ofsTlv > offsetof(Thread, mv_tlv));
        ASSERT(ofsTlv < sizeof(Thread));

        return reinterpret_cast<Val*>(
            const_cast<BYTE*>(reinterpret_cast<const BYTE*>(this)) + 
            ofsTlv );
    } // Tlv

    Value GetTlv(Val index) const
    {
        return *Tlv(index);
    } // GetTlv

    Val  SetTlv(Val index, Val val)
    {
        return *Tlv(index) = val;
    } // SetTlv

    Val* Tlv(Val index) const
    {
        Int iOffset = ToTlvOffset(index);
            ASSERT(iOffset < sizeof(*this));

        Val* p = reinterpret_cast<Val*>(
            const_cast<BYTE*>(reinterpret_cast<const BYTE*>(this)) + 
            iOffset );

        return p;
    } // Tlv

    static Int ToTlvIndex(Int ofsTlv)
    {
        ASSERT(ofsTlv > offsetof(Thread, mv_tlv));
        ASSERT(ofsTlv < sizeof(Thread));

        return (ofsTlv - offsetof(Thread, mv_tlv)) / sizeof(Val);
    } // ToTlvIndex

    static Int ToTlvOffset(Int iTlv)
    {
        ASSERT(0 != iTlv);
        return iTlv * sizeof(Val) + offsetof(Thread, mv_tlv);
    } // ToTlvOffset

    static Int ToTlvOffset(Val index)
    {
        return Fixnum::Decode_(index) * sizeof(Val) + offsetof(Thread, mv_tlv);
    } // ToTlvOffset

    ////////////////////////////////////////////////////////////
    //
    // Values
    //
    Val SetValues()
    {
        m_n = Fixnum::Encode(0);
        return mv_value[0] = nil;
    } // SetValues

    Val SetValues(Value a)
    {
        m_n = Fixnum::Encode(1);
        return mv_value[0] = a;
    } // SetValues

    Val SetValues(Value a, Value b)
    {
        m_n = Fixnum::Encode(2);
        mv_value[1] = b;
        return mv_value[0] = a;
    } // SetValues

    Val SetValues(Value a, Value b, Value c)
    {
        m_n = Fixnum::Encode(3);
        mv_value[2] = c;
        mv_value[1] = b;
        return mv_value[0] = a;
    } // SetValues

    Val SetValues(Value a, Value b, Value c, Value d)
    {
        m_n = Fixnum::Encode(4);
        mv_value[3] = d;
        mv_value[2] = c;
        mv_value[1] = b;
        return mv_value[0] = a;
    } // SetValues

    Val SetValues(Value a, Value b, Value c, Value d, Value e)
    {
        m_n = Fixnum::Encode(5);
        mv_value[4] = e;
        mv_value[3] = d;
        mv_value[2] = c;
        mv_value[1] = b;
        return mv_value[0] = a;
    } // SetValues

    Val SetValues(
        Value a, Value b, Value c, Value d, Value e,
        Value f )
    {
        m_n = Fixnum::Encode(6);
        mv_value[5] = f;
        mv_value[4] = e;
        mv_value[3] = d;
        mv_value[2] = c;
        mv_value[1] = b;
        return mv_value[0] = a;
    } // SetValues

    Val SetValues(
        Value a, Value b, Value c, Value d, Value e,
        Value f, Value g )
    {
        m_n = Fixnum::Encode(7);
        mv_value[6] = g;
        mv_value[5] = f;
        mv_value[4] = e;
        mv_value[3] = d;
        mv_value[2] = c;
        mv_value[1] = b;
        return mv_value[0] = a;
    } // SetValues

    Val SetValues(
        Value a, Value b, Value c, Value d, Value e,
        Value f, Value g, Value h )
    {
        m_n = Fixnum::Encode(8);
        mv_value[7] = h;
        mv_value[6] = g;
        mv_value[5] = f;
        mv_value[4] = e;
        mv_value[3] = d;
        mv_value[2] = c;
        mv_value[1] = b;
        return mv_value[0] = a;
    } // SetValues

    Val SetValues(
        Value a, Value b, Value c, Value d, Value e,
        Value f, Value g, Value h, Value i )
    {
        m_n = Fixnum::Encode(9);
        mv_value[8] = i;
        mv_value[7] = h;
        mv_value[6] = g;
        mv_value[5] = f;
        mv_value[4] = e;
        mv_value[3] = d;
        mv_value[2] = c;
        mv_value[1] = b;
        return mv_value[0] = a;
    } // SetValues

    Val SetValues(
        Value a, Value b, Value c, Value d, Value e,
        Value f, Value g, Value h, Value i, Value j )
    {
        m_n = Fixnum::Encode(10);
        mv_value[9] = j;
        mv_value[8] = i;
        mv_value[7] = h;
        mv_value[6] = g;
        mv_value[5] = f;
        mv_value[4] = e;
        mv_value[3] = d;
        mv_value[2] = c;
        mv_value[1] = b;
        return mv_value[0] = a;
    } // SetValues

    public: bool Interrupt(Val);

    // SuspendScope
    public: class SuspendScope
    {
        Thread* m_pThread;

        public: SuspendScope(Thread* p ) : m_pThread(p)
            { ::SuspendThread(p->m_hThread); }

        public: ~SuspendScope()
            { ::ResumeThread(m_pThread->m_hThread); }
    }; // SuspendScope

    // SaveContext
    class SaveContext
    {
        Val     m_fn;
        Val     m_n;
        Val     mv_value[Host::Multiple_Values_Limit];

        public: SaveContext()
        {
            Thread* p = Get();

            m_fn = p->m_fn;
            m_n  = p->m_n;

            ::memcpy(
                mv_value,
                p->mv_value,
                Fixnum::Decode_(m_n) * sizeof(Val) );
        } // SaveContext

        public: ~SaveContext()
        {
            Thread* p = Get();

            p->m_fn = m_fn;
            p->m_n  = m_n;

            ::memcpy(
                p->mv_value,
                mv_value,
                Fixnum::Decode_(m_n) * sizeof(Val) );
        } // ~SaveContext
    }; // SaveContext
}; // Thread


void* __fastcall SVC_C_stack_alloc(Thread*, size_t);
Area* __fastcall SVC_C_alloc_cons_area(Thread*, size_t);
Area* __fastcall SVC_C_alloc_symb_area(Thread*, size_t);
Area* __fastcall SVC_C_alloc_reco_area(Thread*, size_t);
Area* __fastcall SVC_C_alloc_bino_area(Thread*, size_t);
Area* __fastcall SVC_C_alloc_code_area(Thread*, size_t);

} // Kernel

#endif //!defined(INCLUDE_kernel_thread_h)
