//////////////////////////////////////////////////////////////////////////////
//
// evcl - compiler - cg - Assembler
// cg/cg_asm.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cg/cg_asm.h#4 $
//
#if !defined(INCLUDE_compiler_cg_asm_h)
#define INCLUDE_compiler_cg_asm_h

#include "./cg_defs.h"
#include "../ir/ir_pass.h"

namespace Compiler
{

////////////////////////////////////////////////////////////
//
// Code AsmAnnon
//
class AsmAnnon : 
    public Object,
    public DLinkSite_<AsmAnnon>
{
    protected: int      m_eType;
    protected: uint     m_nAddress;
    protected: Operand* m_pOperand;

    public: AsmAnnon() :
        m_eType(static_cast<uint>(-1)),
        m_nAddress(0),
        m_pOperand(NULL) {}

    public: AsmAnnon(
        int         eType,
        Operand*    pOperand,
        uint        nAddress )
    {
        m_eType     = eType;
        m_pOperand  = pOperand;
        m_nAddress  = nAddress;
    } // AsmAnnon

    public: uint     GetAddress() const { return m_nAddress; }
    public: Operand* GetOperand() const { return m_pOperand; }
    public: int      GetType()    const { return m_eType; }

    public: uint IncAddress(int iInc) { return m_nAddress += iInc; }
}; // AsmAnnon


//////////////////////////////////////////////////////////////////////
//
// CgBaseAsmPass
//
class CgBaseAsmPass : public ModulePass
{
    //protected: class AsmAnnon;
    protected: class AsmBBlock;
    protected: class AsmContext;
    protected: class AsmFunction;

    protected: class Span;
        protected: class AlignSpan;
        protected: class JumpSpan;

    ////////////////////////////////////////////////////////////
    //
    // Span
    //
    protected: class Span :
        public DLinkSite_<Span>,
        public WorkListItem_<Span>
    {
        public: enum Kind
        {
            Kind_Anchor,
            Kind_Align,
            Kind_Jump,
        }; // Kind

        public: uint m_nAddr;
        public: uint m_ofsCode;

        protected: Kind m_eKind;
        public: Kind GetKind() const { return m_eKind; }

        public: Span() :
            m_eKind(Kind_Anchor),
            m_nAddr(static_cast<uint>(-1)),
            m_ofsCode(static_cast<uint>(-1)) {}

        public: Span(Kind eKind, uint nAddr, uint ofsCode) :
            m_eKind(eKind),
            m_nAddr(nAddr),
            m_ofsCode(ofsCode) {}

        public: template<class T> T* DynamicCast()
        {
            if (GetKind() != T::GetKind_()) return NULL;
            return reinterpret_cast<T*>(this);
        } // StaticCast

        public: template<class T> T* StaticCast()
        {
            ASSERT(GetKind() == T::GetKind_());
            return reinterpret_cast<T*>(this);
        } // StaticCast
    }; // Span

    typedef DLinkAnchor_<Span> SpanList;

    ////////////////////////////////////////////////////////////
    //
    // Context
    //
    protected: class Context
    {
        public: Context();

        enum
        {
            Default_BufSize = 16 * 1024,
            Default_Extent  = 2 * 1024,
        }; // enum

        protected: SpanList m_oSpans;
            public: SpanList* GetSpans() { return &m_oSpans; }

        protected: uint8*       m_prgbBuffer;
        protected: uint8*       m_pbBuffer;
        protected: uint         m_cbBuffer;
            public: uint GetOffset() const
                { return static_cast<uint>(m_pbBuffer - m_prgbBuffer); }

            public: BYTE* GetBuffer(uint ofs) const
                { return m_prgbBuffer + ofs; }

            public: void Reset()
            {
                m_nAddress = 0;
                m_pbBuffer = m_prgbBuffer;
                m_oSpans.RemoveAll();
            } // Reset

            public: uint8 FetchU8(uint ofs)
            {
                ASSERT(ofs < GetOffset());
                return m_prgbBuffer[ofs];
            } // FetchU8

            public: void PatchS32(uint ofs, int32 i)
                { PatchU32(ofs, static_cast<uint32>(i)); }

            public: void PatchS8(uint ofs, int8 i) { PatchU8(ofs, i); }

            public: void PatchU8(uint, uint8);
            public: void PatchU32(uint, uint32);
            public: void PatchU64(uint, uint64);

            public: void Advance(uint, uint);
            public: void EmitU8(uint8);
            public: void EmitU16(uint16);
            public: void EmitU32(uint32);
            public: void EmitU64(uint64);

            protected: void enlarge_buffer();

        protected: uint m_nAddress;
            public: uint GetAddress() const { return m_nAddress; }
            public: uint IncAddress(uint n) { return m_nAddress += n; }

        protected: Function* m_pFunction;
            public: Function* GetFunction() const { return m_pFunction; }

            public: Function* SetFunction(Function* p)
            {
                ASSERT(NULL != p);
                p->SetExtension(new AsmFunction());
                m_pFunction = p;
                Reset();
                return p;
            } // SetFunction
    }; // Context

    protected: Context m_oContext;

    // m_pInsn
    //  Current processed instruction. This member variable is used for
    //  friendly error message.
    protected: Instruction* m_pInsn;

    ////////////////////////////////////////////////////////////
    //
    // AsmBBlock
    //
    protected: class AsmBBlock :
        public Object
    {

        public: AsmBBlock(uint nAddress, uint ofsStart)
        {
            m_nAddress  = nAddress;
            m_ofsStart  = ofsStart;
            m_ofsEnd    = ofsStart;
        } // AsmBBlock

        protected: uint m_nAddress;
            public: uint GetAddress() const { return m_nAddress; }
            public: uint SetAddress(uint n) { return m_nAddress = n; }
            public: uint IncAddress(int  n) { return m_nAddress += n; }

        protected: uint m_ofsStart;
            public: uint GetStartOffset()  const { return m_ofsStart; }
            public: uint SetStartOffset(uint n)  { return m_ofsStart = n; }

        protected: uint m_ofsEnd;
            public: uint GetEndOffset()  const { return m_ofsEnd; }
            public: uint SetEndOffset(uint n)  { return m_ofsEnd = n; }
    }; // AsmBBlock

    ////////////////////////////////////////////////////////////
    //
    // AsmFunction
    //
    protected: class AsmFunction :
        public Object,
        public DLinkAnchor_<AsmAnnon>
    {
        protected: typedef DLinkAnchor_<AsmAnnon> AnnonList;

        public: class EnumAnnon : public AnnonList::Enum
        {
            public: EnumAnnon(Function* pFun) :
                AnnonList::Enum(pFun->GetExtension<AsmFunction>()) {}
        }; // EnumAnnon
    }; // AsmFunction

    ////////////////////////////////////////////////////////////
    //
    // Span
    //
    protected: class AlignSpan : public Span
    {
        public: static Kind GetKind_() { return Kind_Align; }
        public: uint m_nAlign;  // 4, 8, 16

        public: AlignSpan(uint nAddr, uint ofsCode, uint n) :
            Span(GetKind_(), nAddr, ofsCode),
            m_nAlign(n) {}
    }; // AlignSpan

    protected: CgBaseAsmPass(LPCWSTR pwsz) : ModulePass(pwsz) {}

    // Annotation
    protected: void annotate(int, Operand*);

    ////////////////////////////////////////////////////////////
    //
    // Code buffer related
    //
    protected: uint8 fetchU8(uint ofs) { return m_oContext.FetchU8(ofs); }
    protected: uint getAddress()       { return m_oContext.GetAddress(); }
    protected: uint getOffset()        { return m_oContext.GetAddress(); }

    ////////////////////////////////////////////////////////////
    //
    // Emitters
    //
    protected: void emit_s8(int8 i)   { emit_u8(static_cast<uint8>(i)); }
    protected: void emit_s16(int16 i) { emit_u16(static_cast<uint16>(i)); }
    protected: void emit_s32(int32 i) { emit_u32(static_cast<uint32>(i)); }
    protected: void emit_s64(int64 i) { emit_u64(static_cast<uint64>(i)); }

    protected: void emit_u8(uint8 n)   { m_oContext.EmitU8(n); }
    protected: void emit_u16(uint16 n) { m_oContext.EmitU16(n); }
    protected: void emit_u32(uint32 n) { m_oContext.EmitU32(n); }
    protected: void emit_u64(uint64 n) { m_oContext.EmitU64(n); }

    protected: virtual Val  make_function(Function*) = 0;
    protected: virtual void process_instruction(Instruction*) = 0;
    protected: virtual void realize_function(Function*) = 0;

    protected: void unsupported(Instruction* p)
    {
        error(L"ASM doesn't support ~S.", make_string(p->GetMnemonic()));
    } // unsupported


    protected: virtual void process_module(Module*);

    protected: Module* m_pModule;

    protected: void process_bblock(BBlock*);
    protected: void process_function(Function*);

    protected: static uint compute_path_size(Function*, Function*);

    protected: static int           get_tag(Val);
    protected: static UpVarDefInsn* get_UPVARDEF(Function*, Variable*);

    protected: Function* get_upvar_holder(Function*, Variable*);
    protected: int       get_upvar_offset(Register*, Register*);
}; // CgBaseAsmPass


// AsmCopyReg
struct AsmCopyReg
{
    uint    m_nUse;
    uint    m_iReg;

    AsmCopyReg* Reset(uint eRx)
    {
        m_iReg = eRx;
        m_nUse = 0;
        return this;
    } // Reset
}; // AsmCopyReg

// AsmCopyTask
class AsmCopyTask :
    public Atom,
    public WorkListItem_<AsmCopyTask>
{
    public: AsmCopyReg*    m_pRd;
    public: AsmCopyReg*    m_pRx;
    public: AsmCopyTask(AsmCopyReg* pRd, AsmCopyReg* pRx) :
        m_pRd(pRd), m_pRx(pRx) {}
}; // AsmCopyTask

// CopyList
typedef WorkList_<AsmCopyTask> AsmCopyTaskList;

} // Compiler

#endif // !defined(INCLUDE_compiler_cg_asm_h)
