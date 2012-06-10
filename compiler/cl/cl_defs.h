//////////////////////////////////////////////////////////////////////////////
//
// evcl - compiler - Common Lisp Parser Declarations
// compiler/cl/cl_defs.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cl/cl_defs.h#35 $
//
#if !defined(INCLUDE_compiler_cl_defs_h)
#define INCLUDE_compiler_cl_defs_h

#include "../cm/cm_fns.h"
#include "../cm/cm_pass.h"
#include "../cm/cm_session.h"

#include "../ir/ir_defs.h"
#include "../ir/ir_pass.h"

namespace Compiler
{

//////////////////////////////////////////////////////////////////////
//
// ClParser
//
class ClParser : public IrPass
{
    public: ClParser();
    public: void Run();
    protected: Function* run(Val, Ty, Val);

    protected: enum { MaxFormLength = 1 << 30 };

    protected: class Context;
    protected: class LexEnv;

    protected: class FunDef;
    protected: class FunDcl;
    protected: class ParamDef;
    protected: class VarDef;
    protected: class VarDcl;

    protected: Val m_funtab;   // lexical functions
    protected: Val m_vartab;   // lexical varibles
    protected: Val m_ltv_counter;

    protected: Context* m_pContext;
        public: Context* GetContext()
            { return m_pContext; }

        public: Context* SetContext(Context* pContext)
            { return m_pContext = pContext; }

    ////////////////////////////////////////////////////////////
    //
    // NameDcl
    //
    protected: class NameDcl :
        public Object
    {
        public: enum Class
        {
            Class_VarDcl,
            Class_VarDef,
            Class_VarPcl,
            Class_FunDcl,
            Class_FunDef,
            Class_FunPcl,
            Class_ParamDef,
        }; // Class

        public: enum Flag
        {
            Flag_Inline         = 1 << 0,
            Flag_NotInline      = 1 << 1,
            Flag_DynamicExtent  = 1 << 2,
        }; // Flag

        public: enum Usage
        {
            Usage_None,
            Usage_Ignore,
            Usage_Ignorable,
            Usage_Used,
        }; // Usage

        protected: LexEnv*  m_pLexEnv;
        protected: Val      m_name; // (or symbol value-cell)
        protected: Val      m_ty;
        protected: Val      m_datum;
        protected: Class    m_eClass;
        protected: Usage    m_eUsage;
        protected: uint     m_nFlags;

        public: NameDcl(Class eClass, Val name = nil, Val datum = nil) :
            m_eClass(eClass),
            m_name(name),
            m_eUsage(Usage_None),
            m_datum(datum),
            m_nFlags(0),
            m_pLexEnv(NULL) {}

        public: Class GetClass()   const { return m_eClass; }
        public: Val   GetDatum()   const { return m_datum; }
        public: Val   GetName()    const { return m_name; }
        public: Val   GetTy()      const { return m_ty; }
        public: Val   SetTy(Val ty)      { return m_ty = ty; }

        public: LexEnv* GetLexEnv() const    { return m_pLexEnv; }
        public: LexEnv* SetLexEnv(LexEnv* p) { return m_pLexEnv = p; }

        public: Usage GetUsage() const  { return m_eUsage; }
        public: Usage SetUsage(Usage e) { return m_eUsage = e; }

        public: uint GetFlags() const { return m_nFlags; }
        public: uint ClrFlags(uint n) { return m_nFlags &= ~n; }
        public: uint SetFlags(uint n) { return m_nFlags |= n; }

        public: bool GetFlag(uint n)
            { return 0 != (m_nFlags & n); }

        public: template<class T> T* DynamicCast()
            { return Is<T>() ? reinterpret_cast<T*>(this) : NULL; }

        public: template<class T> bool Is() const
            { return GetClass() == T::GetClass_(); }

        public: template<class T> T* StaticCast()
            { ASSERT(Is<T>()); return reinterpret_cast<T*>(this); }
    }; // NameDcl

    ////////////////////////////////////////////////////////////
    //
    // VarDcl
    //
    protected: class VarDcl :
        public NameDcl,
        public DLinkSite_<VarDcl>
    {
        public: static Class GetClass_() { return Class_VarDcl; } 

        public: enum Kind
        {
            Kind_Anchor,
            Kind_Undef,
            Kind_Lexical,
            Kind_Special,
            Kind_SymbolMacro,
            Kind_Constant,
        }; // Kind

        protected: VarDcl* m_pOuter;
            public: VarDcl* GetOuter()    const { return m_pOuter; }
            public: VarDcl* SetOuter(VarDcl* p) { return m_pOuter = p; }

        protected: Kind m_eKind;
            public: Kind GetKind() const
                { return m_eKind; }

            public: bool IsSpecial() const
                { return Kind_Special == m_eKind; }

            public: void MarkSpecial()
                { m_eKind = Kind_Special; }

        public: VarDcl() : 
            NameDcl(Class_VarDcl),
            m_eKind(Kind_Anchor),
            m_pOuter(NULL) {}

        public: VarDcl(
            Kind    eKind,
            Val     name,
            Val     datum,
            Class   eClass = Class_VarDcl,
            Ty      ty = ty_t ) :
                NameDcl(eClass, name, datum),
                m_eKind(eKind),
                m_pOuter(NULL)
        {
            ASSERT(symbolp(name));
            m_ty = ty;
        } // VarDcl

        public: Variable* GetVar() const
            { return m_datum->StaticCast<Variable>(); }
    }; // VarDcl

    // VarPcl
    class VarPcl : public VarDcl
    {
        public: static Class GetClass_() { return Class_VarPcl; }

        public: VarPcl(Kind eKind, Val name, Val datum, Val ty) :
            VarDcl(eKind, name, datum, GetClass_(), ty) {}
    }; // VarPcl

    // VarDef
    class VarDef :
        public VarDcl,
        public WorkListItem_<VarDef>
    {
        public: static Class GetClass_() { return Class_VarDef; } 

        protected: Val m_initform;
            public: Val GetInitForm() const { return m_initform; }

        public: VarDef(
            Kind        eKind,
            Val         name,
            Variable*   pVar,
            Val         initform ) :
                VarDcl(eKind, name, Fixnum::Encode(pVar), GetClass_()),
                m_initform(initform) {}

        public: Function* GetOwner() const;
    }; // VarDef

    // ParamDefSite
    class ParamDefSite :
        public DLinkSite_<ParamDefSite, ParamDef> {};

    ////////////////////////////////////////////////////////////
    //
    // ParamDef
    //
    class ParamDef :
        public VarDef,
        public ParamDefSite
    {
        public: Val         m_key;
        public: VarDef*     m_pSupVar;  // for supplied-p
        public: Register*   m_pRx;

        public: ParamDef(
            Kind        eKind = Kind_Anchor,
            Val         name = nil, 
            Variable*   pVar = NULL,
            Val         initform = nil ) :
                VarDef(eKind, name, pVar, initform),
                m_pSupVar(NULL),
                m_pRx(NULL),
                m_key(nil) {}
    }; // ParamDef

    ////////////////////////////////////////////////////////////
    //
    // ParamList
    //
    class ParamList :
        public DLinkAnchor_<ParamDefSite, ParamDef>
    {
        public: ParamDef* Append(ParamDef* rParamDef)
        {
            return Append_(rParamDef);
        } // Append
    }; // ParamList

    ////////////////////////////////////////////////////////////
    //
    // LambdaList
    //
    class LambdaList
    {
        public: ParamList m_oReqs;
        public: ParamList m_oOpts;
        public: ParamDef*   m_pRest;
        public: ParamList m_oKeys;
        public: Val         m_key;

        // BUGBUG: Since simple class system can't distinguish VarDef
        // and ParamDef, we keep auxilliary variables here instead of
        // LexEnv.
        public: ParamList m_oAuxs;

        public: LambdaList() : m_pRest(NULL), m_key(nil) {}
    }; // LambdaList

    ////////////////////////////////////////////////////////////
    //
    // FunDcl
    //
    class FunDcl :
        public NameDcl,
        public DLinkSite_<FunDcl>
    {
        public: static Class GetClass_() { return Class_FunDcl; } 

        public: enum Kind
        {
            Kind_Anchor,
            Kind_Undef,
            Kind_Function,
            Kind_Macro,
            Kind_SpecialOperator,
        }; // Kind

        protected: FunDcl* m_pOuter;
            public: FunDcl* GetOuter() const { return m_pOuter; }
            public: void SetOuter(FunDcl* p) { m_pOuter = p; }

        protected: Kind m_eKind;
            public: Kind GetKind() const { return m_eKind; }

        public: FunDcl(
            Kind    eKind = Kind_Anchor,
            Val     name = nil,
            Val     datum = nil,
            Class   eClass = Class_FunDcl,
            Ty      ty = ty_function ) :
                NameDcl(eClass, name, datum),
                m_eKind(eKind),
                m_pOuter(NULL)
            {
                ASSERT(symbolp(name) || setf_cell_p(name));
                m_ty = ty;
            } // FunDcl
    }; // FunDcl

    // FunPcl
    class FunPcl : public FunDcl
    {
        public: static Class GetClass_() { return Class_FunPcl; }

        public: Val m_alist;

        public: Val GetAlist() const
        { 
            ASSERT(Kind_Function == GetKind());
            return GetDatum();
        } // GetAlist

        public: FunPcl(Kind eKind, Val name, Val datum, Ty ty) :
            FunDcl(eKind, name, datum, GetClass_(), ty)
        {
            m_pLexEnv = NULL;
        } // FunPcl
    }; // FunPcl

    // LexEnv
    class LexEnv
    {
        public: enum Kind
        {
            Kind_unknown,
            Kind_flet,
            Kind_labels,
            Kind_lambda,
            Kind_let,
            Kind_locally,
            Kind_macrolet,
            Kind_symbol_macrolet,
            Kind_toplevel,
        }; // Kind

        private: LexEnv* operator =(const LexEnv*)
            { CAN_NOT_HAPPEN(); }

        protected: typedef DLinkAnchor_<FunDcl> FunDclList;
        protected: typedef DLinkAnchor_<VarDcl> VarDclList;

        protected: Kind m_eKind;
            public: Kind GetKind() const { return m_eKind; }

        protected: Function* m_rOwner;
            public: Function* GetOwner() const { return m_rOwner; }

        protected: Val m_ty;
            public: Val GetTy() const { return m_ty; }
            public: Val SetTy(Val ty) { return m_ty = ty; }

        // For generating lambda name
        protected: LexEnv* m_pOuter;
            public: LexEnv* GetOuter() const    { return m_pOuter; }
            public: LexEnv* SetOuter(LexEnv* p)
                { ASSERT(this != p); return m_pOuter = p; }

        protected: FunDclList m_oFunctions;
        protected: VarDclList m_oVariables;

        protected: OptimizeQualities m_oSavedQualities;

        public: LexEnv(
            Kind        eKind,
            Function*   pOwner,
            Ty          value_ty = ty_values_rest_t ) :
                m_eKind(eKind),
                m_pOuter(NULL),
                m_rOwner(pOwner),
                m_ty(value_ty)
        {
            m_oSavedQualities = Session::Get()->m_oOptimizeQualities;
        } // LexEnv

        public: ~LexEnv()
        {
            Session::Get()->m_oOptimizeQualities = m_oSavedQualities;
        } // ~LexEnv

        // Add
        public: FunDcl* Add(FunDcl* pFunDcl)
        {
            return m_oFunctions.Append_(pFunDcl);
        } // Add

        // Add
        public: VarDcl* Add(VarDcl* pVarDcl)
        {
            return m_oVariables.Append_(pVarDcl);
        } // Add

        // EnumFun
        public: class EnumFun : public FunDclList::Enum
        {
            public: EnumFun(LexEnv* pLexEnv) :
                FunDclList::Enum(&pLexEnv->m_oFunctions) {}
        }; // EnumFun

        // EnumVar
        public: class EnumVar : public VarDclList::Enum
        {
            public: EnumVar(LexEnv* pLexEnv) :
                VarDclList::Enum(&pLexEnv->m_oVariables) {}
        }; // EnumVar
    }; // LexEnv

    ////////////////////////////////////////////////////////////
    //
    // FunDef
    //
    class FunDef : public FunDcl
    {
        public: static Class GetClass_() { return Class_FunDef; } 

        public: Val         m_forms;
        public: LambdaList  m_oLambdaList;
        public: LexEnv      m_oLexEnv;

        public: Function* GetFunction() const
            { return GetDatum()->StaticCast<Function>(); }

        public: FunDef(Val name, Function* pFun) :
            FunDcl(Kind_Function, name, Fixnum::Encode(pFun), GetClass_()), 
            m_forms(nil),
            m_oLexEnv(LexEnv::Kind_lambda, pFun) {}
    }; // FunDef

    ////////////////////////////////////////////////////////////
    //
    // Callee
    //
    struct Callee
    {
        Operand*    m_pSx;          // callee operand
        Ty          m_ty;           // function
        bool        m_fNotInline;

        Callee() :
            m_pSx(Obj_Void),
            m_ty(ty_function),
            m_fNotInline(false) {}
    }; // Callee

    ////////////////////////////////////////////////////////////
    //
    // Context
    //
    class Context
    {
        protected: BBlock* m_pCurr;
            public: BBlock* GetCurr()     const { return m_pCurr; }
            public: BBlock* SetCurr(BBlock* p)  { return m_pCurr = p; }

        protected: BBlock* m_pSucc;
            public: BBlock* GetSucc()     const { return m_pSucc; }
            public: BBlock* SetSucc(BBlock* p)  { return m_pSucc = p; }

        protected: LexEnv*  m_pLexEnv;
            public: LexEnv* GetLexEnv() const { return m_pLexEnv; }

        public: Function* GetFunction() const;

        public: enum Linkage
        {
            Linkage_Jump,
            Linkage_Next,
            Linkage_Phi,
            Linkage_Return,
            Linkage_Unreachable,
        }; // Linkage

        public: Context(Function*, LexEnv*);
        public: Linkage GetLinkage()  const;

        public: void RestoreSucc(BBlock* pSucc)
            { if (NULL != pSucc) m_pSucc = pSucc; }

        public: BBlock* SetContinue()
        {
            if (Linkage_Next == GetLinkage())
            {
                return NULL;
            }
            else
            {
                BBlock* pSucc = m_pSucc;
                m_pSucc = m_pCurr;
                return pSucc;
            }
        } // SetContinue

        public: void SetCurrSucc(BBlock* rCurr, BBlock* pSucc)
        {
            m_pCurr = rCurr;
            m_pSucc = NULL != pSucc ? pSucc : rCurr;
        } // SetCurrSucc

        public: Operand* SetUnreachable()
            { m_pCurr = NULL; return Obj_Unreachable; }

        public: class LexEnvScope
        {
            protected: Context* m_pContext;

            public: LexEnvScope(Context* pContext, LexEnv* pLexEnv) :
                m_pContext(pContext)
            {
                ASSERT(pLexEnv->GetOwner() == m_pContext->GetFunction());
                pLexEnv->SetOuter(m_pContext->m_pLexEnv);
                m_pContext->m_pLexEnv = pLexEnv;
            } // LexEnvScope

            public: ~LexEnvScope()
            {
                m_pContext->m_pLexEnv = m_pContext->m_pLexEnv->GetOuter();
            } // ~LexEnvScope
        }; // LexEnvScope

        public: class BindingScope
        {
            public: BindingScope(LexEnv::Kind, Context*, Ty value_ty);
            public: ~BindingScope();

            protected: LexEnv   m_oLexEnv;
                public: LexEnv* GetLexEnv() { return &m_oLexEnv; }

            protected: Context* m_pContext;
        }; // BindingScope
    }; // Context

    ////////////////////////////////////////////////////////////
    //
    // ContextScope
    //
    class ContextScope
    {
        protected: ClParser*    m_pParser;
        protected: Context*     m_pSaved;

        public: ContextScope(ClParser* pParser, Context* pContext) :
            m_pParser(pParser)
        {
            m_pSaved = pParser->GetContext();
            pParser->SetContext(pContext);
        } // ContextScope

        public: ~ContextScope()
        {
            m_pParser->SetContext(m_pSaved);
        } // ~ContextScope
    }; // ContextScope

    ////////////////////////////////////////////////////////////
    //
    // Expected Type Information
    //
    enum ExpectContext
    {
        ExpectContext_Argument,
        ExpectContext_Assign,
        ExpectContext_Bind,
        ExpectContext_Callee,
        ExpectContext_KeyName,
        ExpectContext_KeyVal,
        ExpectContext_Operand,
        ExpectContext_Unreachable,
        ExpectContext_Value,
    }; // ExpectContext

    struct Expect
    {
        ExpectContext   Context;
        Val             Name;
        Ty              Type;
        Val             Nth;

        Expect(Ty ty) :
            Context(ExpectContext_Value),
            Type(ty),
            Name(nil),
            Nth(Fixnum::Encode(0)) {}

        Expect(Val name, Ty ty) :
            Context(ExpectContext_Value),
            Type(ty),
            Name(name),
            Nth(Fixnum::Encode(0)) {}

        Expect(ExpectContext eContext, Ty ty) :
            Context(eContext),
            Type(ty),
            Name(nil),
            Nth(Fixnum::Encode(0)) {}

        Expect(const VarDef* pVarDef) :
            Context(ExpectContext_Bind),
            Name(pVarDef->GetName()),
            Type(pVarDef->GetTy()),
            Nth(Fixnum::Encode(0)) {}
    }; // Expect

    struct ExpectArgument : public Expect
    {
        ExpectArgument(Val name, uint nNth, Ty ty) :
            Expect(ExpectContext_Argument, ty)
        {
            Name    = name;
            Nth     = Fixnum::Encode(nNth);
        } // ExpectArgument
    }; // ExpectArgument

    struct ExpectAssign : public Expect
    {
        ExpectAssign(Ty ty, Val name) :
            Expect(ExpectContext_Assign, ty)
        {
            Context = ExpectContext_Assign;
            Name    = name;
        } // ExpectAssign
    }; // ExpectAssign

    struct ExpectOperand : public Expect
    {
        ExpectOperand(Val name, uint nNth, Ty ty) :
            Expect(ExpectContext_Operand, ty)
        {
            Name    = name;
            Nth     = Fixnum::Encode(nNth);
        } // ExpectOperand
    }; // ExpectOperand

    ////////////////////////////////////////////////////////////
    //
    // Environment
    //
    void       activateFreeDcls();
    void       activateLexEnv();
    FunDcl*    activateFunDcl(FunDcl*);
    VarDcl*    activateVarDcl(VarDcl*);
    FunDcl*    addFunDcl(FunDcl*);
    VarDcl*    addVarDcl(VarDcl*);
    void       closeLexEnv();
    void       deactivateLexEnv();

    FunDcl*    getFunDcl(Val);
    FunPcl*    getFunPcl(Val);
    FunPcl*    getFunPclAux(Val, Val);

    VarDcl*    getVarDcl(Val);
    VarDcl*    getVarDclAux(Val, Val);

    bool       isSpecialVariable(Val);
    Variable*  newVariable(Val);

    FunDcl*     parseDclFunName(Val);
    NameDcl*    parseDclFunNameOrVarName(Val);
    VarDcl*     parseDclVarName(Val);

    void markFunUse(FunDcl*);
    void markVarUse(VarDcl*);

    ////////////////////////////////////////////////////////////
    //
    // eval-when
    //

    // ToplevelFormScope
    //
    // See parseCompoundForm and parse_eval_when
    //
    class ToplevelFormScope
    {
        protected: ClParser* m_pParser;
        protected: bool m_fToplevelForm;

        public: ToplevelFormScope(ClParser* p) :
            m_pParser(p),
            m_fToplevelForm(p->m_fToplevelForm)
        {
            m_pParser->m_fToplevelForm = false;
        } // ToplevelFormScope

        public: ~ToplevelFormScope()
        {
            m_pParser->m_fToplevelForm = m_fToplevelForm;
        } // ~ToplevelFormScope

        public: void StillToplevel()
        {
            m_pParser->m_fToplevelForm = m_fToplevelForm;
        } // StillToplevel
    }; // ToplevelFormScope

    // True if current processing form is considered as toplevel form.
    bool m_fToplevelForm;


    ////////////////////////////////////////////////////////////
    //
    // Macro
    //
    Val callMacroExpander(Val, Val);

    ////////////////////////////////////////////////////////////
    //
    // Miscellaneous
    //
    ParamDef* makeParamDef(Val, Val = nil);
    VarDef*   makeVarDef(Val, Val = nil);

    Function* GetFunction()
        { return GetContext()->GetFunction(); }

    LexEnv* GetLexEnv()
        { return GetContext()->GetLexEnv(); }

    Register* newRegister(Val form)
        { Register* pRx = new Register(); pRx->SetForm(form); return pRx; }

    ////////////////////////////////////////////////////////////
    //
    // Variable
    //
    Register* internVarCell(Variable*);
    void mergeUpVars(Function*);

    ////////////////////////////////////////////////////////////
    //
    // Warnings
    //
    Operand* check_syntax(Val, int, int, LPCWSTR);

    #define CHECK_SYNTAX(min, max, syntax) \
    { \
        Operand* pSx = check_syntax(form, \
            min, max, L##syntax ); \
        if (NULL != pSx) return pSx; \
    }

    void     bound_to_extra_value(VarDef*);
    Operand* ignore_form(Val);
    Operand* ignore_args(Val);
    void     malformed_binding(Val);
    Operand* malformed_form(Val);
    Operand* not_function(Val);
    void     unexpected_type(const Expect*, Ty, Val);
    void     unportable_form(Val);
    Operand* unreachable_form(Val);
    Operand* useless_form(Val);

    Operand* CanNotHappen()
        { warn(L"Can't happen!"); return emitLinkage(Obj_Unreachable); }

    ////////////////////////////////////////////////////////////
    //
    // Emitters
    //
    OpenBindInsn*   emitBind(VarDef*, Operand*, OpenBindInsn* = NULL);
    Register*       emitBool(Bool*);
    Operand*        emitCast(const Expect*, Val, Operand*);
    Operand*        emitCall(const Expect*, Val, Callee*, Values*);
    Instruction*    emitInsn(Instruction*);
    Operand*        emitLinkage(Operand*);
    Register*       emitRuntimeCast(Ty, Register*, uint = 0);
    Operand*        emitRestoreValues(Operand*);
    Operand*        emitReturn(Operand*);
    Operand*        emitSaveValues(Operand*);
    Operand*        emitSucc();
    Operand*        emitUnreachable();
    void            emitUnwind(Frame*);
    void            emitUnwinds(Frame*);
    Operand*        emitWeakRestoreValues(Operand*);
    Operand*        emitWeakSaveValues(Operand*);

    Operand* emitCall(const Expect*, Val, Ty, Val, Operand*);
    Operand* emitCall(const Expect*, Val, Ty, Val, Operand*, Operand*);

    Operand* emitCall(
        const Expect*   pExpect,
        Val             form,
        Ty              ty,
        Val             fname,
        Operand*        pSx,
        Val             sy )
    {
        return emitCall(pExpect, form, ty, fname, pSx, NewLiteral(sy));
    } // emitCall

    Operand* emitCast(const Expect* p, Val form, Val x)
        { return emitCast(p, form, NewLiteral(x)); }

    ////////////////////////////////////////////////////////////
    //
    // Control Flow
    //
    BBlock*      newBBlock();
    Operand*     endPhiContext(Ty);
    BBlock*      startPhiContext(Ty);

    ////////////////////////////////////////////////////////////
    //
    // Optimization
    //
    static bool optimize_VARDEF(Variable*);

    ////////////////////////////////////////////////////////////
    //
    // Parsers
    //
    typedef Operand* (ClParser::*ParserT)(const Expect*, Val);
    void install_parsers();

    Operand* parseBlockAux(const Expect*, Val, Val);
    Operand* parseCompoundForm(const Expect*, Val);
    Operand* parse_cons(const Expect*, Val);
    Operand* parseFunctionForm(const Expect*, Val, FunDcl*);
    Operand* parseFunctionFormAux(const Expect*, Val, FunDcl*);
    Operand* parseForm(const Expect*, Val);
    Operand* parseForms(const Expect*, Val);
    Operand* parseForm1(const Expect*, Val);
    bool     parseForm2(const Expect*, Ty, Val, Operand**, Operand**);
    Operand* parseReturnAux(const Expect*, Val, Val, Val);
    Operand* parseLiteral(const Expect*, Val);
    Operand* parseSymbol(const Expect*, Val);

    Operand* parseTypePredicate(const Expect*, Val, Ty);
    Operand* processTypep(const Expect*,Val, Ty, Operand*);

    Val  parse_declarations(Val, Function* = NULL);
    void parse_declare(Val, Function*);
        void parse_declare_declaration(Val, Function*);
        void parse_declare_dynamic_extent(Val, Function*);
        void parse_declare_ftype(Val, Function*);
        void parse_declare_ignore(Val, Function*);
        void parse_declare_ignorable(Val, Function*);
        void parse_declare_inline(Val, Function*);
        void parse_declare_lambda_name(Val, Function*);
        void parse_declare_notinline(Val, Function*);
        void parse_declare_optimize(Val, Function*);
        void parse_declare_special(Val, Function*);
        void parse_declare_type(Val, Function*);
        void parse_declare_values(Val, Function*);

    Val computeFunctionName(Val, Val);
    void parseFunctionBindings(Val);
    void parseVariableBindings(Val);

    // Lambda
    Ty computeFunctionType(const LambdaList*, Ty);
    Function* parse_lambda(const Expect*, Val);
    Val parseLambdaList(LexEnv*, Val, LambdaList*);
    void processLambda(Val, Function*, LexEnv*, const LambdaList*, Val);

    bool processLambdaList(Function*, const LambdaList*);
    OpenBindInsn* processLambdaListOptional(BBlock*, ParamDef*, Bool*);

    // Funcall
    Operand* parseCallee(Val, Callee*);
    Operand* parseArgs(Val, Callee*, Val);
    Operand* parseCall(const Expect*, Val, Callee*, Val);

    Operand* parseArg(Val fname, Ty ty, uint k, Val form)
    {
        ExpectArgument oExpect(fname, k, ty);
        return parseForm1(&oExpect, form);
    } // parseArg

    Operand* parseOpd(Val fname, Ty ty, uint k, Val form)
    {
        ExpectOperand oExpect(fname, k, ty);
        return parseForm1(&oExpect, form);
    } // parseOpd

    // Parsers
    #define declare_parser(mp_name) \
        Operand* parse_##mp_name(const Expect*, Val);

    #define declare_parser_setf(mp_name) \
        Operand* parse_setf_##mp_name(const Expect*, Val form);

    #include "./cl_parser.inc"

    #define define_special_operator(mp_name) \
        Operand* ClParser::parse_##mp_name(const Expect* pExpect, Val form)

    #define define_parser(mp_name) \
        Operand* ClParser::parse_##mp_name(const Expect* pExpect, Val form)

    #define define_parser_setf(mp_name) \
        Operand* ClParser::parse_setf_##mp_name(const Expect* pExpect, Val form)

    // ParserTable
    class ParserTable
    {
        struct Entry
        {
            Val     m_name;
            ParserT m_pfn;
        }; // Entry

        protected: uint m_cParsers;
            public: uint GetCount() { return m_cParsers; }

        protected: Entry m_rgoEntry[511];

        public: ParserT Get(Val) const;
        public: void Set(Val, ParserT);

        public: ParserTable() :
            m_cParsers(0)
        {
            evcl_memset(m_rgoEntry, 0, sizeof(m_rgoEntry));
        } // ParserTable
    }; // ParserTable

    protected: static ParserTable s_oParserTable;

    Operand* parseEqAux(const Expect*, Val, Operand*, Operand*);
    Operand* parseLetAux(const Expect*, Val, OpenBindInsn*);

    Operand* parse_cmp(const Expect*, Val, const char16*, Ty, Val);

    ////////////////////////////////////////////////////////////
    //
    // Frame related methods
    //
    Frame* m_pFrame;

    Frame* popFrame()
    {
        Frame* pFrame = m_pFrame;
        m_pFrame = m_pFrame->GetOuter();
        return pFrame;
    } // popFrame

    Frame* pushFrame(Frame* pFrame)
        { pFrame->SetOuter(m_pFrame); return m_pFrame = pFrame; }

    // BindFrame
    class BindFrame : public Frame
    {
        public: static Val GetFrameKind_() { return Qlet; }

        public: BindFrame(Function* pOwner) :
            Frame(pOwner, Qlet) {}
    }; // BindFrame

    // BlockFrame
    class BlockFrame : public Frame
    {
        public: static Val GetFrameKind_() { return Qblock; }

        private: BlockFrame* operator =(const BlockFrame*)
            { CAN_NOT_HAPPEN(); }

        public: BlockFrame(
            Function*       pOwner,
            Val             name,
            const Expect*   pExpect,
            BBlock*         pLocalXp,
            BBlock*         pNonlocalXp ) :
                Frame(pOwner, GetFrameKind_(), name),
                m_pExpect(pExpect),
                m_pLocalXp(pLocalXp),
                m_pNonlocalXp(pNonlocalXp),
                m_pVar(NULL) {}

        public: const Expect*   m_pExpect;
        public: BBlock*         m_pLocalXp;
        public: BBlock*         m_pNonlocalXp;
        public: Variable*       m_pVar;
    }; // BlockFrame

    // CatchFrame
    class CatchFrame : public Frame
    {
        public: static Val GetFrameKind_() { return Qcatch; }

        private: CatchFrame* operator =(const CatchFrame*)
            { CAN_NOT_HAPPEN(); }

        public: CatchFrame(
            Function*       pOwner,
            Operand*        pTag,
            const Expect*   pExpect,
            BBlock*         pLocalXp,
            BBlock*         pNonlocalXp ) :
                Frame(pOwner, GetFrameKind_(), nil),
                m_pTag(pTag),
                m_pExpect(pExpect),
                m_pLocalXp(pLocalXp),
                m_pNonlocalXp(pNonlocalXp) {}

        public: const Expect*   m_pExpect;
        public: Operand*        m_pTag;
        public: BBlock*         m_pLocalXp;
        public: BBlock*         m_pNonlocalXp;
    }; // CatchFrame

    // FinallyFrame
    class FinallyFrame : public Frame
    {
        public: static Val GetFrameKind_() { return Kfinally; }

        public: Function* GetFinally() const
            { return m_pDatum->StaticCast<Function>(); }

        public: FinallyFrame(
            Function*   pOwner,
            Function*   pFinally ) :
                Frame(pOwner,  GetFrameKind_())
        {
            m_pDatum = pFinally;
        } // FinallyFrame
    }; // FinallyFrame

    // SimpleFrame
    class SimpleFrame : public Frame
    {
        public: static Val GetFrameKind_() { return Kcode; }

        public: SimpleFrame(
            Function*   pOwner ) :
                Frame(pOwner, GetFrameKind_()) {}
    }; // SimpleFrame

    // TagbodyFrame
    class TagbodyFrame :
        public Frame
    {
        public: static Val GetFrameKind_() { return Qtagbody; }

        public: TagbodyFrame(Function* pOwner) :
                Frame(pOwner, GetFrameKind_(), Fixnum::Encode(0)) {}

        // Tag
        public: class Tag : public WorkListItem_<Tag>
        {
            protected: BBlock* m_pBBlock;
                public: BBlock* GetBBlock() const { return m_pBBlock; }

            protected: Val m_name;
                public: Val GetName() const { return m_name; }

            protected: TagbodyFrame* m_pFrame;
                public: TagbodyFrame* GetFrame() const { return m_pFrame; }

            protected: bool m_fDefined;
                public: bool IsDefined() const { return m_fDefined; }
                public: void Def()             { m_fDefined = true; }

            protected: bool m_fUsed;
                public: bool IsUsed() const { return m_fUsed; }
                public: void Use() { m_fUsed = true; }

            public: Variable* m_pVar;

            public: Tag(TagbodyFrame* pFrame, Val name, BBlock* pBBlock) :
                m_pBBlock(pBBlock),
                m_pFrame(pFrame),
                m_name(name),
                m_fDefined(false),
                m_fUsed(false),
                m_pVar(NULL) {}
        }; // Tag

        public: typedef WorkList_<Tag> TagList;

        protected: TagList m_oTags;

        public: Tag* AddTag(Tag* pTag)
        {
            return m_oTags.Push(pTag);
        } // AddTag

        public: Tag* FindTag(Val name) const
        {
            foreach (EnumTag, oEnum, this)
            {
                Tag* pTag = oEnum.Get();
                if (pTag->GetName() == name)
                {
                    return pTag;
                }
            } // for each tag
            return NULL;
        } // FindTag

        public: class EnumTag : public TagList::Enum
        {
            public: EnumTag(const TagbodyFrame* p) :
                TagList::Enum(&p->m_oTags) {}
        }; // Enum
    }; // TagbodyFrame

    ////////////////////////////////////////////////////////////
    //
    // Compilation Options
    //
    bool option_no_type_check();
    bool option_simple_type_check();
    bool option_check_index();
    bool option_check_parameter_count(Function*);
    bool option_check_type();
    bool option_check_undefined_function(FunPcl*);
    bool option_check_unbound_variable(Val);
}; // ClParser

} // Compiler

#endif //!defined(INCLUDE_compiler_cl_defs_h)
