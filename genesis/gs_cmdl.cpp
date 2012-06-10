#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - command loop
// genesis/gs_cmdl.cpp
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/genesis/gs_cmdl.cpp#15 $
//
#include "./gs_lisp.h"

#include "../kernel/ke_frame.h"
#include "../kernel/ke_memory.h"


namespace Kernel
{
    bool collect_garbage(size_t*, size_t*);
    void unwind_finally_frame(Frame*);
} // Kernel

namespace Genesis
{

const size_t k_cbGcThreshold = 1024 * 1024 * 4;

Int TLV_Acmdl_conditionA;
Int TLV_Acmdl_levelA;

// decode_frame
static Val decode_frame(Frame* pFrame)
{
    Val type;
    {
        char16 wsz[4];
            wsz[0] = static_cast<char16>(pFrame->GetType() >> 24);
            wsz[1] = static_cast<char16>((pFrame->GetType() >> 16) & 0xFF);
            wsz[2] = static_cast<char16>((pFrame->GetType() >>  8) & 0xFF);
            wsz[3] = 0;

        type = intern(wsz, PACKAGE_keyword);
    } // type

    Int iFrame = pFrame->ToInt();

    if (pFrame->GetType() == Frame::Type_Function)
    {
        iFrame = reinterpret_cast<Int>(
            pFrame->StaticCast<FunctionFrame>()->m_pval );
    }

    Val frob;
    {
        switch (pFrame->GetType())
        {
        case Frame::Type_Bind:
        {
            frob = list(
                make_int(iFrame),
                type,
                make_int(pFrame->GetOuter()->ToInt()) );

            Val last = cddr(frob);
            foreach (BindFrame::Enum, oEnum, pFrame->StaticCast<BindFrame>())
            {
                Val name = oEnum.Get()->m_name;

                if (fixnump(name))
                {
                    Int idx = name->ToInt() - offsetof(Thread, mv_tlv);
                        idx /= sizeof(Val);
                    Val tlvrec = svref(VAR(Atlv_vectorA), idx);
                    name = tlv_record_name(tlvrec);
                }

                last = setf_cdr(list(name), last);
            } // for each bind
            break;
        } // bind

        case Frame::Type_Function:
            frob = list(
                make_uint(reinterpret_cast<UInt>(
                    pFrame->StaticCast<FunctionFrame>()->m_pvRA )),
                type,
                pFrame->StaticCast<FunctionFrame>()->m_fn,
                pFrame->StaticCast<FunctionFrame>()->m_ip );

            dbg_format(L"~S~%", frob);
            break;

        default:
            frob = list(
                make_int(iFrame),
                type,
                make_int(pFrame->GetOuter()->ToInt()) );
            break;
        } // switch frame
    } // frob

    return frob;
} // decode_frame

// backtrace
Val backtrace()
{
    Val anchor = list(0);
    Val last = anchor;

    foreach (Thread::EnumFrame, oEnum, MiniThread::Get())
    {
        Frame* pFrame = oEnum.Get();
        last = setf_cdr(list(decode_frame(pFrame)), last);
    } // for each frame

    return cdr(anchor);
} // backtrace

} // Genesis


namespace
{

using namespace Genesis;


struct Command
{
    LPCWSTR         m_pwsz;
    UINT            m_nMin;
    UINT            m_nMax;
    void (*m_pfn)(Val);
}; // Command

static void remember_values()
{
    bool fShow = TLV(Astandard_inputA) == TLV(Aterminal_ioA);

    MiniThread* p = MiniThread::Get();

    if (Fixnum::Encode(0) == p->m_n)
    {
        if (fShow) format(Qt, L"; No value~%");
    }
    else if (Fixnum::Encode(1) == p->m_n)
    {
        Val val = p->mv_value[0];
        set_tlv(TLV_AAA, TLV(AA));      // (setq *** **)
        set_tlv(TLV_AA, TLV(A));        // (setq ** *)
        set_tlv(TLV_A, val);            // (setq * (car vals))

        set_tlv(TLV_SSS, TLV(SS));      // (setq /// //)
        set_tlv(TLV_SS, TLV(S));        // (setq // /)
        set_tlv(TLV_S, list(val));      // (setq / vals)

        if (fShow) format(Qt, L"; ~S: ~S~%", type_of(val), val);
    }
    else
    {
        Val vals = nil;
        {
            Int iIndex = Fixnum::Decode_(p->m_n);
            while (iIndex >= 1)
            {
                iIndex -= 1;
                vals = cons(p->mv_value[iIndex], vals);
            } // while
        } // vals

        set_tlv(TLV_AAA, TLV(AA));    // (setq *** **)
        set_tlv(TLV_AA, TLV(A));      // (setq ** *)
        set_tlv(TLV_A, car(vals));    // (setq * (car vals))

        set_tlv(TLV_SSS, TLV(SS));    // (setq /// //)
        set_tlv(TLV_SS, TLV(S));      // (setq // /)
        set_tlv(TLV_S, vals);        // (setq / vals)

        if (fShow)
        {
            Val nth = Fixnum::Encode(0);
            for (Val x = vals; ! endp(x); x = cdr(x))
            {
                format(Qt, L"; [~D] ~S: ~S~%",
                    nth,
                    type_of(car(x)),
                    car(x) );

                nth = add_xx(nth, 1);
            } // for
        }
    } // if
} // remember_values


// remember_value
static void remember_value(Val a)
{
    MiniThread::Get()->m_n = Fixnum::Encode(1);
    MiniThread::Get()->mv_value[0] = a;
    remember_values();
} // remember_value


// command_backtrace
static void command_backtrace(Val)
{
    Val nth = Fixnum::Encode(0);
    foreach (Thread::EnumFrame, oEnum, MiniThread::Get())
    {
        Frame* pFrame = oEnum.Get();

        char16 wsz[4];
            wsz[0] = static_cast<char16>(pFrame->GetType() >> 24);
            wsz[1] = static_cast<char16>((pFrame->GetType() >> 16) & 0xFF);
            wsz[2] = static_cast<char16>((pFrame->GetType() >>  8) & 0xFF);
            wsz[3] = 0;

        Int iFrame = pFrame->ToInt();

        if (pFrame->GetType() == Frame::Type_Function)
        {
            iFrame = reinterpret_cast<Int>(
                pFrame->StaticCast<FunctionFrame>()->m_pval );
        }

        format(t, L"; [~D] ~7,'0X~X ~A",
            nth,
            Fixnum::Encode(iFrame >> 4),
            Fixnum::Encode(iFrame & 15),
            intern(wsz, PACKAGE_keyword) );

        if (pFrame->GetType() == Frame::Type_Function)
        {
            format(t, L" ~A +~X",
                pFrame->StaticCast<FunctionFrame>()->m_fn,
                pFrame->StaticCast<FunctionFrame>()->m_ip );
        }

        format(t, L"~%");

        nth = add_xx(nth, 1);
    } // for each frame
} // command_backtrace


static void command_compiler_macroexpand_1(Val args)
{
    Val form = nil == args ? TLV(A) : first(args);

    if (! consp(form))
    {
        return;
    }

    Val expander = compiler_macro_function(car(form));

    if (nil != expander)
    {
        remember_value(funcall(expander, form, nil));
    }
} // command_m1

// command_disassemble
static void command_disassemble(Val args)
{
    Val fname = nil == args ? TLV(A) : first(args);
    disassemble(fname);
} // command_dis


// command_frame
static void command_frame(Val args)
{
    Val nth = first(args);
    Val n = Fixnum::Encode(0);
    foreach (Thread::EnumFrame, oEnum, MiniThread::Get())
    {
        Frame* pFrame = oEnum.Get();
        Val frob = decode_frame(pFrame);
        if (n == nth)
        {
            format(t, L"; Frame[~D]: ~S~%", n, frob);
            remember_value(third(frob));
            break;
        }
        n = add_xx(n, 1);
    } // for each frame
} // command_frame


// command_gc
static void command_gc(Val)
{
    size_t cbBefore, cbAfter;
    collect_garbage(&cbBefore, &cbAfter);

    format(TLV(Aerror_outputA), L"; Heap:  ~D => ~D KB~%;~%",
        Fixnum::Encode(cbBefore / 1024),
        Fixnum::Encode(cbAfter / 1024) );
} // command_gc

// command_inspect
static void command_inspect(Val args)
{
    Val fname = nil == args ? TLV(A) : first(args);
    inspect(fname);
} // command_dis

static void command_macroexpand_1(Val args)
{
    Val form = nil == args ? TLV(A) : first(args);
    remember_value(macroexpand_1(form));
} // command_m1


//////////////////////////////////////////////////////////////////////
//
// Command Table
//
static void command_help(Val);
static const Command k_rgoCommand[] =
{
    { L"BT",    0, 0, command_backtrace },
    { L"CM1",   0, 1, command_compiler_macroexpand_1},
    { L"DIS",   0, 1, command_disassemble },
    { L"FR",    1, 1, command_frame },
    { L"GC",    0, 0, command_gc },
    { L"HELP",  0, 0, command_help },
    { L"I",     0, 1, command_inspect },
    { L"M1",    0, 1, command_macroexpand_1 },
}; // k_rgoCommand

static void command_help(Val)
{
    for (
        const Command* pRunner = &k_rgoCommand[0];
        pRunner < &k_rgoCommand[lengthof(k_rgoCommand)];
        pRunner++ )
    {
        format(t, L"; ~A~%",
            intern(pRunner->m_pwsz, PACKAGE_keyword) );
    } // for each command
} // command_help


static const Command* get_command(Val x)
{
    if (! symbolp(x))
    {
        return NULL;
    }

    LPCWSTR pwsz = x->Decode<Symbol>()->m_name->Decode<SimpleString>()->
        GetElements();

    for (
        const Command* pRunner = &k_rgoCommand[0];
        pRunner < &k_rgoCommand[lengthof(k_rgoCommand)];
        pRunner++ )
    {
        if (0 == ::lstrcmpW(pRunner->m_pwsz, pwsz))
        {
            return pRunner;
        }
    } // for each command

    return NULL;
} // get_command


static void process_command(const Command* pCmd)
{
    Val args = nil;
    UINT cArgs = 0;

    while (cArgs < pCmd->m_nMin)
    {
        args = cons(read(), args);
        cArgs += 1;
    } // while

    while (cArgs < pCmd->m_nMax)
    {

        Val ch = read_char_no_hang();
        while (Character::Encode(' ') == ch || Character::Encode(0x0A) == ch)
        {
            ch = read_char_no_hang();
        } // while

        if (nil == ch)
        {
            break;
        }

        unread_char(ch);

        args = cons(read(), args);
        cArgs += 1;
    } // for

    pCmd->m_pfn(nreverse(args));
} // process_command

} // namespace

namespace Genesis
{

//////////////////////////////////////////////////////////////////////
//
// command_loop
//
void
command_loop()
{
    MiniThread* p = MiniThread::Get();
    Val eof = list(nil);

    if (TLV(Astandard_inputA) != TLV(Aterminal_ioA))
    {
        // Dummy frame for call_lisp.
        BindFrameScope oLet(1);
            oLet.Bind(TLV_Acmdl_levelA, Fixnum::Encode(0));

        for (;;)
        {
            Val form = read(nil, nil, eof);
            if (eof == form) break;
            eval(form);
            p->GcFence(nil);
        }
        ::ExitProcess(0);
        // NOTREACHD
    } // not interactive

    Val level = add_xx(TLV(Acmdl_levelA), 1);

    BindFrameScope oLet(1);
        oLet.Bind(TLV_Acmdl_levelA, Fixnum::Encode(level));

    Val line = Fixnum::Encode(0);

    for (;;)
    {
        p->GcFence(nil);

        BindFrameScope oLet(1);
            oLet.Bind(TLV_Acmdl_levelA, level);

        line = add_xx(line, 1);

        // Print prompt
        //prompt:
        {
            format(t, L"~A[~D]",
                package_pretty_name(TLV(ApackageA)),
                line );

            for (
                Val nth = Fixnum::Encode(0);
                cmp_xx(nth, level) < 0;
                nth = add_xx(nth, 1) )
            {
                format(Qt, L">");
            } // for nth
            format(t, L" ");
        } // promot

        #if 0
        // Note: We should check newline after read function.
        {
            Val ch = read_char(nil, nil);
            if (ch == Character::Encode(0x0A))
            {
                goto prompt;
            }
            if (ch != nil)
            {
                unread_char(ch, nil);
            }
        }
        #endif

        Val form = read(nil, nil, eof);
        if (eof == form) return;

        {
            const Command* pCmd = get_command(form);
            if (NULL != pCmd)
            {
                process_command(pCmd);
                continue;
            }
        }

        set_tlv(TLV__, form);       // (setq - form)

        p->m_n = Fixnum::Encode(1);
        p->mv_value[0] = eval(form);

        set_tlv(TLV_PPP, TLV(PP));  // (setq +++ ++)
        set_tlv(TLV_PP,  TLV(P));   // (setq ++ +)
        set_tlv(TLV_P,   TLV(_));   // (setq + -)

        remember_values();
    } // forever
} // command_loop

} // Genesis

namespace CommonLisp
{

// invoke_debugger
void __declspec(noreturn) invoke_debugger(Val cond)
{
    format(t, L"; Error: ~A~%", cond);
    if (TLV(Astandard_inputA) != TLV(Aterminal_ioA)) ::ExitProcess(1);

    BindFrameScope oLet(1);
        oLet.Bind(TLV_Acmdl_conditionA, cond);

    command_loop();
} // invoke_debugger

} // CommonLisp
