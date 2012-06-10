#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - kernel - x64 Frame
// arch/x64/kernel/gen_ke_frame.cpp
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x64/kernel/x64_ke_thread.cpp#7 $
//
// Description:
//  This file contains implementaiton of following Generic frame methods:
//      FinallyFrame::Unwind
//      XferFrame::Transfer
//
#include "./x64_ke_thread.h"

#include "./x64_ke_frame.h"
#include "./x64_ke_layout.h"
#include "./x64_ke_mach.h"

#include "../../../mini/mini_lisp.h"

// EXCEPTION_DISPOSITION
#include <excpt.h>

//extern HINSTANCE g_hInstance;

namespace Kernel
{

extern HINSTANCE g_hSelf;

using namespace X64;

void
install_service_aux(Thread* p, int iOffset, void* pv)
{
    ASSERT(0 == iOffset % sizeof(Val));

    void** pJump = reinterpret_cast<void**>(p);
    pJump[iOffset / sizeof(Val)] = pv;
} // X64Initializer::install_service


// install_service
void
install_service(Thread* p, int iOffset, Val fun)
{
    if (symbolp(fun))
    {
        fun = symbol_function(fun);
    }
    if (SVC_fixnum_one == iOffset)
    {
        install_service_aux(p, iOffset, fun);
    }
    else
    {
        install_service_aux(
            p,
            iOffset, fun->Decode<FunObj>()->GetCodeVec() );
    }
} // install_service

typedef enum _UNWIND_OP_CODES {
    UWOP_PUSH_NONVOL = 0, /* info == register number */
    UWOP_ALLOC_LARGE,     /* no info, alloc size in next 2 slots */
    UWOP_ALLOC_SMALL,     /* info == size of allocation / 8 - 1 */
    UWOP_SET_FPREG,       /* no info, FP = RSP + UNWIND_INFO.FPRegOffset*16 */
    UWOP_SAVE_NONVOL,     /* info == register number, offset in next slot */
    UWOP_SAVE_NONVOL_FAR, /* info == register number, offset in next 2 slots */
    UWOP_SAVE_XMM128,     /* info == XMM reg number, offset in next slot */
    UWOP_SAVE_XMM128_FAR, /* info == XMM reg number, offset in next 2 slots */
    UWOP_PUSH_MACHFRAME   /* info == 0: no error-code, 1: error-code */
} UNWIND_CODE_OPS;

typedef uint8 UBYTE;
typedef uint16 USHORT;

//warning C4201: nonstandard extension used : nameless struct/union
#pragma warning(disable: 4201)
typedef union _UNWIND_CODE {
    struct {
        UBYTE CodeOffset;
        UBYTE UnwindOp : 4;
        UBYTE OpInfo   : 4;
    };
    USHORT FrameOffset;
} UNWIND_CODE, *PUNWIND_CODE;

#define UNW_FLAG_EHANDLER  0x01
#define UNW_FLAG_UHANDLER  0x02
#define UNW_FLAG_CHAININFO 0x04

typedef struct _UNWIND_INFO {
    UBYTE Version       : 3;
    UBYTE Flags         : 5;
    UBYTE SizeOfProlog;
    UBYTE CountOfCodes;
    UBYTE FrameRegister : 4;
    UBYTE FrameOffset   : 4;
    UNWIND_CODE UnwindCode[1];

    #if 0
        UNWIND_CODE MoreUnwindCode[((CountOfCodes + 1) & ~1) - 1];
        union {
            OPTIONAL ULONG ExceptionHandler;
            OPTIONAL ULONG FunctionEntry;
        };
        OPTIONAL ULONG ExceptionData[];
    #endif
} UNWIND_INFO, *PUNWIND_INFO;

typedef struct _DISPATCHER_CONTEXT
DISPATCHER_CONTEXT, *PDISPATCHER_CONTEXT;

typedef EXCEPTION_DISPOSITION (*PEXCEPTION_ROUTINE) (
    IN PEXCEPTION_RECORD ExceptionRecord,
    IN ULONG64 EstablisherFrame,
    IN OUT PCONTEXT ContextRecord,
    IN OUT PDISPATCHER_CONTEXT DispatcherContext
);

typedef struct _DISPATCHER_CONTEXT {
    ULONG64 ControlPc;
    ULONG64 ImageBase;
    PRUNTIME_FUNCTION FunctionEntry;
    ULONG64 EstablisherFrame;
    ULONG64 TargetIp;
    PCONTEXT ContextRecord;
    PEXCEPTION_ROUTINE LanguageHandler;
    PVOID HandlerData;
} DISPATCHER_CONTEXT, *PDISPATCHER_CONTEXT;

Val map_ra_to_fn(UInt);

/// <summary>
///   Callback function of RtlInstallFunctionTableCallback.
/// </summary>
RUNTIME_FUNCTION*
RuntimeFunctionCallback(DWORD64 nPC, void* pvCookie)
{
    ASSERT(NULL != pvCookie);

    DEBUG_PRINTF(L"pc=%p context=%p\r\n", nPC, pvCookie);

    Val blob = MiniThread::Get()->AllocBinVec(
        CLASSD_unsigned_byte_32_vector,
        Fixnum::Encode(100) );

    uint8* pRunner = reinterpret_cast<uint8*>(
        blob->Decode<DataVector>() + 1 );

    Val fn = map_ra_to_fn(nPC);
    FunObj* pFunObj = fn->Decode<FunObj>();

#if 1
    format(t, L"; RuntimeFunctionCallback pc=~X ~S~%",
        Fixnum::Encode(nPC), fn );
    dbg_format(L"; RuntimeFunctionCallback pc=~X ~S~%",
        Fixnum::Encode(nPC), fn );
#endif

    RUNTIME_FUNCTION* pFunInfo = reinterpret_cast<RUNTIME_FUNCTION*>(
                    pRunner );
        pRunner += sizeof(*pFunInfo);

    UNWIND_INFO* pUnwindData = reinterpret_cast<UNWIND_INFO*>(pRunner);
        pRunner += sizeof(*pUnwindData);


    pFunInfo->BeginAddress = static_cast<DWORD>(
        reinterpret_cast<Int>(pFunObj->GetCodeVec()) -
        Memory::GetStart()->ToInt() );

    pFunInfo->EndAddress = pFunInfo->BeginAddress +
        pFunObj->GetDesc()->m_cbCodeVec;

    pFunInfo->UnwindData = static_cast<DWORD>(
        reinterpret_cast<Int>(pUnwindData) - Memory::GetStart()->ToInt() );

    pUnwindData->Version = 1;
    pUnwindData->Flags   = 0;
    pUnwindData->SizeOfProlog = 8;

    uint cbAlloc = pFunObj->GetFrameSize() - 8;

    if (0 == cbAlloc)
    {
        return NULL;
    }

    if (cbAlloc <= 128)
    {
        pUnwindData->CountOfCodes = 1;

        pUnwindData->UnwindCode[0].UnwindOp = UWOP_ALLOC_SMALL;
        pUnwindData->UnwindCode[0].OpInfo   = (cbAlloc / 8) - 1;
        pUnwindData->UnwindCode[0].CodeOffset = 0;
    }
    else
    {
        // Stack size is 136 to 512K-8.
        pUnwindData->CountOfCodes = 2;

        pUnwindData->UnwindCode[0].UnwindOp = UWOP_ALLOC_LARGE;
        pUnwindData->UnwindCode[0].OpInfo   = 0;
        pUnwindData->UnwindCode[0].CodeOffset = 0;
        pUnwindData->UnwindCode[1].FrameOffset =
            static_cast<USHORT>(cbAlloc / 8);
    }

    switch (pFunObj->GetFrameType())
    {
    case FunObj::FrameType_Fixed:
        pUnwindData->FrameRegister = 0;
        pUnwindData->FrameOffset = 0;
        break;

    case FunObj::FrameType_Restify:
        pUnwindData->FrameRegister = 15;
        pUnwindData->FrameOffset = (cbAlloc - 8) / 16;
        break;

    default:
        CAN_NOT_HAPPEN();
    } // switch

    return pFunInfo;
} // RuntimeFunctionCallback

/// <summary>
///   Host machine specific thread initialization.
/// </summary>
void
Thread::MachInit()
{
    // Note: every thread MUST install services.

    install_service(this, SVC_error, Qthread_error_hook);

    install_service(this, SVC_alloc_bino_area, Qalloc_bino_area);
    install_service(this, SVC_alloc_code_area, Qalloc_code_area);
    install_service(this, SVC_alloc_cons_area, Qalloc_cons_area);
    install_service(this, SVC_alloc_reco_area, Qalloc_reco_area);

    install_service(this, SVC_fixnum_one,         Fixnum::Encode(1));
    install_service(this, SVC_arity_error,        QDarity_error);
    install_service(this, SVC_not_function,       QDnot_function);
    install_service(this, SVC_type_error,         QDtype_error);
    install_service(this, SVC_undefined_function, QDundefined_function);
    install_service(this, SVC_unbound_variable,   QDunbound_variable);

    //install_service(this, SVC_go, QDgo);

    //install_service_aux(this, SVC_stack_alloc, SVC_C_stack_alloc);

    BOOLEAN fSucceeded = ::RtlInstallFunctionTableCallback(
        3,  // table identifier
        reinterpret_cast<DWORD64>(Memory::GetStart()),  // base address
        1 << 30,    // length
        RuntimeFunctionCallback,
        (void*) 0x12345,    // context
        NULL );
    if (! fSucceeded)
    {
        Debugger::Fail(L"RtlInstallFunctionTableCallback");
    }
} // Thread::MachInit

} // Kernel
