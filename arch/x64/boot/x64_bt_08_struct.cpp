#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - boot - 08 Structure
// arch/x64/boot/x64_bt_08_struct.inc
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/arch/x64/boot/x64_bt_08_struct.cpp#4 $
//
// Description:
//  This file contains functions for "08 Structure".
//      structure-instance-access
//      (setf structure-instance-access)
//
#include "./x64_bt_builder.h"

namespace Boot
{

void X64Builder::build_08_Struct()
{

// (structure-instance-access instance index)
defun("STRUCTURE-INSTANCE-ACCESS", 2, 2)
  Label error_$r0;
  Label error_$r1;

    lea($r2, ea($r0, - StructureObject::Tag));
    test($r2, Val_::Mask_Tag);
    jne(error_$r0);

    test($r1, Fixnum::TagMask); // CF=0
    jne(error_$r1);

    mov($r0,
        ea($r0,
           offsetof(StructureObject, mv_slot) - StructureObject::Tag,
           $r1 ) );
    ret();

  label(error_$r0);
    mov($r1, Qstructure_object);
    call(ea($rtcb, SVC_type_error));

  label(error_$r1);
    mov($r0, $r1);
    mov($r1, Q("ext:sequence-index"));
    call(ea($rtcb, SVC_type_error));
end_defun()


// (setf funcallable-structure-instance-access)
defun_setf(L"STRUCTURE-INSTANCE-ACCESS", 3, 3)
  Label error_$r1;
  Label error_$r2;

    lea($r3, ea($r1, -StructureObject::Tag));
    test($r3, Val_::Mask_Tag);
    jne(error_$r1);

    test($r2, Fixnum::TagMask); // CF=0
    jne(error_$r2);

    mov(ea($r1,
           offsetof(StructureObject, mv_slot) - StructureObject::Tag,
           $r2 ),
        $r0 );
    ret();

  label(error_$r1);
    mov($r0, $r1);
    mov($r1, Qstructure_object);
    call(ea($rtcb, SVC_type_error));

  label(error_$r2);
    mov($r0, $r2);
    mov($r1, Q("ext:sequence-index"));
    call(ea($rtcb, SVC_type_error));
end_defun()

} // X64Builder::build_08_Struct
} // Boot
