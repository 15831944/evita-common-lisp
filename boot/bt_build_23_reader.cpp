#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - boot - build - 23 Reader
// boot/bt_build_23_reader.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: /proj/evcl3/boot/bt_build_23_reader.cpp 26 2006-10-08 14:57:34 yosi $
//
#include "../mini/mini_lisp.h"

namespace Boot
{

//////////////////////////////////////////////////////////////////////
//
// Boot Readtable
//
Val make_standard_readtable()
{
    static const UINT k_rgnCharInfo[128] =
    {
        Readtable::Type_Invalid | Readtable::Trait_Invalid,                       // 0x00 C-@
        Readtable::Type_Invalid | Readtable::Trait_Invalid,                       // 0x01 C-a
        Readtable::Type_Invalid | Readtable::Trait_Invalid,                       // 0x02 C-b
        Readtable::Type_Invalid | Readtable::Trait_Invalid,                       // 0x03 C-c
        Readtable::Type_Invalid | Readtable::Trait_Invalid,                       // 0x04 C-d
        Readtable::Type_Invalid | Readtable::Trait_Invalid,                       // 0x05 C-e
        Readtable::Type_Invalid | Readtable::Trait_Invalid,                       // 0x06 C-f
        Readtable::Type_Invalid | Readtable::Trait_Invalid,                       // 0x07 C-g
        Readtable::Type_Invalid | Readtable::Trait_Invalid, // Backspacce         // 0x08 C-h
        Readtable::Type_Space   | Readtable::Trait_Invalid, // Tab                // 0x09 C-i
        Readtable::Type_Space   | Readtable::Trait_Invalid, // Linefeed           // 0x0A C-j
        Readtable::Type_Invalid | Readtable::Trait_Invalid,                       // 0x0B C-k
        Readtable::Type_Space   | Readtable::Trait_Invalid, // Page               // 0x0C C-l
        Readtable::Type_Space   | Readtable::Trait_Invalid, // Return             // 0x0D C-m
        Readtable::Type_Invalid | Readtable::Trait_Invalid,                       // 0x0E C-n
        Readtable::Type_Invalid | Readtable::Trait_Invalid,                       // 0x0F C-o

        Readtable::Type_Invalid | Readtable::Trait_Invalid,                       // 0x10 C-p
        Readtable::Type_Invalid | Readtable::Trait_Invalid,                       // 0x11 C-q
        Readtable::Type_Invalid | Readtable::Trait_Invalid,                       // 0x12 C-r
        Readtable::Type_Invalid | Readtable::Trait_Invalid,                       // 0x13 C-s
        Readtable::Type_Invalid | Readtable::Trait_Invalid,                       // 0x14 C-t
        Readtable::Type_Invalid | Readtable::Trait_Invalid,                       // 0x15 C-u
        Readtable::Type_Invalid | Readtable::Trait_Invalid,                       // 0x16 C-v
        Readtable::Type_Invalid | Readtable::Trait_Invalid,                       // 0x17 C-w
        Readtable::Type_Invalid | Readtable::Trait_Invalid,                       // 0x18 C-x
        Readtable::Type_Invalid | Readtable::Trait_Invalid,                       // 0x19 C-y
        Readtable::Type_Invalid | Readtable::Trait_Invalid,                       // 0x1A C-z
        Readtable::Type_Invalid | Readtable::Trait_Invalid,                       // 0x1B C-[
        Readtable::Type_Invalid | Readtable::Trait_Invalid,                       // 0x1C C-\ .
        Readtable::Type_Invalid | Readtable::Trait_Invalid,                       // 0x1D C-]
        Readtable::Type_Invalid | Readtable::Trait_Invalid,                       // 0x1E C-^
        Readtable::Type_Invalid | Readtable::Trait_Invalid,                       // 0x1F C-_

        Readtable::Type_Space   | Readtable::Trait_Invalid,// Space           // 0x20
        Readtable::Type_Cons    | Readtable::Trait_Alphabetic,                    // 0x21 !
        Readtable::Type_Tmacro  | Readtable::Trait_Alphabetic,                    // 0x22 "
        Readtable::Type_Nmacro  | Readtable::Trait_Alphabetic,                    // 0x23 #
        Readtable::Type_Cons    | Readtable::Trait_Alphabetic,                    // 0x24 $
        Readtable::Type_Cons    | Readtable::Trait_Alphabetic,                    // 0x25 %
        Readtable::Type_Cons    | Readtable::Trait_Alphabetic,                    // 0x26 &
        Readtable::Type_Tmacro  | Readtable::Trait_Alphabetic,                    // 0x27 '
        Readtable::Type_Tmacro  | Readtable::Trait_Alphabetic,                    // 0x28 (
        Readtable::Type_Tmacro  | Readtable::Trait_Alphabetic,                    // 0x29 )
        Readtable::Type_Cons    | Readtable::Trait_Alphabetic,                    // 0x2A *
        Readtable::Type_Cons    | Readtable::Trait_Alphabetic| Readtable::Trait_Plus,        // 0x2B +
        Readtable::Type_Tmacro  | Readtable::Trait_Alphadigit,                    // 0x2C ,
        Readtable::Type_Cons    | Readtable::Trait_Alphabetic| Readtable::Trait_Minus,       // 0x2D -
        Readtable::Type_Cons    | Readtable::Trait_Alphabetic| Readtable::Trait_Decimal | Readtable::Trait_Dot, // 0x2E .
        Readtable::Type_Cons    | Readtable::Trait_Alphabetic| Readtable::Trait_Ratio,       // 0x2F /

        Readtable::Type_Cons    | Readtable::Trait_Alphadigit,                    // 0x30 0
        Readtable::Type_Cons    | Readtable::Trait_Alphadigit,                    // 0x31 1
        Readtable::Type_Cons    | Readtable::Trait_Alphadigit,                    // 0x32 2
        Readtable::Type_Cons    | Readtable::Trait_Alphadigit,                    // 0x33 3
        Readtable::Type_Cons    | Readtable::Trait_Alphadigit,                    // 0x34 4
        Readtable::Type_Cons    | Readtable::Trait_Alphadigit,                    // 0x35 5
        Readtable::Type_Cons    | Readtable::Trait_Alphadigit,                    // 0x36 6
        Readtable::Type_Cons    | Readtable::Trait_Alphadigit,                    // 0x37 7
        Readtable::Type_Cons    | Readtable::Trait_Alphadigit,                    // 0x38 8
        Readtable::Type_Cons    | Readtable::Trait_Alphadigit,                    // 0x39 9
        Readtable::Type_Cons    | Readtable::Trait_Package,                       // 0x3A :
        Readtable::Type_Tmacro  | Readtable::Trait_Alphabetic,                    // 0x3B ;
        Readtable::Type_Cons    | Readtable::Trait_Alphabetic,                    // 0x3C <
        Readtable::Type_Cons    | Readtable::Trait_Alphabetic,                    // 0x3D =
        Readtable::Type_Cons    | Readtable::Trait_Alphabetic,                    // 0x3E >
        Readtable::Type_Cons    | Readtable::Trait_Alphabetic,                    // 0x3F ?

        Readtable::Type_Cons    | Readtable::Trait_Alphabetic,                    // 0x40 @
        Readtable::Type_Cons    | Readtable::Trait_Alphadigit,                    // 0x41 A
        Readtable::Type_Cons    | Readtable::Trait_Alphadigit,                    // 0x42 B
        Readtable::Type_Cons    | Readtable::Trait_Alphadigit,                    // 0x43 C
        Readtable::Type_Cons    | Readtable::Trait_Alphadigit | Readtable::Trait_Dmarker,    // 0x44 D
        Readtable::Type_Cons    | Readtable::Trait_Alphadigit | Readtable::Trait_Emarker,    // 0x45 E
        Readtable::Type_Cons    | Readtable::Trait_Alphadigit | Readtable::Trait_Fmarker,    // 0x46 F
        Readtable::Type_Cons    | Readtable::Trait_Alphadigit,                    // 0x47 G
        Readtable::Type_Cons    | Readtable::Trait_Alphadigit,                    // 0x48 H
        Readtable::Type_Cons    | Readtable::Trait_Alphadigit,                    // 0x49 I
        Readtable::Type_Cons    | Readtable::Trait_Alphadigit,                    // 0x4A J
        Readtable::Type_Cons    | Readtable::Trait_Alphadigit,                    // 0x4B K
        Readtable::Type_Cons    | Readtable::Trait_Alphadigit | Readtable::Trait_Lmarker,    // 0x4C L
        Readtable::Type_Cons    | Readtable::Trait_Alphadigit,                    // 0x4D M
        Readtable::Type_Cons    | Readtable::Trait_Alphadigit,                    // 0x4E N
        Readtable::Type_Cons    | Readtable::Trait_Alphadigit,                    // 0x4F O

        Readtable::Type_Cons    | Readtable::Trait_Alphadigit,                    // 0x50 P
        Readtable::Type_Cons    | Readtable::Trait_Alphadigit,                    // 0x51 Q
        Readtable::Type_Cons    | Readtable::Trait_Alphadigit,                    // 0x52 R
        Readtable::Type_Cons    | Readtable::Trait_Alphadigit | Readtable::Trait_Smarker,    // 0x53 S
        Readtable::Type_Cons    | Readtable::Trait_Alphadigit,                    // 0x54 T
        Readtable::Type_Cons    | Readtable::Trait_Alphadigit,                    // 0x55 U
        Readtable::Type_Cons    | Readtable::Trait_Alphadigit,                    // 0x56 V
        Readtable::Type_Cons    | Readtable::Trait_Alphadigit,                    // 0x57 W
        Readtable::Type_Cons    | Readtable::Trait_Alphadigit,                    // 0x58 X
        Readtable::Type_Cons    | Readtable::Trait_Alphadigit,                    // 0x59 Y
        Readtable::Type_Cons    | Readtable::Trait_Alphadigit,                    // 0x5A Z
        Readtable::Type_Cons    | Readtable::Trait_Alphabetic,                    // 0x5B [
        Readtable::Type_Sescape | Readtable::Trait_Alphabetic,                    // 0x5C \ .
        Readtable::Type_Cons    | Readtable::Trait_Alphabetic,                    // 0x5D ]
        Readtable::Type_Cons    | Readtable::Trait_Alphabetic,                    // 0x5E ^
        Readtable::Type_Cons    | Readtable::Trait_Alphabetic,                    // 0x5F _

        Readtable::Type_Tmacro  | Readtable::Trait_Alphabetic,                    // 0x60 `
        Readtable::Type_Cons    | Readtable::Trait_Alphadigit,                    // 0x61 a
        Readtable::Type_Cons    | Readtable::Trait_Alphadigit,                    // 0x62 b
        Readtable::Type_Cons    | Readtable::Trait_Alphadigit,                    // 0x63 c
        Readtable::Type_Cons    | Readtable::Trait_Alphadigit | Readtable::Trait_Dmarker,    // 0x64 d
        Readtable::Type_Cons    | Readtable::Trait_Alphadigit | Readtable::Trait_Emarker,    // 0x65 e
        Readtable::Type_Cons    | Readtable::Trait_Alphadigit | Readtable::Trait_Fmarker,    // 0x66 f
        Readtable::Type_Cons    | Readtable::Trait_Alphadigit,                    // 0x67 g
        Readtable::Type_Cons    | Readtable::Trait_Alphadigit,                    // 0x68 h
        Readtable::Type_Cons    | Readtable::Trait_Alphadigit,                    // 0x69 i
        Readtable::Type_Cons    | Readtable::Trait_Alphadigit,                    // 0x6A j
        Readtable::Type_Cons    | Readtable::Trait_Alphadigit,                    // 0x6B k
        Readtable::Type_Cons    | Readtable::Trait_Alphadigit | Readtable::Trait_Lmarker,    // 0x6C l
        Readtable::Type_Cons    | Readtable::Trait_Alphadigit,                    // 0x6D m
        Readtable::Type_Cons    | Readtable::Trait_Alphadigit,                    // 0x6E n
        Readtable::Type_Cons    | Readtable::Trait_Alphadigit,                    // 0x6F o

        Readtable::Type_Cons    | Readtable::Trait_Alphadigit,                    // 0x70 p
        Readtable::Type_Cons    | Readtable::Trait_Alphadigit,                    // 0x71 q
        Readtable::Type_Cons    | Readtable::Trait_Alphadigit,                    // 0x72 r
        Readtable::Type_Cons    | Readtable::Trait_Alphadigit | Readtable::Trait_Smarker,    // 0x73 s
        Readtable::Type_Cons    | Readtable::Trait_Alphadigit,                    // 0x74 t
        Readtable::Type_Cons    | Readtable::Trait_Alphadigit,                    // 0x75 u
        Readtable::Type_Cons    | Readtable::Trait_Alphadigit,                    // 0x76 v
        Readtable::Type_Cons    | Readtable::Trait_Alphadigit,                    // 0x77 w
        Readtable::Type_Cons    | Readtable::Trait_Alphadigit,                    // 0x78 x
        Readtable::Type_Cons    | Readtable::Trait_Alphadigit,                    // 0x79 y
        Readtable::Type_Cons    | Readtable::Trait_Alphadigit,                    // 0x7A z
        Readtable::Type_Cons    | Readtable::Trait_Alphabetic,                    // 0x7B {
        Readtable::Type_Mescape | Readtable::Trait_Alphabetic,                    // 0x7C |
        Readtable::Type_Cons    | Readtable::Trait_Alphabetic,                    // 0x7D }
        Readtable::Type_Cons    | Readtable::Trait_Alphabetic,                    // 0x7E ~
        Readtable::Type_Cons    | Readtable::Trait_Invalid,// Rubout          // 0x7F Del
    }; // k_rgnCharInfo

    Val readtable = make_readtable();

    Readtable* p = readtable->Decode<Readtable>();

    Val vector = p->m_vector;

    Int iLength = Fixnum::Decode_(length(vector));

    for (Int iIndex = 0; iIndex < iLength; iIndex++)
    {
        setf_svref(Fixnum::Encode(k_rgnCharInfo[iIndex]), vector, iIndex);
    } // for

    p->m_table = make_hash_table(Qeq);

    setf_svref(
        cons(svref(vector, '#'), make_hash_table(Qeq)),
        vector,
        '#' );

    return readtable;
} // make_standard_readtable

} // Boot
