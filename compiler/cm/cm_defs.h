//////////////////////////////////////////////////////////////////////////////
//
// evcl - compiler - pre-compiled header
// compiler_defs.h
//
// Copyright (C) 1996-2006 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cm/cm_defs.h#12 $
//
#if !defined(INCLUDE_compiler_cm_defs_h)
#define INCLUDE_compiler_cm_defs_h

#include "../mini/mini_defs.h"

//////////////////////////////////////////////////////////////////////
//
// Import Images
//
namespace Image
{
    using namespace Kernel;
    #include "_img_object.h"

    // Note: Due to bug of MSC14/x64, we don't use Val const name = val.
    #define QAdd    QP
    #define QDiv    QS
    #define QMul    QA
    #define QSub    Q_

    #define QAdd2   QPS2
    #define QDiv2   QSS2
    #define QMul2   QAS2
    #define QSub1   Q_S1
    #define QSub2   Q_S2

    #define Q1Plus  Q1P
    #define Q1Minus Q1_

    #define QEq     QE
    #define QNe     QSE
    #define QGe     QGE
    #define QGt     QG
    #define QLe     QLE
    #define QLt     QL

    #define QEq2    QES2
    #define QNe2    QSES2
    #define QGe2    QGES2
    #define QGt2    QGS2
    #define QLe2    QLES2
    #define QLt2    QLS2

    #define QCharEq     QcharE
    #define QCharNe     QcharSE
    #define QCharGe     QcharGE
    #define QCharGt     QcharG
    #define QCharLe     QcharLE
    #define QCharLt     QcharL

    #define QCharEq2    QcharES2
    #define QCharNe2    QcharSES2
    #define QCharGe2    QcharGES2
    #define QCharGt2    QcharGS2
    #define QCharLe2    QcharLES2
    #define QCharLt2    QcharLS2

    #define QCharCiEq   Qchar_equal
    #define QCharCiNe   Qchar_not_equal
    #define QCharCiGe   Qchar_not_lessp
    #define QCharCiGt   Qchar_greaterp
    #define QCharCiLe   Qchar_not_greaterp
    #define QCharCiLt   Qchar_lessp

    #define QCharCiEq2  Qchar_equalS2
    #define QCharCiNe2  Qchar_not_equalS2
    #define QCharCiGe2  Qchar_not_lesspS2
    #define QCharCiGt2  Qchar_greaterpS2
    #define QCharCiLe2  Qchar_not_greaterpS2
    #define QCharCiLt2  Qchar_lesspS2

} // Image

using namespace Image;

#include "../big/big_lisp.h"


//////////////////////////////////////////////////////////////////////
//
// Declare compiler classes
//
namespace Compiler
{
    typedef Val Ty;

    class Module;
    class Session;
    class Pass;
    class OptimizeQualities;

    class Target;

    class BBlock;
    class Instruction;
    class OperandBox;

    class Operand;
        class Function;
        class Integer;
        class Label;
        class Literal;
        class Variable;

        class Output;
            class Bool;
            class Register;
            class Phyisical;

    class CfgEdge;
    class CgEdge;


    class Timee
    {
        private: const char16*  m_pwszName;
        private: uint           m_nStart;

        public: Timee(const char16* pwsz) :
            m_nStart(::GetTickCount()),
            m_pwszName(pwsz) {}

        public: ~Timee();

        public: const char16* GetName() const { return m_pwszName; }
    }; // Timee
} // Compiler

#endif //!defined(INCLUDE_compiler_cm_defs_h)
