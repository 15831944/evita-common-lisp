#include "precomp.h"
//////////////////////////////////////////////////////////////////////////////
//
// evcl - genesis - 23 Reader
// genesis/gs_23_reader.cpp
//
// This file is part of Evita Common Lisp.
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/genesis/gs_23_reader.cpp#2 $
//

#include "./gs_lisp.h"


#include "../big/big_12_float.h"

namespace Genesis
{

namespace
{

//////////////////////////////////////////////////////////////////////
//
// Reader
//
// Lisp Reader Syntax
//
// Escape characters:
//    \        single-escape
//    |        multiple-escape
//
// Macro characters:
//  0203 "        terminate        string
//  0203 #        non-terminate    dispatch macro
//  0203 '        terminate        quote
//  0203 (        terminate        list
//  0203 )        terminate        error
//  0203 ;        terminate        comment
//  0908 `        terminate        backquote macro
//  0908 ,        terminate        error
//
// Sharp distpatch macro
//          ##        reference                 #1#
//  0203    #'        function                  (FUNCTION foo)
//  0919    #(        vector                    #(1 2 3)
//          #)        error
//          #*        bit-vector                #*0101101
//  0920    #+        conditional               #+condition
//  0920    #-        conditional               #-condition
//  0903    #.        read time eval
//          #:        uninterned symbol         #:symbol
//          #<        error                     #<Pacakge ...>
//          #=        label                     #=foo
//          #>        error
//          #A        array                     #A(1 2 3)
//  0203    #B        binary                    #B01010
//  1007    #C        complex                   #C(1 2)
//  0203    #O        octal                     #O117
//          #P        pathname                  #P"foo.lisp"
//          #R        radix                     #3R1212
//          #S        structure                 #S(foo name keiko)
//  0203    #X        hexadecimal               #X1234
//  0203    #\        character                 #\A
//  0203    #|        multiple line comment     #| comment |#
//
class Reader
{
    Val  m_stream;
    bool m_fRecursive;
    bool m_fPresevingWhiteSpace;

    struct Token
    {
        enum Type
        {
            Type_None,
            Type_Delimiter,
            Type_Dot,
            Type_Eof,
            Type_Macro,
            Type_Token
        }; // Type

        Val     m_value;
        WCHAR   m_rgwchChar[100];
        uint32  m_rgnAttr[100];
        uint    m_nLength;

        Token()
        {
            m_value = nil;
            m_nLength = 0;
        } // Token
    } m_oToken;

    // Run
    public: Val Run(
        Val     stream,
        bool    fEofError,
        Val     eof_value,
        bool    fRecursive,
        bool    fPresevingWhiteSpace )
    {
        m_stream                = stream;
        m_fRecursive            = fRecursive;
        m_fPresevingWhiteSpace  = fPresevingWhiteSpace;
        main(nil, fEofError, eof_value);
        return m_oToken.m_value;
    } // Run

    // addTokenChar
    void addTokenChar(Val ch, Int nAttr)
    {
        ASSERT(characterp(ch));

        if (m_oToken.m_nLength >= lengthof(m_oToken.m_rgwchChar))
        {
            error(L"Too long token.");
        }

        m_oToken.m_rgwchChar[m_oToken.m_nLength] = Character::ToCode(ch);
        m_oToken.m_rgnAttr[m_oToken.m_nLength]   = static_cast<uint32>(nAttr);
        m_oToken.m_nLength += 1;
    } // addTokenChar

    // extraInfixArg
    void extraInfixArg(Val ch)
    {
        error(L"#~C expect no argument.", ch);
    } // extra_infxi_arg

    // macroBackquote - enclosed with si:backquote
    Val macroBackquote()
    {
        BindFrameScope oLet(1);
            oLet.Bind(TLV_Aread_backquoteA, t);

        Val obj = read(m_stream, t, nil, t);
        return isSuppress() ? nil : list(Qbackquote, obj);
    } // macroBackquote

    // macroComma
    Val macroComma()
    {
        Val ch = read_char(m_stream);
        Val op;
        {
            if (Character::Encode('@') == ch)
            {
                op = Qunquote_splicing;
            }
            else if (Character::Encode('.') == ch)
            {
                op = Qunquote_nsplicing;
            }
            else
            {
                unread_char(ch, m_stream);
                op = Qunquote;
            }
        } // op
        Val obj = read(m_stream, t, nil, t);
        if (isSuppress())
        {
            return nil;
        }
        else if (nil != TLV(Aread_backquoteA))
        {
            return list(op, obj);
        }
        else
        {
            error(L"Comma(,) used outside backquote.");
        }
    } // macroComma

    // macroDoubleQuote - read string
    Val macroDoubleQuote()
    {
        WCHAR rgwchString[1024];
        uint  cwchString = 0;

        enum
        {
            State_Normal,
            State_Escape,
        } eState = State_Normal;

        for (;;)
        {
            Val ch = read_char(m_stream);

            switch (eState)
            {
            case State_Normal:
                if (Character::Encode(0x22) == ch)
                {
                    return make_string(rgwchString, cwchString);
                }
                else if (Character::Encode('\\') == ch)
                {
                    eState = State_Escape;
                    continue;
                }

                break;

            case State_Escape:
                eState = State_Normal;
                break;
            } // switch

            if (cwchString < lengthof(rgwchString))
            {
                rgwchString[cwchString] = Character::ToCode(ch);
                cwchString += 1;
            }
        } // for
    } // macroDoubleQuote

    // macro open parenthesis - read list
    Val macroOpenParen()
    {
        Val line_number = stream_line_number(m_stream);

        BindFrameScope oLet(1);
            oLet.Bind(TLV_Aread_start_line_numberA, line_number);

        Val list = macroOpenParenAux(line_number);
        return rememberLineNumber(list, line_number);
    } // macroOpenParen

    // rememberLineNumber
    Val rememberLineNumber(Val obj, Val line_number)
    {
        if (nil == line_number)
        {
            return obj;
        }

        Val htb = TLV(Aread_line_number_tableA);
        if (hash_table_p(htb))
        {
            setf_gethash(line_number, obj, htb);
        }
        return obj;
    } // rememberLineNumber

    // macroOpenParenAux
    Val macroOpenParenAux(Val linenum)
    {
        Val head = list(nil);
        Val tail = head;

        enum
        {
            State_Car,
            State_List,
            State_Cdr,
            State_Last,
        } eState = State_Car;

        for (;;)
        {
            Token::Type eType = main(Character::Encode(0x29), true);
            switch (eState)
            {
            finish:
                return cdr(head);

            cons:
            {
                Val kons = list(m_oToken.m_value);
                Val newnum = stream_line_number(m_stream);
                if (newnum != linenum)
                {
                    rememberLineNumber(kons, newnum);
                    linenum = newnum;
                }
                tail = setf_cdr(kons, tail);
                break;
            } // cons

            case State_Car:
                switch (eType)
                {
                case Token::Type_Delimiter:
                    goto finish;
                case Token::Type_Dot:
                    error(L"No object before dot.");
                default:
                    eState = State_List;
                    goto cons;
                } // switch

            case State_List:
                switch (eType)
                {
                case Token::Type_Delimiter:
                    goto finish;
                case Token::Type_Dot:
                    eState = State_Cdr;
                    break;
                default:
                    goto cons;
                } // switch
                break;

            case State_Cdr:
                switch (eType)
                {
                case Token::Type_Delimiter:
                    error(L"Missing object after dot.");
                case Token::Type_Dot:
                    error(L"Dot after dot.");
                    break;
                default:
                    setf_cdr(m_oToken.m_value, tail);
                    eState = State_Last;
                    break;
                } // switch
                break;

            case State_Last:
                switch (eType)
                {
                case Token::Type_Delimiter:
                    goto finish;
                case Token::Type_Dot:
                    error(L"Dot after object.");
                    break;
                default:
                    error(L"Expect right parenthesis.");
                    break;
                } // switch
                break;

            default:
                CAN_NOT_HAPPEN();
            } // switch
        } // for
    } // macroOpenParen

    // macroCloseParen
    void macroCloseParen()
    {
        for (;;)
        {
            if (Character::Encode(0x29) != read_char(m_stream, nil))
            {
                break;
            }
        } // for

        if (! isSuppress())
        {
            error(L"Too many right parenthesis.");
        }
    } // macroCloseParen

    // macroQuote - enclosed with si:quote
    Val macroQuote()
    {
        Val obj = read(m_stream, t, nil, t);
        return isSuppress() ? nil : list(Qquote, obj);
    } // macroQuote

    // macro semi colon - skip after newline
    void macroSemiColon()
    {
        for (;;)
        {
            Val ch = read_char(m_stream, nil);
            if (Keof == ch)
            {
                return;
            }

            if (Character::Encode(0x0A) == ch)
            {
                return;
            }
        } // for
    } // macroSemiColon

    // macroSharp
    //  1. Collects digits as argument.
    //  2. Dispatch by subchar after argument.
    //  3. Returns true if read object, or false if read nothing.
    bool macroSharp()
    {
        Val arg = nil;
        Val ch = read_char(m_stream);
        {
            uint cDigits = 0;
            uint nArg = 0;

            for (;;)
            {
                if (ch < Character::Encode('0') ||
                    ch > Character::Encode('9') )
                {
                    if (cDigits >= 1)
                    {
                        arg = Fixnum::Encode(nArg);
                    }
                    break;
                }

                cDigits += 1;
                nArg *= 10;
                nArg += Character::ToCode(ch) - '0';
            } // for
        }

        switch (Character::ToCode(ch))
        {
        case 0x27:
            m_oToken.m_value = list(
                Qfunction, 
                read(m_stream, Qt, nil, Qt ));
            return true;

        case 0x28:
            m_oToken.m_value = macroSharpOpenParen(ch, arg);
            return true;

        case '+':
        case '-':
            return macroSharpPlus(ch, arg);

        case '.':
            m_oToken.m_value = macroSharpDot(ch, arg);
            return true;

        case ':':
            m_oToken.m_value = macroSharpColon(ch, arg);
            return true;

        case 'B': case 'b':
        case 'O': case 'o':
        case 'R': case 'r':
        case 'X': case 'x':
            m_oToken.m_value = macroSharpR(ch, arg);
            return true;

        case 'C': case 'c':
            m_oToken.m_value = macroSharpC(ch, arg);
            return true;

        case '\\':
            m_oToken.m_value = macroSharpBackslash(ch, arg);
            return true;

        case '|':
            macroSharpVerticalBar(ch, arg);
            return false;

        default:
            error(L"Unsupported sharp subchar ~S.", ch);
        } // switch
    } // macroSharp

    // macro #\ - character
    Val macroSharpBackslash(Val ch, Val arg)
    {
        if (nil != arg) extraInfixArg(ch);
        unread_char(ch, m_stream);

        if (Token::Type_Eof == readToken(nil))
        {
            error(Qreader_eof_error, Kstream, m_stream);
        }

        if (1 == m_oToken.m_nLength)
        {
            return Character::Encode(m_oToken.m_rgwchChar[0]);
        }

        if (5 == m_oToken.m_nLength &&
            ('U' == m_oToken.m_rgwchChar[0] ||
             'u' == m_oToken.m_rgwchChar[0] ))
        {
            Val code = Fixnum::Encode(0);
            for (int i = 1; i <= 4; i++)
            {
                Val x = digit_char_p(m_oToken.m_rgwchChar[i], 16);
                if (nil == x)
                {
                    code = nil;
                    break;
                }
                code = ash(code, 4);
                code = add(code, x);
            } // for i

            if (nil != code)
            {
                return Character::Encode(static_cast<char16>(
                    Fixnum::Decode_(code) ));
            }
        } // if

        m_oToken.m_rgwchChar[m_oToken.m_nLength] = 0;

        Val name = make_string(
            m_oToken.m_rgwchChar,
            m_oToken.m_nLength );

        return name_char(name);
    } // macroSharpBackslash

    // macroSharpC -- #c(real imag)
    Val macroSharpC(Val ch , Val arg)
    {
        if (nil != arg) extraInfixArg(ch);
        Val data = read(m_stream, t, nil, t);
        if (isSuppress()) return nil;

        if (! consp(data) || ! consp(cdr(data)) || nil != cddr(data))
        {
            error(L"#~C expect two elements list.", ch);
        }
        return complex(first(data), second(data));
    } // macroSharpC

    // macroSharpColon - #:name
    Val macroSharpColon(Val, Val)
    {
        Token::Type eType = readToken(nil);

        if (isSuppress()) return nil;
        if (Token::Type_Token != eType) error(L"Invalid symbol name.");

        m_oToken.m_rgwchChar[m_oToken.m_nLength] = 0;
        return make_symbol(m_oToken.m_rgwchChar);
    } // macroSharpColon

    // macroSharpDot
    Val macroSharpDot(Val, Val)
    {
        Val form = read(m_stream, t, nil, t);
        if (isSuppress())
        {
            return nil;
        }
        else if (nil != TLV(Aread_evalA))
        {
            return eval(form);
        }
        else
        {
            return nil;
        }
    } // macroSharpDot

    // macroSharpOpenParen
    Val macroSharpOpenParen(Val, Val arg)
    {
        Val list = macroOpenParen();
        Val n = length(list);
        if (nil != arg && n != arg)
        {
            error(L"Expect ~D elements: ~S", arg, list);
        }
        Val vector = make_vector(n);
        uint nIndex = 0;
        while (! endp(list))
        {
            setf_svref(car(list), vector, nIndex);
                nIndex += 1;
                list = cdr(list);
        } // while
        return vector;
    } // mark_sharp_left_paren

    // macro #nRddddd
    Val macroSharpR(Val subchar, Val arg)
    {
        Int iBase;
        if (Character::Encode('R') == subchar ||
            Character::Encode('r') == subchar )
        {
            if (! fixnump(arg)) error(L"expect argument.");

            iBase = Fixnum::Decode_(arg);

            if (iBase < 1 || iBase > 36) 
            {
                error(L"expect 2 through 36; ~D", arg);
            }
        }
        else
        {
            if (nil != arg) error(L"expect no argument.");

            switch (Character::ToCode(subchar))
            {
            case 'B': case 'b': iBase = 2; break;
            case 'O': case 'o': iBase = 8; break;
            case 'X': case 'x': iBase = 16; break;
            default:
                CAN_NOT_HAPPEN();
            } // switch
        } // if

        Val ch = read_char(m_stream);
        Int iSign = 1;
        if (Character::Encode('-') == ch)
        {
            ch = read_char(m_stream);
            iSign = -1;
        }
        else if (Character::Encode('+') == ch)
        {
            ch = read_char(m_stream);
        }

        Val num = Fixnum::Encode(0);
        uint cDigits = 0;
        for (;;)
        {
            Val digit = digit_char_p(ch, iBase);
            if (nil == digit)
            {
                if (0 == cDigits)
                {
                    error(L"expect digit.");
                }

                char16 wchDigit = Character::ToCode(ch);

                Int nAttr = Fixnum::Decode_(
                    get_char_attr(
                        Character::Encode(wchDigit),
                        TLV(AreadtableA) ));

                Readtable::Type eType = static_cast<Readtable::Type>(
                    nAttr & Readtable::Type_Mask );

                if (Readtable::Type_Tmacro == eType ||
                    Readtable::Type_Space  == eType )
                {
                    unread_char(ch, m_stream);
                    break;
                }

                error(L"expect digit");
            } // if

            num = mul(num, iBase);
            num = add(num, digit);

            cDigits += 1;

            ch = read_char(m_stream, nil);
            if (Keof == ch)
            {
                break;
            }
        } // for

        if (iSign < 0) num = sub(0, num);
        return num;
    } // macroSharpR

    // macro #+ and #-
    bool macroSharpPlus(Val ch, Val arg)
    {
        if (nil != arg) extraInfixArg(ch);

        if (isSuppress())
        {
            read(m_stream, t, nil, t);    // test
            read(m_stream, t, nil, t);    // form
            return false;
        }

        Val test;
        {
            BindFrameScope oLet(1);
                oLet.Bind(TLV_ApackageA, PACKAGE_keyword);

            test = read(m_stream, t, nil, t);
        } // test

        Val result = evalTest(test) ?
            Character::Encode('+') :
            Character::Encode('-');

        if (result == ch)
        {
            m_oToken.m_value = read(m_stream, t, nil, t);
            return true;
        }
        else
        {
            BindFrameScope oLet(1);
                oLet.Bind(TLV_Aread_suppressA, t);

            read(m_stream, t, nil, t);
            return false;
        }
    } // macroSharpPlus

    // evalTest
    bool evalTest(Val expr)
    {
        if (symbolp(expr))
        {
            return nil != memq(expr, TLV(AfeaturesA));
        } // if symbol

        if (consp(expr))
        {
            if (Q(":AND") == car(expr))
            {
                for (
                    Val runner = cdr(expr);
                    ! endp(runner);
                    runner = cdr(runner) )
                {
                    if (! evalTest(car(runner)))
                    {
                        return false;
                    }
                }
                return true;
            } // and

            if (Q(":NOT") == car(expr) &&
                consp(cdr(expr)) &&
                nil == cddr(expr) )
            {
                return ! evalTest(second(expr));
            } // not

            if (Q(":OR") == car(expr))
            {
                for (
                    Val runner = cdr(expr);
                    ! endp(runner);
                    runner = cdr(runner) )
                {
                    if (evalTest(car(runner)))
                    {
                        return true;
                    }
                }
                return false;
            } // or
        } // if cons

        error(L"Invalid feature expression: ~S", expr);
    } // expr

    // macro #| - multiple line comment
    void macroSharpVerticalBar(Val, Val arg)
    {
        if (nil != arg) error(L"expect no argument.");

        enum
        {
            State_Normal,
            State_Bar,
            State_Sharp,
        } eState = State_Normal;

        uint nLevel = 1;

        for (;;)
        {
            Val ch = read_char(m_stream);

            switch (eState)
            {
            case State_Normal:
                switch (Character::ToCode(ch))
                {
                case '#':
                    eState = State_Sharp;
                    break;
                case '|':
                    eState = State_Bar;
                    break;
                } // switch ch
                break;

            case State_Bar:
                switch (Character::ToCode(ch))
                {
                case '#':
                    nLevel -= 1;
                    if (0 == nLevel) return;
                    eState = State_Normal;
                    break;
                case '|':
                    break;
                default:
                    eState = State_Normal;
                    break;
                } // switch ch
                break;

            case State_Sharp:
                switch (Character::ToCode(ch))
                {
                case '#':
                    break;
                case '|':
                    eState = State_Normal;
                    nLevel += 1;
                    break;

                default:
                    eState = State_Normal;
                    break;
                } // switch ch
                break;
            } // switch eState
        } // for
    } // macroSharpVerticalBar

    // Reader main
    protected: Token::Type main(
        Val delimiter,
        bool fEofError,
        Val  eof_value = nil )
    {
      try_again:

        Token::Type eType = readToken(delimiter);

        switch (eType)
        {
        case Token::Type_Delimiter:
            break;

        case Token::Type_Dot:
            if (! characterp(delimiter))
            {
                error(L"Use dot outside parenthesis.");
            }
            break;

        case Token::Type_Eof:
            if (fEofError) error(L"End of file");
            m_oToken.m_value = eof_value;
            eType = Token::Type_Token;
            break;

        case Token::Type_Macro:
            if (! processMacroChar(m_oToken.m_value))
            {
                goto try_again;
            }
            break;

        case Token::Type_None:
            break;

        case Token::Type_Token:
            if (isSuppress()) return Token::Type_None;

            eType = parseToken();
            if (Token::Type_Dot == eType)
            {
                if (! characterp(delimiter))
                {
                    error(L"Use dot outside parenthesis");
                }
            }
            break;
        } // switch

        return eType;
    } // main

    // parseFloat
    Val parseFloat()
    {
        uint nNext;
        bool fMinus = false;
        Val before;
            if (m_oToken.m_rgnAttr[0] & Readtable::Trait_Plus)
            {
                before = parseUInt(1, &nNext, Fixnum::Encode(10));
            }
            else if (m_oToken.m_rgnAttr[0] & Readtable::Trait_Minus)
            {
                before = parseUInt(1, &nNext, Fixnum::Encode(10));
                fMinus = true;
            }
            else
            {
                before = parseUInt(0, &nNext, Fixnum::Encode(10));
            }

        Val after  = Fixnum::Encode(0);
        if (m_oToken.m_rgnAttr[nNext] & Readtable::Trait_Decimal)
        {
            uint nStart = nNext + 1;
            after = parseUInt(nNext + 1, &nNext);
            after = div(after, expt10(nNext - nStart));
        }

        Val exponent = Fixnum::Encode(0);
        Val format = TLV(Aread_default_float_formatA);

        uint nAttr = m_oToken.m_rgnAttr[nNext];
        if (nAttr & Readtable::Trait_FloatMarker)
        {
            if (nAttr & Readtable::Trait_Dmarker)
            {
                format = Qdouble_float;
            }
            else if (nAttr & Readtable::Trait_Emarker)
            {
                // *read-default-float-format*
            }
            else if (nAttr & Readtable::Trait_Fmarker)
            {
                format = Qsingle_float;
            }
            else if (nAttr & Readtable::Trait_Lmarker)
            {
                format = Qlong_float;
            }
            else if (nAttr & Readtable::Trait_Smarker)
            {
                format = Qshort_float;
            }

            exponent = parseIint(nNext + 1, &nNext);
        } // if

        Val fraction = add(before, after);
        Int iExponent = Fixnum::Decode_(exponent);

        Val x;
        switch (format - nil)
        {
        case Qdouble_float - nil:
        case Qlong_float - nil:
            if (0 == iExponent)
            {
                x = DoubleFloatImpl::Make(fraction);
            }
            else if (iExponent > 0)
            {
                x = DoubleFloatImpl::Make(
                    mul(fraction, expt10(iExponent)) );
            }
            else
            {
                x = DoubleFloatImpl::Make(
                    div(fraction, expt10(-iExponent)) );
            }
            break;

        case Qsingle_float - nil:
        case Qshort_float - nil:
            if (0 == iExponent)
            {
                x = SingleFloatImpl::Make(
                    convert_to_float32(fraction) );
            }
            else if (iExponent > 0)
            {
                x = SingleFloatImpl::Make(
                    convert_to_float32(mul(fraction, expt10(iExponent))) );
            }
            else
            {
                x = SingleFloatImpl::Make(
                    convert_to_float32(div(fraction, expt10(-iExponent))) );
            }
            break;

        default:
            CAN_NOT_HAPPEN();
        } // switch format

        return fMinus ? C_neg(x) : x;
    } // parseFloat

    Val parseIint(uint nStart, uint* out_nNext)
    {
        return parseIint(nStart, out_nNext, Fixnum::Encode(10));
    } // parseIint

    Val parseUInt(uint nStart, uint* out_nNext)
    {
        return parseUInt(nStart, out_nNext, Fixnum::Encode(10));
    } // parseIint

    // parseIint
    Val parseIint(uint nStart, uint* out_nNext, Val baze)
    {
        if (m_oToken.m_rgnAttr[nStart] & Readtable::Trait_Plus)
        {
            return parseUInt(nStart + 1, out_nNext, baze);
        }
        else if (m_oToken.m_rgnAttr[nStart] & Readtable::Trait_Minus)
        {
            return sub(0, parseUInt(nStart + 1, out_nNext, baze));
        }
        else
        {
            return parseUInt(nStart, out_nNext, baze);
        }
    } // parseIint

    // parseUInt
    Val parseUInt(uint nStart, uint* out_nNext, Val baze)
    {
        Val num = Fixnum::Encode(0);

        uint nIndex;
        for (nIndex = nStart; nIndex < m_oToken.m_nLength; nIndex++)
        {
            uint nAttr = m_oToken.m_rgnAttr[nIndex];
            if (! (nAttr & Readtable::Trait_Digit)) break;

            Val digit = digit_char_p(
                Character::Encode(m_oToken.m_rgwchChar[nIndex]),
                baze );

            if (nil == digit) break;

            num = mul(num, baze);
            num = add(num, digit);
        } // for

        *out_nNext = nIndex;

        return num;
    } // parseUInt

    enum Analysis
    {
        Analysis_Int10,
        Analysis_Integer,
        Analysis_Ratio,
        Analysis_Float,
        Analysis_Symbol,
    }; // Analysis

    // analyzeToken
    //  Analyze token and returns class of token.
    //      symbol
    //      rational
    //          [<sign>] <digit>+ ['/' <digit>+]
    //      float
    //          [<sign>] <digit>+ ['.' <digit>* <marker> <sign> <digit>+>]
    //
    private: Analysis analyzeToken()
    {
        uint nStart = 0;

        if (m_oToken.m_rgnAttr[0] & Readtable::Trait_Sign)
        {
            nStart = 1;
        }

        enum RatState
        {
            RatState_Not,
            RatState_Start,
            RatState_Num,
            RatState_Slash,
            RatState_Den,
        } eRational = RatState_Start;

        enum FltState
        {
            FltState_Not,
            FltState_Start,
            FltState_BeforeDecimal,
            FltState_Decimal,
            FltState_AfterDecimal,
            FltState_Marker,
            FltState_ExponentSign,
            FltState_Exponent,
        } eFloat = FltState_Start;

        for (uint nRunner = nStart; nRunner < m_oToken.m_nLength; nRunner++)
        {
            uint nAttr = m_oToken.m_rgnAttr[nRunner];
            Val ch = Character::Encode(m_oToken.m_rgwchChar[nRunner]);

            switch (eRational)
            {
            case RatState_Not:
                break;

            case RatState_Start:
                eRational = RatState_Not;
                if (nAttr & Readtable::Trait_Digit &&
                    nil != digit_char_p(ch) )
                {
                    eRational = RatState_Num;
                }
                break;

            case RatState_Num:
                eRational = RatState_Not;
                if (nAttr & Readtable::Trait_Digit &&
                    nil != digit_char_p(ch) )
                {
                    eRational = RatState_Num;
                }
                else if (nAttr & Readtable::Trait_Ratio)
                {
                    eRational = RatState_Slash;
                }
                break;

            case RatState_Slash:
                eRational = RatState_Not;
                if (nAttr & Readtable::Trait_Digit &&
                    nil != digit_char_p(ch) )
                {
                    eRational = RatState_Den;
                }
                break;

            case RatState_Den:
                eRational = RatState_Not;
                if (nAttr & Readtable::Trait_Digit &&
                    nil != digit_char_p(ch) )
                {
                    eRational = RatState_Den;
                }
                break;

            default:
                CAN_NOT_HAPPEN();
            } // switch RatState

            switch (eFloat)
            {
            case FltState_Not:
                break;

            case FltState_Start:
                eFloat = FltState_Not;
                if (nAttr & Readtable::Trait_Decimal)
                {
                    eFloat = FltState_Decimal;
                }
                else if (nAttr & Readtable::Trait_Digit &&
                         nil != digit_char_p(ch, 10) )
                {
                    eFloat = FltState_BeforeDecimal;
                }
                break;

            case FltState_BeforeDecimal:
                eFloat = FltState_Not;
                if (nAttr & Readtable::Trait_Decimal)
                {
                    eFloat = FltState_Decimal;
                }
                else if (nAttr & Readtable::Trait_Digit &&
                         nil != digit_char_p(ch, 10) )
                {
                    eFloat = FltState_BeforeDecimal;
                }
                else if (nAttr & Readtable::Trait_FloatMarker)
                {
                    eFloat = FltState_Marker;
                }
                break;

            case FltState_Decimal:
                eFloat = FltState_Not;
                if (nAttr & Readtable::Trait_Digit &&
                         nil != digit_char_p(ch, 10) )
                {
                    eFloat = FltState_AfterDecimal;
                }
                break;

            case FltState_AfterDecimal:
                eFloat = FltState_Not;
                if (nAttr & Readtable::Trait_Digit &&
                         nil != digit_char_p(ch, 10) )
                {
                    eFloat = FltState_AfterDecimal;
                }
                else if (nAttr & Readtable::Trait_FloatMarker)
                {
                    eFloat = FltState_Marker;
                }
                break;

            case FltState_Marker:
                eFloat = FltState_Not;
                if (nAttr & Readtable::Trait_Digit &&
                         nil != digit_char_p(ch, 10) )
                {
                    eFloat = FltState_Exponent;
                }
                else if (nAttr & Readtable::Trait_Sign)
                {
                    eFloat = FltState_ExponentSign;
                }
                break;

            case FltState_ExponentSign:
                eFloat = FltState_Not;
                if (nAttr & Readtable::Trait_Digit &&
                         nil != digit_char_p(ch, 10) )
                {
                    eFloat = FltState_Exponent;
                }
                break;

            case FltState_Exponent:
                eFloat = FltState_Not;
                if (nAttr & Readtable::Trait_Digit &&
                         nil != digit_char_p(ch, 10) )
                {
                    eFloat = FltState_Exponent;
                }
                break;

            default:
                CAN_NOT_HAPPEN();
            } // switch FltState
        } // for

        switch (eRational)
        {
        case RatState_Num:
            return Analysis_Integer;
        case RatState_Den:
            return Analysis_Ratio;
        } // switch RatState

        switch (eFloat)
        {
        case FltState_Decimal:
            return Analysis_Int10;

        case FltState_AfterDecimal:
        case FltState_Exponent:
            return Analysis_Float;
        } // switch FltState

        return Analysis_Symbol;
    } // analyzeToken

    private: static bool isSuppress()
    {
        return nil != TLV(Aread_suppressA);
    } // isSuppress

    // parseSymbol
    private: Val parseSymbol()
    {
        enum State
        {
            State_Package,
            State_Colon,
            State_Symbol
        } eState = State_Package;

        uint cColons = 0;
        uint nStart = 0;
        for (uint nIndex = 0; nIndex < m_oToken.m_nLength; nIndex++)
        {
            if (m_oToken.m_rgnAttr[nIndex] & Readtable::Trait_Package)
            {
                switch (eState)
                {
                case State_Package:
                    cColons = 1;
                    eState = State_Colon;
                    break;

                case State_Colon:
                    when (2 == cColons) error(L"Too many colons.");
                    eState = State_Colon;
                    cColons = 2;
                    break;


                case State_Symbol:
                    error(L"Invalid placed colon");
                } // switch
            }
            else if (m_oToken.m_rgnAttr[nIndex] & Readtable::Trait_Alphadigit)
            {
                if (State_Colon == eState)
                {
                    eState = State_Symbol;
                    nStart = nIndex;
                }
            }
            else
            {
                error(L"Invalid character for symbol.");
            }
        } // for

        Val package;

        if (0 == cColons)
        {
            package =  MiniThread::Get()->GetTlv(TLV_ApackageA);
        }
        else if (cColons == nStart)
        {
            package = PACKAGE_keyword;
            cColons = 2;
        }
        else
        {
            StackString oName(
                    m_oToken.m_rgwchChar,
                    nStart - cColons );

            Val name = oName.Encode();

            package = find_package(name);
            if (! packagep(package))
            {
                error(L"No such package ~S", name);
            }
        } // if

        StackString oName(
            m_oToken.m_rgwchChar + nStart,
            m_oToken.m_nLength - nStart );

        Val name = oName.Encode();

        switch (cColons)
        {
        case 0:
        case 2:
            return intern(name, package);

        case 1:
        {
            Val status;
            Val sym = find_symbol(name, package, &status);

            if (Kexternal == status)
            {
                return sym;
            }

            if (Kinherited == status)
            {
                error(L"Can't access inherited symbol: ~S", sym);
            }
            else if (Kinternal == status)
            {
                error(L"Can't access internal symbol: ~S", sym);
            }
            else
            {
                error(L"No such symbol ~S in ~S", name, package);
            }
        } // 1

        default:
            CAN_NOT_HAPPEN();
        } // switch cColons
    } // parseSymbol

    // Parse token
    protected: Token::Type parseToken()
    {
        if (0 == m_oToken.m_nLength)
        {
            // Symbol whose name is empty string!
            m_oToken.m_value = intern(
                make_string(L""),
                MiniThread::Get()->GetTlv(TLV_ApackageA) );
            return Token::Type_Token;
        }

        // Check dot only token, such as ".", "..", and so on.
        {
            uint cDots = 0;
            for (uint nIndex = 0; nIndex < m_oToken.m_nLength; nIndex++)
            {
                if (! (m_oToken.m_rgnAttr[nIndex] & Readtable::Trait_Dot))
                {
                    cDots = 0;
                    break;
                }

                cDots += 1;
            } // for

            when (1 == cDots) return Token::Type_Dot;
            when (cDots >= 2) error(L"Can't use dot only token.");
        }

        uint nNext;
        switch (analyzeToken())
        {
        case Analysis_Int10:
            m_oToken.m_value = parseIint(0, &nNext, Fixnum::Encode(10));
            break;

        case Analysis_Integer:
            m_oToken.m_value = parseIint(0, &nNext, TLV(Aread_baseA));
            break;

        case Analysis_Ratio:
        {
            Val num = parseIint(0, &nNext, TLV(Aread_baseA));
            Val den = parseUInt(nNext + 1, &nNext, TLV(Aread_baseA));
            m_oToken.m_value = div(num, den);
            break;
        } // Analysis_Ratio

        case Analysis_Float:
            m_oToken.m_value = parseFloat();
            break;

        case Analysis_Symbol:
            m_oToken.m_value = parseSymbol();
            break;

        default:
            CAN_NOT_HAPPEN();
        } // switch Analysis

        return Token::Type_Token;
    } // parseToken

    // process macro char
    private: bool processMacroChar(Val ch)
    {
        switch (Character::ToCode(ch))
        {
        case 0x22:
            m_oToken.m_value = macroDoubleQuote();
            break;

        case '#':
            return macroSharp();

        case 0x27:
            m_oToken.m_value = macroQuote();
            break;

        case '(':
            m_oToken.m_value = macroOpenParen();
            break;

        case ')':
            macroCloseParen();
            return false;

        case ',':
            m_oToken.m_value = macroComma();
            break;

        case ';':
            macroSemiColon();
            return false;

        case '`':
            m_oToken.m_value = macroBackquote();
            break;

        default:
            error(L"Undefined macro character ~S.", ch);
        } // switch

        return true;
    } // processMacroChar

    // readCharWithAttr
    Val readCharWithAttr(Int* out_nAttr)
    {
        Val ch = read_char(m_stream, nil);
        if (! characterp(ch))
        {
            return nil;
        }

        *out_nAttr = Fixnum::Decode_(
            get_char_attr(ch,  MiniThread::Get()->GetTlv(TLV_AreadtableA)) );

        return ch;
    } // internal_read_char

    // readToken
    private: Token::Type readToken(Val delimiter)
    {
        m_oToken.m_nLength = 0;

        Val ch;
        Int nAttr;

        step_1:
            ch = readCharWithAttr(&nAttr);
            if (! characterp(ch))
            {
                return Token::Type_Eof;
            }

            if (delimiter == ch)
            {
                return Token::Type_Delimiter;
            }
            goto step_2;

        step_2:
            switch (static_cast<Readtable::Type>(nAttr & Readtable::Type_Mask))
            {
            case Readtable::Type_Cons:
                goto step_7;

            case Readtable::Type_Nmacro:
            case Readtable::Type_Tmacro:
                m_oToken.m_value = ch;
                return Token::Type_Macro;

            case Readtable::Type_Space:
                goto step_1;

            case Readtable::Type_Sescape:
                ch = read_char(m_stream);
                addTokenChar(
                    ch,
                    Readtable::Type_Cons | Readtable::Trait_Alphabetic );
                goto step_8;

            case Readtable::Type_Mescape:
                goto step_9;

            default:
                error(L"Invalid character");
            } // switch

        step_7:
        {
            Val rdcase = readtable_case(
                MiniThread::Get()->GetTlv(TLV_AreadtableA) );

            if (Kupcase == rdcase)
            {
                ch = char_upcase(ch);
            }
            else if (Kdowncase == rdcase)
            {
                ch = char_downcase(ch);
            }
            else if (Kpreserve == rdcase)
            {
                // nothing to do
            }
            else
            {
                error(L"NYI: readtable-case=~S", rdcase);
            }

            addTokenChar(ch, nAttr);
            goto step_8;
        } // step_7

        step_8:
            ch = readCharWithAttr(&nAttr);
            if (! characterp(ch))
            {
                goto step_10;
            }

            switch (static_cast<Readtable::Type>(nAttr & Readtable::Type_Mask))
            {
            case Readtable::Type_Cons:
                goto step_7;

            case Readtable::Type_Nmacro:
                goto step_7;

            case Readtable::Type_Tmacro:
                unread_char(ch, m_stream);
                goto step_10;   // terminate token

            case Readtable::Type_Space:
                goto step_10;   // teminate token

            case Readtable::Type_Sescape:
                ch = read_char(m_stream);
                addTokenChar(
                    ch,
                    Readtable::Type_Cons | Readtable::Trait_Alphabetic );
                goto step_8;

            case Readtable::Type_Mescape:
                goto step_9;

            default:
                error(L"Invalid character");
            } // switch

        step_9:
            ch = readCharWithAttr(&nAttr);
            if (! characterp(ch))
            {
                error(L"reader-eof-error");
            }

            switch (static_cast<Readtable::Type>(nAttr & Readtable::Type_Mask))
            {
            case Readtable::Type_Mescape:
                goto step_8;

            case Readtable::Type_Sescape:
                ch = read_char(m_stream);
                break;

            case Readtable::Type_Invalid:
                error(L"Invalid character");
            } // switch

            addTokenChar(
                ch,
                Readtable::Type_Cons | Readtable::Trait_Alphabetic );
            goto step_9;

        step_10:
            return Token::Type_Token;
    } // readToken
}; // Reader

} // namespace

//////////////////////////////////////////////////////////////////////////////
//
// 23 Reader
//

// get_char_attr
Val get_char_attr(Val ch, Val readtable)
{
    char16 wchChar = Character::ToCode(ch);
    Readtable* pReadtable = readtable->Decode<Readtable>();

    Val attr;
    if (wchChar < 128)
    {
        attr = svref(pReadtable->m_vector, wchChar);
    }
    else
    {
        if (! hash_table_p(pReadtable->m_table))
        {
            attr = nil;
        }
        else
        {
            attr = gethash(ch, pReadtable->m_table);
        }
    } // if

    if (fixnump(attr))
    {
        return attr;
    }

    if (consp(attr))
    {
        return car(attr);
    }

    return Fixnum::Encode(Readtable::Type_Cons | Readtable::Trait_Alphabetic);
} // get_char_attr

} // Genesis

namespace CommonLisp
{

using namespace Genesis;

//////////////////////////////////////////////////////////////////////
//
// read
//
Val read(Val stream, Val eof_error_p, Val eof_value, Val recursivep)
{
    Reader oReader;

    if (nil == stream)
    {
        stream = TLV(Astandard_inputA);
    }
    else if (t == stream)
    {
        stream = TLV(Aterminal_ioA);
    }

    check_type(stream, stream);

    return oReader.Run(
        stream,
        nil != eof_error_p,
        eof_value,
        nil != recursivep,
        false );
} // read

} // CommonLisp
