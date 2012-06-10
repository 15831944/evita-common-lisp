Last Modified At: Sun Jun 20, 2004

This directory contains test harness and test data of regex facility.
Test cases are sniffed from various regex package available on the Internet.


HS Format
=========
regex-test-hs.txt
    From Henry Spencer's regular  expression library.
    http://www.arglist.com/regex/rxspencer-alpha3.8.g3.tar.gz 

Perl Format
===========

Files
-----
    1. regex-test-perl584.txt
        From perl 5.8.4 t/op/re_tests
        http://www.perl.com/
        field 1: pattern, optionally encoded in single-quote(') with modifier
        field 2: y, n, c (compile error),
                 b - perl bug shuold be skipped
                 B - perl bug should be skipped

    2. regex-test-perl504.txt
        From perl 5.004 t/op/re_tests. I forgot exact Perl version number. :-<
        http://www.perl.com/

    3. regex-test-evita.txt
        From Evita COM components

Hint
----
Try to use perl -Dr or 'use re "debug"' pragma for what Perl compiles regex
into byte-code.


PCRE Format
===========
URI: http://www.pcre.org/

Files
-----
    1. regex-test-pcre1.test
    2. regex-test-pcre2.test
    3. regex-test-pcre3.test
    4. regex-test-pcre4.test
    5. regex-test-pcre5.test
    6. regex-test-cl-ppcre.test     -- From CL-PPCRE

Format of test input (under testdata directory)
\S  => start of pattern. May span multiple line.
\s  => target string in single line.

Modifiers
    + => print $'
    8 => UTF-8 mode

    g => use "while" instead of "if"
    i => ignore-case
    m => multiline
    s => dot all
    x => extended

pcretest specific modifier
    A           => anchored
    D           => enable debugging
    E           => dollar end only
    G           => like "g"
    I           => emit information about compiled expression.
    L<locale>   =>  Set locale
    M           => emit size of memory of compiled expression
    N           => no auto capture
    P           => Use POSIX wrapper
    S           => call pcre_study()
    U           => Ungreedy. (not documented)
    X           => extra

* testdata2 contains PCRE extensions:
    (?(R) then | else)  -- test recursive call
    (?1) (?2)           -- recursive call
    (?C[num])           -- call out
    (?P<name>...)       -- named capture
    (?>name)            -- recursive call
    (?U)                -- ungreedy (not documented)



See also:
CL-PPCRE - portable Perl-compatible regular expressions for Common Lisp
http://weitz.de/cl-ppcre/
Test cases of CL-PPCRE is copy of PCRE. So, PCRE test cases cover them.

See also:
Onigrama (regex for ruby)
http://www.geocities.jp/kosako1/oniguruma/
