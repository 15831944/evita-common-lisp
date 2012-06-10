@if ""=="%_echo%" echo off
://////////////////////////////////////////////////////////////////////
://
:// make-test-case - Test Case Generator For Regex Facility
:// make-test-case.cmd
://
:// This file is NOT part of Evita Common Lisp.
://
setlocal

set perl_exe=-
for %%x in (
    \src\perl\perl-5.8.4\perl.exe
    \src\perl-5.8.4\perl.exe
) do (
    if exist %%x (
      set perl_exe=%%x
      goto end_perl_exe
    )
)
:end_perl_exe
    if "-"=="%perl_exe%" (
        echo "No perl.exe"
        goto end
    )

set pcre_dir=-
for %%x in (
    \src\regex\pcre-4.3\testdata
    \src\pcre-4.3\testdata
) do (
    if exist %%x (
        set pcre_dir=%%x
        goto end_pcre_dir
    )
)

:end_pcre_dir

set cl_ppcre_dir=-
for %%x in (
    \src\lisp\cl-ppcre-0.7.7
    \src\cl-ppcre-0.7.7
) do (
    if exist %%x (
        set cl_ppcre_dir=%%x
        goto end_cl_ppcre_dir
    )
)
:end_cl_ppcre_dir

if not ""==%1 goto %1

call :perl
call :pcre
call :cl_ppcre
goto end

:perl
    for %%i in (evita 504 584) do (
        echo Generate Perl test case %%i
        %perl_exe% .\perl2lisp.pl retest-perl-%%i.txt
    )
goto end

:pcre
    if "-"=="%pcre_dir%" (
        echo No PCRE source directory.
        echo Skip generating test case.
        goto end
    )

    for %%i in (1 2 3 4 5) do (
        echo Generate PCRE test case %%i
        %perl_exe% .\pcre2lisp.pl %pcre_dir%\testinput%%i pcre-%%i.retest
    )
goto end

:cl_ppcre
    if "-"=="%cl_pcre_dir%" (
        echo No CL-PPCRE source directory.
        echo Skip generating test case.
        goto end
    )

    %perl_exe% .\pcre2lisp.pl %cl_ppcre_dir%\testinput pcre-cl.retest
goto end

:end
