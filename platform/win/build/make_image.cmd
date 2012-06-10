@if not defined _echo echo off
:////////////////////////////////////////////////////////////////////////
://
:// Build Script For Evita Common Lisp
:// build.cmd
://
:// @(#)$Id: //proj/evcl3/mainline/platform/win/build/make_image.cmd#7 $
://

if not defined srcdir goto error_SRCDIR

goto do_%1

:do_
    for %%p in (%platforms%) do (
        for %%c in (%configs%) do (
            call :make_image %%c %%p
        )
    )
    exit/b 0

:do_debug
:do_debut
:do_release
    if "%2" == "" (
        for %%p in (%platforms%) do (
            call :make_%1 %%p
        )
        exit/b 0
    )
    call :make_%1 %2
    exit/b 0

:do_win32
:do_x64
    if "%2" == "" (
        for %%c in (%configs%) do (
            call :make_%%c %1
        )
        exit/b 0
    )
    call :make_%2 %1
    exit/b 0

:do_clean
    rmdir/s/q %srcdir%\release\win32\lisp
    exit/b 0

goto end

:make_debut
    setlocal
        cd %srcdir%\release\%1

        console.exe -dll vanilla.dll -image genesis.image ^
            < %srcdir%\lisp\debut\debut-01-prepare.lisp

        if not "%ERRORLEVEL%"=="0" (
            echo FAILED: debut release\%1
            exit/b 1
        )

        echo.
        echo Generating image ========================================
        echo.

        console.exe -dll vanilla.dll -image genesis.image ^
            < %srcdir%\lisp\debut\debut-02-save.lisp

        if not "%ERRORLEVEL%"=="0" (
            echo FAILED: debut release\%1
            exit/b 1
        )

        echo Generated release\%1\debut.image
    endlocal
    exit/b 0

:make_image
    setlocal
        cd %srcdir%\%1\%2

        console.exe -dll genesis.dll -image _boot.image ^
            < %srcdir%\lisp\genesis\g00-loadup.lisp

        if not "%ERRORLEVEL%"=="0" (
            echo FAILED: genesis load %1\%2
            exit/b 1
        )

        if not exist _untested.image goto fail_load

        console.exe -dll vanilla.dll -image _untested.image ^
            < %srcdir%\lisp\genesis\g90-check.lisp

        if not "%ERRORLEVEL%"=="0" (
            echo FAILED: test %1\%2
            exit/b 1
        )

        copy/y _untested.image genesis.image

        echo Generated %1\%2\genesis.image
    endlocal
    exit/b 0

:make_debug
    call :make_image debug %1
    exit/b 0

:make_release
    call :make_image release %1
    exit/b 0

:fail_load
    echo.
    echo !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    echo !!!
    echo !!!        Failed to load %1\%2\genesis files.
    echo !!!
    echo !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    echo.
    exit/b 1

:error_SRCDIR
    echo You must define SRCDIR.
    exit/b 1

:end
