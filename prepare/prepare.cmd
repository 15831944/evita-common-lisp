@if not defined _echo echo off
setlocal

if ""=="%2" (
    echo.
    echo Usage: %0 outdir platform
    echo    outdir      Output directiory
    echo    platform    "Win32" or "x64"
    exit/b 1
)

set OutDir=%1
set platform=%2
set arch.mk=..\arch\%platform%\prepare\%platform%_prepare.mk

nmake -nologo -f prepare.mk OutDir=%1

exit/b 0



::: NOT USED
cd
echo 1=%1
echo 2=%2
echo arch.mk=%arch.mk%

if exist %arch.mk% (
    del %OutDir%\_boot.txt
    nmake -nologo -f %arch.mk% OutDir=%1
)
::: END OF NOT USED
