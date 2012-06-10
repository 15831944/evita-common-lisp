OBJS= \
    $(OutDir)\x64_ke_call_lisp.obj

ML64="$(VCINSTALLDIR)\bin\x86_amd64\ml64.exe" -nologo

build: $(OutDir)\_boot.txt

$(OutDir)\_boot.txt: $(OBJS)
    @echo "done" > $@

$(OutDir)\x64_ke_call_lisp.obj: ..\arch\x64\kernel\x64_ke_call_lisp.asm
    $(ML64) /Fo $@ /Zd /c $?
