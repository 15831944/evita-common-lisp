October 9, 2006

"$(VCINSTALLDIR)\bin\x86_amd64\ml64.exe" /c /W3 /WX /Zd /Fo$(IntDir)\$(InputName).obj /Fl$(IntDir)\$(InputName).lst /c $(InputPath)

/Zd -- line number information
/W3 -- warning level 3
/WX -- treat warning as error
/Fo -- object file
/Fl -- listing file
/c  -- assemble only (no link)

