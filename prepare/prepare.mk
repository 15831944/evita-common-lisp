build: $(OutDir)\_boot.txt


$(OutDir)\_boot.txt : \
  $(OutDir)\_boot.image \
  $(OutDir)\_img_object.h \
  $(OutDir)\boot.exe
    @echo Create _img_object_h and _boot.image
    cd $(OutDir)
    $(OutDir)\boot.exe
    @if exist genesis.image del genesis.image
    @echo done > $@

$(OutDir)\_boot.image $(OutDir)\_img_object.h:
    echo Generate $@


clean:
    del $(OutDir)\_img_object.h
    del $(OutDir)\_boot.image

rebuild: clean build
