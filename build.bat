@echo off

pushd .\resources
del ..\build\z1-dpcm.sfc

asar.exe -v --symbols=wla --symbols-path=..\build\z1-dpcm.sym ..\main.asm ..\build\z1-dpcm.sfc
popd