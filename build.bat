@echo off

pushd .\resources
del ..\build\test.sfc

asar.exe -v --symbols=wla --symbols-path=..\build\test.sym ..\test.asm ..\build\test.sfc
popd