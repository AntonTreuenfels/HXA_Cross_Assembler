@echo off

rem - meant to be called by TESTALL.BAT
rem - TESTONE assem file

rem - run the test

%1 %2

rem - check the test series

call awk.bat testchk.awk %2

rem - erase unneeded files

if exist *.lst del *.lst > NUL
if exist *.err del *.err > NUL
if exist *.obj del *.obj > NUL
if exist *.hex del *.hex > NUL
if exist *.s19 del *.s19 > NUL
if exist *.s28 del *.s28 > NUL
if exist *.s37 del *.s37 > NUL
if exist *.001 del *.001 > NUL
if exist *.002 del *.002 > NUL
if exist *.005 del *.005 > NUL
if exist *.006 del *.006 > NUL
if exist srec*.* del srec*.* > NUL
if exist obj*.* del obj*.* > NUL

