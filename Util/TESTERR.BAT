@echo off
echo I am in charge here (call me Al).

rem - check errorlevel returned by one test

if "%1==" goto help
if !%1==!/? goto help

rem - does test exist ?

if exist test%1.a goto begin
echo Test%1.a not found!
goto done

:begin

rem - does HXA executable exist ?

if not exist hxa_t.exe call make_t.bat

rem - erase results of any previous runs of this test

if exist test%1.lst del test%1.lst > NUL
if exist test%1.err del test%1.err > NUL
if exist test%1.obj del test%1.obj > NUL
if exist test%1.hex del test%1.hex > NUL

rem - run the test

hxa_t.exe test%1.a

rem - check errorlevel

if not errorlevel 7 goto check6
echo Errorlevel=7 (warn, error, fatal)
goto done

:check6
if not errorlevel 6 goto check5
echo Errorlevel=6 (error, fatal)
goto done

:check5
if not errorlevel 5 goto check4
echo Errorlevel=5 (warn, fatal)
goto done

:check4
if not errorlevel 4 goto check3
echo Errorlevel=4 (fatal)
goto done

:check3
if not errorlevel 3 goto check2
echo Errorlevel=3 (warn, error)
goto done

:check2
if not errorlevel 2 goto check1
echo Errorlevel=2 (error)
goto done

:check1
if not errorlevel 1 goto report0
echo Errorlevel=1 (warn)
goto done

:report0
echo Errorlevel=0

goto done

:help

echo Check errorlevel returned by one HXA test program
echo.
echo Useage: TESTERR num
echo.
echo  num - three digit test number (+ letter suffix, if any)

:done
