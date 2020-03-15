@echo off
echo I am in charge here (call me Al).

rem - run all versions of one test

if "%1==" goto help
if !%1==!/? goto help

rem - does test exist ?

if exist test%1?.a goto begin
echo Test%1 not found!
goto done

:begin

rem - erase any old results

if exist testchk.txt del testchk.txt

rem - run the test(s)

if exist test%1.a call regress1.bat test%1
if exist test%1b.a call regress1.bat test%1b
if exist test%1c.a call regress1.bat test%1c
if exist test%1d.a call regress1.bat test%1d

if exist test%1e.a call regress1.bat test%1e
if exist test%1f.a call regress1.bat test%1f
if exist test%1g.a call regress1.bat test%1g
if exist test%1h.a call regress1.bat test%1h
if exist test%1i.a call regress1.bat test%1i
if exist test%1j.a call regress1.bat test%1j
if exist test%1k.a call regress1.bat test%1k
if exist test%1l.a call regress1.bat test%1l
if exist test%1m.a call regress1.bat test%1m
if exist test%1n.a call regress1.bat test%1n
if exist test%1o.a call regress1.bat test%1o
if exist test%1p.a call regress1.bat test%1p
if exist test%1q.a call regress1.bat test%1q
if exist test%1r.a call regress1.bat test%1r
if exist test%1s.a call regress1.bat test%1s
if exist test%1t.a call regress1.bat test%1t
if exist test%1u.a call regress1.bat test%1u
if exist test%1v.a call regress1.bat test%1v
if exist test%1w.a call regress1.bat test%1w
if exist test%1x.a call regress1.bat test%1x

edit testchk.txt

goto done

:help

echo Run all variants of one test in both old and new HXA versions
echo.
echo Useage: REGRESS num
echo.
echo  num - three digit test number

:done
