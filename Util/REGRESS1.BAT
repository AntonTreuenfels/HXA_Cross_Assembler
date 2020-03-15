@echo off
rem - run one regression test

rem - erase results of any previous test runs

if exist *.ols del *.ols
if exist *.oer del *.oer
if exist *.oob del *.oob
if exist *.ohe del *.ohe
if exist *.o19 del *.o19
if exist *.o28 del *.o28
if exist *.o37 del *.o37

if exist *.lst del *.lst
if exist *.err del *.err
if exist *.obj del *.obj
if exist *.hex del *.hex
if exist *.s19 del *.s19
if exist *.s28 del *.s28
if exist *.s37 del *.s37

rem - run the original test

hxa_org.exe %1.a

rem - rename the results of the original test series

if exist %1.lst ren %1.lst %1.ols
if exist %1.err ren %1.err %1.oer
if exist %1.obj ren %1.obj %1.oob
if exist %1.hex ren %1.hex %1.ohe
if exist %1.s19 ren %1.s19 %1.o19
if exist %1.s28 ren %1.s28 %1.o28
if exist %1.s37 ren %1.s37 %1.o37

if exist abs1.hex ren abs1.hex abs1.ohe
if exist abs2.hex ren abs2.hex abs2.ohe
if exist abs3.hex ren abs3.hex abs3.ohe
if exist abs4.hex ren abs4.hex abs4.ohe

if exist rel1.hex ren rel1.hex rel1.ohe
if exist rel2.hex ren rel2.hex rel2.ohe
if exist rel3.hex ren rel3.hex rel3.ohe
if exist rel4.hex ren rel4.hex rel4.ohe

if exist abs1.s19 ren abs1.s19 abs1.o19
if exist abs2.s19 ren abs2.s19 abs2.o19
if exist abs3.s19 ren abs3.s19 abs3.o19
if exist abs4.s19 ren abs4.s19 abs4.o19

if exist rel1.s19 ren rel1.s19 rel1.o19
if exist rel2.s19 ren rel2.s19 rel2.o19
if exist rel3.s19 ren rel3.s19 rel3.o19
if exist rel4.s19 ren rel4.s19 rel4.o19

if exist abs1.s28 ren abs1.s28 abs1.o28
if exist abs2.s28 ren abs2.s28 abs2.o28
if exist abs3.s28 ren abs3.s28 abs3.o28
if exist abs4.s28 ren abs4.s28 abs4.o28

if exist rel1.s28 ren rel1.s28 rel1.o28
if exist rel2.s28 ren rel2.s28 rel2.o28
if exist rel3.s28 ren rel3.s28 rel3.o28
if exist rel4.s28 ren rel4.s28 rel4.o28

if exist abs1.s37 ren abs1.s37 abs1.o37
if exist abs2.s37 ren abs2.s37 abs2.o37
if exist abs3.s37 ren abs3.s37 abs3.o37
if exist abs4.s37 ren abs4.s37 abs4.o37

if exist rel1.s37 ren rel1.s37 rel1.o37
if exist rel2.s37 ren rel2.s37 rel2.o37
if exist rel3.s37 ren rel3.s37 rel3.o37
if exist rel4.s37 ren rel4.s37 rel4.o37

rem - run the new test series

hxa_t.exe %1.a

rem - compare the output files

if exist %1.lst fc %1.lst %1.ols >> testchk.txt
if exist %1.err fc %1.err %1.oer >> testchk.txt
if exist %1.obj fc %1.obj %1.oob >> testchk.txt
if exist %1.hex fc %1.hex %1.ohe >> testchk.txt
if exist %1.s19 fc %1.s19 %1.o19 >> testchk.txt
if exist %1.s28 fc %1.s28 %1.o28 >> testchk.txt
if exist %1.s37 fc %1.s37 %1.o37 >> testchk.txt

if exist abs1.hex fc abs1.hex abs1.ohe >> testchk.txt
if exist abs2.hex fc abs2.hex abs2.ohe >> testchk.txt
if exist abs3.hex fc abs3.hex abs3.ohe >> testchk.txt
if exist abs4.hex fc abs4.hex abs4.ohe >> testchk.txt

if exist rel1.hex fc rel1.hex rel1.ohe >> testchk.txt
if exist rel2.hex fc rel2.hex rel2.ohe >> testchk.txt
if exist rel3.hex fc rel3.hex rel3.ohe >> testchk.txt
if exist rel4.hex fc rel4.hex rel4.ohe >> testchk.txt

if exist abs1.s19 fc abs1.s19 abs1.o19 >> testchk.txt
if exist abs2.s19 fc abs2.s19 abs2.o19 >> testchk.txt
if exist abs3.s19 fc abs3.s19 abs3.o19 >> testchk.txt
if exist abs4.s19 fc abs4.s19 abs4.o19 >> testchk.txt

if exist rel1.s19 fc rel1.s19 rel1.o19 >> testchk.txt
if exist rel2.s19 fc rel2.s19 rel2.o19 >> testchk.txt
if exist rel3.s19 fc rel3.s19 rel3.o19 >> testchk.txt
if exist rel4.s19 fc rel4.s19 rel4.o19 >> testchk.txt

if exist abs1.s28 fc abs1.s28 abs1.o28 >> testchk.txt
if exist abs2.s28 fc abs2.s28 abs2.o28 >> testchk.txt
if exist abs3.s28 fc abs3.s28 abs3.o28 >> testchk.txt
if exist abs4.s28 fc abs4.s28 abs4.o28 >> testchk.txt

if exist rel1.s28 fc rel1.s28 rel1.o28 >> testchk.txt
if exist rel2.s28 fc rel2.s28 rel2.o28 >> testchk.txt
if exist rel3.s28 fc rel3.s28 rel3.o28 >> testchk.txt
if exist rel4.s28 fc rel4.s28 rel4.o28 >> testchk.txt

if exist abs1.s37 fc abs1.s37 abs1.o37 >> testchk.txt
if exist abs2.s37 fc abs2.s37 abs2.o37 >> testchk.txt
if exist abs3.s37 fc abs3.s37 abs3.o37 >> testchk.txt
if exist abs4.s37 fc abs4.s37 abs4.o37 >> testchk.txt

if exist rel1.s37 fc rel1.s37 rel1.o37 >> testchk.txt
if exist rel2.s37 fc rel2.s37 rel2.o37 >> testchk.txt
if exist rel3.s37 fc rel3.s37 rel3.o37 >> testchk.txt
if exist rel4.s37 fc rel4.s37 rel4.o37 >> testchk.txt
