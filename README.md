# HXA_Cross_Assembler
HXA is capable of producing absolute output files in Intel Hex, Motorola SRecord, and raw binary formats, as well as error and listing files (with optional cross-referencing).

HXA is meant to be simple and straightforward to use, requiring little setup. There are also facilities for more advanced projects, such as the equivalent of a simple built-in linker.

The design is fairly processor agnostic. Only one of its source files "knows" anything about a particular CPU, and is easily replaced by another file for another CPU. Instruction sets can also be defined by macros, though raw throughput will decrease due to the additional overhead required compared to a "native" version.

Variants of HXA know very much about real processors but very little about real computers. Details such as ROM routines, I/O locations and file formats must be explicitly provided in source code for each real computer HXA is required to work with. Fortunately in most cases this need only be done once and the information placed in one or more include files.

Currently Supported Processors:

- T_XX Family : an imaginary series of test processors 
- 6502 Family : 6502, 65C02, R65C02, W65C02S and W65C816S

Currently Supported Operating Systems:
- Win95 or higher 
- the Windows version is reported to run under WINE for Linux as well

This version of HXA is written in [Thompson AWK](http://www.tasoft.com/index.html), a compiled version of the normally interpreted [AWK](https://en.wikipedia.org/wiki/AWK) language.
