# Hobby Cross-Assembler (HXA) V0.200 - Instruction Set (65xx Version)

# (c) 2004-2013 by Anton Treuenfels

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

# ---------------------------------

# by Anton Treuenfels

# 5248 Horizon Dr
# Fridley, MN 55421

# e-mail: atreuenfels@earthlink.net

# source language: Thompson AWK

# first created: 01/18/03
# last revision: 07/29/13

# public function prefix: "INS"

# ----------------------------

# Address modes:

#                                   6502    65C02   R65C02  W65C02S W65C816S

# ab    - Absolute                  X       X       X       X       X
# abi   - Absolute Indirect         X       X       X       X       X
# abx   - Absolute X                X       X       X       X       X
# abxi  - Absolute X Indirect               X       X       X       X
# aby   - Absolute Y                X       X       X       X       X
# acc   - Accumulator               X       X       X       X       X
# bmv   - Block Move                                                X
# imm   - Immediate                 X       X       X       X       X
# imp   - Implied                   X       X       X       X       X
# lab   - Long Absolute                                             X
# labi  - Long Absolute Indirect                                    X
# labx  - Long Absolute X                                           X
# lpcr  - Long PC Relative                                          X
# lzpi  - Long Zero Page Indirect                                   X
# lzpiy - Long Zero Page Indirect Y                                 X
# pcr   - PC Relative               X       X       X       X       X
# sr    - Stack Relative                                            X
# sriy  - Stack Relative Indirect Y                                 X
# zp    - Zero Page                 X       X       X       X       X
# zpi   - Zero Page Indirect                X       X       X       X
# zpiy  - Zero Page Indirect Y      X       X       X       X       X
# zptr  - Zero Page Test Relative                   X       X
# zpx   - Zero Page X               X       X       X       X       X
# zpxi  - Zero Page X Indirect      X       X       X       X       X
# zpy   - Zero Page Y               X       X       X       X       X

# Notes:
# - the above address modes do not always correspond to official WDC
# literature in either name or number, but
# - (a) they do cover everything we need, and 
# - (b) we can do anything we want if it makes our lives easier
# (and is kept completely internal to this file ) !
# - ex: "labi" is not an official mode and exists simply so that "abi"
# can be easily forced to long with a minimum of fuss

# Expression processing by address mode:

# mode      adjustment      storage

# ab*       (none)          UBIT16
# abi       discard ()      UBIT16
# abx*      (none)          UBIT16
# abxi      discard (,x)    UBIT16
# aby*      (none)          UBIT16
# acc       (none)          (none)
# bmv       add ^           UBIT08, UBIT08
# imm       discard #       BIT08
# imp       (none)          (none)
# lab*      (none)          UBIT24
# labi      discard ()      BIT16
# labx*     (none)          UBIT24
# lpcr      (none)          RBIT16
# lzpi      discard []      UBIT08
# lzpiy     discard []      UBIT08
# pcr       (none)          RBIT08
# sr        (none)          UBIT08
# sriy      discard (,s)    UBIT08
# zp*       (none)          UBIT08
# zpi       discard ()      UBIT08
# zpiy      discard ()      UBIT08
# zptr      (none)          UBIT08, RBIT08
# zpx*      (none)          UBIT08
# zpxi      discard (,x)    UBIT08
# zpy*      (none)          UBIT08

# *= can be part of multiple possible modes:
# ab|zp|lab
# ab|lab
# ab|zp
# aby|zpy
# abx|zpx|labx
# abx|zpx

# -----------------------------

# NMOS 6502 instruction set

# Notes:
# - the ROR instruction available after June 1976
# - this is the base instruction set for all 65xx variants

local ins6502 = " \
 ADC 6Dab,7Dabx,79aby,69imm,65zp,71zpiy,75zpx,61zpxi \
 AND 2Dab,3Dabx,39aby,29imm,25zp,31zpiy,35zpx,21zpxi \
 ASL 0Eab,1Eabx,0Aacc,06zp,16zpx \
 BCC 90pcr \
 BCS B0pcr \
 BEQ F0pcr \
 BGE B0pcr \
 BIT 2Cab,24zp \
 BLT 90pcr \
 BMI 30pcr \
 BNE D0pcr \
 BPL 10pcr \
 BRK 00imm,00imp,00zp \
 BVC 50pcr \
 BVS 70pcr \
 CLC 18imp \
 CLD D8imp \
 CLI 58imp \
 CLV B8imp \
 CMP CDab,DDabx,D9aby,C9imm,C5zp,D1zpiy,D5zpx,C1zpxi \
 CPX ECab,E0imm,E4zp \
 CPY CCab,C0imm,C4zp \
 DEC CEab,DEabx,C6zp,D6zpx \
 DEX CAimp \
 DEY 88imp \
 EOR 4Dab,5Dabx,59aby,49imm,45zp,51zpiy,55zpx,41zpxi \
 INC EEab,FEabx,E6zp,F6zpx \
 INX E8imp \
 INY C8imp \
 JMP 4Cab,6Cabi \
 JSR 20ab \
 LDA ADab,BDabx,B9aby,A9imm,A5zp,B1zpiy,B5zpx,A1zpxi \
 LDX AEab,BEaby,A2imm,A6zp,B6zpy \
 LDY ACab,BCabx,A0imm,A4zp,B4zpx \
 LSR 4Eab,5Eabx,4Aacc,46zp,56zpx \
 NOP EAimp \
 ORA 0Dab,1Dabx,19aby,09imm,05zp,11zpiy,15zpx,01zpxi \
 PHA 48imp \
 PHP 08imp \
 PLA 68imp \
 PLP 28imp \
 ROL 2Eab,3Eabx,2Aacc,26zp,36zpx \
 ROR 6Eab,7Eabx,6Aacc,66zp,76zpx \
 RTI 40imp \
 RTS 60imp \
 SBC EDab,FDabx,F9aby,E9imm,E5zp,F1zpiy,F5zpx,E1zpxi \
 SEC 38imp \
 SED F8imp \
 SEI 78imp \
 STA 8Dab,9Dabx,99aby,85zp,91zpiy,95zpx,81zpxi \
 STX 8Eab,86zp,96zpy \
 STY 8Cab,84zp,94zpx \
 TAX AAimp \
 TAY A8imp \
 TSX BAimp \
 TXA 8Aimp \
 TXS 9Aimp \
 TYA 98imp \
"

# CMOS 65C02 instruction set (additions to NMOS 6502 only)

local ins65C02 = " \
 ADC 72zpi \
 AND 32zpi \
 BIT 3Cabx,89imm,34zpx \
 BRA 80pcr \
 CMP D2zpi \
 DEA 3Aimp \
 DEC 3Aacc \
 EOR 52zpi \
 INA 1Aimp \
 INC 1Aacc \
 JMP 7Cabxi \
 LDA B2zpi \
 ORA 12zpi \
 PHX DAimp \
 PHY 5Aimp \
 PLX FAimp \
 PLY 7Aimp \
 SBC F2zpi \
 STA 92zpi \
 STZ 9Cab,9Eabx,64zp,74zpx \
 TRB 1Cab,14zp \
 TSB 0Cab,04zp \
"

# Rockwell R65C02 instruction set (additions to CMOS 65C02 only)

local insR65C02 = " \
 BBR0 0Fzptr BBR1 1Fzptr BBR2 2Fzptr BBR3 3Fzptr \
 BBR4 4Fzptr BBR5 5Fzptr BBR6 6Fzptr BBR7 7Fzptr \
 BBS0 8Fzptr BBS1 9Fzptr BBS2 AFzptr BBS3 BFzptr \
 BBS4 CFzptr BBS5 DFzptr BBS6 EFzptr BBS7 FFzptr \
 RMB0 07zp RMB1 17zp RMB2 27zp RMB3 37zp \
 RMB4 47zp RMB5 57zp RMB6 67zp RMB7 77zp \
 SMB0 87zp SMB1 97zp SMB2 A7zp SMB3 B7zp \
 SMB4 C7zp SMB5 D7zp SMB6 E7zp SMB7 F7zp \
"

# WDC W65C02S instruction set (additions to R65C02 only)

local insW65C02S = " \
 STP DBimp \
 WAI CBimp \
"

# WDC W65C816S instruction set (additions to 65C02 only)

local insW65C816S = " \
 ADC 6Flab,7Flabx,63sr,73sriy,67lzpi,77lzpiy \
 AND 2Flab,3Flabx,23sr,33sriy,27lzpi,37lzpiy \
 BRL 82lpcr \
 CMP CFlab,DFlabx,C3sr,D3sriy,C7lzpi,D7lzpiy \
 COP 02imm,02zp \
 EOR 4Flab,5Flabx,43sr,53sriy,47lzpi,57lzpiy \
 JML DCabi,5Clab,DClabi \
 JMP 5Clab,DClabi \
 JSL 22lab \
 JSR FCabxi,22lab \
 LDA AFlab,BFlabx,A3sr,B3sriy,A7lzpi,B7lzpiy \
 MVN 54bmv \
 MVP 44bmv \
 ORA 0Flab,1Flabx,03sr,13sriy,07lzpi,17lzpiy \
 PEA F4ab,F4imm \
 PEI D4zpi,D4zp \
 PER 62lpcr \
 PHB 8Bimp \
 PHD 0Bimp \
 PHK 4Bimp \
 PLB ABimp \
 PLD 2Bimp \
 REP C2imm \
 RTL 6Bimp \
 SBC EFlab,FFlabx,E3sr,F3sriy,E7lzpi,F7lzpiy \
 SEP E2imm \
 STA 8Flab,9Flabx,83sr,93sriy,87lzpi,97lzpiy \
 STP DBimp \
 SWA EBimp \
 TAD 5Bimp \
 TAS 1Bimp \
 TCD 5Bimp \
 TCS 1Bimp \
 TDA 7Bimp \
 TDC 7Bimp \
 TSA 3Bimp \
 TSC 3Bimp \
 TXY 9Bimp \
 TYX BBimp \
 WAI CBimp \
 WDM 42imm,42imp,42zp \
 XBA EBimp \
 XCE FBimp \
"

# -----------------------------

# instruction tables (cpu dependent)

local opcodeSet

# current mnemonic and allowed address modes

local currMnemonic
local allowedMode

# forced address mode flag

local forceMode

# 8- or 16-bit cpu and register size flags

local cpu16Bit, acc16Bit, ndx16Bit

# direct page and data bank

local directPage, dataBank

# -----------------------------

# variant name

global function INSname() { return( "HXA65" ) }

# reserved symbols - variant identifier and register names

global function INSreserve() { return( "__HXA65__ A S X Y" ) }

# is token a recognized CPU ?
# - 6502 or 65C02 or R65C02 or W65C02S or W65C816S

global function INSiscpu(this) {

    return( this ~ /^6502$|^R?65C02$|^W65C(02|816)S$/ )
}

# return cpu descriptor

global function INSgetdescrip(this) {

    return( this == "W65C816S" ? "T_24_L" : "T_16_L" )
}

# add instructions to current instruction set

local function addins(this) {

    local i, j
    local mnemonic, mode, opcode
    local instruc, addrmode

    # after split, instruction mnemonics are in odd entries of 'instruc',
    # opcodes+address modes in even entries

    i = split( this, instruc, " " )
    do {
        mnemonic = instruc[ i-1 ]
        j = split( instruc[i], addrmode, "," )
        do {
            mode = addrmode[ j ]
            opcode = CKhextonum( substr(mode,1,2) )
            mode = substr( mode, 3 )
            opcodeSet[ mnemonic ][ mode ] = opcode
        } while ( --j )
    } while ( i -= 2 )
}

# setup instruction set for use

global function INSsetcpu(this) {

    cpu16Bit = ( this == "W65C816S" )
    acc16Bit = ndx16Bit = FALSE
    directPage = dataBank = 0

    # set instruction set

    addins( ins6502 )
    if ( this != "6502" ) {
        addins( ins65C02 )
        if ( this != "65C02" ) {
            if ( cpu16Bit )
                addins( insW65C816S )
            else {
                addins( insR65C02 )
                if ( this != "R65C02" )
                    addins( insW65C02S )
            }
        }
    }
}

# is token in current instruction set ?

global function INSisop(this) { return( this in opcodeSet ) }

# -----------------------------

# save opcode

local function save_opcode(mode) {

    CGsavedata( "BIT08", allowedMode[mode] )
}

# evaluate and save 8-, 16- or 24-bit data

local function save_data(type, expr) {

    CGsavedata( type, EXPgetnum(expr) )
}

# -------------------------------

# determine if 8- or 16-bit immediate value is to be used

local function imm16() {

    if ( cpu16Bit ) {
        if ( ndx16Bit && currMnemonic ~ /X|Y/ )
            return( TRUE )
        if ( currMnemonic == "PEA" )
            return( TRUE )
        if ( currMnemonic ~ /BRK|COP|REP|SEP|WDM/ )
            return( FALSE )
        if ( acc16Bit && currMnemonic !~ /X|Y/ )
            return( TRUE )
    }

    return( FALSE )
}

# save immediate opcode and 8- or 16-bit data

local function immediate(expr) {

    if ( forceMode == "" ) {
        save_opcode( "imm" )
        save_data( imm16() ? "BIT16" : "BIT08", substr(expr, 2) )
        return( TRUE )
    }

    return( FALSE )
}

# save opcode with no data

local function save00(mode) {

    if ( forceMode == "" ) {
        save_opcode( mode )
        return( TRUE )
    }

    return( FALSE )
}

# save mode if unforced, else save if forced mode is legal

local function save_mode_expr(mode, type, expr) {

    # forced mode active ?

    if ( forceMode ) {
    
        # - basically only zero page, absolute, long zero page and
        # long absolute modes can possibly be forced to another mode

        if ( !match(mode, /ab|zp/) )
            return( FALSE )

        # zero page ?

        if ( forceMode == "zp" ) {
            mode = "zp" substr( mode, RSTART+RLENGTH )
            type = "BIT08"
        }

        # absolute ?
        
        else if ( forceMode == "ab" ) {
            mode = "ab" substr( mode, RSTART+RLENGTH )
            type = "BIT16"
        }

        # ...must be long

        else {

            # 8- or 16-bit to 24-bit ?

            if ( mode ~ /^(ab|zp)x?$/ ) {
                mode = "lab" substr( mode, 3 )
                type = "BIT24"
            }
            else {

                # if not already a long mode, make it one

                if ( RSTART == 1 )
                    mode = "l" mode

                # size won't change if legal, but remove range-checking

                type = substr( type, 2 )
            }
        }

        # forced mode legal ?

        if ( !(mode in allowedMode) )
            return( FALSE )
    }

    # save opcode and data

    save_opcode( mode )
    save_data( type, expr )

    return( TRUE )
}

# conditionally save opcode and 8-bit data if mode is legal

local function save08c(mode, expr) {

    if ( mode in allowedMode )
        return( save_mode_expr(mode, "UBIT08", expr) )

    return( FALSE )
}

# save opcode and 16-bit data

local function save16(mode, expr) {

    return( save_mode_expr(mode, "UBIT16", expr) )
}

# save zero page test relative opcode and data

local function testrelative(expr1, expr2) {

    if ( save_mode_expr("zptr", "UBIT08", expr1) ) {
        save_data( "RBIT08", expr2 )
        return( TRUE )
    }

    return( FALSE )
}

# save block move opcode and data

local function blockmove(expr1, expr2) {

    if ( save_mode_expr("bmv", "UBIT08", "^" expr2) ) {
        save_data( "UBIT08", "^" expr1 )
        return( TRUE )
    }
    
    return( FALSE )
}

# -----------------------------

# try to resolve multiple possible address modes

local function resolve(abmode, zpmode, lmode, expr) {

    local val
    local mode, type

    # "abmode" is default (as per WDC standard)

    mode = abmode
    type = "UBIT16"

    # do we have a hint ?
    # - "abmode" can be forced to any legal mode, and
    # is guaranteed to be non-null (unlike the other two)

    if ( forceMode )
        return( save_mode_expr(mode, type, expr) )

    # try to evaluate expression

    val = EXPgetnum( expr )

    # if resolved we can select most appropriate size...

    if ( EXPgotnum(val) ) {

        # if there is a zero page mode and value is within zero page...

        if ( (zpmode != "") && \
                (val >= directPage) && (val < directPage + 256) ) {
            val -= directPage
            mode = zpmode
            type = "UBIT08"
        }

        # if there is a long mode...

        else if ( lmode != "" ) {

            # if value is within the data bank use absolute addressing...

            if ( and(val, 0xFFFF0000) == dataBank )
                val -= dataBank

            # ...otherwise use long addressing

            else {
                mode = lmode
                type = "UBIT24"
            }
        }

        # make sure result is int, not float
        # - TAWK 5 (in particular) can do that...

        val = int( val )
    }

    # store

    save_opcode( mode )
    CGsavedata( type, val )
    return( TRUE )
}

# -----------------------------

# check for address mode indirection
# - to be true indirection indicator, initial open parenthesis
# can be balanced only by terminal close parenthesis

local function isindirect(this) {

    local i
    local cnt, atzero
    local paren

    # no leading and trailing parentheses -> no indirection

    if ( this !~ /^\(.+\)$/ )
        return( FALSE )

    # we're just interested in parentheses

    cnt = splitp( this, paren, /\(|\)/ )

    # if balance is reached before end of expression, then not indirect

    atzero = 1
    for ( i = 2; i <= cnt; i++ ) {
        atzero += ( paren[i] == "(" ) ? 1 : -1
        if ( atzero == 0 )
            return( i == cnt )
    }

    # malformed (so not truly balanced, eh ?)

    return( FALSE )
}

# -------------------------------

# clip "(indirect,X|S)" expression to "indirect"

local function clipi(this) { return( substr(this, 2, rindex(this, ",")-2) ) }

# clip "[indirect]" expression to "indirect"

local function clipb(this) { return( substr(this, 2, length(this)-2) ) }

# -------------------------------

# zero-field expression
# - all: acc (implicit), imp

local function zero_expr() {

    if ( "acc" in allowedMode )
        return( save00("acc") )

    if ( "imp" in allowedMode )
        return( save00("imp") )

    # not legal

    return( FALSE )
}

# -------------------------------

# single-field expression
# - 6502:  ab, abi, acc (explicit), imm, pcr, zp, zpxi
# - 65c02, r65c02, w65c02s: abxi, zpi
# - w65c816s: lab, lpcr, lzpi

local function single_expr(expr) {

    # immediate ?

    if ( expr ~ /^#/ )
        return( ("imm" in allowedMode) ? immediate(expr) : FALSE )

    # accumulator (explicit) ?

    if ( expr ~ /^A$/i )
        return( ("acc" in allowedMode) ? save00("acc") : FALSE )

    # absolute or zero page indirect ?

    if ( isindirect(expr) ) {

        # x-indexed indirect ?

        if ( expr ~ /, *X *\)$/i ) {

            if ( "abxi" in allowedMode )
                return( save16("abxi", clipi(expr)) )

            return( save08c("zpxi", clipi(expr)) )
        }

        # indirect

        if ( "abi" in allowedMode )
            return( save16("abi", expr) )

        return( save08c("zpi", expr) )
    }

    # long ?

    if ( cpu16Bit ) {

        # long indirect ?

        if ( expr ~ /^\[.*\]$/ ) {

            return( save08c("lzpi", clipb(expr)) )
        }

        # long pc relative ?

        if ( "lpcr" in allowedMode )
            return( save_mode_expr("lpcr", "RBIT16", expr) )

        # long absolute ?

        if ( "lab" in allowedMode ) {

            # "ab" and "zp" and "lab" are not mutually exclusive

            if ( "zp" in allowedMode )
                return( resolve("ab", "zp", "lab", expr) )

            if ( "ab" in allowedMode )
                return( resolve("ab", "", "lab", expr) )

            return( save_mode_expr("lab", "UBIT24", expr) )
        }
    }

    # pc relative ?

    if ( "pcr" in allowedMode )
        return( save_mode_expr("pcr", "RBIT08", expr) )

    # absolute ?

    if ( "ab" in allowedMode ) {

        # "ab" and "zp" are not mutually exclusive

        if ( "zp" in allowedMode )
            return( resolve("ab", "zp", "", expr) )

        return( save16("ab", expr) )
    }

    # zero page

    return( save08c("zp", expr) )
}

# -------------------------------

# double-field expression
# - 6502, 65c02: abx, aby, zpx, zpiy, zpy 
# - r65c02, w65c02s: zptr
# - w65c816s: bmv, labx, lzpiy, sr, sriy

local function double_expr(expr1, expr2) {

    # y-indexed ?

    if ( expr2 ~ /^Y$/i ) {

        # zero page or stack relative indirect ?            

        if ( isindirect(expr1) ) {

            # stack-relative ?

            if ( expr1 ~ /, *S *\)$/i )
                return( save08c("sriy", clipi(expr1)) )

            # zero-page

            return( save08c("zpiy", expr1) )
        }

        # long indirect ?

        if ( expr1 ~ /^\[.*\]$/ )
            return( save08c("lzpiy", clipb(expr1)) )

        # absolute ?

        if ( "aby" in allowedMode ) {

            # "aby" and "zpy" are not mutually exclusive

            if ( "zpy" in allowedMode )
                return( resolve("aby", "zpy", "", expr1) )
             
            return( save16("aby", expr1) )
        }

        # zero page

        return( save08c("zpy", expr1) )
    }

    # x-indexed ?

    if ( expr2 ~ /^X$/i ) {

        if ( "labx" in allowedMode )
            return( resolve("abx", "zpx", "labx", expr1) )

        if ( "abx" in allowedMode )
            return( resolve("abx", "zpx", "", expr1) )

        return( save08c("zpx", expr1) )
    }

    # stack relative ?

    if ( expr2 ~ /^S$/i )
        return( save08c("sr", expr1) )

    # block move ?

    if ( "bmv" in allowedMode )
        return( blockmove(expr1, expr2) )

    # zero page test relative ?

    if ( "zptr" in allowedMode )
        return( testrelative(expr1, expr2) )

    # not legal

    return( FALSE )
}

# -------------------------------

# process instruction mnemonic

global function INSdoop(mnemonic, cnt, expr) {

    local legalmode

    currMnemonic = mnemonic
    allowedMode = opcodeSet[ mnemonic ]

    if ( !cnt )
        legalmode = zero_expr()
    else if ( cnt == 1 )
        legalmode = single_expr( expr[1] )
    else if ( cnt == 2 )
        legalmode = double_expr( expr[1], expr[2] )
    else
        legalmode = FALSE

    forceMode = ""

    return( legalmode ? "" : "BadMode" )
}

# -------------------------------

# get ASSUME byte value

local function getbyteval(expr, current, offset) {

    local val

    val = EXPgetinrange( expr, 0x00, 0xff )
    return( EXPgotnum(val) ? val * offset : current )
}

# get ASSUME (almost) word value

local function getwordval(expr, current) {

    local val

    val = EXPgetinrange( expr, 0x0000, 0xff00 )
    return( EXPgotnum(val) ? val : current )
}

# get index or accumulator size flag

local function getsizeflag(expr, current) {

    local val

    val = EXPgetinrange( expr, 8, 16 )
    if ( EXPgotnum(val) ) {
        if ( val == 8 )
            return( FALSE  )
        if ( val == 16 )
            return( TRUE )
        UMerror( "BadValue", CKfmtval(val) )
    }

    return( current )
}

# handle "ASSUME" psop

global function INSdoassume(cmd, arg) {

    if ( cmd == "addr" ) {
        if ( arg == "absolute" )
            forceMode = "ab"
        else if ( (arg == "zeropage") || (cpu16Bit && arg == "direct") )
            forceMode = "zp"
        else if ( cpu16Bit && arg == "long" )
            forceMode = "l"
        else
            return( FALSE )
    }

    else if ( cmd == "zeropage" )
        directPage = getbyteval( arg, directPage, 0x100 )

    else if ( !cpu16Bit )
        return( FALSE )

    else if ( cmd == "index" )
        ndx16Bit = getsizeflag( arg, ndx16Bit )

    else if ( cmd == "accum" )
        acc16Bit = getsizeflag( arg, acc16Bit )

    else if ( cmd == "directpage" )
        directPage = getwordval( arg, directPage )

    else if ( cmd == "databank" )
        dataBank = getbyteval( arg, dataBank, 0x10000 )

    else
        return( FALSE )

    # handled (error or not)
            
    return( TRUE )
}
