# Address Mode Test Generator for HXA65

# by Anton Treuenfels

# for Thompson AWK 4.0

# first created: 02/19/05
# last revision: 07/12/07

# --------------------------

# what this program does:
# - scans a "proc*.a" test file, which tests all legal instructions in
# all legal address modes
# - determines instruction groups, which are defined as instructions
# that share the same set of address modes (one member of the group
# is selected to be the "group representative")
# - generates a "proc*e.a" test program, which tests all instruction
# groups against all address modes to verify that illegal address
# modes are caught (which they are not, actually)
# - generates a "proc*f.a" test program, which catches some illegal
# address modes that make it past the first pass when they trigger
# range violations in the second (all the remaining "uncaught" exceptions
# are essentially "promoted" by HXA65 to legal modes of the instruction
# group in question based on examination of the associated expression)
# - generates a "proc*.b" test program, which tests the ability to
# legally force particular address modes on instruction groups other than
# what HXA might normally choose (and if it would have chosen them anyway,
# removes the range restrictions that would have imposed)
# - generates a "proc*.g" test program, which tests all the illegal
# forced address modes possible (the inverse of the "proc*b.a" test)
# to see if HXA catches them all

# The expected error counts of the "e", "f" and "g" tests are written at
# the end of their respective files and need to be manually copied to the
# "Error(s):" line near the start where the automated check program
# expects to find them.

# the genesis of this program was the realization that doing this by hand
# was not only tedious but also prone to overlooking all kinds of legal
# and illegal combinations
# - an earlier program, FORCE.AWK, was a less ambitious version of this
# program, generating only the "b" and "g" tests

# --------------------------

local legalMode

local currIns

local notLegal

local cpu16Bit

# --------------------------

local FALSE = 0
local TRUE  = 1

# --------------------------

local currMode

local insMode, modeSet, exprSet

local groupRep, groupMem

local baseName, groupName
local cpuLine, orgLine

local badAccepted

local outFile

# --------------------------

local function error(text) { print "Error: " text > "CON" }

# --------------------------

# make output filename

local function makefname(suffix) {

    return( substr(FILENAME, 1, length(FILENAME)-2) suffix )
}

# send to output file

local function write(this) { print this > outFile }
local function write_tb(this) { print "\t\t" this > outFile }
local function write_nl()  { printf( "\n" ) > outFile }

# -------------------------------

BEGINFILE {

    if ( FILENAME !~ /^proc[0-9]{3}\.a$/i ) {
        error( "Bad filename - " FILENAME )
        abort()
    }
}

# -------------------------------

# instruction test line ?
# - ins [expr] [";" mode [extension]]

$1 ~ /^[A-Z][A-Z][A-Z][0-9]?$/ {

    local i
    local expr

    # is this a new mode ?

    if ( index($0, ";") ) {
        if ( $3 == ";" ) {          # most instructions
            currMode = $4
            expr = $2
        }
        else if ( $2 == ";" ) {     # implied or accumulator (implicit)
            currMode = $3
            expr = " "
        }
        else if ( $4 == ";" ) {     # two argument mnemonics
            currMode = $5
            expr = $2 " " $3
        }
        else {
            error( "Can't find mode - " $0 )
            next
        }

        # save the instruction mnemonic and the mode
        # - saves only first use of each mode

        insMode[ $1 ][ currMode ] = ".T."

        # if new mode, save it

        if ( !(currMode in modeSet) )
            modeSet[ currMode ] = ".T."
    }

    # it may be a new expression

    else {

        expr = $2
        for ( i = 3; i <= NF; i++ )
            expr = expr " " $i
    }

    # if a new expression, save it

    if ( !(expr in exprSet[currMode]) )
        exprSet[ currMode ][ expr ] = ".T."

    next
}

$1 == ".cpu" { cpuLine = $0 }

$1 == ".org" { orgLine = $0 }

FNR == 1 { baseName = $0 }

FNR == 2 { groupName = $0 }

# -------------------------------

# check if we're dealing with a 16-bit processor

local function flag16bit() {

    local i

    # set flag

    cpu16Bit = index( groupName, "16" )

    # if we are, add "extra" hidden address mode not found in processor test
    # - it's there just to make long force mode easy

    if ( cpu16Bit ) {
        insMode[ "JML" ][ "labi" ] = ".T."
        insMode[ "JMP" ][ "labi" ] = ".T."
        modeSet[ "labi" ] = ".T."
        for ( i in exprSet["abi"] )
            exprSet[ "labi" ][ i ] = ".T."
    }
}      

# determine which instructions have same address modes
# - we only need to test one of each group (the representative)

local function find_group_reps() {

    local ins, rep, mode
    local insrep, is_rep
    local i_allowed, r_allowed

    for ( ins in insMode ) {

        insrep = ""
        i_allowed = insMode[ ins ]
        for ( rep in groupRep ) {
            r_allowed = groupRep[ rep ]
            if ( length(i_allowed) != length(r_allowed) )
                continue
            is_rep = 1
            for ( mode in i_allowed ) {
                if ( !(mode in r_allowed) ) {
                    is_rep = 0
                    break
                }
            }

            if ( is_rep ) {
                insrep = rep
                break
            }
        }

        if ( insrep == "" ) {
            groupRep[ ins ] = i_allowed
            insrep = ins
        }

        groupMem[ insrep ][ ins ] = ".T."
    }
}

# -------------------------------

# write set of checked instructions

local function write_members(ins) {

    write_tb( "; check:" )
    for ( i in groupMem[ins] )
        write_tb( "; " i )
    write_nl()
}

# make an instruction line

local function make_ins(ins, expr, mode) {

    local text

    text = ins "\t\t" expr
    return( sprintf("%-16s ; %s", text, mode) )
}

# -------------------------------

# can an instruction be accepted as a different mode ?
# - may still be rejected as out of range during the second pass

local function accept_alt(mode, allowed) {

    if ( mode ~ /^ab/ ) {
        if ( mode == "ab" ) {
            if ("pcr" in allowed )
               return( "pcr" )
            if ( "lpcr" in allowed )
               return( "lpcr" )
        }
        if ( mode == "abi" )
            return( "zpi" in allowed ? "zpi" : "labi" )
        if ( "l" mode in allowed )
            return( "l" mode )
        return( "zp" substr(mode, 3) )
    }

    if ( mode == "acc_i" )
        return( "imp" )

    if ( mode == "imp" )
        return( "acc_i" )

    if ( mode ~ /^lab/ ) {
        if ( mode == "lab" ) {
            if ( "pcr" in allowed )
                return( "pcr" )
            if ( "lpcr" in allowed )
                return( "lpcr" )
        }
        if ( substr(mode, 2) in allowed )
            return( substr(mode, 2) )
        return( "zp" substr(mode, 4) )
    }

    if ( mode ~ /l?pcr/ ) {
        if ( "pcr" in allowed )
            return( "pcr" )
        if ( "lpcr" in allowed )
            return( "lpcr" )
        if ( "lab" in allowed )
            return( "lab" )
        return( ("ab" in allowed) ? "ab" : "zp" )
    }

    if ( mode ~ /^zp/ ) {
        if ( mode == "zp" ) {
            if ("pcr" in allowed )
               return( "pcr" )
            if ( "lpcr" in allowed )
               return( "lpcr" )
        }
        if ( mode == "zpi" )
            return( "abi" )
        if ( "l" mode in allowed )
            return( "l" mode )
        if ( "lab" substr(mode, 3) in allowed )
            return( "lab" substr(mode, 3) )
        return( "ab" substr(mode, 3) )
    }

    return( "none" )
}

# determine if an illegal mode that was accepted in
# pass one should still be accepted in pass two
# - based on whether or not the expression result is in-range

local function still_accept(mode, altmode) {

    if ( (mode ~ /^ab/) && (altmode ~ /^zp/) )
        return( FALSE )
    if ( (mode ~ /^lab/) && (altmode ~ /^(ab|l?zp|l?pcr)/) )
        return( FALSE )
    if ( (mode ~ /^l?pcr/) && (altmode ~ /^zp/) )
        return( FALSE )
    if ( (mode ~ /^(ab|zp)/) && (altmode == "pcr") )
        return( FALSE )

    return( TRUE )
}


# write illegal address modes test 1

local function write_badmodes1_test() {

    local ins, mode, expr
    local wrt_ins, wrt_mode, err_cnt
    local altmode

    err_cnt = 0
    for ( ins in groupRep ) {
        write_members( ins )
        write( " -" )                   # backward branch target
        for ( mode in modeSet ) {
            wrt_mode = mode
            if ( mode in groupRep[ins] )
                wrt_ins = toupper( ins )
            else {
                wrt_ins = tolower( ins )
                altmode = accept_alt( mode, groupRep[ins] )
                if ( altmode in groupRep[ins] ) {
                    wrt_mode = wrt_mode "\t(accepted as '" altmode "' mode)"
                    badAccepted[ ins ][ mode ] = altmode
                }
                else
                    err_cnt += length( exprSet[mode] )
            }

            # write the test(s)

            for ( expr in exprSet[mode] )
                write_tb( make_ins(wrt_ins, expr, wrt_mode) )
        }
        write( " +" )                   # forward branch target
        write_nl()
    }
    write( "; errs = " err_cnt )
    write_nl()
}

# -------------------------------

# write illegal address modes test 2

local function write_badmodes2_test() {

    local ins, mode, expr
    local wrt_mode, err_cnt
    local altmode

    err_cnt = 0
    for ( ins in badAccepted ) {
        write_members( ins )
        write( " -" )                   # backward branch target
        for ( mode in badAccepted[ins] ) {
            altmode = badAccepted[ ins ][ mode ]
            if ( still_accept(mode, altmode) )
                wrt_mode = mode "\t(accepted as '" altmode "' mode)"
            else {
                wrt_mode = mode "\t(rejected)"
                err_cnt += length( exprSet[mode] )
            }

            # write the test(s)

            for ( expr in exprSet[mode] )
                write_tb( make_ins(tolower(ins), expr, wrt_mode) )
        }
        write( " +" )                   # forward branch target
        write_nl()
    }

    write( "; errs = " err_cnt )
    write_nl()
}

# -------------------------------

# determine if forced mode is legal
# - can only force to another mode that is legal for the instruction,
# - this is (and should always remain) the same as the test in
# "A_INS65X.AWK"
# - except instead of TRUE/FALSE it returns NEWMODE/NULL

local function legal_forced(mode, forceMode, allowedMode) {

    local type                  # new

    # forced mode active ?

     if ( forceMode ) {
    
        # - basically only zero page, absolute, long zero page and
        # long absolute modes can possibly be forced to another mode

        if ( !match(mode, /ab|zp/) )
#            return( FALSE )
            return( "" )        # new

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
#            return( FALSE )
            return( "" )        # new
        
    }

    # save opcode and data

#    save_opcode( mode )
#    save_data( type, expr )

#    return( TRUE )

    return( mode )              # new
}

# determine if a forced mode instruction will be accepted

local function accept_force(ins, mode, forcemode) {

    local allowed

    allowed = groupRep[ ins ]

    # a legal starting mode ?
    # - if not check if could be taken for another mode

    if ( !(mode in allowed) ) {
        mode = accept_alt( mode, allowed )
        if ( !(mode in allowed) )
            return( "" )
    }

    # is the force legal ?

    return( legal_forced(mode, forcemode, allowed) )
}

# write a forced mode instruction

local function write_force(ins, mode, forcetype, forcemode) {

    local expr

    if ( (forcetype == "zeropage") && cpu16Bit )
        forcetype = "direct"

    for ( expr in exprSet[mode] ) {
        write_tb( "assume\taddr:" forcetype )
        write_tb( make_ins(ins, expr, mode) " -> " forcemode )
    }
}

# write legal forced modes test

local function write_legal_forced_test() {

    local ins, mode
    local lforce, abforce, zpforce

    lforce = abforce = zpforce = ""
    for ( ins in groupRep ) {
        write_members( ins )
        write( " -" )                   # backward branch target
        for ( mode in modeSet ) {
            if ( cpu16Bit )
                lforce = accept_force( ins, mode, "l" )
            abforce = accept_force( ins, mode, "ab" )
            zpforce = accept_force( ins, mode, "zp" )
            if ( lforce || abforce || zpforce ) {
                write_tb( "; " mode )
                write_nl()
                if ( lforce )
                    write_force( tolower(ins), mode, "long", lforce )
                if ( abforce )
                    write_force( tolower(ins), mode, "absolute", abforce )
                if ( zpforce )
                    write_force( tolower(ins), mode, "zeropage", zpforce )
                write_nl()
            }
        }
        write( " +" )                   # forward branch target
        write_nl()
    }
}

# write illegal forced modes test

local function write_illegal_forced_test() {

    local ins, mode
    local lforce, abforce, zpforce
    local err_cnt

    err_cnt = 0
    lforce = abforce = zpforce = ".T."
    for ( ins in groupRep ) {
        write_members( ins )
        write( " -" )                   # backward branch target
        for ( mode in modeSet ) {
            if ( cpu16Bit )
                lforce = accept_force( ins, mode, "l" )
            abforce = accept_force( ins, mode, "ab" )
            zpforce = accept_force( ins, mode, "zp" )
            if ( !lforce || !abforce || !zpforce ) {
                write_tb( "; " mode )
                write_nl()
                if ( !lforce ) {
                    write_force( tolower(ins), mode, "long", "l*" )
                    err_cnt += length( exprSet[mode] )
                }
                if ( !abforce ) {
                    write_force( tolower(ins), mode, "absolute", "ab*" )
                    err_cnt += length( exprSet[mode] )
                }
                if ( !zpforce )  {
                    write_force( tolower(ins), mode, "zeropage", "zp*" )
                    err_cnt += length( exprSet[mode] )
                }
                write_nl()
            }
        }
        write( " +" )                   # forward branch target
        write_nl()
    }

    write( "; errs = " err_cnt )
    write_nl()
}

# -------------------------------

# write divider

local function write_div() {

    write_nl()
    write( "; -------------------------------" )
    write_nl()
}

# write generator name

local function write_gen() {

    write_nl()
    write( "; generated by MODE65.AWK - " ctime() )
    write_nl()
}

# write test file header

local function write_header(testid, desc) {

    local errtest
    local ndx

    errtest = testid >= "e"

    if ( errtest ) {
        ndx = index( baseName, "Test" )
        write( substr(baseName, 1, ndx-1) "Error " substr(baseName, ndx) testid )
        write( groupName )
        write( "; error: " desc )
        write_gen()
        write( "; warning(s):" )
        write_nl()
        write( "; error(s):" )
        write( testid == "f" ? "; - out of range" : "; - illegal address mode" )
        write_nl()
        write( "; fatal:" )
    }
    else {
        write( baseName testid )
        write( groupName )
        write( "; ok: " desc )
        write_gen()
        write( "; no errors to detect" )
    }

    write_div()
    write_tb( ".listfile" )
    write_tb( ".errfile" )
    if ( errtest )
        write_tb( ".maxerr\t5000\t\t; increase default count" )
    write_div()
    write( cpuLine )
    write( orgLine )
    write_div()
    write( "; backward reference" )
    write_nl()
    write( "bakbyt\t.equ\t$12" )
    write( "bakwrd\t.equ\t$1234" )
    if ( cpu16Bit ) {
        write( "bak24\t.equ\t$123456" )
        write( "bak24b\t.equ\t$789ABC" )
    }
    write_nl()
}

# write test file trailer

local function write_trailer() {

    write( "; forward reference" )
    write_nl()
    write( "fwdbyt\t.equ\t$12" )
    write( "fwdwrd\t.equ\t$1234" )
    if ( cpu16Bit ) {
        write( "fwd24\t.equ\t$123456" )
        write( "fwd24b\t.equ\t$789ABC" )
    }
    write_div()
    write_tb( ".end" )
}

END {

    # check 16-bit processor

    flag16bit()

    # find single instructions that represent groups with
    # the same instruction modes
    # - rep is first alphabetically in group

    find_group_reps()

    # write illegal address modes 1

    outFile = makefname( "e.a" )

    write_header( "e", "illegal address modes, caught first pass" )
    write_badmodes1_test()
    write_trailer()

    close( outFile )

    # write illegal address modes 2

    outFile = makefname( "f.a" )

    write_header( "f", "illegal address modes, caught second pass" )
    write_badmodes2_test()
    write_trailer()

    close( outFile )

    # write legal forced modes test file

    outFile = makefname( "b.a" )

    write_header( "b", "forced address modes" )
    write_legal_forced_test()
    write_trailer()

    close( outFile )

    # write illegal forced modes test file

    outFile = makefname( "g.a" )

    write_header( "g", "illegal forced address modes" )
    write_illegal_forced_test()
    write_trailer()

    close( outFile )
}
