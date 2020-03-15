# a script to examine the output of an HXA test

# for Thompson AWK 4.0

# by Anton Treuenfels

# first created: 04/22/04
# last revision: 03/14/10

# ------------------

local testName

local pcBits

local warnCnt, errCnt, fatalCnt

local warnFnd, errFnd, fatalFnd

local badTest

local outFile = "testchk.txt"

# ------------------

local function out(this) { print this >> outFile }
local function blankline() { print "" >> outFile }

local function bad(mesg) {

    out( "Error: " mesg )
    badTest += 1
}

local function should() { bad( "*Should* have been" ) }
local function shouldnot() { bad( "Should *not* have been" ) }

# ------------------

# if field is numeric, take value
# - otherwise number not specified; assume value is zero

local function typecount(this) {

    return( this ~ /^[0-9]/ ? this + 0 : 0 )
}

# get file output name
# - usually based on test name, but occasionally not

local function outputname(type) {

    local i
    local pat
    local name

    type = tolower( type )

    # can only set once

    if ( type in testName )
        return

    # what field is psop ?

    pat = regex( type, "i" )       
    for ( i = 1; $i !~ pat; i++ )
        ;

    # default name ?
    # - which we also use if specified name is not a literal (ie., punting)

    if ( (i == NF) || ($(i+1) !~ /^".+"$|^<.+>$/) ) {

        # single output filename based on source filename

        if ( !index(type, "by") )
            name = FILENAME

        # multiple output filenames based on segment names
        # - we always use this name as first segment name for output
        # - although there should be more than one, this is all we'll check

        else
            name = "ABS1"
    }

    # specified name or template

    else {
        name = $(i+1)
        name = substr( name, 2, length(name)-2 )
    }

    testName[ type ] = name
}

# change filename extension

local function replaceext(name, ext) {

    local field

    split( name, field, "." )
    return( field[1] "." ext )
}

# check if an output file of a given type exists

local function fexists(type, ext) {

    local fname, asname
    local size

    if ( !(type in testName) )
        return( "" )

    fname = testName[ type ]

    # default output name ?

    if ( fname == FILENAME ) {
        fname = replaceext( FILENAME, ext )
        asname = ""
    }

    # specified name or template

    else {

        # name contains template ?

        if ( index(fname, "%") ) {

            # we will check for only the first of multiple output files

            fname = sprintf( fname, 1 )
        }

        # name is first segment name ?

        else if ( fname == "ABS1" )
            fname = replaceext( fname, ext )

        asname = " (as " toupper(fname) ")"
    }

    size = filesize( fname )
    if ( size >= 0 ) {
        s = sprintf("%s size=%d/$%02X%s", toupper(type), size, size, asname)
        out( s )
        return( fname )
    }

    out( toupper(type) " not found" asname )
    return( "" )
}

# check an error count

local function checkcount(type, should, does) {

    if ( should != does )
        bad( type " count should= " should " does= " does )
}

# ------------------

# save filename and check test type

BEGINFILE {

    delete( testName )

    badTest = pcBits = 0
    warnCnt = errCnt = fatalCnt = 0
    warnFnd = errFnd = fatalFnd = 0
}

# first we determine what *should* have happened
# - which isn't much if it's an "ok" file !

# we don't care for most lines

FNR < 6 { next }

$0 ~ /^[ \t]*$/ { next }

# get error type counts

NF == 3 && $2 ~ /warn/i { warnCnt = typecount($NF) }

NF == 3 && $2 ~ /error/i { errCnt = typecount($NF) }

NF == 3 && $2 ~ /fatal/i { fatalCnt = typecount($NF) }

# get output filenames

/listfile/i { outputname( "listfile" ) }

/errfile/i { outputname( "errfile" ) }

/objfile/i { outputname( "objfile" ) }

/hexfile/i { outputname( "hexfile" ) }

/srecfile/i {

    local effective

    if ( $0 !~ /assume/i )
        outputname( "srecfile" )

    else if ( match($0, /s19|s28|s37/i) ) {
        effective = substr( $0, RSTART, 3 )
        if ( effective == "s19" )
            pcBits = 16
        else if ( effective == "s28" )
            pcBits = 24
        else
            pcBits = 32
    }
}

/objbyseg/i { outputname( "objbyseg" ) }

/hexbyseg/i { outputname( "hexbyseg" ) }

/srecbyseg/i { outputname( "srecbyseg" ) }

/objbyblock/i { outputname( "objbyblock" ) }

/hexbyblock/i { outputname( "hexbyblock" ) }

/srecbyblock/i { outputname( "srecbyblock" ) }

# check for program counter size

/t_[0-9][0-9]_[lm]/i {

    match( $0, /_[0-9][0-9]_/ )

    pcBits = substr( $0, RSTART+1, 2 ) + 0
}

# we've tried to arrange things so that the last important parameter
# of each test is the size of the program counter

pcBits { exit }

# ------------------

# for for existence or non-existence of a given output file

local function checkfor(type, ext) {

    local fname
    local expected

    fname = fexists( type, ext )
    expected = (type in testName) && !errCnt && !fatalCnt

    # found file ?

    if ( fname && !expected )
        shouldnot()

    # didn't find file

    else if ( !fname && expected )
        should()     
}

# now check what should have happened against what *did* happen

END {

    local fname, ext

    # test header in output file

    out( "# ------------------" )
    out( "Results for " FILENAME )
    blankline()

    # is there a LIST file ?
    # - always should be if no errors (because always specified to be one)
    # - should never be if any errors (because then no list output)

    checkfor( "listfile", "lst" )

    # is there an ERROR file ?
    # - if so, it still might consist of just ECHO output,
    # so we have to look more closely

    if ( (fname = fexists("errfile", "err")) ) {

        # examine error report

        fopen( fname, "rt" )
        while ( (getline < fname) > 0 ) {
            if ( $1 == ";" ) {
                if ( $2 == "-" ) {
                    if ( $0 ~ /expanded/i )
                        bad( "Bad error message index" )
                }
                else if ( $0 ~ /[0-9]:[ \t]*$/ )
                    bad( "Source line missing" )
            }
            else if ( $1 ~ /^[0-9]/ ) {
                if ( $2 ~ /warn/i )
                    warnFnd += $1
                else if ( $2 ~ /error/i )
                    errFnd += $1
                else if ( $2 ~ /fatal/i )
                    fatalFnd += $1
            }
        }
        close( fname )
    }

    # should there be an ERROR file ?
    # - doesn't catch case where ECHO output should have created one
    # even in the absence of actual errors, though

    else if ( ("errfile" in testName) && (warnCnt || errCnt || fatalCnt) )
        should()
    
    # regarding errors, make sure what we found is what we expected

    checkcount( "Warn", warnCnt, warnFnd )
    checkcount( "Error", errCnt, errFnd )
    checkcount( "Fatal", fatalCnt, fatalFnd )

    # set extension for SREC file

    if ( pcBits == 32 )
        ext = "s37"
    else if ( pcBits == 24 )
        ext = "s28"
    else
        ext = "s19"
            
    # check for OBJ, HEX and SREC files

    checkfor( "objfile", "obj" )
    checkfor( "hexfile", "hex" )
    checkfor( "srecfile", ext  )

    # check for --BYSEG files

    checkfor( "objbyseg", "obj" )
    checkfor( "hexbyseg", "hex" )
    checkfor( "srecbyseg", ext )

    # check for --BYBLOCK files

    checkfor( "objbyblock", "obj" )
    checkfor( "hexbyblock", "hex" )
    checkfor( "srecbyblock", ext )

    blankline()

    if ( !badTest )
        out( "PASSED" )
    else
        out( "*FAILED*" )

    blankline()
}
