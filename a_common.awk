# Hobby Cross-Assembler (HXA) V0.201 - Miscellaneous Functions and Values

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

# source language: Thompson AWK 4.0

# first created: 01/24/03
# last revision: 09/17/13

# public function prefix: "CK"

# - global constants and variables can be freely read by any other module
# - global variables can only be written by using functions provided here

# ----------------------------
# globally available constants
# ----------------------------

# version id's

# one decimal digit per nybble - lowest three nybbles are for minor
# version numbers and bug fix indicators
# - and no, we're not likely to get 99,999 major version numbers :)

global function CKvernum() { return( 0x00000201 ) }
global function CKverstr() { return( INSname() " v0.201" ) }

# generally for inter-module failure-related function returns

global CKfail           # type -> "uninitialized" (deliberately)

# everyone should know and agree on the meaning of these !

global TRUE  = 1
global FALSE = 0

# blank or comment line (ignored)
# - if first column is "*" or first non-space is ";", it's a comment line

global CKignoreLine = /^([ \t]*(;|$)|\*)/

# field separators

global CKfieldSep = ","     # default (could be changed, though not so far)
local fsStopChar

# character, string and regular expression literal operand patterns
# - basic pattern is delimiter, (1) any single char except delimiter or
# backslash or (2) backslash plus next char as a pair, delimiter

global CKcharLitToken  = /^'([^'\\]|\\.)+'/
global CKstrLitToken   = /^"([^"\\]|\\.)*"/
global CKregexLitToken = /^\/([^/\\]|\\.)+\/i?/

# char and string literal escape codes

local allEscPat = /\\(.|(\$|0?X)[0-9A-F]+|([0-9]|0[A-F])[0-9A-F]*H)/i

# regular expression literal escape codes
# - we don't match parts TAWK's regex conversion will handle itself

local regexEscPat = /\\(s|(\$|0X)[0-9A-F]+|([0-9]|0[A-F])[0-9A-F]*H)/i

# opt-hex string patterns
# - radix indicator optional; body must consist of hex char pairs
# - leading zero not required for "-H" suffix pattern

global CKoptHexPat = /^(\$|0?X)?([0-9A-F][0-9A-F])+$|^([0-9A-F][0-9A-F])+H$/i

# output file types ( "--FILE" and "--BYSEG" )
# - if index exists, that type should be output
# - if name at index is null, use default name
# - otherwise use name as-is ("--FILE") or as template ("--BYSEG")

local outputType

# -------------------
# variables
# -------------------

# current local label name scope depth

local scopeDepth = 0

# assembler internal limit values array

local iMax

# user-named and internal pass timers array

local timerStart, timerStop

# character conversion array

local convertChar

# source directory path

local srcPath

# -------------------

# populate an internal array from an "equality" string
# - an initialization function

global function CKpopulate(array, data) {

    local i, j
    local equate, qndx, val
    local evalue, datndx

    i = split( data, evalue, " " )
    do {
        equate = evalue[ i ]
        qndx = rindex( equate, "=" )
        val = substr( equate, qndx+1 )
        j = split( substr(equate,1,qndx-1), datndx, "+" )
        do {
            array[ datndx[j] ] = val
        } while ( --j )
    } while ( --i )
}

# populate an internal array index with a constant value
# - often we care only that index exists, so there is a default value
# - an initialization function

global function CKpopulateNdx(array, data, constval) {

    local i
    local ndx
    local value

    value = argcount() > 2 ? constval : ".T."
    i = split( data, ndx, " " )
    do {
        array[ ndx[i] ] = value
    } while ( --i )
}

# -------------------

INIT {

    local i

    # default char conversion is to self (ASCII)

    for ( i = 0; i < 256; i++ )
        convertChar[ i ] = i

    # field split scan stop characters

    fsStopChar[ "," ] = /[,"'\\\(\/]/
    fsStopChar[ "=" ] = /[="'\\]/
    fsStopChar[ ":" ] = /[:"'\\]/

    # lowest level of block structure stack is "dummy"
    # - so last actual block structure needs no special treatment

    CKpushblock( dummyBlk )

    # - this is left over from when we needed to retain all significant
    # digits when a pc value was used as an array index (ie., when ordering
    # segments for output) - but we don't do that any more
    # - TAWK's default value ("%.6g") isn't good enough to retain up
    # to the 32 bits necessary (about 10 decimal digits)
    # - setting CONVFMT makes this happen globally automatically;
    # it also affects any float->string conversion which is not part
    # of an output statement (eg., "print")

    CONVFMT = "%.10g"

   # initial maximum scope nesting and user stack depths

    CKdomax( "MAXDEPTH", 128 )
    CKdomax( "MAXSTACK", 128 )
}

# -------------------

# convert hex string to numeric value

global function CKhextonum(this) { return(("0x" this) + 0) }

# ------------------

# check if arg is fail value or not

global function CKok(this) { return( typeof(this) != "uninitialized" ) }

# ------------------

# return count of name references, else default "not referenced" string

global function CKgetuse(name, count) {

    if ( (name in count) && (count[name] > 0) )
        return( count[name] )

    return( "-" )
}

# ------------------

local unPrintable = /[^\x20-\x7E]/

# make sure output string contains only printable characters

global function CKprintable(this) {

    local ch

    # convert tabs to spaces, then convert remaining non-printables

    if ( this ) {
        this = translate( this, "\t", " " )
        if ( match(this, unPrintable) ) {
            do {
                ch = substr( this, RSTART, 1 )
                substr( this, RSTART, 1 ) = sprintf( "\\x%02X", ord(ch) )
            } while ( match(this, unPrintable, RSTART+4) )
        }
    }

    return( this )
}

# ------------------

# format value for display

global function CKfmtval(this) {

    local thistype

    thistype = typeof( this )

    # string ?
    # - null strings rendered as a pair of quotes

    if ( thistype == "string" )
        return( this ? CKprintable(this) : "\"\"" )

    # not a number ?

    if ( thistype != "int" && thistype != "float" )
        return( "[ =" thistype "]" )

    # integer value ?
    # - can be in floating point form

    if ( (this > 0x80000000) && (this < 0x7fffffff) )
        return( sprintf("%d ($%02X)", this, this) )

    # float outside integer range

    return( sprintf("%.14g", this) )
}

# perform range check

global function CKinrange(val, min, max) {

    if ( val < min )
        UMerror( "BadRngLo", CKfmtval(val) " < " CKfmtval(min) )
    else if ( val > max )
        UMerror( "BadRngHi", CKfmtval(val) " > " CKfmtval(max) )
    else
        return( TRUE )

    return( FALSE )
}

# perform range check of internal limit value
# - "type" is the index in iMax[] of the value to check
# - "type" is also the error message table index if over limit value

global function CKcheckmax(type, curr) {

    if ( curr > iMax[type] )
        UMfatal( type, CKfmtval(curr) " > " CKfmtval(iMax[type]) )
}

# handle "MAX--" psop
# - MAX-- num_expr
# - note psop name is used as the array index
# - also used to set the default initial value at startup time

global function CKdomax(type, val) { iMax[ type ] = val  }

# -------------------

# simple tag/value checks (debugging aids)
# - commented out for release versions (saves a tiny bit of space !)

global function CKshow(tag, val) {

    print tag "= " CKfmtval(val) > "check.txt"
}

global function CKshowa(tag, this) {

    local i

    CKshow( "Array " tag, length(this) )
    for ( i in this )
        CKshow( tag "[" i "]", this[i] )
}

# -------------------

# check if new value matches old value

global function CKsameval(new, old) {

    if ( new == old )
        return( TRUE )

    UMwarn( "UniqVal", CKfmtval(old) )
    return( FALSE )
}

# verify set-once-only arg is unique
# - after first time, only identical use is ok
# - TRUE and FALSE just indicate first use, not error condition

global function CKunique(val, ndx, array) {

    if ( !(ndx in array) ) {
        array[ ndx ] = val
        return( TRUE )
    }

    CKsameval( val, array[ndx] )
    return( FALSE )
}

# -------------------

# check if global name matches built-in reserved name

global function CKisreserved(this) {

    return( SYMisreserved(this) || PSOPispseudo(this) || INSisop(this) ) 
}

# verify global name is unique

global function CKgoodname(this) {

    # all current callers have done this already

#    this = toupper( this )

    if ( this !~ SYMglobal ) {
        UMerror( "NeedGlb", this )
        return( FALSE )
    }

    # a built-in name ?

    if ( CKisreserved(this) ) {
        UMreserved( this )
        return( FALSE )
    }

    # already user-defined ?

    if ( SYMexists(this) || MACismacro(this) ) {
        UMdupname( this )
        return( FALSE )
    }

    return( TRUE )
}

# verify names match

global function CKmatch(name_p, name_e) {

    if ( name_p == name_e )
        return( TRUE )

    UMerror( "MatchName", name_e )
    return( FALSE )
}

# -------------------

# convert any literal escape codes to single chars

local function doesc(this, escpat) {

    local rs, rl
    local ch

    # is there an escape char ?

    if ( !index(this, "\\") )
        return( this )

    # convert all codes present (but not any "created" escape sequences)
    # - won't match escape which is last char of input

    rs = 1
    while ( match(this, escpat, rs) )  {

        ch = substr( this, RSTART+1, RLENGTH-1 )
        rs = RSTART
        rl = RLENGTH

        # single char escape ?

        if ( length(ch) == 1 )

            # ...mnemonic escape ?
            # - if no match for translation, literal escape (no change)

            ch = translate( ch, "0bfnrstv", "\x00\b\f\n\r\x20\t\v" )

        # hexadecimal escape

        else {
            match( ch, /[1-9A-F][0-9A-F]*/i )
            if ( RLENGTH < 3 )
                ch = substr( ch, RSTART, RLENGTH )
            else
                ch = substr( ch, RSTART + RLENGTH - 2, 2 )
            ch = sprintf("%c", CKhextonum(ch) )
        }

        # replace and skip past

        substr( this, rs++, rl ) = ch

    }

    return( this )
}

# handle escape codes in delimited char and string literals

global function CKdoescapes(this) {

    return( doesc(substr(this, 2, length(this) - 2), allEscPat) )
}

# convert regular expression literals to regular expressions

global function CKregex(this) {

    local re

    # the TAWK function regex() complains directly to the console
    # and halts HXA if it doesn't like what it gets fed
    # - it would be nicer if we got an error return (say, a null string)
    # - we make certain simple validity checks before calling regex(),
    # but it's been claimed it's not possible to verify a regex literal
    # is valid simply by comparing it against another regex

    re = substr( this, 2, rindex(this, "/") - 2 )

    # a simple check that certain regex metachars always occur in pairs

    if ( match(re, /(\([^)]*|\[[^]]*|\{[^}]*)$/) ) {
        if ( (RSTART == 1) || (substr(re, RSTART-1, 1) != "\\") )
            return( CKfail )
    }

    # hex and '/s' escapes handled by doesc()...

    re = doesc( re, regexEscPat )

    # ...all other escapes handled by regex()

    return( this ~ /i$/ ? regex(re, "i") : regex(re) )
}

# -------------------

# split string into fields
# - but must not split where separator is escaped or within literal
# - or, if separator is ",", within possible function call

global function CKsplitfield(str, result, fsep) {

    local i, j
    local ch, pat
    local stopch, inparen
    local fcount, fval
    local field

    # no split char -> no split processing
    # - but do remove any trailing spaces

    if ( !index(str, fsep) ) {
        if ( match(str, /[ \t]+$/) )
            result[ 1 ] = substr( str, 1, RSTART-1 )
        else
            result[ 1 ] = str
        return( 1 )
    }

    # check if split necessary

    i = j = 1
    inparen = fcount = 0
    stopch = fsStopChar[ fsep ]
    while ( match(str, stopch, j) ) {

        # assume we're going to restart just after this stop char

        j = RSTART + 1

        # field separator ?

        ch = substr( str, RSTART, 1 )
        if ( ch == fsep ) {
            field[ ++fcount ] = substr( str, i, RSTART - i )
            i = j
            continue
        }

        # open parenthesis ?
        # - removes field split char from stops, adds close parenthesis

        if ( ch == "(" ) {
            inparen++
            stopch = /["'\\\(\/\)]/
            continue
        }

        # close parenthesis ?

        if ( ch == ")" ) {
            if ( --inparen == 0 )
                stopch = fsStopChar[ fsep ]
            continue
        }

        # must be double quote, single quote, slash or escape char
        # - we'll try to skip to its end (any stop chars in it - don't)

        if ( ch == "\"" )
            pat = CKstrLitToken
        else if ( ch == "'" )
            pat = CKcharLitToken
        else if ( ch == "/" )
            pat = CKregexLitToken
        else
            pat = allEscPat

        # if we find a match to pattern, restart after it
        # - if no match there's a good chance it's an input error,
        # but we'll restart just after the stop char anyway

        if ( match(str, pat, RSTART) )
            j = RSTART + RLENGTH
    }

    # unconditionally take everything left
    # - if i == 1 here, then any split char was escaped or within literal
    # - does this happen often enough to be worth checking for ?
    # - if i > length(str), then the last char in str must have been
    # a field separator
    # - which is an input error we want to catch anyway !

    field[ ++fcount ] = substr( str, i )

    # eliminate leading and trailing whitespace and save result
    # - if that's all we have to start with, the field is blank
    # - which is defined as an error to HXA

    i = fcount
    do {
        fval = field[ i ]
        if ( !match(fval, /[^ \t]/) ) {
            UMerror( "BadField", "#" i "> <" str )
            fcount = 0
        }
        else {
            if ( RSTART > 1 )
                fval = substr( fval, RSTART )
            if ( match(fval, /[ \t]+$/) )
                fval = substr( fval, 1, RSTART-1 )
            result[ i ] = fval
        }
    } while ( --i )

    return( fcount )
}

# -------------------

# join fields into string

global function CKjoinfields(this) {

    local i
    local text

    i = length( this )
    text = this[ i ]
    while ( --i )
        text = this[ i ] CKfieldSep text

    return( text )
}    

# -------------------

# limit field count

global function CKmaxfields(field, i) {

    local j

    j = length( field )
    while ( ++i <= j )
        UMignored( field[i] )
}

# split string on equate, get left and right pieces

global function CKsplitequate(str, chkesc, result) {

    local i
    local field

    if ( CKok(str) && CKsplitfield(str, field, "=") > 1 ) {
        CKmaxfields( field, 2 )
        for ( i = 1; i <= 2; i++ )
            result[ i ] = chkesc ? doesc( field[i], allEscPat ) : field[ i ]
        return( TRUE )
    }

    UMerror( "NeedEquate", str )
    return( FALSE )
}

# --------------------------
# character conversion
# --------------------------

# get end value for character conversion

local function xlate_end(specifier, beg) {

    local end

    # a single char ?
    # - end = beg, then

    if ( length(specifier) == 1 )
        return( beg )

    # a character range ?
    # - ie., "beg-end"

    else if ( specifier ~ /^.-.$/ ) {
        end = ord( substr(specifier, 3, 1) )
        if ( end < beg )
            UMnoeffect( specifier )
        return( end )
    }

    # something else...
    # - negative return value makes sure nothing happens

    else {
        UMerror( "NeedChar", specifier )
        return( -1 )
    }
}

# handle "XLATE" psop
# - XLATE a=b
# - XLATE a-b=c
# - XLATE a=b-c
# - XLATE a-b=c-d

global function CKdoxlate(dest, src) {

    local dstval, dstend, srcval, srcend

    # get source and destination values

    dstval = ord( dest )
    dstend = xlate_end( dest, dstval )
    srcval = ord( src )
    srcend = xlate_end( src, srcval )

    # "a-b=c-d" form
    # - until destination range has only one entry left or source exhausted

    while ( (dstval < dstend) && (srcval < srcend) )
       convertChar[ dstval++ ] = srcval++

    # "a=b", "a-b=c" and "a=b-c" forms
    # - also completes "a-b=c-d" form, repeating "d" as often as needed

    while ( (dstval <= dstend) && (srcval <= srcend) )
       convertChar[ dstval++ ] = srcval
}

# convert char/integer to number

global function CKchtonum(this) { return( convertChar[ord(this)] ) }
global function CKxlate(this) { return( convertChar[and(this,0xff)] ) }

# -----------------------------
# Block structure discipline
# -----------------------------

# - blocks can nest but not cross

# block types:

local  dummyBlk     = "__DUMMY"
global CKifBlk      = "__IF"
global CKincBlk     = "__INCLUDE"
global CKmacDefBlk  = "__MACDEF"
global CKmacExpBlk  = "__MACEXP"
global CKmacNestBlk = "__MACNEST"
global CKrptDefBlk  = "__RPTDEF"
global CKrptExpBlk  = "__RPTEXP"
global CKsegBlk     = "__SEGMENT"
global CKwhlDefBlk  = "__WHLDEF"
global CKwhlExpBlk  = "__WHLEXP"

# "looking for crossed block" multi-types:

global CKmacroBlk   = "MAC"
global CKrepeatBlk  = "RPT"
global CKwhileBlk   = "WHL"
global CKexpandBlk  = "EXP"

# block stack:

local blockType, blockLine
local blockPtr = 0

# -----------------------------

# is there a (non-dummy) block on the stack ?

global function CKblockstacked() { return( blockPtr > 1 ) } 

# push block

global function CKpushblock(type) {

    blockType[ ++blockPtr ] = type
    blockLine[ blockPtr ]   = SRCremember()
}

# pop block

global function CKpopblock() {

    SRCforget( blockLine[blockPtr--] )
}

# get master line number of expansion block start
# - successive calls return successive master line numbers

global function CKexpansionline(beg, errline) {

    local i
    local linenum

    # find the first expansion block start after "beg" master line
    # - loop is never entered during second pass (no block stack active)

    i = 1
    while ( ++i <= blockPtr ) {
        if ( index(blockType[i], CKexpandBlk) ) {
            linenum = SRCrecall( blockLine[i] )
            if ( linenum > beg )
                return( linenum )
        }
    }

    # this is just the original error's master line number
    # - and if not a line that triggered an expansion, not in block stack

    return( errline )
}

# report errors for all blocks which aren't supposed to be where they are

local function blockerrs(type) {

    SRCsavemaster()
    while( !index(blockType[blockPtr], type) ) {
        SRCsetmaster( SRCrecall(blockLine[blockPtr]) )
        UMerror( "BadBlock" )
        CKpopblock()
    }
    SRCresetmaster()
}

# block type mismatch detected

global function CKbadblock(sought) {

    local i

    # is a target type in the block stack at all ?
    # - if so, nested block(s) unclosed
    # - in general recovery difficult (many lines potentially affected)

    for ( i = blockPtr; i > 0; --i ) {
        if ( index(blockType[i], sought) ) {
            blockerrs( sought )
            UMfatal( "BadNest" )
        }
    }

    # no, so just an orphan (misplaced psop)

    UMerror( "BadBlock" )
}    

# get type of top open block on stack

global function CKtopblocktype() { return( blockType[blockPtr] ) }

# check type of top open block on stack

global function CKtopblock(type) {

    if ( CKtopblocktype() == type )
        return( TRUE )

    CKbadblock( type )
    return( FALSE )
}

# begin a new block scope

global function CKnewscope(type) {

    CKpushblock( type )
    SYMpushlocals()
    CKcheckmax( "MAXDEPTH", ++scopeDepth )
}

# restore an old block scope

global function CKoldscope() {

    if ( --scopeDepth < 0 )
        UMnoway( "CKOLDSCOPE" )
    else {
        SYMpoplocals()
        CKpopblock()
    }
}

# -----------------------------
# User Stack
# -----------------------------

local userStack         # the user stack
local userStkPtr = 0    # the user stack pointer

# is user stack empty ?

global function CKeos() { return( !userStkPtr ) }

# handle "PUSHS" psop
# - PUSHS str_expr

global function CKdopush(this) {

    userStack[ userStkPtr++ ] = this
    CKcheckmax( "MAXSTACK", userStkPtr )
}

# can user stack be popped ?
# - reports error if it cannot (ie., is empty)

global function CKcanpop() {

    if ( userStkPtr )
        return( TRUE )

    UMerror( "StkEmpty" )
    return( FALSE )
}

# pop off top user stack entry

global function CKdopop() { return( userStack[--userStkPtr] ) }

# can user stack be peeked ?
# - reports error if it cannot (ie., stack empty or index out of range)

global function CKcanpeek(ndx) {

    return( CKcanpop() && CKinrange(ndx, 1, userStkPtr) )
}

# "peek" user stack entry

global function CKdopeek(ndx) { return( userStack[userStkPtr-ndx] ) }

# -----------------------------

# end of pass one
# - errors found are not fatal because assembly will end anyway

global function CKendsource() {

    # check for unclosed blocks

    blockerrs( dummyBlk )

    # is user stack empty ?

    if ( !CKeos() )
        UMerror( "StkNotEmpty" )

    # recover memory (probably normally minimal)

    delete( blockType )
    delete( blockLine )
    delete( userStack )
}

# ----------------------
# time-related functions
# ----------------------

# handle "STARTTIMER" psop

global function CKdostarttimer(name) {

    if ( !(name in timerStart) )
        timerStart[ name ] = time()
    else
        UMdupname( name )
}

# handle "STOPTIMER" psop

global function CKdostoptimer(name) {

    if ( !(name in timerStart) )
        UMundefname( name )
    else if ( !(name in timerStop) )
        timerStop[ name ] = time()
    else
        UMnoeffect( name )
}

# read timer value
# - returned value is in seconds (and accurate only to nearest second)

global function CKreadtimer(name) {

    local endtime

    if ( !(name in timerStart) ) {
        UMundefname( name )
        return( 0 )
    }

    endtime = ( name in timerStop ) ? timerStop[name] : time()
    return( endtime - timerStart[name] )
}

# report elapsed time
# - "HRS:MIN:SEC"

global function CKelapsedtime(secs) {

    local hrs, mins

    hrs = mins = 0
    while ( secs >= 3600 ) {
        ++hrs
        secs -= 3600
    }
    while ( secs >= 60 ) {
        ++mins
        secs -= 60
    }
    return( sprintf("%02d:%02d:%02d", hrs, mins, secs) )
}

# handle "SHOWTIMER" psop

global function CKdoshowtimer(name) {

    UMdoecho( name "= " CKelapsedtime(CKreadtimer(name)) )
}

# --------------------------

# return name of pass timer

global function CKpasstimer(num) {

    return( num == 1 ? "__PASS1" : "__PASS2" )
}

# start assembly pass

global function CKbeginpass(number) {

    CKdostarttimer( CKpasstimer(number) )
}

# end assembly pass

global function CKendpass(number) {

    CKdostoptimer( CKpasstimer(number) )
}

# --------------------------

# multi-file output filename templates

local bysegTemplate = /^[^%]+%0[1-9]?[dxX][^%]*$/

# handle "--FILE", "--BYSEG" and "--BYBLOCK" psops
# - "--FILE" [name]
# - "--BYSEG" [name]
# - "--BYBLOCK" [name ]

global function CKdofilename(type, name ) {

    if ( !index(type, "BY") || name == "" || name ~ bysegTemplate )
        CKunique( toupper(name), type, outputType )
    else
        UMerror( "BadSegFile", name )
}

# has an output type been specified ?

global function CKoutput(type) { return( type in outputType ) }

# replace filename extension
# - actually replaces everything after first period (if any)

local function replaceext(name, ext) {

    local field

    split( name, field, "." )
    return( field[1] "." ext )
}

# get name of output file

global function CKgetfname(ndx, ext) {

    local name

    # name specified ?
    # - if not, use first source file name as default

    name = outputType[ ndx ]
    if ( name == "" )
        name = replaceext( SRCfile(1), ext )

    return( name )
}

# get name of output file (if specified)

global function CKcheckfname(ndx, ext) {

    if ( CKoutput(ndx) )
        return( CKgetfname(ndx, ext) )

    return( "" )
}

# get name of segment output file

global function CKgetsegname(ndx, ext, segnum) {

    local name

    # template name specified ?
    # - if not, use segment name as default

    name = outputType[ ndx ]
    if ( name == "" )
        name = replaceext( PCgetsegname(segnum), ext )

    # use segment number to create actual name

    else
        name = sprintf( name, segnum )

    return( name )
}

# locate end of path information in filename
# - non-zero if there is a path, else zero

local function pathend(name) {

    local pos

    # is there a rightmost directory separator ?

    if ( (pos = rindex(name, "\\")) )
        return( pos )

    # look for a device specifier

    return( index(name, ":") )
}

# get base name of file (ie., without any path information)

global function CKbasename(this) {

    return( substr(this, pathend(this)+1) )
}

# save path of initial source file (if any)
# - all input and output filenames are pre-pended with this same path
# unless they specify a device or an absolute path from root directory

global function CKsavepath(name) {

    local pos

    srcPath = ""

    if ( (pos = pathend(name)) )
        srcPath = substr( name, 1, pos )
}

# get full pathname of file
# - device names have a ":" suffix; absolute paths start with "\"
# - an absolute path on another drive has both, but that's more than we need

global function CKgetfpath(this) {

    if ( srcPath && ( this !~ /:|^\\/) )
        this = srcPath this

    return( toupper(this) )
}

# try to open a file for reading or writing

global function CKfopen(name, mode) {

    local fd

    name = CKgetfpath( name )
    if ( (fd = fopen(name, mode)) )
        UMfilestatus( index(mode, "r") ? "Fread" : "Fwrite", name )
    else
        UMerror( "NotOpen", name )

    return( fd )
}
