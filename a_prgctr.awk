# Hobby Cross-Assembler (HXA) V0.200 - Program Counter Management

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

# e-mail: hxa@earthlink.net

# source language: Thompson AWK 4.0

# first created: 03/11/03
# last revision: 08/24/13

# public function prefix: "PC"

# ----------------------------

local progCtr = 0           # THE program counter (= next storage address)
local pcMax   = 0           # max legal pc value
local pcMaxOne              # one more than pcMax
local pcWidth               # effective pc bit width

local monoMaxProgCtr = 0    # maximum monolithic program counter recorded

local possibleJump = FALSE  # flag: non-consecutive data storage possible
local nojumpPC              # actual consecutive storage address

local pcValid = FALSE       # flag: program counter is valid
local dataLegal = FALSE     # flag: data can be stored

local explicitSegs = FALSE  # flag: explicit segments used
local segCnt = 0            # the count of segments (implicit and explicit)

local segOrder              # segment order
local segName               # segment name
local segProgCtr            # segment program counter
local segType               # segment type (for segment map listing)

local segAbsOrg             # segment absolute origin
local segAbsEnd             # segment absolute end

local segBase               # segment base address (zero if absolute origin)

local segRelOrg             # relative origin segment flag (if exists)
local segRelEnd             # relative end segment flag (if exists)

local segNoData             # uninitialized segment flag (if exists)
local segCommon             # common segment flag (if exists)
local segOffset             # segment offset in binary output (if exists)
local segPadTo              # segment padding boundary (if exists)
local segPadVal             # segment pad value (if exists)

local currSegNum            # number of current segment
local currSegName           # name of current segment

local absFlag = TRUE        # segment relative/absolute flag

local segNameStk            # nested segment stack
local segNdx                # segment stack index

# - HXA programs can be monolithic (one block) or segmented (multiple blocks)
# - in either case the program counter can range from 0..2^32-1
# (if the program counter width is 32 bits, less if it is smaller)
# - in monolithic programs the program counter is always reported to other
# modules as an absolute value
# - in segmented programs the program counter may be reported to other
# modules as an absolute value or a coded segment:offset value
# - we code the segment:offset into one value because outside this module
# it's usually (not always) more convenient in that form
# - we can fit segment (10 bits) and offset (32 bits) into one double,
# which has 56 significant digits:
# - bits 00-31: offset within segment (0..2^32-1)
# - bit  32:    not used
# - bits 33-42: segment number (1..2^10-1)
# - bits 43-55: not used
# - note TAWK's automatic conversion to doubles values which
# don't fit into 32-bit signed integers means:
# 0x07fffffff + 1 = 0x100000000 (and not 0x080000000)

local segCode = 0               # coded segment number
local segCodeVal = 2^33         # segment coding value

# ----------------------------

INIT { 

    # 10-bit limit on segment count could be changed up or down if needed

    CKdomax( "MaxSeg", 1023 )
}

# ----------------------------

# report non-fatal error in current segment

local function segerror(this) { UMerror(this, currSegName) }

# ----------------------------

# is data segmented ?

global function PCsegmented() { return( explicitSegs ) }

# is the program counter valid for reading ?

global function PCcanget() { return( pcValid ) }

# ----------------------------

# bad, bad program counter !

local function pcinvalid() {

    # CPU set ?

    if ( !CGgetcpu() )
        UMerror( "NoCPU" )

    # currently in an explicit segment ?

    if ( currSegNum )
        UMfatal( "BadPC", currSegName )

    # currently between explicit segments ?

    if ( explicitSegs )  
        UMerror( "BadSegOut" )

    UMfatal( "BadPC" )   
}

# check if currently within a segment (when must be)
# - if monolithic program or between segments, fatal error

local function validsegment() {

    # segmented program ?

    if ( !explicitSegs )
        UMfatal( "BadSegOut" )

    # within a segment ?

    if ( !currSegNum )
        pcinvalid()
}

# check if program counter is valid

local function pcvalid() {

    if ( !pcValid )
        pcinvalid()
}

# ----------------------------

# check if address value is legal (to simply have)

local function pclegalplusone(addr) {

    # legal between zero and pcMax+1

    if ( (addr >= 0) && (addr <= pcMaxOne) )
        return

    # second check will actually report error
    # - extra time doesn't matter because it's fatal anyway

    CKinrange( addr, 0, pcMaxOne )
    pcinvalid()
}

# check if address value is legal (to store data at)

global function PClegal(addr) {

    if ( pcMax )
        return( CKinrange(addr, 0, pcMax) )
    else
        pcinvalid()
}

# ----------------------------

# bump segment counter

local function bump_segcnt() { CKcheckmax( "MaxSeg", ++segCnt ) }

# ----------------------------

# set pc change flags
# - pcValid   -> program counter is valid
# - dataLegal -> legal to store data (monolithic program or data segment)

local function setpcvalid() {

    pcValid = TRUE
    dataLegal = !explicitSegs || !(currSegNum in segNoData)
}

local function setpcinvalid() { pcValid = dataLegal = FALSE }

# ----------------------------

# get program counter (for symbol value and expression evaluation)
# - actual absolute value if in monolithic program or
# absolute origin segment, otherwise coded relative value

global function PCget() {

    if ( pcValid )
        return( absFlag ? progCtr : progCtr + segCode )

    pcinvalid()
}

# get program counter (for data storage)

global function PCread() {

    # not legal to store data ?

    if ( !dataLegal ) {

        # pc invalid ?

        pcvalid()

        # valid, so we're in an "uninitialized" segment
        # - and trying to store data in it

        UMfatal( "SegIsND", currSegName )
    }

    # monolithic program counter might have jumped ?
    # - done so later we can easily output by "segment"

    if ( possibleJump ) {
        if ( nojumpPC != progCtr ) {
            bump_segcnt()
            segAbsOrg[ segCnt ] = progCtr
            segCode = segCnt * segCodeVal
        }
        possibleJump = FALSE
    }

    # data addresses are always coded

    return( progCtr + segCode )
}

# -------------------

# PC width larger than a given value ?

global function PCwider(this) { return( pcWidth > this ) }

# set program counter limit values
# - "pcMaxOne" is the value immediately after the last legal byte location.
# We need it because storing a value in the last legal location causes
# the pc to advance to an illegal location. This is ok as long it's only
# one byte and we don't try to store anything past the last legal location

local function setpcmax(bits) {

    pcWidth = bits
    pcMaxOne = 2 ^ bits
    pcMax = pcMaxOne - 1
}

# set program counter width

global function PCsetwidth(bits) {

    # not previously set ?
    # - eg., for cpu initialization

    if ( !pcWidth )
        setpcmax( bits )

    # set narrower ?
    # - eg., for hex file output

    else if ( bits < pcWidth ) {
        setpcmax( bits )

        # check that program counter and max saved implicit segment
        # program counter are still legal
        # - doesn't matter if we're using explicit segments
        # - even though we don't show exactly which segment is out of range,
        # we at least show the "narrowing" line which triggered problems

        pclegalplusone( progCtr )
        pclegalplusone( monoMaxProgCtr )
    }
}

# get length of a formatted address (for listing)
# - based on full cpu pc width (un-narrowed)

global function PCgetformatlen() {

    # 16 bits or less ?

    if ( pcWidth <= 16 )
        return( 4 )         # "%04X"

    # 24 bits or less ?

    if ( pcWidth <= 24 )
        return( 7 )         # "%02X:%04X"

    # more than 24 bits

    return( 9 )             # "%04X:%04X"
}

# return formatted address value for program listing
# - "addr" is an absolute address value

global function PCformat(addr) {

    local fmt

    # 16 bits or less ?

    if ( pcWidth <= 16 )
        return( sprintf("%04X", addr) )

    # more than 16 bits

    fmt = (pcWidth <= 24) ? "%02X:%04X" : "%04X:%04X"

    return( sprintf(fmt, addr/2^16, addr%2^16) )
}

# -------------------

# set up check for non-consecutive data storage in monolithic program

local function monojump() {

    # enable check the next time a data address is requested
    # - thus saved value is most recent data storage address, and multiple
    # changes to PC without data storage (by eg., "DS") won't matter
    # if final change restores saved value before next data store

    if ( !possibleJump ) {
        possibleJump = TRUE
        nojumpPC = pcValid ? progCtr : -1
    }

    # note maximum program counter value
    # - in monolithic programs we save this value in case the program counter
    # is later "narrowed", when we use it to check that the maximum observed
    # value is still legal

    if ( progCtr > monoMaxProgCtr )
        monoMaxProgCtr = progCtr
}

# -------------------

# check if current segment already set to incompatible type

local function goodsegtype(bad) {

    local err

    validsegment()

    if ( index(bad, "AO") && (currSegNum in segAbsOrg) )
        err = "SegIsAO"
    else if ( index(bad, "AE") && (currSegNum in segAbsEnd) )
        err = "SegIsAE"
    else if ( index(bad, "RE") && (currSegNum in segRelEnd) )
        err = "SegIsRE"
    else if ( index(bad, "RO") && (progCtr || (currSegNum in segRelOrg)) )
        err = "SegIsRO"
    else if ( index(bad, "PD") && (currSegNum in segPadTo) )
        err = "SegIsPD"
    else if ( index(bad, "ND") && (currSegNum in segNoData) )
        err = "SegIsND"
    else
        return( TRUE )

    segerror( err )
    return( FALSE )
}

# -------------------

# valid CPU address value ?

local function validorg(addr) {

    # CPU set ?

    if ( !pcMax )
        pcinvalid()

    # within range ?

    pclegalplusone( addr )
}

# set "absolute" segment type

local function setabsoluteseg(label, addr, absarray, badtype) {

    local first_setting

    first_setting = !(currSegNum in absarray)

    if ( first_setting ) {
        if ( goodsegtype(badtype) )
            absarray[ currSegNum ] = addr
        else
            return( FALSE )
    }

    else if ( !CKsameval(addr, absarray[currSegNum]) )
        return( FALSE )

    # needed for all ABSEND and second or later ABSORG

    if ( label != "" )
        SYMadd( label, addr )

    # first setting of ABSORG needs to set pc also

    return( first_setting )
}

# handle "ORG" psop
# - [label] ORG addr

global function PCdoorg(label, addr) {

    # legal CPU address ?

    validorg( addr )

    # segmented program ?
    # - ie., "SEGMENT" before "ORG"

    if ( explicitSegs ) {

        if ( !setabsoluteseg(label, addr, segAbsOrg, "AE|RE|RO") ) {
            CGsavenonobjnum( 0, addr )
            return
        }
    }

    # monolithic program

    else {
        monojump()
    }

    absFlag = TRUE
    setpcvalid()

    progCtr = addr
    SYMhere( label )

    # save for listing

    CGsavenonobjnum( addr, addr )
}

# handle "ABSEND" psop
# - [label] ABSEND addr
# - segment is still relative until fix-up (ABSEND flags HOW to fix-up)

global function PCdoabsend(label, addr) {

    validorg( addr )
    setabsoluteseg( label, addr, segAbsEnd, "AO|RE|RO|PD" )
    CGsavenonobjnum( 0, addr )
}

# set relative segment type

local function setrelativeseg(relarray, badlist) {

    if ( !(currSegNum in relarray) && goodsegtype(badlist) )
        relarray[ currSegNum ] = ".T."
}

# handle "RELORG" psop
# - RELORG

global function PCdorelorg() { setrelativeseg(segRelOrg, "AO|AE|RE") }

# handle "RELEND" psop
# - RELEND

global function PCdorelend() { setrelativeseg(segRelEnd, "AO|AE|RO|PD") }

# -------------------------------------

# get segment origin
# - all but absolute origin segments have "zero" origin during pass one

local function segorigin() {

    return( (currSegNum in segAbsOrg) ? segAbsOrg[currSegNum] : 0 )
}

# -------------------------------------

# set current segment

local function setcurrseg(name) {

    currSegName = name
    currSegNum = segOrder[ name ]

    # non-common segments set pc to last fragment value,
    # common segments set pc to segment origin

    if ( !(currSegNum in segCommon) )
        progCtr = segProgCtr[ currSegNum ]
    else
        progCtr = segorigin()

    segCode = currSegNum * segCodeVal

    absFlag = currSegNum in segAbsOrg
    setpcvalid()
}

# clear current segment

local function clearcurrseg() {

    # update segment pc if we've reached a new, larger size
    # - which we may not have if a "common" segment (or no pc change)

    if ( segProgCtr[currSegNum] < progCtr )
        segProgCtr[ currSegNum ] = progCtr

    currSegNum = 0
    setpcinvalid()
}

# handle "USESEGMENTS" pseudo-op
# - USESEGMENTS
# - not essential but can be handy for clarifying programmer intent

global function PCdousesegments() {

    # fatal error if the monolithic model is already in effect

    if ( !explicitSegs ) {
        if ( !pcValid )             # "ORG" not used yet
            explicitSegs = TRUE
        else                        # "ORG" already used
            UMfatal( "BadSegUse" )
    }
}

# handle "SEGMENT" pseudo-op
# - SEGMENT name

global function PCdosegment(name) {

    # guarantee segmented model in effect

    PCdousesegments()

    # is this a new segment ?
    # - if so, initialize it

    if ( !(name in segOrder) ) {
        bump_segcnt()
        segOrder[ name ]  = segCnt
        segName[ segCnt ] = name
        segProgCtr[ segCnt ] = 0
    }        

    # new segment is nested ?

    if ( currSegNum ) {
        segNameStk[ segNdx++ ] = currSegName
        clearcurrseg()
    }

    # set segment for use

    setcurrseg( name )

    # isolate segment locals

    CKnewscope( CKsegBlk )
}

# handle "ENDS" pseudo-op
# - ENDSEGMENT [name]

global function PCdoends(name) {

    # is "segment" the top open block type ?

    if ( !CKtopblock(CKsegBlk) )
        return

    # if there is a name, does it match ?

    if ( name && !CKmatch(name, currSegName) )
        return

    # lose segment locals

    CKoldscope()

    # clear segment

    clearcurrseg()

    # pop nesting segment ?

    if ( segNdx )
        setcurrseg( segNameStk[--segNdx] )
}

# -------------------

# check if count value is plausible for use by DS/FILL/PADTO

local function plausible(cnt) {

    # program counter must currently be valid

    pcvalid()

    # can't explicitly use a value outside valid program counter range
    # - pc can still be pushed past the maximum during execution, of course
    # - cnt == 0 passed without comment, but nothing should happen
    # - we know count is non-zero if CKinrange() called, but check against
    # zero anyway so that error message for negative values is understandable

    return ( cnt && CKinrange(cnt, 0, pcMaxOne) )
}

# handle "FILL" pseudo op
# - FILL count [, hex$]

global function PCdofill(count, val) {

    if ( plausible(count) )       
        CGfill( count, val )
}

# pad to an address boundary

local function padto(boundary, start, val) {

    local bytes

    # modulus gives us the number of bytes *from* the last boundary...

    bytes = start % boundary

    # ...which is zero if we're on the boundary now...

    if ( bytes ) {

        # ...what we need is the number *to* the next boundary

        bytes = boundary - bytes

        CGfill( bytes, val )
    }

    return( bytes )
}

# handle "PADTO" pseudo-op
# - PADTO boundary [, hex$]

global function PCdopadto(boundary, val) {

    if ( plausible(boundary) ) {

        # implicit segments ?

        if ( !explicitSegs )
            padto( boundary, progCtr, val )

        # only one PADTO boundary and value per explicit segment
        # - not compatible with --END or uninitialized segments

        else if ( goodsegtype("AE|RE|ND") ) {
            CKunique( boundary, currSegNum, segPadTo )
            CKunique( val, currSegNum, segPadVal )
        }
    }
}

# -------------------

# set "uninitialized segment" flag

local function setnodata() {

    validsegment()

    # flag already set ?

    if ( currSegNum in segNoData )
        return( TRUE )

    # not compatible with padded segments

    if ( goodsegtype("PD") ) {

        # no data allowed in uninitialized segments...
        # ...which may have happened already if pc incremented...
        # - ORG used by itself is okay

        if ( progCtr != segorigin() )
            segerror( "SegIsID" )

        # set "uninitialized segment" flags

        else {
            segNoData[ currSegNum ] = ".T."
            setpcvalid()
            return( TRUE )
        }
    }

    # flag not set

    return( FALSE )
}

# handle "UNINITIALIZED" psop
# - UNINITIALIZED

global function PCdouninitialized() { setnodata() }

# handle "COMMON" psop
# - COMMON

global function PCdocommon() {

    if ( setnodata() )
        segCommon[ currSegNum ] = ".T."
}

# -------------------

# add value to program counter

global function PCadd(val) {

    pcvalid()

    # update program counter

    progCtr += val
    pclegalplusone( progCtr )
}

# handle "DS" psop
# - [label] DS val

global function PCdods(val) {

    # explicit segments ?
    # - implicitly sets current segment to "uninitialized" (if possible)

    if ( explicitSegs ) {
        if ( !setnodata() || !plausible(val) )
            return
    }

    # monolithic program

    else
        monojump()

    CGsavenonobjnum( PCget(), val )

    PCadd( val )
}

# -------------------

# get segment name given number

global function PCgetsegname(num) {

    return( (num > 0) && (num <= segCnt) ? segName[num] : "" )
}

# get segment number given name

global function PCgetsegnum(name) {

    return( (explicitSegs && (name in segOrder)) ? segOrder[name] : CKfail )
}

# check if segment is resolved (absolute)

local function isabsolute(i) {

    return( (i in segAbsOrg) && (i in segAbsEnd) )
}

# get segment absolute start address

global function PCgetsegbeg(i) {

    return( (i in segAbsOrg) ? segAbsOrg[i] : CKfail )
}

# get segment absolute end address

global function PCgetsegend(i) {

    return( (i in segAbsEnd) ? segAbsEnd[i] : CKfail )
}

# get segment length

global function PCgetseglen(i) {

    return( isabsolute(i) ? segAbsEnd[i] - segAbsOrg[i] : CKfail )
}

# get segment data offset within object file

global function PCgetsegoff(i) {

    return( (i in segOffset) ? segOffset[i] : CKfail )
}

# -------------------

# handle any segment padding

local function checksegpad(i, start) {

    # pad boundary value is replaced by count of padding bytes

    if ( i in segPadTo )
        segPadTo[ i ] = padto( segPadTo[i], start, segPadVal[i] )
}

# set segment absolute start and end addresses
# - for all segment types except absolute origin

local function setsegaddr(i, beg, end) {

    pclegalplusone( beg )
    segBase[ i ] = segAbsOrg[ i ] = beg
    pclegalplusone( end )
    progCtr = segAbsEnd[ i ] = end
}

# -------------------

# make sure all segments are absolute
# - any errors are reported as belonging to last source line

global function PCmakeabsolute() {

    local i
    local resolved, unresolved
    local offset

    # implicit segments only (so all already absolute) ?

    if ( !explicitSegs )
        return( TRUE )

    UMmarkerr()

    # make all segments absolute

    unresolved = segCnt

    do {
        resolved = 0

        for ( i = 1; i <= segCnt; i++ ) {

            # already resolved ?

            if ( isabsolute(i) )
                continue

            # make sure any error messages report proper segment
            # - also puts some array data values in file-scope scalars            

            setcurrseg( segName[i] )

            # make sure we have max size of "common" segments

            if ( i in segCommon )
                progCtr = segProgCtr[ i ]

            # absolute origin segment ?
            # - but no absolute end yet, as that ruled out above

            if ( i in segAbsOrg ) {

                checksegpad( i, progCtr )
                segBase[ i ] = 0
                segAbsEnd[ i ] = progCtr
                segType[ i ] = "SegIsAO"
                ++resolved
            }

            # absolute end segment ?
            # - but no absolute origin yet, as that ruled out above

            else if ( i in segAbsEnd ) {

                setsegaddr( i, segAbsEnd[i] - progCtr, segAbsEnd[i] )
                segType[ i ] = "SegIsAE"
                ++resolved
            }

            # relative end segment ?

            else if ( i in segRelEnd ) {

                # successor segment has absolute origin ?

                offset = PCgetsegbeg( i+1 )
                if ( CKok(offset) ) {

                    setsegaddr( i, offset - progCtr, offset )
                    segType[ i ] = "SegIsRE"
                    ++resolved
                }
            }

            # must be relative origin segment
            # - set start to end of previous segment
            # - padding is tricky: it must be based on the current absolute
            # address of the program counter but must be stored at the
            # current relative address of that same program counter

            else {

                # predecessor segment has absolute end ?

                offset = PCgetsegend( i-1 )
                if ( CKok(offset) ) {
                    checksegpad( i, offset + progCtr )
                    setsegaddr( i, offset, offset + progCtr )
                    segType[ i ] = "SegIsRO"
                    ++resolved
                }
            }

            clearcurrseg()

        }

        # resolved at least one segment and at least one still unresolved ?

    } while ( resolved && (unresolved -= resolved) )

    # verify all segments now absolute
    # - as long as we're at it we'll determine binary offset value

    offset = 0
    for ( i = 1; i <= segCnt; i++ ) {
        if ( !isabsolute(i) )
            UMerror( "BadSegAbs", segName[i] )
        else if ( !(i in segNoData) ) {
            segOffset[ i ] = offset
            offset += PCgetseglen( i )
        }
    }

    # successfully resolved all segments ?

    return( !UMerrdiff() )
}

# -------------------

# get segment number of relative address

global function PCgetseg(addr) {

    return( int(addr/segCodeVal) )
}

# get absolute address

global function PCgetabs(addr) {

    local offset

    # already absolute ?

    if ( addr < segCodeVal )
        return( addr )

    # get offset part of address

    offset = addr % segCodeVal

    # explicit segments ?
    # - add segment base address to relative offset
    # - except base address is always zero for absolute origin segments,
    # so offset itself is absolute

    if ( explicitSegs )
        return( segBase[PCgetseg(addr)] + offset )

    # monolithic program

    else
        return( offset )
}

# check if object is coded relative addr

global function PCgotrel(this) {

    return( (typeof(this) == "float") && (this >= segCodeVal) )
}

# -------------------

# show segment value map line

local function showsegval(type, val) {

    SRClist( sprintf(UMexpandtext(type), EXPint(val), val) )
}

# show segment map

global function PCshowsegs() {

    local i
    local padbytes

    SRCmesg( "SegCols" )

    for ( i = 1; i <= segCnt; i++ ) {

        SRCnewline()
        SRClist( sprintf(UMexpandtext("SegName"), i , segName[i]) )

        SRCmesg( segType[i] )

        # an uninitialized segment ?

        if ( i in segNoData ) {
            SRCmesg( "SegIsND" )
            if ( i in segCommon )
                SRCmesg( "SegIsCO" )
            showsegval( "SegBeg", PCgetsegbeg(i) )
            showsegval( "SegSize", PCgetseglen(i) )
        }

        # has data            

        else {
            padbytes = ( i in segPadTo ) ? segPadTo[ i ] : 0

            showsegval( "SegOff", PCgetsegoff(i) )
            showsegval( "SegBeg", PCgetsegbeg(i) )
            showsegval( "SegData", PCgetseglen(i) - padbytes )
            if ( padbytes )
                showsegval( "SegPad", padbytes )
        }

        showsegval( "SegEnd", PCgetsegend(i) )
    }
}
