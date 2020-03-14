# Hobby Cross-Assembler (HXA) V0.201 - User Source File Management

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

# first created: 05/16/03
# last revision: 09/09/13

# public function prefix: "SRC"

# -------------------

# file descriptors

local inFile, outFile

# source tracking

local currFile
local currLine
local currMaster

local masterLine
local saveMaster

# file inclusion stack

local incStkFile, incStkLine, incStkPos
local incDepth = 0

# files used

local fileMaster, fileName, fileLine
local fileCnt

local readOnce                  # files tagged as "read once only"

# input text storage

local listSource                # flag: list source text ?
local listExpand                # flag: list expansion text ?

local currText                  # last source line read

local expandText                # saved source lines (from expansions)
local sourceText                # saved source lines (from files)

# source listing control

local listCtrl                  # list control options

local trueFlag
local falseFlag
local rootFlag

local takenFlag = TRUE          # default for conditional branches

local listLineNum = 0           # flag: show leading line number (also count)

# source cross-reference

local xrefHistory               # cross-reference history
local xrefListLine              # listing line (as opposed to source line)

# source listing format control

local pageMax = 255             # max page width, length

local pagWid                    # page width  (zero = no upper limit)
local pagLen                    # page length (zero = no upper limit)
local lineSpc                   # line spacing

local topMrg, lftMrg            # top and left margins
local botMrg, rgtMrg            # bottom and right margins

local pageFormat                # non-default values

local txtWid                    # printable width (if applicable)
local pageLine                  # line number on page
local pageBot                   # last printable line on page

local breakMin                  # minimum left breakpoint for long lines

local lftIndent                 # leading indent spaces to create left margin
local listLineNumFmt            # leading line number

# -------------------

# set line counts to zero
# - when masterLine is zero, any errors found are not associated
# with any source line, eg., a file read/write error
# - when currLine is non-zero, it is possible for the source line number
# of an error to *be* currLine (not always, but often enough to be useful)

local function zerolinecnts() { masterLine = currLine = 0 }

# get master line number

global function SRCgetmaster() { return( masterLine ) }

# set master line number

global function SRCsetmaster(this) { masterLine = this }

# save/restore master line number
# - during periods of temporary re-setting, which are used to report
# errors associated with earlier master lines but only discovered now

global function SRCsavemaster()  { saveMaster = masterLine }
global function SRCresetmaster() { masterLine = saveMaster }

# -------------------

# set list save flags
# - during the first pass some data is saved solely for listing purposes
# - but if it can be determined that no listing of a line is possible,
# some of that data can be safely ignored (ie., not saved)
# - for now we are mostly concerned about:
# - expansion lines, which by default aren't listed, where our concern
# is mostly about the text itself
# - false conditional branches, which by default aren't listed, where
# our concern is mostly about tracking which branch is which
# - either way, even if we do not save these lines initially we will save
# them later if we determine that code and/or error reporting happens

local function setlistflag() {

    listSource = listCtrl[ "OBJECT" ] && listCtrl[ "SOURCE" ] \
        && ( !incDepth || listCtrl["INCLUDES"] )

    listExpand = listSource && listCtrl[ "MACROS" ]
}

# set page-related list defaults

local function setpagedefault() {

    pagWid = 75
    pagLen = 0

    topMrg = rgtMrg = botMrg = lftMrg = 0

    lineSpc = 1
}

INIT {

    # list flags:                                   default:

    # object:   master object code list flag        TRUE
    # source:   list source code                    TRUE
    # includes: list include files                  TRUE
    # macros:   list macro expansions               FALSE
    # untaken:  list untaken conditional branches   FALSE

    # allequ:   local and variable label equates    FALSE

    # labels:   master symbol table list flag       TRUE
    # autos:    list auto-generated labels          FALSE

    # xref:     macro/global label cross reference  FALSE

    # segments: master segment map list flag        TRUE

    # stats:    master stats list flag              FALSE

    # linenums: master list line numbering flag     FALSE
    # linewrap: master line wrapping flag           TRUE

    # all:      "meta" flag (sets all others)       don't care

    CKpopulateNdx( listCtrl, \
        "OBJECT SOURCE INCLUDES LABELS SEGMENTS LINEWRAP", TRUE )

    CKpopulateNdx( listCtrl, \
        "MACROS UNTAKEN AUTOS XREF STATS LINENUMS ALLEQU ALL", FALSE )

    setlistflag()

    zerolinecnts()

    setpagedefault()
}

# -----------------------------

# get text of line (slightly faster if text known to be from file)

global function SRCtextf(ndx) { return( sourceText[ndx] ) }

# get text of line (from file or expansion)

global function SRCtext(ndx) {

    return( ndx in sourceText ? sourceText[ndx] : expandText[ndx] )
}

# -----------------------------

# make sure current source line is saved
# - may be needed later for listing or error reporting purposes
# - on second pass (when we're not reading source) only error reporting
# calls this when expression resolution fails - but any such expression
# must have generated data during first pass and so will already be saved

global function SRCremember() {

    # line not already saved ?
    # - can only happen when expanding macros whose text is not
    # going to be listed
    # - if caller saves return value this newly-saved line
    # can be "forgotten" later

    if ( MACexpanding() && !(masterLine in expandText) ) {
        expandText[ masterLine ] = currText
        return( masterLine )
    }

    # line saved already (and so un-deletable)

    return( -masterLine )
}

# set source line index to saved value

global function SRCrecall(ndx) {

    return( ndx > 0 ? ndx : -ndx )
}

# delete saved source line
# - if possible, then not needed for any future use (no data generated)

global function SRCforget(ndx) {

    if ( ndx > 0 )
        delete( expandText[ndx] )
}

# -----------------------------

# find master line number of nearest previous user source line
# - ie., the nearest previous non-expansion line 

global function SRCnearest(master) {

    # can we report the most recent source line read ?

    if ( currLine && (currMaster <= master) )
        return( currMaster )

    # search for it

    while ( !(master in sourceText) )
        --master
    return( master )
}

# get the index of the master file record of a given master line number

local function masterfile(master) {

    local i

    # - fileMaster[1] = 0 and master > 0, so this search
    # should always succeed
    # - if only one source file or first pass error call, search is quick
    # since we always want the current file (first one looked at)
    # - if more than one file we might look at more than most recent
    # file during (1) second pass error call or (2) recovery of the name of
    # the first file read (for default output file names)

    for ( i = fileCnt; i > 0; --i ) {
        if ( master > fileMaster[i] )
            return( i )
    }

    UMnoway( "MASTERFILE" )
}

# get the source file name given a master line (user or macro)

global function SRCfile(master) {

    return( fileName[masterfile(master)] )
}

# find source line number nearest given master by counting them

local function nearestsource(ndx, master, linenum) {

    while ( ndx++ < master ) {
        if ( ndx in sourceText )
            linenum++
    }

    return( linenum )
}

# get the source file line number given a master line (user or macro)

global function SRCline(master) {

    local i

    # can we report the most recent source line read ?

    if ( currLine && (currMaster <= master) )
        return( currLine )

    # okay, count source lines (always works, but slower)

    i = masterfile( master )

    return( nearestsource(fileMaster[i], master, fileLine[i]) ) 
}

# -----------------------------

# fetch next source line

global function SRCnextline() {

    # is an expansion block active ?
    # - if so, we know the next line will be non-null (so there will be
    # only one fetch) and "attached" to the last program source line

    if ( MACexpanding() ) {
        currText = MACgetline()
        ++masterLine
        if ( listExpand )
            expandText[ masterLine ] = currText
    }

    # is there a line in the current source file ?

    else {

        do {

            if ( (getline(currText) < inFile) <= 0 )
                return( "" )

            # save all text read from file(s)

            sourceText[ ++masterLine ] = currText
            ++currLine

            # skip blank and comment lines

        } while ( currText ~ CKignoreLine )

        # save for possible error reporting

        currMaster = masterLine
    }

    # remove comment at end of line (if any)
    # - comment starts if first char of second or later field is ";"
    # - we do a "fast" check followed by a "thorough" check if necessary

    if ( index(currText, ";") && match(currText, /[ \t]+;/) )
        return( substr(currText, 1, RSTART - 1) )

    return( currText )
}

# -----------------------------

# set list control change flag
# - only one change can happen per one source line

local function setlistctrl(value, flag) {

    if ( flag )
        trueFlag[ masterLine ] = value
    else
        falseFlag[ masterLine ] = value
}

# set conditional branch result
# - in order to cut down on storing unnecessary data:
# (a) we record only if there has actually been a change, and
# (b) even then only if the current line could possibly be listed
# (which it can't if it isn't stored, which is the default for expansions)

global function SRCsetcond(flag) {

    if ( flag != takenFlag ) {
        takenFlag = flag
        if ( listSource && ((masterLine in sourceText) || listExpand) )
            setlistctrl( "IFCOND", flag )
    }
}

# show EQUATE value in object section ?

global function SRCshowequ(label) {

    return( (label ~ SYMglbLabel) || listCtrl["ALLEQU"] )
}

# -----------------------------
# Assembly 'Include' files
# -----------------------------

# set current source file

global function SRCsetfile(name, line, pos) {

    if ( inFile )
        close( inFile )

    # try to open file for reading
    # - negative line number flags open for binary read

    if ( (inFile = CKfopen(name, (line >= 0) ? "rt" : "rb")) ) {

        # position file pointer
        # - first byte of file is at offset zero

        if ( pos > 0 )
            fseek( inFile, pos )

        # record for error-reporting
        # - note masterLine and currLine are one less than
        # first actual line in new file

        fileName[ ++fileCnt ] = currFile = toupper( name )
        fileLine[ fileCnt ]   = currLine = line
        fileMaster[ fileCnt ] = SRCgetmaster()
    }

    # successful if non-zero

    return( inFile )
}

# set root file flag

local function setrootflag(state) {

    rootFlag[ masterLine ] = state
    setlistflag()
}

# is include stack non-empty ?
# - if so, restore topmost source file

global function SRCpopfile() {

    # - note that if we can't restore topmost file,
    # we keep trying until we run out of them

    while ( (incDepth > 0) && CKtopblock(CKincBlk) ) {
        CKoldscope()
        if ( --incDepth == 0 )
            setrootflag( TRUE )
        if ( \
            SRCsetfile(incStkFile[incDepth], \
                       incStkLine[incDepth], \
                       incStkPos[incDepth]   \
                       ) \
         )
            return( TRUE )
    }

    return( FALSE )
}

# push source file info

local function pushfile(name, line, pos) {

    incStkFile[ incDepth ]  = name
    incStkLine[ incDepth ]  = line
    incStkPos[ incDepth ] = pos

    if ( ++incDepth == 1 )
        setrootflag( FALSE )

    CKnewscope( CKincBlk )
}

# handle "INCLUDE" psop
# - INCLUDE filename
# - we do this in a way that lets us take advantage of another
# function's error-handling during source file switching

global function SRCdoinclude(name) {

    # used within an expansion block ?
    # - not allowed because expansions have read priority over files,
    # so next line would come from expansion and not the included file

    if ( MACexpanding() ) {
        UMerror( "BadInExp" )
        return
    }

    # tagged as READONCE ?

    name = toupper( name )
    if ( name in readOnce )
        return

    # stack current file

    pushfile( currFile, currLine, ftell(inFile) )

    # stack include file

    pushfile( name, 0, 0 )

    # "restore" include file

    SRCpopfile()
}

# handle "INCBIN" pseudo op
# - INCBIN filename [[, count] [, start]]

global function SRCdoincbin(name, cnt, beg) {

    local data

    # stack current file

    pushfile( currFile, currLine, ftell(inFile) )

    # try to open include file

    if ( SRCsetfile(name, -1, beg) ) {

        # default is to take entire file

        if ( cnt < 1 )
            cnt = filesize( name )

        # - should there be a warning if cnt > filesize() - beg ?

        # read until end of file or have count

        while ( !feof(inFile) && (cnt > 0) ) {
            data = fread( cnt > 512 ? 512 : cnt, inFile )
            cnt -= length( data )
            CGsavestr( data )
        }
    }

    # restore current file

    SRCpopfile()
}

# warn about circular or previous inclusions

local function alreadyincluded(this) {

    UMwarn( this == currFile ? "CircInc" : "PrevInc", this )
}

# handle "READONCE" psop
# - READONCE

global function SRCdoreadonce() {

    local i
    local fname
    local stillactive

    if ( !(currFile in readOnce) ) {

        # okay, don't start reading this file again

        readOnce[ currFile ] = ".T."

        # we can only get here if 'currFile' has never been completely read
        # - because if it had, READONCE would already have been processed
        # - so 'currFile' has been read from its start to the point READONCE
        # appears
        # - it is best if 'currFile' has not INCLUDED any other files prior
        # to READONCE, but even so it is not necessarily a problem unless
        # - 1) any inclusions by 'currFile' are themselves not yet fully read
        # - 2) AND such an inclusion itself managed to INCLUDE 'currFile'
        # - 3) AND managed to reach the READONCE while the original read
        # of 'currFile' did not
        # - 4) AND something happened which should not have
        # - if all that occured we might like to provide a clue why

        # is the (1)-(3) sequence happening now ?
        # - we can't tell anything about (4), though

        stillactive = FALSE
        for ( i = 0; i < incDepth; i++ ) {
            fname = incStkFile[ i ]
            if ( stillactive )
                alreadyincluded( fname )
            else
                stillactive = (fname == currFile)
        }

        if ( stillactive )
            alreadyincluded( currFile )

        # has the current file INCLUDED anything before now ?
        # - it is possible to deliberately arrange things so that READONCE
        # occurs after any inclusions are complete but then prevents
        # future inclusions necessary to produce the 'expected' result
        # - and if it can be done deliberately it can be done accidentally
        # - we only show that an inclusion has happened, as we cannot
        # distinguish between 'calls' and 'returns' in the files read list

        else {
            for ( i = 1; i < fileCnt; i++ ) {
                fname = fileName[ i ]

                if ( fname == currFile ) {
                    alreadyincluded( fileName[i+1] )
                    break
                }

                # check for duplicate base names
                # - might be a different path to same file
                # - might the same file in a different location
                # - might be a different file in a different location
                # - a more elaborate check would use file sizes, times, etc
                # - it is possible for a duplicate base name to also be
                # in the include stack, but we only check here so as to
                # avoid multiple warnings for one name

                else if ( CKbasename(fname) == CKbasename(currFile) )
                    UMwarn( "PrevInc", fname )
            }
        }

    }
}

# -------------------

# handle "END" psop
# root file:
# - [label] END [startaddr] (monolithic)
# - END [startaddr] (segmented)
# - ignores all following text in root file
# - halts pass one
# include file:
# - [label] END (monolithic)
# - END (segmented)
# - ignores all following text in include file (if no other open block)
# - halts pass one (if open block of another type active)

global function SRCdoend(addr) {

    # open block(s) ?
    # - will be flagged as errors if pass one terminates

    if ( CKblockstacked() )
        UMwarn( "OddUse" )

    # within root file ?

    if ( !incDepth )
        CGsetstart( addr )

    # within include file

    else {

        # can't set start address here

        if ( addr )
            UMignored( addr )

        # not within an open block of another type within include file ?

        if ( CKtopblocktype() == CKincBlk ) {
            if ( SRCpopfile() )
                return
        }

        # open block of another type (or could not restore previous file)

    }

    # killing any expansion blocks stops reading from them...

    MACkillexpansion()

    # ...and closing current file stops reading from it

    close( inFile )
}

# -----------------------------

# turn listing option(s) on/off
# - unknown flags are silently ignored

local function setlistopt(flags, state) {

    local i, j, ndx
    local optlist

    for ( i = CKsplitfield(flags, optlist, CKfieldSep); i > 0; --i ) {
        ndx = optlist[ i ]
        if ( ndx == "ALL" ) {
            for ( j in listCtrl )
                listCtrl[ j ] = state
        }
        else if ( ndx in listCtrl )
            listCtrl[ ndx ] = state
    }
}

# handle "LIST--" psops
# - LIST-- [option [[,option]..]]

global function SRCdolistopt(type, optcnt, optlist) {

    local i
    local opt, state

    # if no option given, default to OBJECT list flag
    # - ie., can choose to list only sections of code

    if ( !optcnt )
        opt = "OBJECT"

    # otherwise verify options are legal

    else {

        for ( i = 1; i <= optcnt; i++ ) {
            opt = optlist[ i ] = toupper( optlist[i] )
            if ( !(opt in listCtrl) )
                UMignored( opt )
        }

        opt = CKjoinfields( optlist )
    }
            
    # we'll process option flags now so any master list switches given
    # will be in effect at start of listing

    state = ( type == "LISTON" )
    setlistopt( opt, state )

    # we'll also save flags so we can process list switches
    # again when we're actually performing list
    # - correct SEGMENTS flag in particular must be protected

    setlistctrl( opt, state )

    # update expansion text save flag

    setlistflag()
}

# -----------------------------
# Listing File Support
# -----------------------------

# A page:

#       +---pagWid-----------------------------------+
#       |                     |                      |
#       |    Margin Area   topMrg                    |
#    pagLen                   |                      |
#       |          +---txtWid-------------+          |
#       |          |                      |          |
#       |          |    Printable Area    |          |
#       |       txtLen                    |          |
#       |          |                      |          |
#       |--lftMrg--|                      |--rgtMrg--|
#       |          |                      |          |
#       |          |                      |          |
#       |          |                      |          |
#       |          |                      |          |
#       |          |                      |          |
#       |          +----------------------+          |
#       |                     |                      |
#       |                  botMrg                    |
#       |                     |                      |
#       +--------------------------------------------+

# set page format value
# - an error here prevents listing output (as all errors do)

local function setformat(ndx, new, old, min, max) {

    # new val in range and really new ?

    if ( CKinrange(new, min, max) ) {
        if ( CKunique(new, ndx, pageFormat) )
            return( new )
    }

    # new val is out-of-range or same as previously set

    return( old )
}

# check if page format has any printable area
# - width requires that at least the first character of an output line
# actually be output regardless of whatever might be added to that line
# as it is broken up to be output on multiple lines (line numbers, etc.)
# - length requires that at least one line can be output on each page
# - if page width or length is zero, output along that dimension always
# succeeds because these are flags that mean "as much as necessary"

local function badpage(wid, len) {

    local badwid, badlen

    badwid = pagWid && ( wid < 1 )
    if ( badwid )
        UMwarn( "BadPgWid", wid )

    badlen = pagLen && ( len < 1 )
    if ( badlen )
        UMwarn( "BadPgLen", len )

    return( badwid || badlen )
}

# check page area
# - depending on the order in which page size and margins are set,
# any warning might not ultimately matter, but if one does we want to
# know which line did it

local function checkarea() {

    badpage( pagWid - lftMrg - rgtMrg, pagLen - topMrg - botMrg )
}

# handle "PAGESIZE" psop
# - PAGESIZE width [, length]

global function SRCdopagesize(pwid, plen) {

    pagWid = setformat( "PWID", pwid, pagWid, 0, pageMax )
    pagLen = setformat( "PLEN", plen, pagLen, 0, pageMax )

    checkarea()
}

# handle "MARGINS" psop
# - MARGINS top, lft [, bot[, rgt]]

global function SRCdomargins(top, lft, bot, rgt) {

    local max

    max = pageMax - 1

    topMrg = setformat( "MTOP", top, topMrg, 0, max )
    lftMrg = setformat( "MLFT", lft, lftMrg, 0, max )
    botMrg = setformat( "MBOT", bot, botMrg, 0, max )
    rgtMrg = setformat( "MRGT", rgt, rgtMrg, 0, max )

    checkarea()
}

# handle "LINESPACE" psop
# - LINESPACE val

global function SRCdolinespace(val) {

    lineSpc = setformat( "LSPC", val, lineSpc, 1, pageMax - 1 )
}

# handle "PAGE" psop
# - PAGE

global function SRCdopage() { pageFormat[ SRCgetmaster() ] = ".T." }

# -----------------------------

# set minimum break point for long lines

local function setbreakmin(this) {

    breakMin = this

    # verify derived page values "work"
    # - an error here is fatal because the output device may not be
    # compatible with either wrong values or a re-setting of default values,
    # eg., output "file" is "PRN" under MS-DOS

    if ( badpage(txtWid - breakMin, pageBot - topMrg) )
        UMfatal( "BadPage" )
}

# possible "OBJECT" section line formats

# <- lftIndent -><linetype><-  object  -><-       text         ->

# <- lftIndent -><linetype><-  spaces  -><-       text         ->

# <- lftIndent -><linetype><- linenum  -><-  object  -><- text ->

# <- lftIndent -><linetype><- linenum  -><-  spaces  -><- text ->
#                |                                              |
#           left margin                                   right margin

# set additional page values derived from user-settable values

local function makeformats(linecnt) {

    local digits

    # printable area

    pageBot = pagLen - botMrg
    txtWid  = pagWid - lftMrg - rgtMrg

    # indent all lines from left edge ?

    if ( lftMrg )
        lftIndent = strdup( " ", lftMrg )

    # show line numbers in listing ?

    if ( listCtrl["LINENUMS"] ) {

        # set count (doubles as flag; how handy!)

        listLineNum = 1

        # how many digits shall we display ?
        # - based only on "OBJECT" section line count, but most often will
        # have a leading zero, which ought to accomodate all other sections
        # - log() yields natural log, not base 10 log

        digits = int( log(linecnt)/log(10) + 2 )

        # create format for showing line numbers

        listLineNumFmt = "%0" digits "d "

        # account for line numbers in printable width

        txtWid -= digits + 1
    }

    # an indent for non-object-generating source lines and those
    # broken due to length
    # - we have to wait until 'txtWid' is set before we can do this because
    # there is a "reality check" involved
    # - object: type addr hex hex hex hex src: "?XXXX  XX XX XX XX src"
    # - no object: (blank) src:                "                   src" 
    #                                           ----!----!----!----!----!

    setbreakmin( PCgetformatlen() + 15 )
}

# --------------------------------

local function min(val1, val2) { return( val1 < val2 ? val1 : val2 ) }

# output blank line(s)

local function skipline(cnt) {

    while ( cnt-- > 0 )
        print "" > outFile
}

# output text
# - adds left indent and line number (if any)
# - adds newline
# - handles pagination

local function putline(this) {

    local skipcnt

    # top margin ?

    if ( pagLen && (pageLine == 0 || pageLine > pageBot) ) {
        skipline( topMrg )
        pageLine = topMrg + 1
    }

    # indent ?

    if ( lftMrg )
        printf( "%s", lftIndent ) > outFile

    # line numbering active ?

    if ( listLineNum )
        printf( listLineNumFmt, listLineNum++ ) > outFile

    print this > outFile

    # not paginating ?

    if ( !pagLen )
        skipline( lineSpc - 1 )

    # paginating

    else {

        skipline( min(lineSpc - 1, pageBot - pageLine) )
        pageLine += lineSpc
        if ( pageLine > pageBot )
            skipline( botMrg )
    }
}

# skip to bottom of page
# - plus one to get to top of next page

local function formfeed() {

    if ( pagLen ) {
        skipline( pagLen - pageLine + 1 )
        pageLine = 0
    }
}

# --------------------------------

# send a newline to list file

global function SRCnewline() { putline( "" ) }

# --------------------------------

# find a place to break a long line

local function breakpoint(this, min, max) {

    local b1, b2

    b1 = rindex( this, ",", max )
    b2 = rindex( this, " ", max )

    if ( b1 > b2 )
        return( b1 )
    if ( b2 > min )
        return( b2 )
    return( max )
}

# send a line to list file
# - breaks up long lines

global function SRClist(this) {

    local brkpnt

    # blank line ?

    if ( this ~ /^[ \t]+$/ )
        this = ""

    # non-zero page width and too long to list completely on one line ?
    # - if so, line will be either wrapped (default) or truncated
    # - for wrapped lines, note the portion of "this" after "brkpnt+1"
    # is always non-null

    while ( pagWid && (length(this) > txtWid) ) {
        if ( listCtrl["LINEWRAP"] ) {
            brkpnt = breakpoint( this, breakMin, txtWid )
            putline( substr(this, 1, brkpnt) )
            this = strdup( " ", breakMin ) substr( this, brkpnt+1 )
        }
        else # truncate excess (which also breaks the loop)
            this = substr( this, 1, txtWid )
    }

    putline( this )
}

# -----------------------------

# show expanded listing text

global function SRCmesg(this) { SRClist(UMexpandtext(this)) }

# show labeled value

global function SRCshowval(label, val) {

    SRClist( UMexpandtext(label) "= " val )
}

# check whether section is to be listed (and show header if so)

local function showsection(this) {

    if ( listCtrl[this] ) {
        SRCnewline()
        SRClist( "*** " UMexpandtext(this) )
        SRCnewline()
        return( TRUE )
    }

    return( FALSE )
}

# show section sub-header

global function SRCsubheader(this, count) {

    SRCmesg( this )
    SRCnewline()
    if ( !count )
        SRCmesg( "NoListing" )

    return( count )
}

# -----------------------------
# Cross-Reference Support
# -----------------------------

# record cross-reference event
# - if more than one event for same line, only last will be saved
# - eg., macro names are checked for global name conflicts at definition

local function xrefevent(name, event) {

    if ( listCtrl["XREF"] ) {
        xrefHistory[ name ][ masterLine ] = event
        xrefListLine[ masterLine ] = 0
    }
}

# record macro name event

global function SRCmacDefine(name ) { xrefevent(name, "Def") }
global function SRCmacExpand(name)  { xrefevent(name, "Exp") }
global function SRCmacUndef(name)   { xrefevent(name, "Und") }

# record global name event

global function SRCglbEquate(name)    { xrefevent(name, "Equ") }
global function SRCglbReference(name) { xrefevent(name, "Ref") }

# note which listing line a cross-reference appears on (if any)
# - distinct from source line cross-reference

local function list_xref(i) {

    # line numbers on, cross-reference on, something recorded for this line ?

    if ( listLineNum && listCtrl["XREF"] && (i in xrefListLine) )
        xrefListLine[ i ] = listLineNum
}

# show events associated with one name

local function showhistory(name) {

    local i, ndx
    local master, lastmaster
    local srcfl, lastfl
    local srcln, lastln
    local s, event, count, lstln
    local listfmt, nlstfmt
    local eventlist
    local x_master, x_count, x_srcln, x_srcfl

    eventlist = xrefHistory[ name ]

    # - all events necessarily have an associated source file and line,
    # but not all events necessarily have an associated listing line
    # - unlisted events which happen within loops have the same
    # source file and line for each occurance, but no listing line
    # - it's just clutter to show that again and again, so we plan
    # to show only a total count instead
    # - this first loop collects that count (if needed)

    lastfl = ""
    ndx = 0
    for ( master in eventlist ) {

        srcfl = SRCfile( master )
        if ( srcfl != lastfl ) {
            x_srcfl[ ndx+1 ] = srcfl
            srcln = SRCline( master )
            lastln = 0
            lastfl = srcfl
        }
        else
            srcln = nearestsource( lastmaster, master, srcln )

        if ( srcln != lastln || xrefListLine[master] ) {
            x_master[ ++ndx ] = master
            x_srcln[ ndx ]  = srcln
        }
        else
            ++x_count[ ndx ]

        lastmaster = master
        lastln = srcln
    }

    # so we don't have to keep looking these up...

    listfmt = UMexpandtext( "XRefList" )
    nlstfmt = UMexpandtext( "XRefNlst" )

    # the general format of a cross reference line is:
    #    event count [listline] sourceline [sourcefile]
    # where items in brackets may be blank or default

    # show name

    SRClist( sprintf(UMexpandtext("XRefName"), name) )

    # show cross-reference event list

    for ( i = 1; i <= ndx; i++ ) {

        master = x_master[ i ]
        event = eventlist[ master ]
        count = ( i in x_count ) ? x_count[ i ] + 1 : 1
        lstln = xrefListLine[ master ]
        srcln = x_srcln[ i ]
        if ( i in x_srcfl )
            srcfl = (x_srcfl[i] != SRCfile(1)) ? CKgetfpath(x_srcfl[i]) : ""

        # show one cross-reference event

        if ( lstln )
            s = sprintf( listfmt, event, count, lstln, srcln, srcfl )
        else
            s = sprintf( nlstfmt, event, count, srcln, srcfl )
        SRClist( s )
    }

    # show total event count

    SRClist( sprintf(UMexpandtext("XRefCnts"), length(eventlist)) )

    SRCnewline()
}

# cross-reference listing

local function showxref() {

    local name

    # if anything recorded, show all global names and macros
    # - alphabetical order

    if ( SRCsubheader("XRefCols", length(xrefHistory)) ) {
        for ( name in xrefHistory )
            showhistory( name )
    }
}

# -----------------------------

# set title string

global function SRCdotitle(this) { CKunique(this, "TITL", pageFormat) }

# show header/title string

local function showheader() {

    local s

    # user specified title ?

    if ( "TITL" in pageFormat )
        s = pageFormat[ "TITL" ]

    # default title

    else
        s = CKverstr() " " UMexpandtext("ListFile") " - " \
            CGgetcpu() " " UMexpandtext( "InsSet" ) " - " ctime()

    # user may specify no title...

    if ( s )
        SRClist( s )
}

# -----------------------------

# list one address and its data in ASCII hex

local function listhex(ndx, typ, source) {

    local i
    local hex
    local addr, data

    addr = CGdataaddr( ndx )
    data = CGdatacode( ndx )
    hex = ""
    i = length( data )
    do {
        hex = sprintf( " %02X", data[i] ) hex
    } while ( --i )

    if ( source )
        hex = sprintf( "%-12s %s", hex, source )

    # the address and its data (and possibly the source code producing it)

    SRClist( typ PCformat(addr) " " hex )

    # if data is pc-relative, show absolute address it is based on

    if ( CGreldata(ndx) )
        SRClist( typ " [ =" PCformat(CGabsdata(ndx)) " ]" )
}

# list source code along w/ any object code

local function showsource(linecnt) {

    local i, ndx
    local typ
    local listbranch, isroot
    local flags

    if ( !linecnt ) {
        SRCmesg( "NoListing" )
        return
    }

    # listing control

    listbranch = isroot = TRUE

    # loop through every possible source line
    # - not all may actually have been recorded

    for ( i = 1; i <= linecnt; i++ ) {

        # process TRUE special flags in source line record
        # - set here so lines which set them are shown

        if ( i in trueFlag ) {
            flags = trueFlag[ i ]
            if ( flags == "IFCOND" )
                listbranch = TRUE
            else
                setlistopt( flags, TRUE )
        }

        # assume line is not listed

        typ = ""

        # listing possible ?

        if ( listCtrl["OBJECT"] ) {

            # user source available ?

            if ( i in sourceText )
                typ = isroot ? " " : "+"

            # expansion text available ?

            else if ( i in expandText )
                typ = isroot ? "^" : ">"
        }

        # do we have source text that might be listed ?

        if ( typ ) {

            # object code generated ?
            # - if so, always list (use multiple lines if necessary)

            if ( (ndx = CGfirstdata(i)) ) {
                list_xref( i )
                listhex( ndx, typ, SRCtext(i) )
                while ( (ndx = CGnextdata(ndx)) )
                    listhex( ndx, typ, "" )
            }

            # ...non-object-generating source line
            # in descending priority, don't list if:
            # - source listing turned off
            # - within unlisted include file
            # - within unlisted macro expansion
            # - within unlisted untaken conditional branch

            else if ( listCtrl["SOURCE"] ) {
                if ( isroot || listCtrl["INCLUDES"] ) {
                    if ( (i in sourceText) || listCtrl["MACROS"] ) {
                        if ( listbranch ) {
                            list_xref( i )
                            SRClist( typ strdup(" ", breakMin-1) SRCtext(i) )
                            if ( i in pageFormat )
                                formfeed()
                        }
                    }
                }
            }
        }

        # process FALSE special flags in source line record
        # - set here so lines which set them are shown

        if ( i in falseFlag ) {
            flags = falseFlag[ i ]
            if ( flags == "IFCOND" )
                listbranch = listCtrl[ "UNTAKEN" ]
            else
                setlistopt( flags, FALSE )
        }

        # rootfile flag set here so it takes effect on *next* line

        if ( i in rootFlag )
            isroot = rootFlag[ i ]

    }
}

# -----------------------------

# show rate-per-second

local function showrate(msg, numer, denom) {

    SRCshowval( msg, sprintf("%0.1f", numer/(denom ? denom : 1)) )
}

# show assembly statistics

local function showstats(linecnt) {

    local p1secs, p2secs

    p1secs = CKreadtimer( CKpasstimer(1) )
    SRCshowval( "P1Time", CKelapsedtime(p1secs) )
    SRCnewline()

    SRCshowval( "SrcLine", length(sourceText) )
    SRCshowval( "ExpLine", linecnt - length(sourceText) )
    SRCshowval( "TotLine", linecnt )
    SRCnewline()

    showrate( "LinesSec", linecnt, p1secs )
    SRCnewline()

    p2secs = CKreadtimer( CKpasstimer(2) )
    SRCshowval( "P2Time", CKelapsedtime(p2secs) )
    SRCnewline()

    SRCshowval( "DataVal", CGgetStoreCnt() )
    SRCnewline()

    showrate( "ValSec", CGgetStoreCnt(), p2secs )
    SRCnewline()

    SRCshowval( "TotTime", CKelapsedtime(p1secs+p2secs) )
    SRCnewline()

    SRCshowval( "ObjSize", CGgetObjectSize() )
    SRCnewline()
}

# create assembly list file

global function SRCshow() {

    local linecnt
    local name

    linecnt = SRCgetmaster()
    zerolinecnts()

    name = CKcheckfname( "LISTFILE", "LST" )
    if ( name ) {
        makeformats( linecnt )
        outFile = CKfopen( name, "wt" )      
        if ( outFile ) {

            showheader()

            if ( showsection("OBJECT") )
                showsource( linecnt )

            setbreakmin( 18 )

            if ( showsection("LABELS") ) {
                SYMshow( listCtrl["AUTOS"] )
                MACshow()
            }

            if ( showsection("XREF") )
                showxref()

            if ( PCsegmented() && showsection("SEGMENTS") )
                PCshowsegs()

            if ( showsection("STATS") ) {
                showstats( linecnt )
                EXPstats()
            }

            close( outFile )
        }
    }

    # recover memory

    delete( expandText )
    delete( sourceText )

    delete( trueFlag )
    delete( falseFlag )

    delete( pageFormat )

    delete( xrefHistory )
    delete( xrefListLine )

    MACfree()
    SYMfree()
}
