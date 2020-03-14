# Hobby Cross-Assembler (HXA) V0.201 - User Messages (Error and Informational)

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

# first created: 01/18/03
# last revision: 09/12/13

# public function prefix: "UM"

# -----------------------------

# constants

local mesgText

# variables

local errCnt = 0
local warnCnt = 0
local fatalCnt
local warnPrv

local markNdx, markErr

# assembler error output logging

local errlogFile = 0

# -----------------------------
# Default Message Texts
# -----------------------------

# internal HXA error messages

local initIntErr = "\
 BadMsg=Expanded text not found+\
 NoWay=Can't happen+\
"

# general status messages

local initStatus = "\
 Fread=Reading+\
 Fwrite=Writing+\
 BegOne=Pass One started+\
 EndOne=Pass One ended+\
 BegTwo=Pass Two started+\
 EndTwo=Pass Two ended+\
 Quit=Assembly halted+\
"

# source code error reporting messages

local initErrReport = "\
 Warn=Warning in+\
 Error=Error in+\
 Fatal=Fatal Error in+\
 BadLine=While processing line+\
 BadPsop=While processing pseudo opcode+\
 BadMacro=While processing macro+\
 BadOp=While processing CPU mnemonic+\
 BadLabel=While processing label+\
 Fault=Source-defined fault+\
 HaveWarn=Warnings detected+\
 HaveErr=Errors detected+\
 HaveFatal=Fatal error detected+\
"

# limit reached fatal messages

local initLimitFatal = "\
 MAXERR=Error count+\
 MAXWARN=Warning count+\
 MAXDEPTH=Nesting depth+\
 MAXPUTBACK=Putback count+\
 MAXSTACK=User stack full+\
 MaxSeg=Segment count+\
"

# odd use warning messages

local initOddWarn = "\
 OddVal=Unusual value+\
 OddUse=Unusual use+\
"

# token warning/error messages

local initTokErr = "\
 Ignored=Unexpected token ignored+\
 BadToken=Unexpected token+\
 BadField=Unexpected blank field+\
"

# name\symbol\label error messages

local initNameErr = "\
 ReservedName=Name reserved by assembler+\
 DupName=Name already in use+\
 UndefName=Name not found+\
 MatchName=Name does not match+\
"

# expected token not found error messages

local initNeedErr = "\
 NeedToken=Expecting opcode or label name+\
 NeedOpcode=Expecting opcode+\
 NeedLabel=Expecting label name+\
 NeedGlb=Expecting global name+\
 NeedFile=Expecting filename+\
 NeedBOE=Expecting operand+\
 NeedNum=Expecting number+\
 NeedStr=Expecting string+\
 NeedNumOp=Expecting numeric operator+\
 NeedStrOp=Expecting string operator+\
 NeedRegex=Expecting regular expression+\
 NeedOpenP=Expecting open parenthesis+\
 NeedEOE=Expecting end of expression+\
 NeedChar=Expecting character code+\
 NeedEquate=Expecting equate pattern+\
 NeedArgName=Expecting macro argument name+\
 NeedArgVal=Expecting macro argument value+\
 NeedArgDflt=Expecting default argument value+\
 NeedHex=Expecting hex character pairs+\
"

# expression evaluation error/fatal messages

local initExprErr = "\
 BadOpenP=Unmatched open parenthesis+\
 BadCloseP=Unexpected close parenthesis+\
 BadFncArg=Unexpected function argument+\
 BadEOE=Unexpected end of expression+\
 DivZero=Divide by zero+\
"

# range error messages

local initRngErr = "\
 BadValue=Invalid value+\
 BadRngLo=Value less than mimimum+\
 BadRngHi=Value greater than maximum+\
"

# block processing error messages

local initBlockErr = "\
 BadBlock=Matching block structure not found+\
 BadNest=Unclosed nested block+\
 BadOutExp=Legal only inside block expansion+\
 BadInExp=Legal only outside block expansion+\
"

# cpu/instruction set error messages

local initInsErr = "\
 BadCPU=Unknown CPU+\
 NoCPU=CPU type not set+\
 BadPC=Invalid program counter+\
 BadMode=Invalid address mode+\
 BadAddr=Address out of range+\
 BadBranch=Branch out of range+\
 CPUFault=CPU Fault+\
 InsFault=Instruction Fault+\
"

# explicit segment info/error messages

local initSegErr = "\
 SegIsAO=Segment is absolute origin+\
 SegIsAE=Segment is absolute end+\
 SegIsRE=Segment is relative end+\
 SegIsRO=Segment is relative origin+\
 SegIsID=Segment is initialized+\
 SegIsPD=Segment is padded+\
 SegIsND=Segment is uninitialized+\
 SegIsCO=Segment is common+\
 BadSegUse=Program is monolithic+\
 BadSegOut=Legal only inside segment fragment+\
 BadSegAbs=Cannot make segment absolute+\
 BadSegFile=Invalid segment filename template+\
"

# program listing messages

local initProgList = "\
 ListFile=Listing File+\
 InsSet=Instruction Set+\
 NoListing=    (No data recorded)+\
 OBJECT=Object Code Listing+\
 LABELS=Symbol Table Listing+\
 SymNumCols=Numeric Name     Ref Cnt   Hex Value    Dec Value+\
 SymNumVals=Numeric Value    Ref Cnt   Hex Value    Dec Value+\
 SymStrCols=String Name      Ref Cnt   Value+\
 SymNum= %-18s %4s   $%8.02X  %11.11g+\
 SymStr= %-18s %4s  \"%s\"+\
 MacCols=Macro Name       Exp Cnt+\
 MacList= %-18s %4s+\
 XREF=Cross-Reference Listing+\
 XRefCols=  Event   Cnt   Listing  Source+\
 XRefName= %-18s+\
 XRefList=   %3s    %3d     %04d    %04d  %s+\
 XRefNlst=   %3s    %3d      -      %04d  %s+\
 XRefCnts=          %3d Events+\
 STATS=Assembly Statistics Listing+\
 SrcLine=Source lines+\
 ExpLine= Macro lines+\
 TotLine= Total lines+\
 DataVal=Data Values+\
 P1Time=Pass One time+\
 P2Time=Pass Two time+\
 TotTime=   Total time+\
 LinesSec=Lines/Sec+\
 ValSec=Values/Sec+\
 ObjSize=Object Bytes+\
 ExpCache=Expressions cached+\
 CacheHits= Cache hits+\
 SEGMENTS=Segment Map Listing+\
 SegCols=Num - Name      Hex Value    Dec Value+\
 SegName=%03d - %s+\
 SegOff=  Object Offset $%8.04X  %11.0f+\
 SegBeg=  Beg Address   $%8.04X  %11.0f+\
 SegSize=  Memory Size   $%8.02X  %11.0f+\
 SegData=  Data Bytes    $%8.02X  %11.0f+\
 SegPad=  Pad Bytes     $%8.02X  %11.0f+\
 SegEnd=  End Address   $%8.04X  %11.0f+\
"

# miscellaneous messages

local initMisc = "\
 NoEffect=No effect+\
 NotOpen=Can't open file+\
 UniqVal=Value already set+\
 CircInc=Circular inclusion of+\
 PrevInc=Previous inclusion of+\
 BadDef=Definition ignored+\
 BadPgWid=No printable width+\
 BadPgLen=No printable length+\
 BadPage=Bad page format+\
 StkEmpty=User stack empty+\
 StkNotEmpty=User stack not empty+\
 BadAssert=Assertion failed+\
"

# -----------------------------

# populate the message text array
# - there appears to be a bug in index() if a start position is specified
# and there is no match - the value returned is one less than the start
# position, not zero (as it's defined to be)
# - which mattered when a different approach to splitting up the
# initialization strings was used

local function addmesg(data) {

    local i
    local spec, eq, ndx, txt
    local mesgspec

    i = splitp( data, mesgspec, /[^ \+][^\+]+/ )
    do {
        spec = mesgspec[ i ]
        eq = index( spec, "=" )
        ndx = substr( spec, 1, eq - 1 )
        txt = substr( spec, eq + 1 )
        mesgText[ ndx ] = txt
    } while ( --i )
}

INIT {

    # internal error messages

    addmesg( initIntErr )

    # general status messages

    addmesg( initStatus )

    # source code error reporting messages

    addmesg( initErrReport )

    # limit reached messages

    addmesg( initLimitFatal )

    # token warning/error messages

    addmesg( initTokErr )

    # name\symbol\label messages

    addmesg( initNameErr )

    # odd use messages

    addmesg( initOddWarn )

    # expected token not found messages

    addmesg( initNeedErr )

    # conditional and/or macro block processing messages

    addmesg( initBlockErr )

    # cpu/instruction set error messages

    addmesg( initInsErr )

    # expression evaluation error/fatal messages

    addmesg( initExprErr )

    # range error messages

    addmesg( initRngErr )

    # explicit segment messages

    addmesg( initSegErr )

    # program listing messages

    addmesg( initProgList )

    # miscellaneous messages

    addmesg( initMisc )

    # set default error and warning limits

    CKdomax( "MAXERR", 25 )
    CKdomax( "MAXWARN", 50 )
}

# -----------------------------

# flag message element (often the token choked upon)

local function flag(this) {

    return( ": <" this ">" )
}

# get expanded message text

global function UMexpandtext(ndx) {

    if ( ndx in mesgText )
        return( mesgText[ndx] )
    else
        return( mesgText["BadMsg"] flag(ndx) )
}

# get expanded message text with flagged cause (if any)

local function expandcause(ndx, cause) {

    if ( CKok(cause) && cause )
        return( UMexpandtext(ndx) flag(cause) )
    else
        return( UMexpandtext(ndx) )
}

# -----------------------------
# write text to standard output
# -----------------------------

# show status message to user (

global function UMstatus(ndx) {

    local text

    text = ( ndx != "Version" ) ? UMexpandtext( ndx ) : CKverstr()

    print "***** " CKprintable(text) " *****"
}

global function UMfilestatus(ndx, name) {

    print CKprintable( UMexpandtext(ndx) ": " name )
}

# write message to standard output and error file (if any)

local function msgout(this) {

    this = CKprintable( this )
    print this
    if ( errlogFile )
        print this > errlogFile
}

# -----------------------------

# write error message w/o changing status of error log file
# - this is the error-reporting function used by the other
# error-reporting functions

local function firestop(ndx, cause) {

    msgout( "; *** " expandcause(ndx, cause) )
}

# handle "ECHO" psop
# - ECHO [mesg]
# - if no error log file, try to open one and then write message
# - note we can't use other existing error-checked functions to open
# an error log file, because if an error occurs we'll generate a
# runaway recursion !

global function UMdoecho(text) {

    local name

    if ( !errlogFile ) {
        name = CKcheckfname( "ERRFILE", "ERR" )
        if ( name ) {
            name = CKgetfpath( name )
            errlogFile = fopen( name, "wt" )
            if ( !errlogFile )
                firestop( "NotOpen", name )
        }
    }

    msgout( text )
}

# show circumstance and source text as part of error message

local function showsource(line, ndx) {

    local i
    local circumstance, token

    # this is the original line as the user wrote it
    # - comments have not been removed
    # - in a block expansion the entire line may be a comment

    $0 = SRCtext( ndx )

    # what does the assembler think is happening ?
    # - it may not be the same as what the user thinks !

    circumstance = "BadLine"
    token = ""
    for ( i = 1; (i <= NF) && ($i !~ /^;/); i++ ) {
        token = toupper( $i )
        if ( PSOPispseudo(token) ) {
            circumstance = "BadPsop"
            break
        }
        if ( MACismacro(token) ) {
            circumstance = "BadMacro"
            break
        }
        if ( INSisop(token) ) {
            circumstance = "BadOp"
            break
        }
        if ( i == 2 ) {                 # $1 must have been label...
            token = toupper( $1 )
            break
        }
        if ( SYMislabel(token) )
            circumstance = "BadLabel"
        else {                          # ...because if not we bailed
            circumstance = "BadToken"
            break
        }
    }

    msgout( "; - " expandcause(circumstance, token) )
    msgout( sprintf("; %04d:  %s", line, $0) )
}

# show error message and source code that triggered it

local function showmesg(type, ndx, cause) {

    local errln, srcln, expln
    local srcfl

    errln = SRCgetmaster()

    # no valid current source line (ie., before/between/after passes) ?

    if ( !errln )
        firestop( ndx, cause )

    else {

        SRCremember()

        # show the user source line nearest to error line
        # - they may not be the same if error occured during expansion

        srcln = expln = SRCnearest( errln )
        srcfl = CKgetfpath( SRCfile(srcln) )

        UMdoecho( "; >>>>>  " UMexpandtext(type) " " srcfl )
        showsource( SRCline(srcln), srcln )

        # if the error occured during block expansion, show the line
        # that actually caused the error and its offset from the current
        # user source line
        # - during the first pass we can also show any intervening lines
        # that themselves started expansions (ie., the block nesting)

        while ( errln != expln ) {
            expln = CKexpansionline( expln, errln )
            showsource( expln - srcln, expln )
        }

        # (finally) show what HXA doesn't like

        msgout( "; - " expandcause(ndx, cause) )
        msgout( "" )
    }

}

# -------------------------

# save current error count

global function UMmarkerr() { markErr[ ++markNdx ] = errCnt }

# check if error count changed since being saved

global function UMerrdiff() { return( markErr[ markNdx-- ] != errCnt ) }

# check if error count changed since being saved (do not change index)

global function UMerrsame() { return( markErr[ markNdx ] == errCnt ) }

# -------------------------

# show error count (if any)

local function showerrcnt(count, type) {

    if ( count )
        UMdoecho( count " " UMexpandtext(type) )

    # double logical not leaves zero at zero, changes non-zero to one

    return( !!count )
}

# quit on error

global function UMquitonerr() {

    local errcode

    # errcode is bit-mapped (SET= TRUE, CLEAR= FALSE)
    # - bit 0 : warning(s) happened
    # - bit 1 : error(s) happened
    # - bit 2 : fatal error happened

    errcode  = showerrcnt( warnCnt - warnPrv, "HaveWarn" )
    errcode += showerrcnt( errCnt, "HaveErr" ) * 2
    errcode += showerrcnt( fatalCnt, "HaveFatal" ) * 4

    # quit if error(s)

    if ( errcode > 1 ) {
        UMstatus( "Quit" )
        abort( errcode )
    }

    # save warn count through this pass
    # - next pass will show only warn count from that pass

    warnPrv = warnCnt
}

# --------------------------
# handle fatal error message
# --------------------------

# show fatal error message to user

global function UMfatal(ndx, cause) {

    showmesg( "Fatal", ndx, argcount() > 1 ? cause : "" )
    fatalCnt = 1
    UMquitonerr()
}

# "internal error" fatal error

global function UMnoway(this) { UMfatal( "NoWay", this ) }

# -------------------------------
# handle non-fatal error messages
# -------------------------------

# show non-fatal error message to user

global function UMerror(ndx, cause) {

    showmesg( "Error", ndx, argcount() > 1 ? cause : "" )
    CKcheckmax( "MAXERR", ++errCnt )
}

# "undefined name" error

global function UMundefname(this) { UMerror( "UndefName", this ) }

# "duplicate name" error

global function UMdupname(this) { UMerror( "DupName", this ) }

# "reserved name" error

global function UMreserved(this) { UMerror( "ReservedName", this ) }

# ----------------------------------
# handle non-fatal warning messages
# ----------------------------------

# show non-fatal warning to user

global function UMwarn(ndx, cause) {

    showmesg( "Warn", ndx, argcount() > 1 ? cause : "" )
    CKcheckmax( "MAXWARN", ++warnCnt )
}

# "token ignored" warning

global function UMignored(this) { UMwarn( "Ignored", this ) }

# "odd label" warning

global function UModdlabel(this) { UMwarn( "OddUse", this) }

# "odd value" warning

global function UModdval(this) { UMwarn( "OddVal", CKfmtval(this) ) }

# "no effect" warning

global function UMnoeffect(this) {

    UMwarn( "NoEffect", argcount() ? this : "" )
}

# ------------------

# handle "MESGTEXT" psop

global function UMdomesgtext(ndx, text) {

    if ( ndx in mesgText )
        mesgText[ ndx ] = CKprintable( text )
    else
        UMundefname( ndx )
}

