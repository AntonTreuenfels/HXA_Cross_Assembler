# Hobby Cross-Assembler (HXA) V0.201 - Pseudo Opcode Handler

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

# first created: 03/08/03
# last revision: 09/15/13

# public function prefix: "PSOP"

# - many but not all psops are completely or partially handled here
# - conditional assembly psops are completely handled here

# ----------------------------

# source line "taking" flag

local takeLine = TRUE       # when FALSE, source lines are being skipped

# sets of psops we might watch for when we're skipping lines

local nextCond, endIf

# psops we're actually watching for when we're skipping lines

local targetPsop

# conditional nesting and line skipping

local ifLevel
local foundTrue, inTrueBranch

# built-in pseudo opcodes

# psop processing rules - 1st char= label, 2nd= expression
# - if capitalized, argument type is required
# - if not capitalized, argument type is checked whenever present

# "a" = accepted if present (optional; not evaluated)
# "c" = conditional (numeric or string expression)
# "f" = filename (defaults to name of first file used if not present)
# "g" = global name (literal or string expression)
# "h" = hex string (opt-string)
# "i" = ignored if present (warning message generated)
# "l" = label (assigned current pc value; branch target labels odd)
# "n" = numeric argument (forward reference ok)
# "o" = opt-string (string expression or "as-is")
# "p" = numeric argument (no forward reference; negative odd)
# "q" = equate string (string expression or "as-is")
# "r" = argument required (not evaluated)
# "s" = string constant (numbers converted to one-char strings)
# "u" = user label (assigned expression value; branch target labels odd)

# "+" = one or more identical arguments allowed (each evaluated)
# "&" = one or more identical arguments allowed (evaluated; concatenated)
# "!" = one or more undifferentiated arguments allowed (none evaluated)

local psOpcode

# user-available pseudo-ops

local userCodes = "\
 CPU=iO \
 EQU=UR DS=lP PADTO=iPh FILL=lPh \
 ORG=uP END=la \
 STRING+STRINGR=lS& \
 HEX=lH& \
 MACRO=aa! ENDMACRO=aa \
 EXIT=ii EXITIF=iC \
 PUTBACK=ia! PUTBACKS+PUSHS=iS& \
 UNDEF=iG+ \
 REPEAT+WHILE=aR \
 ENDREPEAT+ENDWHILE=ai \
 IF+ELSEIF+IFDEF+IFNDEF+ASSERT=iR \
 ELSE+ENDIF=ii \
 INCLUDE=iF INCBIN=iFpp READONCE=ii \
 LISTON+LISTOFF=ia! \
 PAGESIZE=iPp MARGINS=iPPpp LINESPACE=iP TITLE=io PAGE=ii \
 LISTFILE+ERRFILE=if \
 OBJFILE+HEXFILE+SRECFILE=if \
 OBJBYSEG+HEXBYSEG+SRECBYSEG=if \
 OBJBYBLOCK+HEXBYBLOCK+SRECBYBLOCK=if \
 MAXERR+MAXWARN+MAXDEPTH+MAXPUTBACK+MAXSTACK=iP \
 ECHO+WARN+ERROR+FATAL=io \
 PSALIAS+MESGTEXT=iQ+ \
 PSNULL=aa! \
 XLATE=iQ+ \
 ASSUME=iO+ \
 STARTTIMER+STOPTIMER+SHOWTIMER=iG \
 SEGMENT=iG ENDSEGMENT=ig ABSEND=uP \
 USESEGMENTS+RELORG+RELEND+UNINITIALIZED+COMMON=ii \
"

# built-in aliases for pseudo ops

local psAlias

local initAlias = "\
 ==EQU *=+$=+ABSORG=ORG \
 STR=STRING REVSTR=STRINGR \
 MAC=MACRO ENDM=ENDMACRO \
 ONEXPAND=PUTBACK PUTSTR=PUTBACKS \
 ENDR=ENDREPEAT ENDW=ENDWHILE \
 ENDS=ENDSEGMENT \
 NODATA=UNINITIALIZED \
"

# "required argument missing" error messages

local needType

local initNeed = "\
 C+N+P=NeedNum F=NeedFile G=NeedGlb H=NeedHex \
 O+S=NeedStr Q=NeedEquate R=BadField \
"

# add one "-BIT--" data type psop and alias (if any)
# - all have the same label and expression argument types

global function PSaddbitop(prefix, basename, alias) {

    local name

    name = prefix basename
    psOpcode[ name ] = "lN+"
    if ( alias )
        psAlias[ prefix alias ] = name
    return( name )
}

# ----------------------------

INIT {

    # pseudo-ops

    CKpopulate( psOpcode, userCodes )

    # built-in aliases for pseudo-ops

    CKpopulate( psAlias, initAlias )

    # required argument missing error messages

    CKpopulate( needType, initNeed )

    # lowest level of conditional nesting is "dummy"
    # - treats entire source program as being within the "TRUE" branch
    # of a conditional (so first actual "IF" needs no special treatment)

    ifLevel = 0
    truebranch()

    # psops we're looking for if we want to check another condition

    CKpopulateNdx( nextCond, "IF IFDEF IFNDEF ELSEIF ELSE ENDIF" )

    # psops we're looking for if we don't want to check another condition

    CKpopulateNdx( endIf, "IF IFDEF IFNDEF ENDIF" )

}

# ----------------------------

# convert token to "normalized" form
# - note acceptance of optional leading '.' or '#' characters
# - allowed for visual distinctiveness ("manifest type", if you wish)

local function normalized(this) {

    if ( this ~ /^[\.#]/ )
        this = substr( this, 2 )
    return( (this in psAlias) ? psAlias[this] : this )
}
 
# check if token is a pseudoop

global function PSOPispseudo(this) {

    return ( normalized(this) in psOpcode )
}

# handle "PSALIAS" psop

local function dopsalias(psop, alias) {

    psop = toupper( psop )
    if ( !PSOPispseudo(psop) )
        UMundefname( psop )
    else {
        alias = toupper( alias )
        if ( CKgoodname(alias) )
            psAlias[ alias ] = normalized( psop )
    }
}

# ----------------------------

# is argument string expression or undelimited string literal ?

local function isstrexpr(this) {

    return ( this ~ CKstrLitToken || this ~ SYMstrLabel_glv )
}

# collect optional string/not-string argument

local function get_optstr(this) {

    local str

    # if not string expression, return "as-is"
    # - escape codes not processed

    if ( !isstrexpr(this) )
        return( this )

    # string expression must be legal and non-null

    str = EXPgetstr( this )
    if ( CKok(str) ) {
        if ( str )
            return( str )
        UModdval( str )
    }

    return( CKfail )
}

# collect hex data

local function get_hex(this) {

    local i
    local hexstr
    local data

    hexstr = get_optstr( this )
    if ( CKok(hexstr) ) {

        if ( match(hexstr, CKoptHexPat) ) {
            match( hexstr, /[0-9A-F]{2,}/i )
            data = ""
            for ( i = RSTART+RLENGTH-2; i >= RSTART; i -= 2 )
                data = sprintf( "%c", CKhextonum(substr(hexstr, i, 2)) ) data
            return( data )
        }
    }

    # null, non-hex char(s) or odd count

    UMerror( "NeedHex", hexstr )
    return( CKfail )
}

# collect filename argument

local function get_fname(this) {

    local name

    # remove any enclosing angle brackets
    # - prior to v0.180 a literal (non-string expression) argument
    # required enclosing angle brackets (for reasons that seem silly now)
    # - now they are not required but are removed if ever present, which
    # means that the argument can simply be another opt-string

    name = get_optstr( this )
    if ( CKok(name) ) {
        gsub( /^<|>$/, "", name )
        if ( name )
            return( name )
    }

    # null name or illegal expression (undisplayable either way)

    UMerror( "NeedFile" )
    return( CKfail )
}

# collect equate string arguments

local function get_equate(this) {

    local flag, str
    local result

    flag = isstrexpr( this )
    str  = get_optstr( this )
    if ( CKsplitequate(str, flag, result) )
        return( result )

    return( CKfail )
}

# collect numeric value (warn if not positive)

local function getposnum(this) {

    local val

    val = EXPgetint( this )
    if ( CKok(val) && (val < 0) )
        UModdval( val )
    return( val )
}

# ----------------------------

# get psop argument expression value

local function getarg(want, this) {

    # non-null ?

    if ( this ) {
        want = toupper( want )
        if ( want == "N" )
            return( EXPgetnum(this) )
        if ( want == "S" )
            return( EXPgetstring(this) )
        if ( want == "O" )
            return( get_optstr(this) )
        if ( want == "C" )
            return( EXPgetcondition(this) )
        if ( want == "G" )
            return( EXPgetglobal(this) )
        if ( want == "P" )
            return( getposnum(this) )
        if ( want == "Q" )
            return( get_equate(this) )
        if ( want == "H" )
            return( get_hex(this) )
        if ( want == "F" )
            return( get_fname(this) )
        if ( want != "I" )              # want = "A", "R"
            return( this )
        UMignored( this )
    }

    # need non-null ?

    else if ( want in needType ) {
        UMerror( needType[want] )
        return( CKfail )
    }

    # default value is zero if numeric, null string otherwise
    # - "ignored" values converted to null string

    return( index("cnp", want) ? 0 : "" )
}

# ----------------------------

# check any label supplied to psop
# - if "this" is non-null, it is guaranteed to have the form of
# a legal user label (so we don't have to check that)
# - note only the EQU psop can cause an error here because so far only
# it requires a label argument ("U") (others can cause warnings only)

local function checklabel(want, this) {    

    # non-null ?

    if ( this ) {
        want = toupper( want )
        if ( want == "U" )
            SYMwarnbranch( this )
        else if ( want == "L" )
            SYMhere( SYMwarnbranch(this) )
        else if ( want == "I" )
            UMignored( this )
#       else if ( want != "A" )
#           UMnoway( "verifylabel" )
    }

    # need non-null ?

    else if ( want == "U" )
        UMerror( "NeedLabel" )
}

# ----------------------------

# enable line skipping until a target psop is encountered

global function PSOPskipto(accept) {

    targetPsop = accept
    takeLine = FALSE
}

# can we dispatch the current input line ?
# - for all dispatches except pseudo ops
# - if not, we're going to ignore it

global function PSOPtaking() { return( takeLine ) }

# can we dispatch the current source line ?
# - for pseudo op dispatch only
# - if skipping enabled, does current line contain target psop ?
# - if not, we're going to ignore it
# - if so, we're going to stop skipping (thus skipping never goes more than
# one level deep - although it can be begun again immediately if desired)

global function PSOPtakeline(psop) {

    if ( !takeLine )
        takeLine = normalized( psop ) in targetPsop

    return( takeLine )
}    

# ----------------------------
# Conditional Assembly Psops
# ----------------------------

# mark conditional branch as true

local function truebranch() {

    SRCsetcond( foundTrue[ifLevel] = inTrueBranch[ifLevel] = TRUE )
}

# mark conditional branch as false and skip to end

local function falsebranch() {

    SRCsetcond( inTrueBranch[ifLevel] = FALSE )
    PSOPskipto( endIf )
}

# check conditional result

local function testif(val) {

    # if couldn't evaluate we're done with this branch altogether

    if ( !CKok(val) )
        falsebranch()

    # if TRUE result mark it and continue this branch

    else if ( val )
        truebranch()

    # if FALSE result mark it and skip to next condition

    else {
        SRCsetcond( foundTrue[ifLevel] = inTrueBranch[ifLevel] = FALSE )
        PSOPskipto( nextCond )
    }
}

# ----------------------------

# handle "IF" psop

local function doif(expr) {

    CKpushblock( CKifBlk )
    if ( inTrueBranch[ifLevel++] )
        testif( EXPgetcondition(expr) )
    else
        falsebranch()
}

# handle "ELSEIF" psop

local function doelseif(expr) {

    if ( CKtopblock(CKifBlk) ) {
        if ( !foundTrue[ifLevel] )
            testif( EXPgetcondition(expr) )
        else
            falsebranch()
    }
}

# handle "ELSE" psop

local function doelse() {

    if ( CKtopblock(CKifBlk) ) {
        if ( !foundTrue[ifLevel] )
            truebranch()
        else
            falsebranch()
    }
}

# handle "ENDIF" psop

local function doendif() {

    if ( CKtopblock(CKifBlk) ) {
        CKpopblock()
        if ( inTrueBranch[--ifLevel] )
            SRCsetcond( TRUE )
        else
            PSOPskipto( nextCond )
    }
}

# ----------------------------

# evaluate name argument to "IFxDEF" psop
# - a two-step procedure

local function evalname(expr, wantit) {

    local name

    name = EXPgetglobal( expr )
    if ( CKok(name) )
        return( wantit ? SYMdefined(name) : !SYMdefined(name) )

    return( CKfail )
}

# handle "IFDEF" psop

local function doifdef(expr) {

    CKpushblock( CKifBlk )
    if ( inTrueBranch[ifLevel++] )
        testif( evalname(expr, TRUE) )
    else
        falsebranch()
}

# handle "IFNDEF" psop

local function doifndef(expr) {

    CKpushblock( CKifBlk )
    if ( inTrueBranch[ifLevel++] )
        testif( evalname(expr, FALSE) )
    else
        falsebranch()
}

# ----------------------------

# handle "ASSUME" psop

local function doassume(str) {

    local this
    local cnt
    local assume

    this = tolower( str )
    if ( (cnt = CKsplitfield(this, assume, ":")) == 1 )
        cnt = CKsplitfield( this, assume, "=" )

    if ( cnt ) {
        CKmaxfields( assume, 2 )
        if ( !CGdoassume(assume[1], assume[2]) ) {
            if ( !INSdoassume(assume[1], assume[2]) ) {
                UMignored( str )
            }
        }
    }
}

# ----------------------------

# dispatch pseudo-op

global function PSOPdopseudo(label, psop, expcnt, expr) {

    local i
    local argcode
    local lbltype, argtype
    local arglen
    local arg, val

    psop = normalized( psop )
    argcode = psOpcode[ psop ]

    # isolate label and first argument types
    # - we always want to check the first argument, because even if
    # there isn't one (ie., expcnt = 0), then we need to check that
    # not having one is legal (ie., expr[1] = "" is okay)

    lbltype = substr( argcode, 1, 1 )
    argtype = substr( argcode, 2, 1 )

    # single argument, single call ?

    if ( length(argcode) == 2 ) {

        # collect argument

        UMmarkerr()
        CKmaxfields( expr, 1 )
        checklabel( lbltype, label )
        val = getarg( argtype, expr[1] )
        if ( UMerrdiff() )
            return

        # "END--" psops

        if ( psop ~ /^END/ ) {
            if ( psop == "ENDIF" )
                doendif()
            else if ( psop == "ENDREPEAT" )
                MACdoendrepeat( label )
            else if ( psop == "ENDWHILE" )
                MACdoendwhile( label )
            else if ( psop == "ENDMACRO" )
                MACdoendmacro( label, val )
            else if ( psop == "ENDSEGMENT" )
                PCdoends( val )
            else if ( psop == "END" )
                SRCdoend( val )
            else
                UMnoway( psop )
        }
            
        else if ( psop == "IF" )
                doif( val )
        else if ( psop == "EQU" )
            SYMdoequ( label, val )
        else if ( psop == "ELSE" )
            doelse()
        else if ( psop == "ELSEIF" )
            doelseif( val )
        else if ( psop == "EXITIF" )
            MACdoexit( val )
        else if ( psop == "EXIT" )
            MACdoexit( TRUE )

        # execute less frequent psops (?)

        else if ( psop == "SEGMENT" )
            PCdosegment( val )
        else if ( psop == "REPEAT" )
            MACdorepeat( label, val )
        else if ( psop == "WHILE" )
            MACdowhile( label, val )
        else if ( psop == "DS" )
            PCdods( val )

        # execute least frequent psops (?)

        else if ( psop == "IFDEF" )
            doifdef( val )
        else if ( psop == "IFNDEF" )
            doifndef( val )
        else if ( psop == "ABSEND" )
            PCdoabsend( label, val )
        else if ( psop == "RELEND" )
            PCdorelend()
        else if ( psop == "RELORG" )
            PCdorelorg()
        else if ( psop == "UNINITIALIZED" )
            PCdouninitialized()
        else if ( psop == "COMMON" )
            PCdocommon()

        else if ( psop == "INCLUDE" )
            SRCdoinclude( val )
        else if ( psop == "READONCE" )
            SRCdoreadonce()

        # user feedback

        else if ( psop == "ECHO" )
            UMdoecho( val )
        else if ( psop == "ERROR" )
            UMerror( "Fault", val )
        else if ( psop == "WARN" )
            UMwarn( "Fault", val )
        else if ( psop == "ASSERT" )
            EXPdoassert( val )

        else if ( psop == "STOPTIMER" )
            CKdostoptimer( val )
        else if ( psop == "STARTTIMER" )
            CKdostarttimer( val )
        else if ( psop == "SHOWTIMER" )
            CKdoshowtimer( val )

        # typically executed once, sometimes a few times

        else if ( psop == "ORG" )
            PCdoorg( label, val )

        else if ( psop == "CPU" )
            CGdocpu( val )

        else if ( psop ~ /FILE|BYSEG|BYBLOCK/ )
            CKdofilename( psop, val )

        else if ( psop ~ /^MAX(ERR|WARN|DEPTH|PUTBACK|STACK)$/ )
            CKdomax( psop, val )

        else if ( psop == "PAGE" )
            SRCdopage()

        else if ( psop == "TITLE" )
            SRCdotitle( val )

        else if ( psop == "LINESPACE" )
            SRCdolinespace( val )

        else if ( psop == "USESEGMENTS" )
            PCdousesegments()

        # execute only once

        else if ( psop == "FATAL" )
            UMfatal( "Fault", val )

        # oops!

        else
            UMnoway( psop )
    }

    # multiple identical arguments get a separate call each time ?

    else if ( index(argcode, "+") ) {

        checklabel( lbltype, label )

        i = 1
        do {
            val = getarg( argtype, expr[i] )
            if ( CKok(val) ) {
                if ( index(psop, "BIT") )
                    CGsavedata( psop, val )
                else if ( psop == "ASSUME" )
                    doassume( val )
                else if ( psop == "UNDEF" )
                    MACdoundef( val )
                else if ( psop == "MESGTEXT" )
                    UMdomesgtext( val[1], val[2] )
                else if ( psop == "PSALIAS" )
                    dopsalias( val[1], val[2] )
                else if ( psop == "XLATE" )
                    CKdoxlate( val[1], val[2] )
                else
                    UMnoway( psop )
            }
        } while ( ++i <= expcnt )
    }

    # multiple string-ish arguments get concatenated for a single call ?

    else if ( index(argcode, "&") ) {

        # collect all arguments

        UMmarkerr()
        checklabel( lbltype, label )
        val = getarg( argtype, expr[1] )
        for ( i = 2; i <= expcnt; i++ ) {
            arg = getarg( argtype, expr[i] )
            if ( UMerrsame() )
                val = val arg
        }
        if ( UMerrdiff() )
            return

        # dispatch

        if ( psop == "PUSHS" )      # only PUSHS accepts a null string...
            CKdopush( val )
        else if ( val == "" )       # ...because only it does not affect code
            UMnoeffect()
        else if ( psop ~ /^STRING/ )
            CGdostr( psop, val )
        else if ( psop == "HEX" )
            CGsavestr( val )
        else if ( psop == "PUTBACKS" )
            MACdoputback( val )
        else
            UMnoway( psop )
    }

    # multiple undifferentiated arguments get a single call ?
    # - or non-call, in the case of PSNULL

    else if ( index(argcode, "!") ) {

        # only check label here because these
        # pseudo ops make other unique checks themselves

        checklabel( lbltype, label )
        if ( psop == "MACRO" )
            MACdomacro( label, expcnt, expr )
        else if ( psop == "PUTBACK" )
            MACdoputback( CKjoinfields(expr) )
        else if ( psop ~ /^LISTO(N|FF)/ )
            SRCdolistopt( psop, expcnt, expr )
        else if ( psop != "PSNULL" )
            UMnoway( psop )
    }

    # fixed number of distinct arguments get a single call

    else {

        # check and collect all arguments
        # - arguments not actually provided default to null string
        # - their evaluation can result in zero or the null string

        UMmarkerr()
        arglen = length( argcode ) - 1
        CKmaxfields( expr, arglen )
        checklabel( lbltype, label )
        for ( i = 1; i <= arglen; i++ ) {
            argtype = substr( argcode, i+1, 1 )
            arg[ i ] = getarg( argtype, (i <= expcnt) ? expr[i] : "" )
        }
        if ( UMerrdiff() )
            return

        # dispatch

        val = arg[ 1 ]                  # eh, maybe saves a little space
        if ( psop == "FILL" )
            PCdofill( val, arg[2] )
        else if ( psop == "PADTO" )
            PCdopadto( val, arg[2] )
        else if ( psop == "INCBIN" )
            SRCdoincbin( val, arg[2], arg[3] )
        else if ( psop == "PAGESIZE" )
            SRCdopagesize( val, arg[2] )
        else if ( psop == "MARGINS" )
            SRCdomargins( val, arg[2], arg[3], arg[4] )
        else
            UMnoway( psop )
    }
}
