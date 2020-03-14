# Hobby Cross-Assembler (HXA) V0.201 - Macro Processing

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

# first created: 02/22/03
# last revision: 09/12/13

# public function prefix: "MAC"

# ------------------------------

# macro definitions

local macBegText, macEndText    # start and end lines of definition text
local macFormalArgs             # definition formal arguments
local macDefaultArgs            # default actual arguments
local macActualArgs             # supplied actual arguments

# "MACRO" definition tracking

local macName

# "MACRO" expansion nesting level
# - tracks current macro block expansions (only)

local macExpLevel = 0

# "REPEAT" and "WHILE" definition tracking

local blockSource, blockControl, blockVal

# "REPEAT" and "WHILE" expansion control

local repeatCount
local whileCond

# line skipping targets

local endMacro, endRepeat, endWhile

# source listing support

local macExpCnt             # macro expansion count

# ------------------------------

INIT {

    # ...for line skipping while collecting definitions

    CKpopulateNdx( endMacro,  "MACRO ENDMACRO" )
    CKpopulateNdx( endRepeat, "REPEAT ENDREPEAT" )
    CKpopulateNdx( endWhile,  "WHILE ENDWHILE" )

    # set default PUTBACK limit

    CKdomax( "MAXPUTBACK", 128 )
}

# ------------------------------

# putback text and tracking counters

local putbackText = ""
local putbackCnt = 0

#  block definition/expansion nesting level
# - zero = not active
# - tracks currently active blocks of all types

local defineLvl = 0
local expandLvl = 0

# indices to current fetch point, block start and block end
# at each expansion level

local sourceNdx = 0
local expandSrc, expandBeg, expandEnd

# ------------------------------

# begin definition block

local function begindefinition(type) {

    # note block start

    CKpushblock( type )

    # note current error count
    # - after definition complete, if this number
    # has changed we can ignore definition

    UMmarkerr()

    # TRUE if un-nested definition, FALSE if nested definition

    return ( 0 == defineLvl++ )
}

# end definition block

local function enddefinition(name) {

    # note block end

    CKpopblock()

    # definition errors ?
    # - only macro definitions actually have names

    if ( UMerrdiff() )
        UMwarn( "BadDef", name )

    # TRUE if un-nested definition, FALSE if nested definition

    return ( --defineLvl == 0 )
}

# ------------------------------

# begin expansion block

local function beginexpansion(type, start, end) {

    local count

    CKnewscope( type )

    expandSrc[ expandLvl ] = sourceNdx
    expandEnd[ ++expandLvl ] = end
    expandBeg[ expandLvl ] = sourceNdx = start
}

# end expansion block

local function endexpansion(label) {

    SYMhere( label )

    sourceNdx = expandSrc[ --expandLvl ]

    CKoldscope()
}

# setup another expansion (for looping)

local function nextexpansion() {

    sourceNdx = expandBeg[ expandLvl ]
}

# set expansion block text fetch index to (just before) end
# - next fetch will retrieve <END(type)> psop

local function exitexpansion() {

    sourceNdx = expandEnd[ expandLvl ] - 1
}

# is expansion active ?

global function MACexpanding() { return( expandLvl ) }

# kill all active expansions

global function MACkillexpansion() { expandLvl = 0 }

# get index of repeat/while expansion source text
# - first expansion level and macro expansions always fetch
# saved text that was read directly from file
# - we make nested expansions fetch from the current position of their
# parent so they will too (and never from expanded text)

local function expandsource() {

    return( expandLvl ? sourceNdx : SRCgetmaster() )
}

# ------------------------------

# handle "PUTBACK-" psops
# - PUTBACK [text [, text [..]]]
# - PUTBACKS [string [, string [..]]]

# PUTBACK re-joins all arguments as text so line is same as input,
# except leading "PUTBACK" and all whitespace between arguments is lost
# (label and opcode fields, if present, are preserved)
# PUTBACKS treats all arguments as strings and concatenates them into one

global function MACdoputback(this) {

    if ( !MACexpanding() )
        UMerror( "BadOutExp" )
    else {
        CKcheckmax( "MAXPUTBACK", ++putbackCnt )
        if ( this !~ CKignoreLine )
            putbackText = this
    }
}

# ------------------------------

# fetch block expansion line
# - returns non-blank non-comment source line
# - formal text arguments replaced with actual arguments if macro expansion

global function MACgetline() {

    local text
    local formalarg, actualarg

    # is there a putback line ?
    # - either zero or one such line

    if ( putbackText ) {
        text = putbackText
        putbackText = ""
    }

    # read from stored source text

    else {
        
        putbackCnt = 0

        do {
            if ( sourceNdx < expandEnd[expandLvl] )
                text = SRCtextf( ++sourceNdx )

            # ...an unclosed block definition within an expansion (fatal)

            else
                CKbadblock( CKexpandBlk )

        } while ( text ~ CKignoreLine )
    }

    # any active macro expansion (and formal argument(s) to expand) ?
    # - because we verified any formal arguments at definition time
    # and verified actual arguments at invocation time,
    # we know if we detect a formal argument we can substitute without
    # needing any more error checks

    if ( macExpLevel && match(text, SYMtxtArg_e) ) {

        # replace formal text arguments    

        do {
            formalarg = toupper( substr(text, RSTART, RLENGTH) )
            actualarg = macActualArgs[ macExpLevel ][ formalarg ]
            substr( text, RSTART, RLENGTH ) = actualarg
        } while ( match(text, SYMtxtArg_e, RSTART + length(actualarg)) )
    }

    return( text )
}

# ----------------------------

# - <EXIT>ing an expansion block skips all source code up to
# the end of the block
# - in general, if that source code was never examined by HXA prior to
# the <EXIT>, any errors it contains will remain undetected
# - in particular, because we want to permit the unconditional <EXIT>
# to be used within an open <IF> conditional block, when we remove any such
# from the top of the block stack we also actively prevent any unbalanced
# nested block errors from being reported
# - removal of "null" nested macro definition blocks is mainly bookkeeping

# handle "EXIT" and "EXITIF" psops
# - EXIT
# - EXITIF cond_expr

global function MACdoexit(yes) {

    local type

    if ( !MACexpanding() )
        UMerror( "BadOutExp" )

    else if ( yes ) {

        # discard nested block types we don't care about

        type = CKtopblocktype()
        while ( type == CKifBlk || type == CKmacNestBlk ) {
            CKpopblock()
            type = CKtopblocktype()
        }

        # the top block should now be an expansion block
        # - if not, there's still an unclosed nested block (fatal error)
        # - make sure repeat/while loops will stop at next <END(type)>

        if ( type == CKrptExpBlk )
            repeatCount[ expandLvl ] = 0
        else if ( type == CKwhlExpBlk )
            whileCond[ expandLvl ] = "0"
        else if ( type != CKmacExpBlk )
            CKbadblock( CKexpandBlk )

        # skip to <END(type)> line

        exitexpansion()
    }
}

# ------------------------------

# is token a defined macro name ?
# - names currently *being* defined are *not* defined

global function MACismacro(this) { return( this in macBegText ) }

# "undefine" a macro

local function doundef(this) {

    delete( macEndText[this] )
    delete( macDefaultArgs[this] )
    delete( macFormalArgs[this] )
    delete( macBegText[this] )    
}

# handle "UNDEF" psop
# - UNDEF name [[,name]..]

global function MACdoundef(this) {

    # record name even if it isn't (and never becomes) a defined macro

    SRCmacUndef( this )
    if ( MACismacro(this) )
        doundef( this )
}

# ------------------------------

# handle "MACRO" psop
# - MACRO name [,arg1 [,arg2 [..]]] (preferred form)
# - name MACRO [arg1 [,arg2 [..]]]

global function MACdomacro(name, formalcnt, formals) {

    local i
    local arg, namendx
    local equate
    local formalargs, checkargs, defaultargs

    # are we expanding a macro now ?
    # - if so, this begins nested definition
    # - note it and continue current expansion

    if ( MACexpanding() ) {
        CKpushblock( CKmacNestBlk )
        return
    }

    # a new macro definition

    begindefinition( CKmacDefBlk )

    # "name MACRO [args]" form of definition ?

    if ( name ) {
        namendx = 0
        name = SYMnormal( name )
    }

    # "MACRO name [args]" form of definition

    else {
        namendx = 1
        name = EXPgetglobal( formals[1] )
        --formalcnt
    }

    # validate name
    # - can't be already defined (anywhere)
    # - can't be in process of being defined (as macro)

    if ( CKok(name) && CKgoodname(name) && (name in macEndText) )
        UMdupname( name )
    if ( !UMerrsame() )
        name = ""

    # validate formal argument list names (if any),
    # and collect default actual arguments (if any)

    for ( i = 1; i <= formalcnt; ++i ) {
        arg = formals[ i + namendx ]

        # default actual argument supplied ?

        if ( index(arg, "=") ) {
            if ( !CKsplitequate(arg, FALSE, equate) )
                continue
            arg = equate[ 1 ]
            defaultargs[ i ] = equate[ 2 ]
        }

        # previous formal argument(s) had a default actual argument ?

        else if ( length(defaultargs) ) {
            UMerror( "NeedArgDflt", arg )
            continue
        }

        # formal name is legal ?

        arg = toupper( arg )
        if ( !(arg ~ SYMtxtArg || arg ~ SYMlblArg) ) {
            UMerror( "NeedArgName", arg )
            continue
        }

        # formal name is not a duplicate of previous name ?

        if ( arg in checkargs ) {
            UMdupname( arg )
            continue
        }

        # an error causes this step to be skipped

        formalargs[ i ] = arg       # for expansion use
        checkargs[ arg ] = i        # for verification use
    }

    # if no name or formal argument errors in this definition,
    # and no previous errors in any nesting definitions,
    # record partial definition
    # - still don't know location of the end of definition
    # - we use its storage location to temporarily store start location

    macName[ defineLvl ] = name
    if ( UMerrsame() ) {
        SRCmacDefine( name )
        macEndText[ name ] = SRCgetmaster()
        if ( length(formalargs) )
            macFormalArgs[ name ] = formalargs
        if ( length(defaultargs) )
            macDefaultArgs[ name ] = defaultargs
    }

    # error or not, skip to matching <ENDMACRO>

    PSOPskipto( endMacro )
}

# ------------------------------

# invoke a macro

global function MACinvoke(name, actualcnt, actualarg) {

    local i
    local formalcnt, formalname, actualtext
    local formalarg, replacement, equate

    # get the formal arguments (if any)

    if ( name in macFormalArgs )
        formalarg = macFormalArgs[ name ]

    formalcnt = length( formalarg )

    # note any extra actual arguments

    if ( actualcnt )
        CKmaxfields( actualarg, formalcnt )

    # create the replacement values (if any)

    UMmarkerr()
    for ( i = 1; i <= formalcnt; ++i ) {
        formalname = formalarg[ i ]

        if ( i <= actualcnt )
            actualtext = actualarg[ i ]
        else if ( i in macDefaultArgs[name] )
            actualtext = macDefaultArgs[ name ][ i ]
        else {
            UMerror( "NeedArgVal", formalname )
            continue
        }

        if ( formalname ~ SYMtxtArg )
            replacement[ formalname ] = actualtext
        else
            equate[ i ] = actualtext
    }

    # missing argument ?

    if ( UMerrdiff() )
        return

    # new expansion block

    beginexpansion( CKmacExpBlk, macBegText[name], macEndText[name] )
    SRCmacExpand( name )
    ++macExpCnt[ name ]

    # save actual text arguments

    macActualArgs[ ++macExpLevel ] = replacement

    # do assignments to label formal arguments
    # - assignments occur in the same order the formal arguments do in
    # the macro definition
    # - we wait until now to do the assigment so that implicit and
    # explicit assignment of formal arguments to labels behave identically
    # (particularly the effects of errors during explicit assigment)

    for ( i in equate )
        SYMdoequ( formalarg[i], equate[i] )

}

# ------------------------------

# verify macro arguments
# - check that text argument names in body match formal text arguments
# - we do this now mainly to speed up later processing, as we have to
# do these checks sometime and it's overall quicker to do it once now
# than at each invocation later
# - checking now also lets us show any error message close to the macro
# definition, rather than at each expansion, so it may be a little easier
# to fix
# - note we don't check *all* possible macro errors here

local function verifymacargs(name) {

    local i, j
    local end, cnt
    local text, argname
    local arg, checkargs

    # skip if the macro definition was in error at the first line
    # - but will continue even if errors in body line formats were reported
    # while looking for the end of the incorrect definition

    if ( name && (name in macEndText) ) {

        # put any formal arguments into a more convenient form for checking
        # - if there aren't any formal arguments we're checking for
        # their improper use within the definition body

        for ( i = length(macFormalArgs[name]); i > 0; i-- )
            checkargs[ macFormalArgs[name][i] ] = ".T."

        macBegText[ name ] = i   = macEndText[ name ]
        macEndText[ name ] = end = SRCgetmaster()
        while ( ++i < end ) {
            text = SRCtextf( i )
            if ( text !~ CKignoreLine ) {
                SRCsetmaster( i )
                cnt = splitp( text, arg, SYMtxtArg_e )
                for ( j = 1; j <= cnt; j++ ) {
                    argname = toupper( arg[j] )
                    if ( !(argname in checkargs) )
                        UMundefname( argname )
                }
            }
        }
        SRCsetmaster( end )
    }
}

# verify MACRO name and ENDMACRO name (if any) match
# - we did not have initial pseudo op processing grab any name
# provided to ENDMACRO because an error in it would stop processing
# (and leave the define stack unbalanced)

local function verifymacnames(atstart, atend) {

    if ( atstart && atend ) {
        atend = EXPgetglobal( atend )
        if ( CKok(atend) )
            CKmatch( atend, atstart )
    }
}

# handle "ENDMACRO" psop
# - [label] ENDMACRO [name]

global function MACdoendmacro(label, name_em) {

    local type, name_m

    type = CKtopblocktype()

    # end of a macro expansion ?
    # - terminate expansion

    if ( type == CKmacExpBlk ) {
        endexpansion( label )
        --macExpLevel
    }

    # end of a nested macro definition during expansion of nesting macro ?
    # - ignore, except for any label

    else if ( type == CKmacNestBlk ) {
        CKpopblock()
        SYMhere( label )
    }

    # terminating a definition ?
    # - name error at first definition line saves a "bad" name
    # - if name being defined is okay:
    # -   verify any formal arguments in definition body
    # -   if ending name provided, check that it matches current name
    # (note that one check at definition time all is we need to do)
    # - check if this is the first time the name has been defined
    # - finish definition
    # - if nested, also go on to finish enclosing definition

    else if ( type == CKmacDefBlk ) {
        name_m = macName[ defineLvl ]
        delete( macName[defineLvl] )
        verifymacargs( name_m )
        verifymacnames( name_m, name_em )
        if ( !UMerrsame() && name_m )
            doundef( name_m )
        else if ( !(name_m in macExpCnt) )
            macExpCnt[ name_m ] = 0
        if ( !enddefinition(name_m) )
            PSOPskipto( endMacro )
    }

    # an orphan !

    else {
        CKbadblock( CKmacroBlk )
    }
}

# -----------------------------------------
# Source Listing Support (macros only)
# -----------------------------------------

# list defined macro names

global function MACshow() {

    local name
    local format

    if ( SRCsubheader("MacCols", length(macExpCnt)) ) {
        format = UMexpandtext( "MacList" )
        for ( name in macExpCnt )         
            SRClist( sprintf(format, name, CKgetuse(name, macExpCnt)) )
    }
}

# free space

global function MACfree() {

    delete( macBegText )
    delete( macEndText )
    delete( macFormalArgs )
    delete( macDefaultArgs )
    delete( macActualArgs )
    delete( macName )

    delete ( macExpCnt )

    delete( repeatCount )
    delete( whileCond )
}

# -----------------------------------------
# common functions for REPEAT and WHILE
# -----------------------------------------

# save starting block definition values
# - there is at most only one block we're defining at a time
# (even if they're nested), so globals are okay
# - try to evaluate control expression (note loops nested within
# this one will not be formally "defined" until this one is expanded,
# so it is okay for control expressions of those loops to depend on values
# that will be set only during that expansion)

local function startblockdef(label, val) {

    SYMhere( label )
    blockSource = expandsource()
    blockVal    = val
}

# ------------------------------

# handle "REPEAT" psop
# - [label] REPEAT count
# - note that only the most recently encountered repeat is defined,
# but that any nested repeats will be defined in turn as they are
# encountered when the outer repeat is expanded (so they work properly)

global function MACdorepeat(label, expr) {

    if ( begindefinition(CKrptDefBlk) )
        startblockdef( label, EXPgetint(expr) )
    PSOPskipto( endRepeat )
}

# handle "ENDREPEAT" psop
# - [label] ENDREPEAT

global function MACdoendrepeat(label) {

    local type, gooddef

    type = CKtopblocktype()

    # end of an expansion block ?

    if ( type == CKrptExpBlk ) {
        if ( --repeatCount[expandLvl] > 0 )
            nextexpansion()
        else
            endexpansion( label )
    }

    # end of a definition block ?

    else if ( type == CKrptDefBlk ) {
        gooddef = UMerrsame()
        if ( !enddefinition("") )
            PSOPskipto( endRepeat )
        else if ( gooddef && (blockVal > 0) ) {
            beginexpansion( CKrptExpBlk, blockSource, expandsource() )
            repeatCount[ expandLvl ] = blockVal
        }
    }

    # an orphan !

    else {
        CKbadblock( CKrepeatBlk )
    }

}

# ------------------------------

# handle "WHILE" psop
# - [label] WHILE cond
# - note that only the most recently encountered while is defined,
# but that any nested whiles will be defined in turn when they
# encountered when the outer while is expanded (so they work properly)

global function MACdowhile(label, expr) {

    if ( begindefinition(CKwhlDefBlk) ) {
        startblockdef( label, EXPgetcondition(expr) )
        blockControl = expr
    }
    PSOPskipto( endWhile )
}

# handle "ENDWHILE" psop
# - [label] ENDWHILE

global function MACdoendwhile(label) {

    local type, gooddef

    type = CKtopblocktype()

    # end of an expansion block ?

    if ( type == CKwhlExpBlk ) {
        if ( EXPgetcondition(whileCond[expandLvl]) )
            nextexpansion()
        else
            endexpansion( label )
    }

    # end of a definition block ?

    else if ( type == CKwhlDefBlk ) {
        gooddef = UMerrsame()
        if ( !enddefinition("") )
            PSOPskipto( endWhile )
        else if ( gooddef && (blockVal != 0) ) {
            beginexpansion( CKwhlExpBlk, blockSource, expandsource() )
            whileCond[ expandLvl ] = blockControl
        }
    }

    # an orphan !

    else {
        CKbadblock( CKwhileBlk )
    }

}

