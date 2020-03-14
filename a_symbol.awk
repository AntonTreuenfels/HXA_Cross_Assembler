# Hobby Cross-Assembler (HXA) V0.201 - Symbol Table Management

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
# last revision: 09/14/13

# public function prefix: "SYM"

# ----------------------------

# public constants

# for ease of consistent definition, all symbol/label patterns
# are defined here (even if not used in this file)

# base label form (default; most others add prefixes/suffixes to this)

global SYMglobal = /^[_A-Z](\.?[_A-Z0-9])*$/i

# label types differ based on their initial and final characters

global SYMglbLabel = /^[_A-Z]/      # global label
local varLabel     = /^]/           # variable label
global SYMlocLabel = /^@/           # local label
local autLabel     = /^&/           # auto label (internal form)

# "user label" - all the forms available to the user (in label field)

local userLabel_glv = /^[]@_A-Z](\.?[_A-Z0-9])*\$?:?$/

# branch target "auto-labels"
# - automatically replaced by assembler-generated labels

# branch target (in label field)

local branchLabel = /^(-\+?|\+-?|:)$/

# branch target references (in expression field)

local fwdtargetRef = /\++/
local baktargetRef = /-+/

# user labels recognized in expressions
# suffixes:
# - b  = branch target (decorated)
# - g  = global
# - l  = local
# - Ub = branch target (undecorated = complete expression)
# - v  = variable

global SYMnumLabel_glv = /^[]@_A-Z](\.?[_A-Z0-9])*\:?/i
global SYMnumLabel_b   = /^:(\++|-+)/
global SYMnumLabel_Ub  = /^(\++|-+)$/

global SYMstrLabel_glv = /^[]@_A-Z](\.?[_A-Z0-9])*\$\:?/i

# macro text formal argument name pattern
# - replaced by the text of its actual argument during macro expansion
# suffix:
# - e = embedded match (within longer string)

global SYMtxtArg   = /^\?(\.?[_A-Z0-9])+$/i
global SYMtxtArg_e = /\?(\.?[_A-Z0-9])+/i

# macro label formal argument name
# - assigned the value of its actual argument during macro expansion

global SYMlblArg = /^[]@](\.?[_A-Z0-9])+\$?$/i

# ----------------------------

# counters so assembler-generated auto-labels are unambiguous

local fwdCnt   = 0
local bkwdCnt  = 0
local localCnt = 0

# local symbol table
# - locals are nested by entering/leaving macro block expansions,
# include files and explicit segments
# - the overall nesting depth is thus the same as the current local nesting
# - locals at current nest depth are cleared whenever a global label
# is encountered (if the global is found in a macro expansion a
# warning message is generated because of potential multiple reference)
# - first symbol used can be local if desired (no restriction)

local localTable
local locDepth = 0

# symbol tables

local reserveTable              # reserved symbols
local symTable                  # user symbols
local refCnt                    # reference count

# ----------------------------

# initialization

INIT {

    # create reserved table of built-in label names
    # - "__HXA__" is common to all variants
    # - "INSreserve()" gets variant-specific names

    CKpopulateNdx( reserveTable, "__HXA__ " INSreserve() )
}

# check if token is reserved symbol
# - ie., a built-in label or function name

global function SYMisreserved(this) {

    return( (this in reserveTable) || EXPisfunc(this) )
}

# ----------------------------

# convert symbol token to "normalized" form
# - we already know that if there is a colon at all, it must be last char

global function SYMnormal(this) {

    if ( index(this, ":") )
        this = substr( this, 1, length(this) - 1 )
    return( toupper(this) )
}

# check if token is a legal label

global function SYMislabel(this) {

    # global, local, variable or branch (anonymous) ?

    return ( (this ~ userLabel_glv) || (this ~ branchLabel) )
}

# check if token is (or could be) a string label

local function strlabel(this) { return( index(this, "$") ) }

# warn if label is branch target

global function SYMwarnbranch(this) {

    if ( this ~ branchLabel )
        UModdlabel( this )
    return( this )
}

# -------------------------------------

# stack/unstack the current local label table

global function SYMpushlocals() { ++locDepth }

global function SYMpoplocals() { delete( localTable[locDepth--] ) }

# -------------------------------------

# create replacement for local label

local function makelocal(this) {

    local typ, replacement

    this = SYMnormal( this )
    if ( this in localTable[locDepth] )
        return( localTable[locDepth][this] )

    # internal form tagged w/ original name for easier listing id,
    # but limited to 16 chars total

    typ = strlabel( this ) ? "$" : "#"
    replacement = sprintf( "&%c%03X_%.10s", typ, ++localCnt, this )

    localTable[ locDepth ][ this ] = replacement
    return( replacement )
}

# create replacements for branch target labels

local function makefwd(num)  { return( sprintf("&+%03X", num) ) }
local function makebkwd(num) { return( sprintf("&-%03X", num) ) }

# replace auto label in expression

global function SYMreplaceauto(this) {

    # branch target label ?

    if ( match(this, fwdtargetRef) )
        return( makefwd(fwdCnt + RLENGTH) )
    if ( match(this, baktargetRef) )
        return( makebkwd(bkwdCnt - RLENGTH + 1) )

    # must be local label

    return( makelocal(this) )
}

# -------------------------------------

# add a symbol to table

global function SYMadd(this, val) {

    local ndx

    # a variable ?

    if ( this ~ varLabel )
        symTable[ SYMnormal(this) ] = val

    # a branch target ?

    else if ( this ~ branchLabel ) {

        if ( this ~ /[+:]/ )
            symTable[ makefwd(++fwdCnt) ] = val
        if ( this ~ /[-:]/ )
            symTable[ makebkwd(++bkwdCnt) ] = val
    }

    # ...must be a local or global

    else {

        # a local ?

        if ( this ~ SYMlocLabel )
            ndx = makelocal( this )

        # ... must be a global

        else {
            ndx = SYMnormal( this )
            if ( SYMisreserved(ndx) ) {
                UMreserved( ndx )
                return
            }
            SRCglbEquate( ndx )
            delete( localTable[locDepth] )
        }

        # new symbol ?

        if ( !(ndx in symTable) )
            symTable[ ndx ] = val

        # multiple assignment okay as long as always same value
        # - otherwise an error, but also a warning as an additional clue

        else if ( symTable[ndx] != val ) {
            UModdlabel( this )
            UMdupname( this )
        }
    }
}

# add a symbol with a value of the current program counter

global function SYMhere(this) {

    local pcval

    if ( this != "" ) {
        pcval = PCget()
        
        # if symbol is string AND pc is absolute, force string value
        # - if pc is relative a string's value will not be resolved
        # until the pc becomes absolute (which is what we want)

        if ( strlabel(this) && !PCgotrel(pcval) )
            pcval = pcval ""

        SYMadd( this, pcval )
    }
}

# handle "EQU" pseudo op
# - label EQU expr

global function SYMdoequ(label, expr) {

    local str, num

    # string label ?

    if ( strlabel(label) ) {
        str = EXPgetstr( expr )
        if ( CKok(str) ) {
            SYMadd( label, str )
            if ( SRCshowequ(label) )
                CGsavenonobjstr( 0, str )
        }
    }

    # numeric label

    else {
        num = EXPgetint( expr )
        if ( CKok(num) ) {
            SYMadd( label, num )
            if ( SRCshowequ(label) )
                CGsavenonobjnum( 0, num )
        }
    }
}

# ----------------------------

# determine if user symbol exists

global function SYMexists(this) {

    # all current callers have already normalized symbol

#    this = SYMnormal( this )

    # global symbol cross-reference (if enabled)

    if ( this ~ SYMglbLabel )
        SRCglbReference( this )

    # user symbols

    if ( this in symTable ) {
        refCnt[ this ] += 1
        return( TRUE )
    }

    # unknown symbols - forward reference to variable label ?

    if ( this ~ varLabel )
        UModdlabel( this )

    return( FALSE )
}

# look up a symbol's value (known to exist)

global function SYMvalue(this) { return( symTable[this] ) }

# determine if reserved or user symbol exists

global function SYMdefined(this) {

    return( (this in reserveTable) || SYMexists(this) )
}

# -------------------------
# Pass Two-only functions
# -------------------------

# make sure all labels represent absolute values

global function SYMmakeabsolute() {

    local label
    local val

    # convert relative pc values to absolute (both numeric and string labels)

    for ( label in symTable ) {
        val = symTable[ label ]
        if ( PCgotrel(val) ) {
            val = PCgetabs( val )
            symTable[ label ] = strlabel( label ) ? val "" : val
        }
    }
}

# ----------------------------
# Source Listing Support
# ----------------------------

# list numeric symbol value

local function shownum(format, name, val) {

    local s

    s = sprintf( format, name, CKgetuse(name, refCnt), EXPint(val), val )
    SRClist( s )
}

# list string symbol value

local function showstr(format, name, val) {

    local s

    s = sprintf( format, name, CKgetuse(name, refCnt), CKprintable(val) )
    SRClist( s )
}

# list symbol table

global function SYMshow(autoflag) {

    local format
    local name, val
    local numsym, strsym, valsym

    # create tables of listable names

    for ( name in symTable ) {
        if ( (name !~ autLabel) || autoflag ) {
            val = symTable[ name ]
            if ( strlabel(name) )
                strsym[ name ] = val                # string alphabetic
            else {
                if ( name !~ autLabel )             # distracting...
                    numsym[ name ] = val            # numeric alphabetic
                if ( name !~ varLabel )             # distracting...
                    valsym[ val ][ name ] = ".T."   # numeric by value
            }
        }
    }

    # list numeric symbols
    # - alphabetic ordering by name (which are all unique)
    # - global and variable symbols

    if ( SRCsubheader("SymNumCols", length(numsym)) ) {

        # name, reference count, hex value, decimal value

        format = UMexpandtext( "SymNum" )
        for ( name in numsym )
            shownum( format, name, numsym[name] )
    }

    SRCnewline()

    # list numeric symbols
    # - numeric ordering by value (which may not unique)
    # - global and local symbols

    if ( SRCsubheader("SymNumVals", length(valsym)) ) {

        # name, reference count, hex value, decimal value

        format = UMexpandtext( "SymNum" )
        for ( val in valsym ) {
            for ( name in valsym[val] )
                shownum( format, name, val )
        }
    }

    SRCnewline()

    # list string symbols
    # - alphabetic ordering by name
    # - global, local and variable symbols

    if ( SRCsubheader("SymStrCols", length(strsym)) ) {

        # name, reference count, value

        format = UMexpandtext( "SymStr" )
        for ( name in strsym )
            showstr( format, name, strsym[name] )
    }

    SRCnewline()
}

# free space

global function SYMfree() {

    delete( localTable )

    delete( symTable )
    delete( refCnt )
}
