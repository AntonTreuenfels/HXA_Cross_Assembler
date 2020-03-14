# Hobby Cross-Assembler (HXA) V0.201 - Top-Level Executive

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

# first created: 03/01/03
# last revision: 09/17/13

# public function prefix: none (other modules cannot call this module)

# ---------------------------------

local exprCnt           # number of expressions found
local exprField         # one entry for each expression

# collect expression following pseudo op, mnemonic or macro (but not label)

local function getexpr(firsttoken) {

    local i

    # start fresh

    delete( exprField )

    # if we have all the tokens, there is no expression field

    if ( firsttoken > NF ) {
        exprCnt = 0
        exprField[ 1 ] = ""
        return( TRUE )
    }

    # if there's only one token left, it's the expression field
    # - we make sure it's a string ('0' is ambiguous)

    if ( firsttoken == NF )
        exprCnt = CKsplitfield( $NF "", exprField, CKfieldSep )

    # if more than one it's a bit harder because the only built-in way
    # to determine where a field begins is slower than we would like,
    # and we can't simply index() the first expression token because it
    # might also match all or part of a preceeding token in the line
    # - eg. "]CHAR = ]CHAR + 1"

    else {
        i = index( $0, $2, index($0, $1) + length($1) )
        if ( firsttoken == 3 )
            i = index( $0, $3, i + length($2) )
        exprCnt = CKsplitfield( substr($0, i), exprField, CKfieldSep )
    }

    # if zero, then field splitting turned up a blank field

    return( exprCnt )
}

# ---------------------------------

# dispatch assembler pseudo opcode

local function dopsop(label, psop, first) {

    if ( PSOPtakeline(psop) && getexpr(first) )
        PSOPdopseudo( label, psop, exprCnt, exprField )
}

# dispatch user-defined macro

local function domacro(label, macro, first) {

    if ( PSOPtaking() && getexpr(first) ) {
        SYMhere( label )
        MACinvoke( macro, exprCnt, exprField )
    }
}

# dispatch CPU opcode (mnemonic)

local function doop(label, op, first) {

    local errndx

    if ( PSOPtaking() && getexpr(first) ) {
        SYMhere( label )
        errndx = INSdoop( op, exprCnt, exprField )
        if ( errndx )
            UMerror( errndx )
    }
}

# dispatch user-defined label

local function dolabel(label) {

    if ( PSOPtaking() )
        SYMhere( label )
}

# ---------------------------------

# process source lines until file end

# accepted line patterns:

# label
# label     opcode
# label     opcode      expression(s)
# label     pseudo-op
# label     pseudo-op   expression(s)
# label     macro
# label     macro       arg(s)
#           opcode
#           opcode      expression(s)
#           pseudo-op
#           pseudo-op   expression(s)
#           macro
#           macro       arg(s)

local function dofile() {

    local token1, token2

    while ( ($0 = SRCnextline()) ) {
        token1 = toupper( $1 )
        if ( PSOPispseudo(token1) )
            dopsop( "", token1, 2 )
        else if ( MACismacro(token1) )
            domacro( "", token1, 2 )
        else if ( INSisop(token1) )
            doop( "", token1, 2 )
        else if ( !SYMislabel(token1) )
            UMerror( "NeedToken", token1 )
        else if ( NF == 1 )
            dolabel( token1 )
        else {
            token2 = toupper( $2 )
            if ( PSOPispseudo(token2) )
                dopsop( token1, token2, 3 )
            else if ( MACismacro(token2) )
                domacro( token1, token2, 3 )
            else if ( INSisop(token2) )
                doop( token1, token2, 3 )
            else
                UMerror( "NeedOpcode", token2 )
        }
    }
}

# ---------------------------------

# do pass one...

BEGIN {

    UMstatus( "Version" )

    # got a filename ?

    if ( ARGC < 2 )
        UMfatal( "NeedFile" )

    UMstatus( "BegOne" )
    CKbeginpass( 1 )

    # can it be opened ?

    CKsavepath( ARGV[1] )
    if ( SRCsetfile(CKbasename(ARGV[1]), 0, 0) ) {

        # process source files until there are no more

        do {
            dofile()
        } while ( SRCpopfile() )

        CKendsource()
    }

    CKendpass( 1 )
    UMstatus( "EndOne" )

    UMquitonerr()

    # stop the automatic input loop from engaging

    ARGI = ARGC
}

# do pass two, source listing and object generation

END {

    UMstatus( "BegTwo" )
    CKbeginpass( 2 )

    if ( PCmakeabsolute() ) {
        SYMmakeabsolute()
        CGresolve()
    }

    CKendpass( 2 )
    UMstatus( "EndTwo" )

    UMquitonerr()

    SRCshow()

    CGputobject()

    exit( 0 )
}

