# Hobby Cross-Assembler (HXA) V0.200 - Instruction Set (CPU Descriptor Only)

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

# source language: Thompson AWK

# first created: 07/25/04
# last revision: 07/29/13

# public function prefix: "INS"

# ----------------------------

# - the test "processors" currently do nothing except set values in other
# HXA modules, which permits all non-processor specific aspects of HXA
# to be tested. This turns out to be most of HXA, actually :)
# - this file also provides a compact look at the functions which must
# be provided here and called elsewhere in order to adapt HXA to other,
# real processors

# Address modes: none

# Instruction set: none

# ----------------------------

# variant name

global function INSname() { return( "HXA_T" ) }

# reserved symbols - variant identifier

global function INSreserve() { return( "__HXA_T__" ) }

# is token a recognized CPU ?
# - here only the standard cpu desciptor: "T_PC_MB(SZ)"
# - "PC" is program counter size ("08" to "32" bits)
# - "MB" is orientation of multi-byte quantities, least- or most-
# significant byte first ("L" or "M")
# = "(SZ)" is optional "byte size", 8- or 16- or 32-bits

global function INSiscpu(this) {

    return( this ~ /^T_[0-9][0-9]_[LM](08|16|32)?$/i )
}

# return cpu descriptor
# - one and the same as name in this case

global function INSgetdescrip(this) { return(this) }

# setup cpu for use
# - nothing to do !

global function INSsetcpu(this) { }

# is token in current instruction set ?
# - there are no instructions in the set

global function INSisop(this) { return(FALSE) }

# process mnemonic and any associated expression(s)
# - since there aren't any mnemonics this shouldn't be called

global function INSdoop(mnemonic, cnt, expr) { return("NoWay") }

# handle "ASSUME" psop

global function INSdoassume(cmd, arg) {

    local str

    # "UMdoecho()" is *not* a function that's required to be called
    # by any cpu customization file; it's used here just to show
    # that the arguments have been passed correctly

    if ( arg == "" )
        arg = "[none]"

    str = "Cmd: " cmd " Arg: " arg

    UMdoecho( str )

    # we like anything but the string "reject"

    return( str !~ /reject/i )
}
