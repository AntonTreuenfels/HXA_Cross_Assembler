# Hobby Cross-Assembler (HXA) V0.201 - Expression Evaluation

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

# first created: 04/09/02
# last revision: 09/05/13

# public function prefix: "EXP"

# - expressions are converted to Reverse Polish Notation (RPN)
# before evaluation

# -----------------------------

# operator tokens

local num1_Oprtr    # unary arithmetic, bitwise, logical and extract
local num2_Oprtr    # binary arithmetic, bitwise and logical
local str2_Oprtr    # binary string logical
local funcToken     # built-in functions

# constants

local opPrecedence, acceptToken

# HXA arithmetic is meant for 32-bit integers, signed and unsigned
# - TAWK automatically converts to doubles values that won't fit
# into 32-bit signed integers
# - this permits intermediate results in expressions to have magnitudes
# larger than will fit into 32-bit integers without necessarily losing
# precision (which is maintained to about 15 decimal digits)
# - as long as the final result fits into 32 bits (about 10 decimal digits),
# HXA generally doesn't care

local minSigned   = -2147483648     # -2^31
local maxSigned   =  2147483647     #  2^31 - 1
local maxUnsigned =  4294967295     #  2^32 - 1

# tokens found in expressions
# - most such tokens are defined and used only here, but some which
# are used in multiple files are defined elsewhere for ease of
# consistency of definition (such as symbol/label tokens)

# hexadecimal literal

local hexNumToken  = /^(\$|0X)[0-9A-F]+|^[0-9][0-9A-F]*H/i

# binary literal

local binNumToken  = /^(%|0B)[01]+|^[01]+B/i

# decimal literal

local decNumToken  = /^[0-9]+D?/i

# program counter

local pcToken      = /^[*$]/

# unary arithmetic, bitwise, logical and extract operators

local num1_OpToken = /^[-+~!<>^]/

# binary arithmetic, bitwise and logical operators

local num2_OpToken = /^([-+*/%&^\|<>]|<<|>>|==|!=|<=|>=)/

# logical short circuit operators

local lsc_OpToken  = /^(\?|\|\||&&)/

# end of sub-expression operators

local eoe_OpToken  = /^[:,]/

# open parenthesis

local openPToken   = /^\(/

# close parenthesis

local closePToken  = /^\)/

# binary string comparison operator

local str2_OpToken = /^(<=?|>=?|==|!=)/

# binary pattern match operator

local pat2_OpToken = /^!?~/

# conversion phase variables

local currToken         # current expression token
local currExpr          # remainder of expression being converted

local rpnExpr           # built-up RPN expression
local rpnPtr

local parseResult       # occasionally used to report result of a step

local needType          # "type mismatch" error messages

local initNeed = " \
 G=NeedGlb N=NeedNum R=NeedRegex S=NeedStr V=BadFncArg \
 N2S=NeedStr S2N=NeedNum \
"

# evaluation caches

local maxCache = 256

local rpnHits, rpnPurge, rpnCache

# built-in functions

# CHR$      evaluate numeric expression and convert to single char string
# CPU$      name of current CPU
# DEFINED   macro name defined ?
# EMPTY     user stack empty ?
# FILE$     current source file name (only; no path)
# FORWARD   string as numeric expression contains forward reference ?
# INDEX     substring position (from left)
# INDEXR    substring position (from right)
# LABEL     label name defined ?
# LEN       string length
# MATCH$    substring extraction (by pattern matching)
# MID$      substring extraction (by numeric index)
# MESG$     built-in message texts
# ORD       numeric value of string character
# PEEK$     selected item from user stack (not removed)
# POP$      top item from user stack (removed)
# SEGBEG    absolute start address of segment
# SEGEND    absolute end address of segment (technically +1)
# SEGLEN    byte length of segment
# SEGOFF    byte offset of segment within binary object file
# STR$      evaluate numeric expression and convert to decimal string
# TIME$     current time and date (a wrapper for ctime(), essentially)
# TOLOWER$  convert string to lower case
# TOUPPER$  convert string to upper case
# VAL       evaluate string as numeric expression
# VER       HXA version number
# VER$      HXA version string
# XLATE     character translation

# built-in function signatures
# - RPN operator, name, argtype [[,argtype]..]

# - if capitalized, argument type is required
# - if not capitalized, argument type is checked whenever present

local initFunc = " \
_1FCHR,CHR$,N \
_0FCPU,CPU$,V \
_1FDEF,DEFINED,G \
_0FEOS,EMPTY,V \
_0FFIL,FILE$,V \
_1FFWD,FORWARD,S \
_3FLDX,INDEX,S,S,n \
_3FRDX,INDEXR,S,S,n \
_1FLBL,LABEL,G \
_1FLEN,LEN,S \
_3FMAT,MATCH$,S,R,n \
_3FMID,MID$,S,N,n \
_1FMSG,MESG$,S \
_2FORD,ORD,S,n \
_1FPEK,PEEK$,n \
_0FPOP,POP$,V \
_1FSGB,SEGBEG,G \
_1FSGE,SEGEND,G \
_1FSGL,SEGLEN,G \
_1FSGO,SEGOFF,G \
_1FSTR,STR$,N \
_0FTIM,TIME$,V \
_1FTLC,TOLOWER$,S \
_1FTUC,TOUPPER$,S \
_1FVAL,VAL,S \
_0FVRN,VER,V \
_0FVRS,VER$,V \
_1FXLA,XLATE,N \
"

# default function argument flag

local defaultArg        # deliberately left uninitialized

# -----------------------------

# set operator precedences
# - "injected" operators are in RPN form

local function add_injected(val, data) {

    CKpopulateNdx( opPrecedence, data, val )
}

# set operator precedence
# - operator is in RPN form

local function add_precedence(val, operator) {

    opPrecedence[ operator ] = val
}

# populate operator name and precedence arrays
# - operators added here are accessible to user
# - other operators added elsewhere are accessible only to assembler

local function add_operator(val, array, data, prefix) {

    local i
    local usrtoken, rpntoken
    local operator

    i = split( data, operator, " " )
    do {
        usrtoken = operator[ i ]
        rpntoken = prefix usrtoken
        array[ usrtoken ] = rpntoken
        add_precedence( val, rpntoken )
    } while ( --i )
}

# populate unary numeric operators

local function add_unary(val, data, prefix) {

    add_operator( val, num1_Oprtr, data, prefix )
}

# populate binary numeric operators

local function add_binary(val, data, prefix) {

    add_operator( val, num2_Oprtr, data, prefix )
}

# populate binary string operators

local function add_string(val, data, prefix) {

    add_operator( val, str2_Oprtr, data, prefix )
}

# populate the function signature array

local function add_signature(val, func) {

    local descrip

    split( func, descrip, "," )
    funcToken[ descrip[2] ] = descrip
    add_precedence( val, descrip[1] )
}

# populate a parsing state

local function add_state(ndx, transitions) {

    CKpopulate( acceptToken[ndx], transitions )
}

# initialization (a long one)

INIT {

    local i
    local reserve
    local func

    # --------------------------------------------------

    # type mismatch error messages

    CKpopulate( needType, initNeed )

    # --------------------------------------------------

    # operator precedence table
    # - matches ANSI C precedence whereever operators match

    # - "injected" conversion operators ("_?C")

    # _1CAUT = auto label
    # _1CAPC = absolute program counter fixup
    # _1CGLB = global name
    # _1CSTR = string literal

    add_injected( 180, "_1CAUT _1CAPC _1CGLB _1CSTR" )

    # - "injected" value lookup operators ("_?V")

    # _0VPC  = program counter
    # _1VSYM = symbol value

    add_injected( 180, "_0VPC _1VSYM" )

    # function tokens and signatures
    # - they are also reserved symbols

    i = split( initFunc, func, " " )
    do {
        add_signature( 180, func[i] )       
    } while ( --i )

    # - string and pattern ops have high precedence so numeric ops
    #   won't try to engage string or pattern operands

    # - "injected" string operators ( "_?S" )

    # _1S! = compare to null string
    # _2S+ = string concatenation

    add_injected( 170, "_1S! _2S+" )

    # string logical:
    # less than, greater than, less or equal
    # greater or equal, equal, not equal

    add_string( 160, "< > <= >= == !=", "_2S" )

    # pattern:
    # match, not match

    add_string( 150, "~ !~", "_2P" )

    # unary arithmetic, bitwise and logical:
    # plus, minus, bitwise NOT, logical NOT
    # unary extract:
    # bits 0->7, bits 8->15, bits 16->31

    add_unary( 140, "+ -", "_1A" )
    add_unary( 140, "~ < > ^", "_1B" )
    add_unary( 140, "!", "_1L" )

    # binary arithmetic:
    # multiply, divide, remainder (aka modulus)

    add_binary( 130, "* / %", "_2A" )

    # binary arithmetic:
    # add, subtract

    add_binary( 120, "+ -", "_2A" )

    # binary bitwise:
    # left shift, right shift

    add_binary( 110, "<< >>", "_2B" )

    # binary logical:
    # less than, greater than, less or equal, greater or equal

    add_binary( 100, "< > <= >=", "_2L" )

    # binary logical:
    # equal, not equal

    add_binary( 90, "== !=", "_2L" )

    # binary bitwise:
    # AND

    add_binary( 80, "&", "_2B" )

    # binary bitwise:
    # exclusive OR

    add_binary( 70, "^", "_2B" )

    # binary bitwise:
    # inclusive OR

    add_binary( 60, "|", "_2B" )

    # internally "&&" and "||" are two operators apiece

    # binary logical:
    # AND

    add_injected( 50, "__1L&& _1L&&" )

    # binary logical:
    # OR

    add_injected( 40, "__1L|| _1L||" )

    # "imaginary" clear all but ternary conditional and open parenthesis

    add_precedence( 25, "ClearNonEOE" )

    # ternary conditional:
    # evaluate one of two expressions

    add_precedence( 20, "__1T?" )
    add_precedence( 15, "__0TF" )
    add_precedence( 10, "__0TT" )

    # "imaginary" "clear stack" operator
    # - this is used to pop the stack but is never pushed on itself
    # - an error occurs when whatever stops it popping isn't what's expected

    add_precedence( 2, "ClearStack" )

    # open parenthesis must be matched by close parenthesis

    add_precedence( 1, "OpenParen" )

    # "imaginary" "end-of-expression" operators
    # - "typed" to indicate how a (sub-)expression may be legally terminated

    add_injected( 0, "EndMarker FncMarker ArgMarker TerMarker" )

    # --------------------------------------------------

    # set up state transition tables for expression recognition
    # - generalized so that any syntactically legal expression
    # is recognized, but does not indicate whether the type
    # of the expression is correct in context (handled seperately)
    # - which is one way of allowing some operators to accept multiple
    # types without duplicating the whole table for every permissible type

    # operands:
    #   B - undecorated branch
    #   F - built-in function
    #   G - global name (has same form as global numeric label)
    #   N - numbers - literal, label, character, program counter
    #   S - strings - literal, label
    #   R - regular expressions - literal
    # operators:
    #   b - binary numeric
    #   e - end-of-expression
    #   l - binary short circuit logical
    #   p - binary pattern match
    #   s - binary string
    #   t - ternary conditional
    #   u - unary numeric
    # injections:
    #   # - symbol lookup (need value of label, not label as name)
    #   + - string concatenation
    #   = - compare to null string (convert to number)

# ----------------------------------------------------------------------

# State Table: Accept All Expressions w/ Post-Type-Checking and Promotion

# the idea is that in any given state we look for a token of a legal type
# - if we find one we return its type and use that as an index into
# the state table to find the next state
# - if we do not find one we return an index into the error message table;
# because these are not in the state table we can easily tell them apart
# - functions are treated as operands of the type they return; we save
# their next state until their arguments are parsed and then "pop"
# that state
# - parenthesized sub-expressions are treated as operands of the type of
# the sub-expressions; we save all currently legal operand types at the open
# parenthesis and check which one we have at the close parenthesis
# (as opposed to making the state table big enough to handle all cases)
# - at end of expression we know what the type of the expression is because
# we know what state we're in; we then check to see if that type is
# the one sought or can be converted to it
# - in many cases the next state for a given token type is always the same,
# and if we do not need it as a flag indicating its type is currently legal
# (ie., it always occurs in assocation with another type we do use as an
# "acceptable type" flag), we can save space in the in-memory state table
# by returning that state directly; again its absence from the known
# acceptable transitions means we handle it seperately
# - some entries in the table are not transitions but special case actions
# - blank spots in the table are error conditions

# format of a row:
# state name | acceptable types | next state [next state [...]]

#        Token   (   )   F   G   N   R   S   b   l   p   s   t   u   e
# State
# I     (FGNRSu  IE      F   G   N   R   S                       u
# IE    (FGNRSu  IE      F   G   N   R   S                       u
# F     (        IF
# IF    ()FGNRSu IE  pop F   G   N   R   S                       u  V
# G     )blte        pop                     Nb# Nl#         IE#    G>N
# Nb    (FGNSu   IE      F   bG  N       bS                      u
# bG    )blte        pop#                    Nb# Nl#         IE#    N#>S
# N     )blte        pop                     Nb  Nl          IE     N>S
# bS    )FSps        pop F               bS+         Sp  Ss
# Nl    (FGNSu   IE      F   bG  N       lS                      u
# Sp    (R       IE                  N
# Ss    (FS      IE      F               sS
# sS    )FSblte      pop F               sS+ Nb  Nl          IE     N>S
# u     (FNSu    IE      F   bG  N       lS                      u
# lS    )FSlpste     pop=F               lS+     Nl= Sp  Ss  IE=    N=>S
# R     )e           pop                                            R
# S     )FSlpste     pop F               S+      Nl= Sp  Ss  IE=    S>G,N

# ----------------------------------------------------------------------

    add_state( "I",  "G=G N=N R=R S=S UB=x Oprnd=NeedBOE" )
    add_state( "IE", "G=G N=N R=R S=S Oprnd=NeedBOE" )
    add_state( "F",  "(=IF Oprtr=NeedOpenP" )
    add_state( "IF", ")=x G=G N=N R=R S=S Oprnd=NeedBOE" )
    add_state( "G",  "l=Nl b=Nb e=G C1=_1VSYM Oprtr=NeedNumOp" )
    add_state( "Nb", "G=bG N=N S=bS Oprnd=NeedNum" )
    add_state( "bG", "l=Nl b=Nb e=N C1=_1VSYM C2=_1VSYM Oprtr=NeedNumOp" )
    add_state( "N",  "l=Nl b=Nb e=N Oprtr=NeedNumOp" )
    add_state( "bS", "S=bS p=Sp Oprtr=NeedStrOp" )
    add_state( "Nl", "G=bG N=N S=lS Oprnd=NeedNum" )
    add_state( "Sp", "R=N Oprnd=NeedRegex" )
    add_state( "Ss", "S=sS Oprnd=NeedStr" )
    add_state( "sS", "S=sS l=Nl b=Nb e=N Oprtr=NeedNumOp" )
    add_state( "u",  "G=bG N=N S=lS Oprnd=NeedNum" )
    add_state( "lS", "S=lS l=Nl p=Sp e=N C1=_1S! C2=_1S! Oprtr=NeedStrOp" )
    add_state( "R",  "e=R Oprtr=NeedEOE" )
    add_state( "S",  "S=S l=Nl p=Sp e=S C1=_1S! Oprtr=NeedStrOp" )

    # end of initialization

}

# --------------------------------------------------
# Conversion of algebraic expression to RPN form
# - ie., expression parsing
# --------------------------------------------------

# add token to RPN expression (array)

local function add_rpn(token) {

    rpnExpr[ ++rpnPtr ] = token
}

# -----------------------------------------------
# operator stack
# -----------------------------------------------

local opStack
local opPtr

# stack item

local function push_op(token) { opStack[ ++opPtr ] = token }

# unstack item

local function pop_op() {

    if ( opPtr > 0 )
        return( opStack[opPtr--] )
    else
        UMnoway( "POP_OP" )
}

# --------------------------

# unstack higher- and equal-precedence operators (conversion phase)
# - add them to the RPN

local function pop_ge(token) {

    local topop

    topop = pop_op()
    while ( opPrecedence[topop] >= opPrecedence[token] ) {
        add_rpn( topop )
        topop = pop_op()
    }

    return( topop )
}

# clear operator stack (conversion phase)
# - returns whatever stopped the pop (so we can check it)

local function pop_clear() {

    return( pop_ge("ClearStack" ) )
}

# stack operator (conversion phase)
# - first pops higher- and equal-precedence operators

local function pop_ge_push(token) {

    push_op( pop_ge(token) )
    push_op( token )
}

# stack operator (conversion phase)
# - first pops higher-precedence operators

local function pop_g_push(token) {

    local topop

    topop = pop_op()
    while ( opPrecedence[topop] > opPrecedence[token] ) {
        add_rpn( topop )
        topop = pop_op()
    }
    push_op( topop )        
    push_op( token )
}


# -----------------------------------------------
# type stack
# -----------------------------------------------

local typeStack
local typePtr

# stack item

local function push_type(token) {

    typeStack[ typePtr++ ] = token
}

# unstack item

local function pop_type() {

    if ( typePtr > 0 )
        return( typeStack[--typePtr] )
    else
        UMnoway( "POP_TYPE" )
}

# -----------------------------------------------
# state stack
# -----------------------------------------------

# - it happens that we can combine the functions
# of the type and state stacks into one stack
# - so we just use these to make it a little clearer which
# function we are using at any particular time

local function push_state(token) { push_type(token) }

local function pop_state() { return( pop_type() ) }

# -----------------------------------------------

# check that actual type of expression matches type we're looking for
# - this is also a very handy place to do "type fixups"

# expression types:

# "G"   - global name
# "N"   - numeric
# "N2S" - string; numbers converted to one-char strings
# "R"   - regular expression
# "S"   - string
# "S2N" - numeric; strings converted by comparison to null string
# "V"   - void

local function type_ok(havetype) {

    local wanttype, optionaltype

    # what type are we looking for ?

    wanttype = pop_type()

    # are we looking for a single type ?
    # - ie., that of expression as a whole

    if ( typeof(wanttype) != "array" ) {

        # remove any "optional argument" flag
        # - currently only NUMBERs can be optional

        optionaltype = wanttype
        wanttype = toupper( wanttype )
    }

    # ...looking for one of multiple possible types
    # - ie., that of subexpression enclosed in grouping parentheses "(..)"

    else {

        # not a legal type ?
        # - if not, we know only operand states allow open parentheses,
        # which in turn are the only things that put arrays on this stack,
        # so we can issue an error we hope is meaningful

        if ( !(havetype in wanttype) ) {
            parseResult = wanttype[ "Oprnd" ]
            return( FALSE )
        }

        # we set next state to legal type's next state

        push_state( wanttype[havetype] )

        # we could quit now if we didn't have to check ternary conditional

        wanttype = havetype 
    }

    # do we need to duplicate type for false branch of ternary conditional ?

    if ( currToken == ":" )
        push_type( wanttype )

    # do we have what we are looking for ?

    if ( havetype == wanttype )
        return( TRUE )

    # clear non-end-of-expression operators off stack

    push_op( pop_ge("ClearNonEOE") )

    # try to do a type fixup (if necessary)

    # do we have a NUMBER  ?

    if ( havetype == "N" ) {

        # are we converting STRINGS to NUMBERS ?
        # - but we have a number !

        if ( wanttype == "S2N" )
            return( TRUE )

        # are we converting NUMBERS to one-char STRINGS ?

        if ( wanttype == "N2S" ) {
            add_rpn( "_1FCHR" )
            return( TRUE )
        }
    }

    # do we have a STRING ?

    else if ( havetype == "S" ) {

        # are we converting NUMBERS to STRINGS ?
        # - but we have a string !

        if ( wanttype == "N2S" )
            return( TRUE )

        # are we converting STRINGS to NUMBERS ?
        # - eg., in conditional expressions

        if ( wanttype == "S2N" ) {
            add_rpn( "_1S!" )
            return( TRUE )
        }
        # are we verifying that STRINGS represent GLOBALS ?

        if ( wanttype == "G" ) {
            add_rpn( "_1CGLB" )
            return( TRUE )
        }
    }

    # do we have a GLOBAL ?

    else if ( havetype == "G" ) {

        # do we need its value, not its name ?
        # - ie., "N", "S2N", "N2S"

        if ( index(wanttype, "N") ) {
            add_rpn( "_1VSYM" )
            if ( wanttype == "N2S" )
                add_rpn( "_1FCHR" )
            return( TRUE )
        }
    }

    # do we have a VOID ?

    else if ( havetype == "V" ) {

        # is what we are looking for OPTIONAL ?
        # - eg., an optional function argument

        if ( optionaltype == tolower(optionaltype) ) {
            add_rpn( defaultArg )
            return( TRUE )
        }
    }
            
    # type mis-match

    parseResult = needType[ wanttype ]
    return( FALSE )
}

# -----------------------------------------------

# check if expression starts with target token
# - cutting off tokens and shortening expression happens only *here*

local function get_token(target) {

    if ( match(currExpr, target) ) {
        currToken = substr( currExpr, RSTART, RLENGTH )
        currExpr = substr( currExpr, RLENGTH + 1 )
        return( TRUE )
    }

    return( FALSE )
}

# -----------------------------------------------

# found a numeric integer literal

local function found_numlit(legal, lits) {

    local i
    local val, num
    local base
    local cnt
    local digit

    # convert the value

    val = 0

    # match drops any leading zeros and base indicator
    # - if match fails, numeric digits are all zeros (and so is number)

    if ( match(currToken, legal) ) {

        base = length( lits ) + 1

        num = toupper( substr(currToken, RSTART, RLENGTH) )

        # convert literal (left-to-right)
        # - result is postive value

        cnt = split( num, digit, "" )
        val = index( lits, digit[1] )
        for ( i = 2; i <= cnt; i++ ) {
            val = val * base + index( lits, digit[i] )
            if ( val > maxUnsigned )
                return( "BadRngHi" )
        }
    }

    add_rpn( val )

    return( "N" )
}

# found a character integer literal

local function found_charlit() {

    local s

    s = CKdoescapes( currToken )
    if ( length(s) != 1 )
        return( "NeedChar" )

    add_rpn( ord(s))
    return( "N" )
}

# found a regular expression literal

local function found_regexlit() {

    local re

    re = CKregex( currToken )
    if ( !CKok(re) )
        return( "NeedRegex" )

    add_rpn( re )
    return( "R" )
}

# found a global or variable label

local function found_label() {

    push_op( "_1VSYM" )
    add_rpn( currToken )
}

# found auto-label

local function found_auto() {

    push_op( "_1VSYM" )
    push_op( "_1CAUT" )
    add_rpn( currToken )
}

# found a built-in function name

local function found_function(nextstate) {

    local i
    local signature

    signature = funcToken[ currToken ]

    # push the state after the function completes
    # - this is mainly for string functions, which can be followed
    # by multiple operator types depending on context.
    # "Nextstate" is essentially that context

    push_state( nextstate )

    # push the function operator token

    push_op( signature[1] )

    # push the type and marker for the last argument

    i = length( signature )

    push_type( signature[i] )
    push_op( "FncMarker" )

    # push previous types and markers (if any)

    while ( --i > 2 ) {
        push_type( signature[i] )
        push_op( "ArgMarker" )
    }
}

# found a close parenthesis

local function found_closep(havetype) {

    local marker

    # type ok ?

    if ( !type_ok(havetype) )
        return( parseResult )

    # unstack any operators and save marker

    marker = pop_clear()

    # not grouping open parenthesis ?

    if ( marker != "OpenParen" ) {

        # ...argument marker(s) ?
        # - if so, function arguments are missing
        # - if argument is required, report error

        while ( marker == "ArgMarker" ) {
            if ( !type_ok("V") )
                return( parseResult )
            marker = pop_op()
        }
                
        # ...the only other legal possibility is that
        # we've terminated a function argument list

        if ( marker != "FncMarker" )
            return( "BadCloseP" )
    }

    # next state has been previously stored

    return( "PopState" )
}

# ---------------------------------

# is token a built-in function name ?

global function EXPisfunc(this) { return(this in funcToken) }

# ---------------------------------

# check if expression starts with a string

local function get_string(nextstate, concat) {

    # literal ?

    if ( get_token(CKstrLitToken) ) {
        if ( concat )
            pop_ge_push( "_2S+" )
        push_op( "_1CSTR" )
        add_rpn( currToken )
        parseResult = "S"
        return( TRUE )
    }

    # label or function name ?

    if ( get_token(SYMstrLabel_glv) ) {

        if ( concat )
            pop_ge_push( "_2S+" )

        currToken = SYMnormal( currToken )

        # local label ?

        if ( currToken ~ SYMlocLabel )
            found_auto()

        # function name ?

        else if ( EXPisfunc(currToken) ) {
            found_function( nextstate )
            parseResult = "F"
            return( TRUE )
        }

        # variable or global label

        else
            found_label()

        parseResult = "S"
        return( TRUE )
    }

    # not a string

    return( FALSE )
}

# ---------------------------------
# get operand token from expression
# ---------------------------------

local function get_operand(accept) {

    # ...string ?
    # - first so that we recognize 'xxx$' before 'xxx'

    if ( "S" in accept ) {

        if ( get_string(accept["S"], FALSE) )
            return( parseResult )
    }

    # ...number (or equivalent) ?

    if ( "N" in accept ) {

        # numeric literal ?

        if ( get_token(hexNumToken) )
            return( found_numlit(/[1-9A-F][0-9A-F]*/i, "123456789ABCDEF") )

        if ( get_token(binNumToken) )
            return( found_numlit(/1[01]*/, "1") )

        if ( get_token(decNumToken))
            return( found_numlit(/[1-9][0-9]*/, "123456789") )

        # function name or non-branch label ?

        if ( get_token(SYMnumLabel_glv) ) {

            currToken = SYMnormal( currToken )

            # local label ?

            if ( currToken ~ SYMlocLabel )
                found_auto()

            # function name ?

            else if ( EXPisfunc(currToken) ) {
                found_function( accept["N"] )
                return( "F" )
            }

            # global name possible ?

            else if ( ("G" in accept) && (currToken ~ SYMglobal) ) {
                add_rpn( currToken )
                return( "G" )
            }

            # variable or global label

            else
                found_label()

            return( "N" )
        }

        # decorated or undecorated branch target label ?
        # - undecorated "UB" flag only occurs in association with "N"

        if ( get_token(SYMnumLabel_b) \
            || (("UB" in accept) && get_token(SYMnumLabel_Ub)) )
        {
            found_auto()
            return( "N" )
        }

        # char ?

        if ( get_token(CKcharLitToken) )
            return( found_charlit() )

        # program counter ?

        if ( get_token(pcToken) ) {
            push_op( "_0VPC" )
            return( "N" )
        }

#    }

    # unary numeric operators are acceptable any time a number is acceptable
    # - so we don't need to perform an explicit test for that
    # - we leave the commented-out test in place for clarity
    # (and in case anything ever changes)

    # ...unary numeric operator ?
    #   arithmetic: - +
    #   bitwise:    ~
    #   logical:    !
    #   extract:    < > ^

#    if ( "u" in accept ) {

        if ( get_token(num1_OpToken) ) {
            push_op( num1_Oprtr[currToken] )
            return( "u" )
        }
    }

    # a grouping open parenthesis is alway acceptable in place of an operand
    # - so we don't need to perform an explicit test for that
    # - we leave the commented-out test in place for clarity
    # (and in case anything ever changes)
    # - we treat "(..)" as a single operand of type ".."
    # - since more than one type can be acceptable and we don't know yet
    # which it will actually be, we save all acceptable and check at
    # close parenthesis to see if we have a legal one
    # - next state will be "initial expression"

    # ...open parenthesis ?

#    if ( "(" in accept ) {

        if ( get_token(openPToken) ) {
            push_type( accept )
            push_op( "OpenParen" )
            return( "IE" )
        }
#    }

    # ...regular expression pattern ?

    if ( "R" in accept ) {

        if ( get_token(CKregexLitToken) )
            return( found_regexlit() )
    }

    # ...zero- or all-optional-argument function ?
    # - ie., no function argument supplied

    if ( ")" in accept ) {

        if ( get_token(closePToken) )
            return( found_closep("V") )
    }

    # whatever it is, we don't want it

    return( accept["Oprnd"] )
}

# ----------------------------------
# get operator token from expression
# ----------------------------------

local function get_operator(accept) {

    local marker

    # ... conditional subexpression operator ?
    #   logical short-circuit:  && ||
    #   ternary conditional:    ?
    # - first so we recognize "&&" and "||" before "&" and "|"

    if ( "l" in accept ) {

        if ( get_token(lsc_OpToken) ) {
           
            # type conversion needed ?

            if ( "C1" in accept )
                pop_ge_push( accept["C1"] )

            # ...logical short circuit AND ?

            if ( currToken == "&&" ) {
                pop_ge_push( "__1L&&" )
                pop_ge_push( "_1L&&" )
                return( "l" )
            }

            # ...logical short circuit OR ?

            if ( currToken == "||" ) {
                pop_ge_push( "__1L||" )
                pop_ge_push( "_1L||" )
                return( "l" )
            }

            # ...must be ternary conditional

            else { # currToken == "?"
                pop_g_push( "__1T?" )
                pop_g_push( "__0TF" )
                push_op( "TerMarker" )
                push_op( "__0TT" )
                return( "IE" )
            }
        }
    }

    # ...end-of-subexpression operator ?
    #   function arg separator: ,
    #   ternary true:           :

    if ( "e" in accept ) {

        if ( get_token(eoe_OpToken) ) {

            marker = ( currToken == ":" ) ? "TerMarker" : "ArgMarker"

            # type conversion needed ?

            if ( "C2" in accept )
                pop_ge_push( accept["C2"] )

            if ( !type_ok(accept["e"]) )
                return( parseResult )

            # if we match the marker we need, begin a new sub-expression

            return( pop_clear() == marker ? "IE" : "BadToken" )
        }
    }

    # ...binary numeric operator ?
    #   arithmetic: - + * / %
    #   bitwise:    & ^ | << >>
    #   logical:    < > == != <= >=

    if ( "b" in accept ) {

        if ( get_token(num2_OpToken) ) {

            # type conversion needed ?

            if ( "C1" in accept )
                pop_ge_push( accept["C1"] )

            pop_ge_push( num2_Oprtr[currToken] )
            return( "b" )
        }
    }

    # ...string ?
    # - ie., implied string concatenation, which we make explicit in RPN form

    if ( "S" in accept ) {

        if ( get_string(accept["S"], TRUE) )
            return( parseResult )
    }

    # ...pattern match operator ?
    #   logical:    ~ !~

    if ( "p" in accept ) {

        if ( get_token(pat2_OpToken) ) {
            pop_ge_push( str2_Oprtr[currToken] )
            return( "p" )
        }
#    }

    # string operators are acceptable any time pattern operators are
    # - so we don't need to perform an explicit test for that
    # - we leave the commented-out test in place for clarity
    # (and in case anything ever changes)

    # ...string comparison operator ?
    #   logical:    < > == != <= >=

#    if ( "s" in accept ) {

        if ( get_token(str2_OpToken) ) {
            pop_ge_push( str2_Oprtr[currToken] )
            return( "Ss" )
        }
    }

    # ...function call open parenthesis ?
    # - ie., the open parenthesis following a function name

    if ( "(" in accept ) {

        if ( get_token(openPToken) )
            return( "(" )
    }

    # ...close parenthesis ?
    # - always acceptable in place of an operator unless we're looking
    # for a function call open parenthesis

    else if ( get_token(closePToken) ) {
        if ( "C2" in accept )
            pop_ge_push( accept["C2"] )
        return( found_closep(accept["e"]) )
    }

    # whatever it is, we don't want it

    return( accept["Oprtr"] )
}

# ----------------------------------------------------------
# convert standard algebraic expression to RPN expression
# ----------------------------------------------------------

local function convert_rpn(expr, wanttype) {

    local i
    local loopcnt
    local state, found
    local accept
    local rpn

    # is the rpn version already cached ?

    if ( expr in rpnCache ) {
        rpnHits++
        return( rpnCache[expr] )
    }

    # initialize for processing expression

    typePtr = opPtr = rpnPtr = loopcnt = 0
    push_type( wanttype )
    push_op( "EndMarker" )

    currExpr = expr

    state = "I"

    # process expression

    while ( currExpr ) {
        
        # skip whitespace

        if ( get_token(/^[ \t]+/) )
            continue

        # how many times have we gone through this loop ?

        ++loopcnt

        # try to match a token we're willing to accept in this state

        currToken = ""
        accept = acceptToken[ state ]
        if ( "Oprnd" in accept )
            found = get_operand( accept )
        else
            found = get_operator( accept )

        # did we find one ?

        if ( found in accept )
             state = accept[ found ]
        else if ( index("IE F Ss u", found) )
            state = found
        else if ( found == "PopState" )
            state = pop_state()
        else {
            UMerror( found, currToken ? currToken : currExpr )
            return( CKfail )
        }
    }

    # no more expression to process, but did we need more ?
    # - which might also mean expression was null to start with
    # (and we're still in initial state, which is not a legal EOE state)

    accept = acceptToken[ state ]
    if ( !("e" in accept) ) {
        UMerror( expr ~ /^[ \t]*$/ ? needType[wanttype] : "BadEOE", expr )
        return( CKfail )
    }

    # type conversion needed ?

    if ( "C2" in accept )
        pop_ge_push( accept["C2"] )

    # check overall expression type

    if ( !type_ok(accept["e"]) ) {
        UMerror( parseResult, expr )
        return( CKfail )
    }

    # any sub-expressions incomplete ?
    # - could be more than one, but without looping one is all we'll report

    if ( (found = pop_clear()) != "EndMarker" ) {

        # unmatched "?"

        if ( found == "TerMarker" )
            UMerror( "BadEOE" )

        # unmatched "(" or incomplete function call
        # - so true either way

        else
            UMerror( "BadOpenP" )

        return( CKfail )
    }

    # - we can assign rpnExpr to a variable directly, but because of
    # reference counting changing rpnExpr will also change the contents of
    # that variable
    # - hence we must disconnect them

    for ( i = 1; i <= rpnPtr; i++ )
        rpn[ i ] = rpnExpr[ i ]

    # worth caching ?
    # - converting expressions to rpn occupies a lot of assembly time,
    # so if an expression is used multiple times it may be worth
    # caching to avoid repeated overhead
    # - multiple use is probably most likely within a macro
    # - it is possible to run out of memory (at least in the MS-DOS version)
    # after caching enough expressions (thousands?), so we make sure
    # that doesn't happen by clearing it every so often

    if ( (loopcnt > 1) && MACexpanding() ) {
        if ( length(rpnCache) >= maxCache ) {
            ++rpnPurge
            delete( rpnCache )
        }                      
        rpnCache[ expr ] = rpn
    }

    return( rpn )
}

# -----------------------------
# RPN Expression Evaluation
# -----------------------------

# evaluation stack

local evalPtr
local evalStk

# -----------------------------

# stack operand

local function push_eval(token) { evalStk[ evalPtr++ ] = token }

# unstack operand

local function pop_eval() {

    if ( evalPtr > 0 )
        return( evalStk[--evalPtr] )
    else
        UMnoway( "POP_EVAL" )
}

# -----------------------------

# check if token is an operator

local function isoperator(this) {

    return( (typeof(this) == "string") && (this in opPrecedence) )
}

# -----------------------------

# - non-function operator execution checks ordered roughly on
# assumed frequency of occurence
# - function operator execution checks ordered alphabetically by token

# -----------------------------

# execute a zero-argument operator

local function do_op0(operator) {

    local val

    # ...program counter ?

    if ( operator == "_0VPC" ) {
        val = PCget()

        # relative ?

        if ( PCgotrel(val) ) {
            push_eval( val )
            return( "_1CAPC" )
        }

        # absolute

        return( val )
    }

    # ...cpu$() ?

    if ( operator == "_0FCPU" )
        return( CGgetcpu() )

    # ...empty() ?

    if ( operator == "_0FEOS" )
        return( CKeos() )

    # ...file$() ?

    if ( operator == "_0FFIL" )
        return( SRCfile(SRCgetmaster()) )

    # ...pop$() ?
    # - empty stack is un-recoverable error

    if ( operator == "_0FPOP" ) {
        if ( CKcanpop() )
            return( CKdopop() )
        else
            return( operator )
    }

    # ...time$()?

    if ( operator == "_0FTIM" )
        return( ctime() )

    # ...ver() ?

    if  ( operator == "_0FVRN" )
        return( CKvernum() )

    # ...ver$() ?

    if ( operator == "_0FVRS" )
        return( CKverstr() )

    # oops!

    UMnoway( operator )
}

# -----------------------------

# test if an argument is "default"

local function isdefault(this) { return( typeof(this) == "uninitialized" ) }

# -----------------------------

# execute a one-argument operator

local function do_op1(operator) {

    local arg1, val
    local mark
    local rpn

    arg1 = pop_eval()
    if ( isoperator(arg1) ) {
        push_eval( arg1 )
        return( operator )
    }

    # ... a value lookup operator ?

    if ( operator ~ /^_1V/ ) {
    
        # ... symbol ?

        if ( operator == "_1VSYM" ) {

            # a known symbol ?

            if ( SYMexists(arg1) ) {

                val = SYMvalue( arg1 )

                # a relative address ?
                # - which is technically a forward reference, since we
                # don't know its absolute value
                # - we will save the value and convert it later, because if
                # this is a variable label its value could change

                if ( PCgotrel(val) ) {
                    push_eval( val )
                    return( "_1CAPC" )
                }

                return( val )
            }

            # forward reference to an unknown symbol
    
            push_eval( arg1  )
            return( operator )
        }

    }

    # ...a conversion operator ?

    if ( operator ~ /^_1C/ ) {

        # ... absolute program counter ?

        if ( operator == "_1CAPC" )
            return( PCgetabs(arg1) )

        # ... auto label ?

        if ( operator == "_1CAUT" )
            return( SYMreplaceauto(arg1) )

        # ... global name ?
        # - failure to match is un-recoverable error

        if ( operator == "_1CGLB" ) {
            val = SYMnormal( arg1 )
            if ( val ~ SYMglobal )
                return( val )
            UMerror( "NeedGlb", arg1 )
            push_eval( arg1 )
            return( operator )
        }            

        # ... literal string ?

        if ( operator == "_1CSTR" ) {
            if ( arg1 == "\"\"" )
                return( "" )
            return( CKdoescapes(arg1) )
        }
    }

    # ...an arithmetic operator ?

    if ( operator ~ /^_1A/ ) {
        if ( operator == "_1A-" )
            return( -arg1 )
        if ( operator == "_1A+" )
            return( arg1 >= 0 ? arg1 : -arg1 )
    }

    # ...a bitwise operator ?

    if ( operator ~ /^_1B/ ) {
        arg1 = EXPint( arg1 )
        if ( operator == "_1B<" )
            return( CGgetlsb(arg1) )
        if ( operator == "_1B>" )
            return( CGgetmsb(arg1) )
        if ( operator == "_1B^" )
            return( CGgetmsw(arg1) )
        if ( operator == "_1B~" )
            return( not(arg1) )
    }

    # ...a logical operator ?
    # - in the case of logical short circuit operators, we already know
    # the result of the left hand side (since short circuit didn't happen),
    # so we don't need it here to complete evaluation

    if ( operator ~ /^_1L/ ) {
        if ( operator == "_1L!" )
            return( !arg1 )
        if ( operator == "_1L&&" || operator == "_1L||" )
            return( arg1 != 0 )
    }

    # ...a function operator ?

    if ( operator ~ /^_1F/ ) {

        # ...chr$() ?

        if ( operator == "_1FCHR" )
            return( sprintf("%c", and(arg1, 0xff)) )
                
        # ...defined() ?

        if ( operator == "_1FDEF" )
            return( MACismacro(arg1) )

        # ...forward() or val() ?
        # - tricky #1: requires recursion
        # - tricky #2: incomplete evaluation of a legal expression leaves
        # one or more elements on the operand stack in addition to the
        # value explicitly returned by eval_rpn
        # forward() - this is a boolean function, so we need to get rid of
        # any such extra elements (we throw away the return value as well)
        # val() - we don't need to do anything special because the extra
        # elements are positioned exactly the same as for any other
        # incomplete evaluation, and will eventually be resolved like them
        # - failure to convert is un-recoverable error (reported already)

        if ( operator == "_1FFWD" || operator == "_1FVAL" ) {
            rpn = convert_rpn( arg1, "N" )
            if ( !CKok(rpn) ) {
                push_eval( arg1 )
                return( operator )
            }

            mark = evalPtr
            val = eval_rpn( rpn )

            if ( operator == "_1FVAL" )
                return( val )

            # expression completely evaluated (ie., no forward reference ?)

            if ( mark == evalPtr )
                return( FALSE )

            # there is a forward reference in the expression

            evalPtr = mark
            return( TRUE )
        }

        # ...label() ?

        if ( operator == "_1FLBL" )
            return( SYMdefined(arg1) )

        # ...len() ?

        if ( operator == "_1FLEN" )
            return( length(arg1) )

        # ...mesg$() ?

        if ( operator == "_1FMSG" )
            return( UMexpandtext(arg1) )

        # ...peek$() ?
        # - empty stack and index out of range are un-recoverable errors

        if ( operator == "_1FPEK" ) {
            val = isdefault( arg1 ) ? 1 : arg1
            if ( CKcanpeek(val) )
                return( CKdopeek(val) )
            else {
                push_eval( arg1 )
                return( operator )
            }
        }

        # ...segbeg(), segend(), seglen(), segoff() ?

        if ( operator ~ /^_1FSG[BELO]/ ) {

            val = PCgetsegnum( arg1 )

            if ( CKok(val) ) {
                if ( operator == "_1FSGB" )
                    val = PCgetsegbeg( val )
                else if ( operator == "_1FSGE" )
                    val = PCgetsegend( val )
                else if ( operator == "_1FSGL" )
                    val = PCgetseglen( val )
                else    # must be "_1FSGO"
                    val = PCgetsegoff( val )

                if ( CKok(val) )
                    return( val )
            }

            push_eval( arg1 )
            return( operator )
        }

        # ...str$() ?
        # - out-of-range is un-recoverable error (reported already)

        if ( operator == "_1FSTR" ) {
            if ( CKinrange(arg1, minSigned, maxUnsigned) )
                return( arg1 "" )
            push_eval( arg1 )
            return( operator )
        }

        # ...tolower$() ?

        if ( operator == "_1FTLC" )
            return( tolower(arg1) )

        # ...toupper$() ?

        if ( operator == "_1FTUC" )
            return( toupper(arg1) )

        # ...xlate() ?

        if ( operator == "_1FXLA" )
            return( CKxlate(arg1) )
    }

    # ..."compare to null string" operator ?

    if ( operator == "_1S!" )
        return( arg1 != "" )

    # oops!

    UMnoway( operator )
}

# -----------------------------

# find start position in string
# - positive values offset from start of string, negative from end
# - returns offset from start of string if within string, else zero

local function startpos(pos, str) {

    local slen

    slen = length( str )

    # negative position means offset from end of string, not start
    # - so -1 = last character of string, -2 = second to last, etc.

    if ( pos < 0 )
        pos += slen + 1

    if ( (pos > 0) && (pos <= slen) )
        return( pos )

    return( 0 )
}

# -----------------------------

# execute a two-argument operator

local function do_op2(operator) {

    local arg1, arg2

    arg1 = pop_eval()
    arg2 = pop_eval()
    if ( isoperator(arg1) || isoperator(arg2) ) {
        push_eval( arg2 )
        push_eval( arg1 )
        return( operator )
    }

    # ... an arithmetic operator ?
    # - divide-by-zero is un-recoverable error

    if ( operator ~ /^_2A/ ) {
        if ( operator == "_2A+" )
            return( arg2 + arg1 )
        if ( operator == "_2A-" )
            return( arg2 - arg1 )
        if ( operator == "_2A*" )
            return( arg2 * arg1 )
        if ( arg1 == 0 ) {
            UMerror( "DivZero" )
            push_eval( arg2 )
            push_eval( arg1 )
            return( operator )
        }                                                                                                                                                                 
        if ( operator == "_2A/" )
            return( int(arg2 / arg1) )
        if ( operator == "_2A%" )
            return( int(arg2 % arg1) )
    }

    # ... a bitwise operator ?

    if ( operator ~ /^_2B/ ) {
        arg2 = EXPint( arg2 )
        arg1 = EXPint( arg1 )
        if ( operator == "_2B&" )
            return( and(arg2, arg1) )
        if ( operator == "_2B|" )
            return( or(arg2, arg1) )
        if ( operator == "_2B^" )
            return( xor(arg2, arg1) )
        if ( operator == "_2B<<" )
            return( shiftl(arg2, arg1) )
        if ( operator == "_2B>>" )
            return( shiftr(arg2, arg1)  )
    }

    # ... a logical operator ?

    if ( operator ~ /^_2L/ ) {
        if ( operator == "_2L==" )
            return( arg2 == arg1 )
        if ( operator == "_2L!=" )
            return( arg2 != arg1 )
        if ( operator == "_2L<" )
            return( arg2 < arg1 )
        if ( operator == "_2L>" )
            return( arg2 > arg1 )
        if ( operator == "_2L<=" )
            return( arg2 <= arg1 )
        if ( operator == "_2L>=" )
            return( arg2 >= arg1 )
    }

    # ... a string operator ?

    if ( operator ~ /^_2S/ ) {
        if ( operator == "_2S+" )
            return( arg2 arg1 )
        if ( operator == "_2S==" )
            return( arg2 == arg1 )
        if ( operator == "_2S!=" )
            return( arg2 != arg1 )
        if ( operator == "_2S<" )
            return( arg2 < arg1 )
        if ( operator == "_2S>" )
            return( arg2 > arg1 )
        if ( operator == "_2S<=" )
            return( arg2 <= arg1 )
        if ( operator == "_2S>=" )
            return( arg2 >= arg1 )
    }

    # ...pattern match operator ?

    if ( operator ~ /^_2P/ ) {
        if ( operator == "_2P~" )
            return( arg2 ~ arg1 )
        if ( operator == "_2P!~" )
            return( arg2 !~ arg1 )
    }

    # ...ord() ?

    if ( operator == "_2FORD" ) {
        if ( arg2 != "" ) {
            if ( isdefault(arg1) )
                return( ord(arg2) )

            if ( (arg1 = startpos(arg1, arg2)) )
                return( ord(substr(arg2, arg1)) )
        }

        return( 0 )
    }
    
    # oops!

    UMnoway( operator )
}

# -----------------------------

# execute a three-argument operator

local function do_op3(operator) {

    local arg1, arg2, arg3
    local pos

    arg1 = pop_eval()
    arg2 = pop_eval()
    arg3 = pop_eval()
    if ( isoperator(arg1) || isoperator(arg2) || isoperator(arg3) ) {
        push_eval( arg3 )
        push_eval( arg2 )
        push_eval( arg1 )
        return( operator )
    }

    # ...function ?

    if ( operator ~ /_3F/ ) {

        # ...index() ?

        if ( operator == "_3FLDX" ) {

            if ( arg2 != "" ) {
                if ( isdefault(arg1) )
                    return( index(arg3, arg2) )

                if ( (arg1 = startpos(arg1, arg3)) ) {
                    pos = index( arg3, arg2, arg1 )
                    if ( pos >= arg1 )
                        return( pos )
                }
            }

            return( 0 )
        }

        # ...indexr() ?

        if ( operator == "_3FRDX" ) {

            if ( arg2 != "" ) {
                if ( isdefault(arg1) )
                    return( rindex(arg3, arg2) )
            
                if ( (arg1 = startpos(arg1, arg3)) ) {
                    pos = rindex( arg3, arg2, arg1 )
                    if ( pos <= arg1 )
                        return( pos )
                }
            }

            return( 0 )
        }

        # ...match$() ?

        if ( operator == "_3FMAT" ) {

            if ( isdefault(arg1) )
                pos = match( arg3, arg2 )
            else if ( (arg1 = startpos(arg1, arg3)) )
                pos = match( arg3, arg2, arg1 )
            else
                pos = 0

            if ( pos )
                return( substr(arg3, RSTART, RLENGTH) )

            return( "" )
        }

        # ...mid$() ?

        if ( operator == "_3FMID" ) {

            if ( (arg2 = startpos(arg2, arg3)) ) {
                if ( isdefault(arg1) )
                    return( substr(arg3, arg2) )
                if ( arg1 > 0 )
                    return( substr(arg3, arg2, arg1) )
            }

            return( "" )
        }

    }

    # oops!

    UMnoway( operator )
}

# -----------------------------

# evaluate RPN expresssion
# - doesn't care if evaluation is complete or not

# the main problem with evaluation is what to do with forward references.
# Consider adding the values of a global and a variable label together:

# global + ]variable

# suppose "global" is not yet known when this expression is evaluated
# during the first pass. One option is to save the whole expression and
# evaluate it again during the second pass. However this leads to a
# problem with "]variable": on the second pass the evaluation will use
# the last value assigned to "]variable", even if it had a different
# (and well-defined) value at the time the expression was first encountered

# what we'll do instead is evaluate as much as we can during the first
# pass, and save only what we can't evaluate

# RPN expressions are easy to evaluate: if an operand appears, put it
# on a stack; if an operator appears, pull any operands needed off
# the stack and push the result back onto the stack. At the end
# there is only one value on the operand stack, which is the result

# we're going to add a wrinkle: if the operator can't be executed,
# push its operands (if any), then the operator, onto the operand stack.
# If we then pull an operator from the stack instead of an operand,
# we know a previous operator failed and the current operator must as well
# (because it needs the operand the operator just pulled should have
# created). So we'll have to push the current operator, along with its
# operands, onto the operand stack as well.

# We can do this as often as necessary for any expression. Each time we
# do, we are using an operator we couldn't execute to protect its
# operands from being disturbed (since they are now below their operator,
# and won't be pulled once that operator is discovered). The net result
# will be that any operator which can be executed will be, and any which
# can't will be saved for later

# Using the previous expression as an example, and assuming
# "global" is not known but "]variable" is, the process looks like this:

# RPN expression:                       op stack:

# 1) global _1VSYM ]variable _1VSYM A+
# 2) _1VSYM ]variable _1VSYM A+         global
# 3) ]variable _1VSYM A+                _1VSYM global
# 4) _1VSYM A+                          ]variable _1VSYM global
# 5) A+                                 (]variable) _1VSYM global
# 6)                                    A+ (]variable) _1VSYM global

# So all we've managed is to find the value of "]variable" - but this
# is enough to make sure that the expression will evaluate correctly
# during the second pass, when "global" is known as well

# - note if we reverse the contents of the op stack we get:

# global _1VSYM (]variable) A+

# which is the RPN expression we need to evaluate during the second pass

# - also note we can even arrange to safely ignore certain sub-expressions
# by skipping over them (even if forward reference is involved the skipped
# sub-expressions won't make it into the second pass RPN expression because
# they're not needed for correct resolution)

# -----------------------------

local function eval_rpn(expr) {

    local i
    local cnt
    local this, arg1
    local skiplevel, saveexpr, uptok, downtok

    cnt = length( expr )

    for ( i = 1; i <= cnt; i++ ) {

        this = expr[ i ]

        # an operand ?

        if ( !isoperator(this) )
            push_eval( this )

        # ...two-argument operator ?

        else if ( this ~ /^_2/ )
            push_eval( do_op2(this) )

        # ...one-argument operator ?

        else if ( this ~ /^_1/ )
            push_eval( do_op1(this) )

        # ...zero-argument operator ?

        else if ( this ~ /^_0/ )
            push_eval( do_op0(this) )

        # ...three-argument operator ?

        else if ( this ~ /^_3/ )
            push_eval( do_op3(this) )

        # ...a skip-ahead operator ?

        # ...ternary end-of-false branch ?
        # - only an end-of-(sub)expression marker with no actual effect

        else if ( this != "__0TF" ) {

            skiplevel = saveexpr = 0

            # ...ternary end-of-true branch ?
            # - ie., we've just finished evaluating it, so skip false one

            if ( this == "__0TT" ) {
                downtok = "__0TF"
                skiplevel = 1
            }

            else {

                # all remaining operators have one argument
                # - if argument is itself an operator, the (sub)expression
                # it is based on couldn't be evaluated
                # - we flag that we have to try again later in that case

                arg1 = pop_eval()
                if ( isoperator(arg1) ) {
                    push_eval( arg1 )
                    push_eval( this )
                    skiplevel = saveexpr = 1
                }

                # ...ternary end-of-conditional branch?
                # - skip ahead if FALSE or can't evaluate

                if ( this == "__1T?" ) {
                    if ( saveexpr )
                        downtok = "__0TF"
                    else if ( !arg1 ) {
                        downtok = "__0TT"
                        skiplevel = 1
                    }
                }

                # ...left branch of logical AND ?
                # - skip ahead if FALSE or can't evaluate

                else if ( this == "__1L&&" ) {
                    if ( saveexpr )
                        downtok = "_1L&&"
                    else if ( !arg1 ) {
                        push_eval( 0 )
                        downtok = "_1L&&"
                        skiplevel = 1
                    }
                }
                
                # ...left branch of logical OR ?
                # - skip ahead if TRUE or can't evaluate

                else if ( this == "__1L||" ) {
                    if ( saveexpr )
                        downtok = "_1L||"
                    else if ( arg1 ) {
                        push_eval( 1 )
                        downtok = "_1L||"
                        skiplevel = 1
                    }
                }

                # ...don't know what this operator is...

                else
                    UMnoway( "EVAL_RPN" )
            }

            # skip ahead ?
            # - note we are altering the loop index here,
            # which is probably not academically sanctioned :-)
            # - we can't run off end of expression because we know it is
            # properly formed, hence the terminating token must be found
            # - can put at top of the main loop, but a bit slower that way

            if ( skiplevel ) {
                uptok = this
                do {
                    this = expr[ ++i ]
                    if ( saveexpr )
                        push_eval( this )
                    if ( isoperator(this) ) {
                        if ( this == downtok )
                            --skiplevel
                        else if ( this == uptok )
                            ++skiplevel
                    }
                } while ( skiplevel )
            }
        }            
    }

    # return top stack element
    # - if expression completely evaluated this will be an operand,
    # and the operand stack will be empty
    # - if not completely evaluated this will be an operator,
    # and the operand stack will not be empty

    return( pop_eval() )
}

# -----------------------------

# process RPN expression
# - handles both complete and incomplete evaluation

local function proc_rpn(expr, forwardok) {

    local i
    local token
    local newexpr

    evalPtr = 0

    token = eval_rpn( expr )

    # was the expression completely evaluated ?
    # - ie., is the evaluation stack empty ?

    if ( !evalPtr ) {

        # a float (ie., not an integer or string) ?
        # - must be in legal range
        # - will be converted to integer later if necessary

        if ( typeof(token) == "float" ) {
            if ( !CKinrange(token, minSigned, maxUnsigned) )
                return( CKfail )
        }

        return( token )
    }

    # expression not completely evaluated
    # - if an evaluation-time error cause incompletion,
    # processing will stop at the end of the current pass
    # - hence we don't care which branch is taken next (so no special check)

    push_eval( token )

    # are we allowed to evaluate it again later ?

    if ( forwardok ) {

        # the elements evalStk[1]..evalStk[evalPtr] (in that order)
        # are the expression we want to evaluate next
        # - we can assign evalStk to a variable directly, but because of
        # reference counting changing evalStk will also change the
        # contents of that variable
        # - hence we must disconnect them

        while ( (i = evalPtr) )
            newexpr[ i ] = pop_eval()

        return( newexpr )

    }

    # report forward reference problem

    while ( evalPtr ) {

        token = pop_eval()

        if  ( isoperator(token) ) {
    
            if ( token == "_1CAPC" )
                UMerror( "BadPC" )

            # can't find global name ?
            # - next stack item will be either the name or the operator
            # "verify string is a global name" (itself followed by name)
            # - if it is the verify operator, then that operator failed
            # and the error was already reported
            # - "stack corruption" doesn't matter (no new expression)

            else if ( token ~/^_1(VSYM|FSG[BELO])$/ ) {
                if ( (token = pop_eval()) != "_1CGLB" ) {
                    if ( CKisreserved(token) )
                        UMreserved( token )
                    else
                        UMundefname( token )
                }
            }
        }
    }

    return( CKfail )
}

# ----------------------------

# convert and evaluate numeric expression
# - returns an int or float (if complete evaluation)
# or an array (if incomplete evaluation)
# or FAIL (if can't evaluate)

global function EXPgetnum(expr) {

    local rpn

    rpn = convert_rpn( expr, "N" )
    if ( CKok(rpn) )
        return( proc_rpn(rpn, TRUE) )

    return( CKfail )
}

# did evaluation result in numeric value ?

global function EXPgotnum(this) {

    return( (typeof(this) == "int") || (typeof(this) == "float") )
}

# resolve expression
# - used during second pass to resolve forward references
# - we know evaluation is incomplete and not just failed
# (second pass would not occur if any expression failed during first pass)

global function EXPresolve(this) { return( proc_rpn(this, FALSE) ) }

# convert any float result to signed integer
# - what TAWK likes as integer (so we can output proper values)

global function EXPint(this) {

    # subtraction of 2^32 does not change bit pattern of mantissa

    if ( this > maxSigned )
        return( int(this - 2^32) )

    # TAWK 5 does not convert minSigned to integer via int(),
    # so we finesse the issue

    if ( this > minSigned )
        return( int(this) )

    return( 0x80000000 )
}

# -----------------------------------------------------------------

# convert and evaluate assertion expression
# - accepts numbers or strings (which are compared to the null string)

global function EXPdoassert(expr) {

    local rpn

    rpn = convert_rpn( expr, "S2N" )
    if ( CKok(rpn) )
        CGcheckassert( proc_rpn(rpn, TRUE) )
}

# -----------------------------------------------------------------
# convert and evaluate constant expressions (no forward reference)
# - these all return FAIL if can't convert or can't evaluate
# -----------------------------------------------------------------

# convert and evaluate a constant

local function get_const(expr, type) {

    local rpn

    rpn = convert_rpn( expr, type )
    if ( CKok(rpn) )
        return( proc_rpn(rpn, FALSE) )

    return( CKfail )
}

# convert and evaluate numeric expression
# - returns a number

global function EXPgetint(expr) { return(get_const(expr, "N")) }

# convert and evaluate string expression
# - returns a string

global function EXPgetstr(expr) { return(get_const(expr, "S")) }

# convert and evaluate string expression
# - accepts strings or numbers (which are converted to one-char strings)
# - returns a string

global function EXPgetstring(expr) { return(get_const(expr, "N2S")) }

# convert and evaluate conditional expression
# - accepts numbers or strings (which are compared to the null string)
# - returns a number

global function EXPgetcondition(expr) { return(get_const(expr, "S2N")) }

# convert and evaluate global name
# - returns a global name

global function EXPgetglobal(expr) { return(get_const(expr, "G")) }

# ----------------------------

# convert, evaluate and range check integer expression

global function EXPgetinrange(expr, min, max) {

    local val

    val = EXPgetint( expr )
    if ( CKok(val) && CKinrange(val, min, max) )
        return( val )

    return( CKfail )
}

# ----------------------------

# report stats

global function EXPstats() {

    local totalcached

    totalcached = length( rpnCache ) + rpnPurge * maxCache

    SRCshowval( "ExpCache", totalcached )
    if ( totalcached )
        SRCshowval( "CacheHits", rpnHits )
}
