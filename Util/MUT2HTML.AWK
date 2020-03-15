# Marked-Up Text File Translator (HTML version main file)

# for Thompson AWK 4.0

# by Anton Treuenfels

# first created: 09/17/06
# last revision: 07/15/09

# public prefix: MUT

# ------------------------------------------------------

# pass number

local passNum

# document full names and filenames

local currDoc           # current full document name
local currDocFile       # current document file name

local docName           # full document names of document files
local docFile           # document files of full document name

# table of contents

local uniqueDocToc
local tocCnt

local masterToc

# topics in table of contents

local currTopic, workTopic

local uniqueDocBranch, docTwigCnt
local masterTopicBranch, masterTopicTwig
local uniqueMasterBranch, uniqueMasterTwig

# SET strings (if any)

local envStr

# errors found

local errCnt

# file reading

local dfltPath
local currFile
local currLine
local incDepth
local inkStack

# file writing

local dataFile
local outFile

# boolean names

global FALSE = 0
global TRUE  = 1

# null data

global NULL  = "__NULL"

# data start flag for lists and tables

local dataStart = "-"

# in-line commands pattern

local inLinePat = /[BILX]<[^<>]+>/i

# ------------------------------------------------------
# User Messages
# ------------------------------------------------------

# status reports

local function status(type, value) {

    if ( argcount() > 1 )
        print type ": " value > "CON"
    else
        print type > "CON"
}

# show and record error message

local function showerr(text) {

    print text > stderr
    print text > "mut.err"
}

# report an error while reading file

global function MUTerr(mesg, token) {

    showerr( ">> ERROR: " currFile ":" currLine )
    showerr( ">> "  $0 )
    if ( argcount() > 1 )
        showerr( ">>  " mesg ": <" token ">" )
    else
        showerr( ">>  " mesg )

    ++errCnt
}

# fatal error

local function fatalerr(mesg, token) {

    status( ">> FATAL: " mesg, token )
    abort()
}

# problems ?

local function quitonerr() {

    if ( errCnt ) {
        status( errCnt " errors found" )
        status( "No output file(s) produced" )
        abort()
    }
}

# debug

local function show(this) { print this > "check.txt" }

# ------------------------------------------------------
# Output Files
# ------------------------------------------------------

# replace filename extension (retains path info, if any)

global function MUTfnewext(this, ext) {

    local pos

    if ( (pos = rindex(this, ".")) > 0 )
        this = substr( this, 1, pos - 1 )        
    return( this "." ext )
}

# try to place file in TEMP directory

local function intempdir(fname) {

    local ndx

    if ( "TEMP" in ENVIRON ) {
        if ( (ndx = rindex(fname, "\\")) > 0 )
            fname = substr( fname, ndx + 1 )
        return( ENVIRON[ "TEMP" ] "\\" fname )
    }

    # otherwise goes in source directory

    else
        return( fname )
}

# ------------------------------------------------------
# Output to intermediate file
# ------------------------------------------------------

global function MUTwrite(this) { print this > outFile }

# ------------------------------------------------------

# look for in-line commands

local function do_inline(text) {

    local i
    local pre, cmd, data
    local pos, link
    local chunk

    i = 0
    if ( match(text, inLinePat) ) {
        do {
            pre = substr( text, 1, RSTART - 1 )
            cmd = toupper( substr(text, RSTART, 1) )
            data = substr( text, RSTART + 2, RLENGTH - 3 )
            text = substr( text, RSTART+RLENGTH )

            if ( pre ) {
                chunk[ ++i ] = "N"
                chunk[ ++i ] = pre
            }

            chunk[ ++i ] = cmd

            # internal link ?

            if ( cmd == "L" ) {
                pos = rindex( data, "|" )
                if ( !pos )
                    pos = rindex( data, "\\" )
                if ( pos ) {
                    link = getxrefdata( substr(data, 1, pos-1) )
                    data = substr( data, pos+1 )
                }
                else {
                    link = getxrefdata( data )
                    if ( (pos = index(data, ":")) )
                        data = substr( data, pos+1 )
                }
                pos = index( link, ":" )
                chunk[ ++i ] = substr( link, 1, pos-1 )
                chunk[ ++i ] = substr( link, pos+1 )
            }

            # external link ?

            else if ( cmd == "X" ) {
                pos = rindex( data, "|" )
                if ( pos ) {
                    link = substr( data, 1, pos-1 )
                    data = substr( data, pos+1 )
                }
                else {
                    link = data
                    if ( (pos = index(data, ":")) )
                        data = substr( data, pos+1 )
                }
                chunk[ ++i ] = link
            }

            # bold, italic, ilink:show, xlink:show

            chunk[ ++i ] = data

        } while ( match(text, inLinePat) )
    }

    if ( text ) {
        chunk[ ++i ] = "N"
        chunk[ ++i ] = text
    }

    return( chunk )
}

# ------------------------------------------------------

# end collection type (if necessary)

local function endcollect() {

    if ( collectType ) {
        if ( passNum > 1 ) {
            if ( collectType == "TEXT" )
                FMTcmd_endtext()
            else if ( collectType == "CODE" )
                FMTcmd_endcode()
            else if ( collectType == "LIST" )
                FMTcmd_endlist()
            else if ( collectType == "TABLE" )
                FMTcmd_endtable()
            else if ( collectType == "EMAIL" )
                FMTcmd_endemail()
        }
        collectType = ""
    }
}

# begin collection type (if necessary)

local function begincollect(type, data) {

    if ( collectType != type ) {
        endcollect()
        collectType = type
        if ( passNum > 1 ) {
            if ( collectType == "TEXT" )
                FMTcmd_begintext( do_inline(data) )
            else if ( collectType == "CODE" )
                FMTcmd_begincode( data )
            else if ( collectType == "LIST" )
                FMTcmd_beginlist( do_inline(data) )
            else if ( collectType == "TABLE" )
                FMTcmd_begintable( data )
            else if ( collectType == "EMAIL" )
                FMTcmd_beginemail( data )
        }
    }
}

# -----------------------------
# 'Include' files
# -----------------------------

# set default path of include files
# - based on the original file named on the command line
# - used if there is a path and include file name doesn't have a path

local function setpath(name) {

    local pos

    if ( !(pos = rindex(name, "\\")) )
        pos = rindex( name, ":" )
    if ( pos )
        dfltPath = substr( name, 1, pos )
    return( name )
}

# set current source file

local function setfile(name, line, pos) {

    # if no path specified, we use default

    if ( dfltPath && !index(name, "\\") )
        name = dfltPath name

    # try to open file for reading

    if ( !fopen(name, "r") ) {
        MUTerr( "Can't open", name )
        return( FALSE )
    }

    # position file pointer

    fseek( name, pos )

    # note new file name and line

    currFile = toupper( name )
    currLine = line

    status( "Reading", currFile )

    # successful switch

    return( TRUE )
}

# is include stack non-empty ?
# - if so, restore topmost source file

local function popfile() {

    local mark

    close( currFile )

    # - note that if we can't restore topmost file,
    # we keep trying until we run out of them

    while ( incDepth > 0 ) {
        mark = incStack[ --incDepth ]
        if ( setfile(mark["FILE"], mark["LINE"], mark["POS"]) )
            return( TRUE )
    }

    return( FALSE )
}

# push source file info

local function pushfile(name, line, pos) {

    local mark

    mark[ "FILE" ] = name
    mark[ "LINE" ] = line
    mark[ "POS" ]  = pos

    incStack[ incDepth++ ] = mark
}

# ------------------------------------------------------

# a double-specification string has a left and a right part
# separated by an optional "|" character
# - if there is no "|" character the left and right parts
# are both the whole string

# get left part of double-spec string

global function MUTgetleft(str) {

    local ndx

    if ( (ndx = rindex(str, "|")) > 0 )
        return( toupper(substr(str, 1, ndx - 1)) )
    else
        return( toupper(str) )
}

# get right part of double-spec string

global function POTgetright(str) {

    local ndx

    if ( (ndx = rindex(str, "|")) > 0 )
        return( substr(str, ndx + 1) )
    else
        return( str )
}

# ------------------------------------------------------

# convert any escaped characters
# - for now there are no patterns except single characters,
# mainly used to prevent premature ends to fields and string literals

local function doescapes(this) {

    local ch

    if ( match(this, /\\./) ) {
        do {
            ch = substr( this, RSTART+1, RLENGTH-1 )
            substr( this, RSTART, RLENGTH ) = ch
        } while ( match(this, /\\./, RSTART+1) )
    }

    return( this )
}


# split string into fields
# - but must not split where separator is escaped or within quoted string

local function splitfield(str, result) {

    local i, j
    local ch, skip
    local fcount
    local field

    # no split char -> no split processing
    # - still, we make sure result is definitely a string ('0' is ambiguous)

    if ( !index(str, ",") ) {
        result[ 1 ] = str ""
        return( 1 )
    }

    # check if split necessary

    i = j = 1
    while ( match(str, /[,"\\]/, j) ) {

        # field separator ?

        if ( (ch = substr(str, RSTART, 1)) == "," ) {
            field[ ++fcount ] = substr( str, i, RSTART - i )
            i = j = RSTART + 1
        }

        # must be quote mark or escape char

        else {

            # if can't match skip pattern, this is last field
            # - and likely error, too

            skip = (ch == "\"") ? /^"([^"\\]|\\.)*"/ : /\\./
            if ( match(str, skip, RSTART) )
                j = RSTART + RLENGTH
            else
                break
        }
    }

    # unconditionally take everything left
    # - note that if i > length(str), then the last char in str
    # must have been a field separator
    # - which is an error we want to catch anyway !

    field[ ++fcount ] = substr( str, i )

    # eliminate leading and trailing spaces from fields

    for ( i in field ) {
        if ( match(field[i], /[^ \t]+([ \t]+[^ \t]+)*/) )
            result[ i ] = doescapes( substr(field[i], RSTART, RLENGTH) )
        else
            result[ i ] = ""
#        else {
#            MUTerr( "Blank field", str )
#            fcount = 0
#        }
    }

    return( fcount )
}

# get string following command

local function getstr(required) {

    if ( NF > 1 ) {
        i = index( $0, $1 ) + length( $1 )
        str = substr( $0, index($0, $2, i) )
        if ( match(str, /[ \t]+$/) )
            return( substr(str, 1, RSTART-1) )
        else
            return( str )
    }

    if ( required )
        MUTerr( "Argument string required" )

    return( "" )
}

# check if data line has "data start" flag

local function getfirst(this) {

    if ( this == dataStart ) {
        $0 = getstr( TRUE )
        if ( $0 )
            return( TRUE )
    }

    return( FALSE )
}

# ------------------------------------------------------

# add topic branches and twigs to master lists

local function addxrefdata(doc) {

    local branch, twig, path
    local xrefnum

    # add to master table of contents

    while ( tocCnt ) {
        masterToc[ doc ][ tocCnt ] = uniqueDocToc[ tocCnt ]
        --tocCnt
    }

    delete( uniqueDocToc )

    # add to master topic lists

    doc = doc ":"

    # a document that is part of the translation group

    masterTopicBranch[ doc ] = "INTERNAL"

    # all topics in document have a reference number > 0

    for ( branch in uniqueDocBranch ) {
        path = doc branch
        masterTopicBranch[ path ] = ++xrefnum
        if ( !(branch in uniqueMasterBranch) )
            uniqueMasterBranch[ branch ] = doc
        else
            uniqueMasterBranch[ branch ] = ""

        twig = uniqueDocBranch[ branch ]
        if ( docTwigCnt[twig] == 1 )
            masterTopicTwig[ doc twig ] = path
        if ( !(twig in uniqueMasterTwig) )
            uniqueMasterTwig[ twig ] = path
        else
            uniqueMasterTwig[ twig ] = ""
    }

    delete( uniqueDocBranch )
    delete( docTwigCnt )
}

# return reference number of full topic branch

global function MUTgetrefnum(doc, branch) {

    return( masterTopicBranch[toupper(doc ":" branch)] )
}

# write cross reference file and number

local function getxrefdata(link) {

    local ndx
    local path

    ndx = toupper( link )
    path = "dummy:dummy"

    # not "doc:---" form ?

    if ( !index(ndx, ":") ) {

        # branch ?

        if ( ndx in uniqueMasterBranch )
            path = uniqueMasterBranch[ ndx ] ndx

        # twig ?

        else if ( ndx in uniqueMasterTwig )
            path = uniqueMasterTwig[ ndx ]

        # can't find unique reference

        else
            MUTerr( "Non-unique branch or twig cross-reference", ndx )
    }

    # "doc:---" form

    else {

        # doc:branch ?

        if ( ndx in masterTopicBranch )
            path = ndx

        # doc:twig ?

        else if ( ndx in masterTopicTwig )
            path = masterTopicTwig[ ndx ]

        # can't find a reference

        else 
            MUTerr( "Non-existent doc: cross-reference", ndx )
    }

    return( path )
}

# check if document file name is same as current document file name

global function MUTdiffdoc(this) { return( this != currDocFile ) }

# ------------------------------------------------------

# collect LIST data
# - note first list line should have data start flag,
# but no check is made for this

local function collect_list(first) {

    FMTcmd_list( first, do_inline($0) )
}        

# collect TABLE data
# - note first table line should have data start flag,
# but no check is made for this

local function collect_table(first) {

    local i
    local cnt
    local cell

    cnt = splitfield( $0, cell )
    FMTcmd_table( first, do_inline(cell[1]) )
    for ( i = 2; i <= cnt; i++ )
        FMTcmd_table( FALSE, do_inline(cell[i]) )
}

# ------------------------------------------------------
# Command handling
# ------------------------------------------------------

# handle "DOC" command
# !doc title
# - "title" is name of current document
# - all input files up to next "!enddoc" belong to current document

local function docmd_doc(title) {

    local ndx

    if ( !title ) 
        return

    ndx = toupper( title )

    # pass one

    if ( passNum == 1 ) {

        # can't have duplicate titles

        if ( ndx in docFile ) {
            MUTerr( "Duplicate document titles" )
            return
        }

        if ( !currDoc )
            docFile[ ndx ] = ".T."
        else
            MUTerr( "Current document not closed" )
    }

    # pass two

    else {

        currDocFile = docFile[ ndx ]
        name = intempdir( MUTfnewext(currDocFile, FMTfext()) )
        outFile = fopen( name, "wt" )
        if ( !outFile )
            fatalerr( "Cannot open output file", name )
        status( "Writing", name )

        FMTcmd_doc( title )
    }

    currDoc = ndx
}

# ------------------------------------------------------

# handle "ENDDOC" command
# !enddoc filename
# - "filename" is the output filename of this document (no extension)

local function docmd_enddoc(fname) {

    local ndx

    docmd_endtopic( TRUE )

    # pass one

    if ( passNum == 1 ) {

        if ( !fname )
            fname = currFile

        ndx = toupper( fname )
        if ( ndx in docName ) {
            MUTerr( "Duplicate output filename", ndx )
            return
        }

        if ( currDoc ) {
            addxrefdata( ndx )
            docName[ ndx ] = currDoc
            docFile[ currDoc ] = ndx
        }
        else
            MUTerr( "No current open document" )
    }

    # pass two

    else {
        FMTcmd_enddoc()
        close( outFile )
    }

    currDoc = ""
}

# ------------------------------------------------------

# handle "SET" command
# !set name=text
# - "name" is a shorthand name to substitute for "text"
# - "text" is an arbitrary string

local function docmd_set(str) {

    local ndx
    local name

    # pass one only

    if ( passNum > 1 )
        return

    if ( !str )
        return

    # no spaces in "name", ends with "="
    
    if ( !match(str, /^[_A-Z][_A-Z0-9]*=/i) ) {
        MUTerr( "Bad SET argument" )
        return
    }

    # no duplicates allowed

    name = "%" toupper( substr(str, 1, RLENGTH-1) ) "%"
    if ( name in envStr ) {
        MUTerr( "Duplicate SET name", name )
        return
    }

    envStr[ name ] = substr( str, RLENGTH+1 )
}

# ------------------------------------------------------

# handle "INCLUDE" command
# !include ["]name["]
# - we do this in a way that lets us take advantage of another
# function's error-handling during source file switching

local function docmd_include(name) {

    if ( !name )
        return

    if ( name ~ /^[<"][^<">]+[">]$/ )
        name = substr( name, 2, length(name) - 2 )

    # stack current file

    pushfile( currFile, currLine, ftell(currFile) )

    # stack include file

    pushfile( name, 0, 0 )

    # "restore" include file

    popfile()
}

# ------------------------------------------------------

# handle "TOPIC" command
# !topic [branch[[\branch]..]\]twig - complete specification
# !topic \twig                      - subtopic of current topic (not reset)
# !topic .\twig                     - subtopic of current topic (reset)
# !topic [[..\]..]..\twig           - parent of current topic

local function docmd_topic(this) {

    local topic
    local ndx, cnt
    local branch
    local twig

    if ( !this )
        return

    docmd_endtopic( FALSE )

    # complete specification ?

    if ( this ~ /^[^\.\\][^\\]*(\\[^\\]+)*$/ )
        currTopic = workTopic = this

    # else must have current topic to manipulate

    else {
        if ( !currTopic ) {
            MUTerr( "No current topic to branch off of", this )
            return
        }

        # one-shot subtopic of current topic ?

        if ( this ~ /^\\[^\\]+$/ )
            workTopic = currTopic this

        # permanent subtopic of current topic ?

        else if ( this ~ /^\.\\[^\\]+$/ )
            currTopic = workTopic = currTopic substr( this, 2 )

        # permanent subtopic of a parent topic of current topic ?

        else if ( match(this, /^\.\.\\[^\\]+/) ) {
            do {
                ndx = rindex( currTopic, "\\" )
                if ( !ndx ) {
                    MUTerr( "Current topic has no parent", currTopic )
                    currTopic = workTopic = ""
                    return
                }
                currTopic = substr( currTopic, 1, ndx-1 )
                this = substr( this, 4 )
            } while ( this ~ /^\.\.\\[^\\]+/ )
            currTopic = workTopic = currTopic "\\" this
        }

        # don't know what it is...

        else {
            MUTerr( "Bad TOPIC format", this )
            currTopic = workTopic = ""
            return
        }
    }

    # topic depth (in document topic tree)

    cnt = split( workTopic, twig, "\\" )

    # pass one

    if ( passNum == 1 ) {
    
        # topic branch is unique in current document ?

        branch = toupper( workTopic )
        if ( branch in uniqueDocBranch ) {
            MUTerr( "Duplicate topic branch", branch )
            currTopic = workTopic = ""
            return
        }

        # record topic for table of contents

        uniqueDocToc[ ++tocCnt ] = workTopic

        # twig shorthand legal only if twig unique in document

        ndx = toupper( twig[cnt] )
        ++docTwigCnt[ ndx ]
        uniqueDocBranch[ branch ] = ndx
    }

    # pass two

    else
        FMTcmd_topic( currDocFile, workTopic, twig[cnt], cnt )
}

# ------------------------------------------------------

# handle "ENDTOPIC" command

local function docmd_endtopic(cleartopic) {

    if ( currTopic ) {
        endcollect()
        if ( passNum > 1 )
            FMTcmd_endtopic()

        if ( cleartopic )
            currTopic = ""
    }
}

# ------------------------------------------------------

# handle "TOC" command
# !toc [depth] [,[doc':'][branch]]

local function docmd_toc(spec) {

    local i
    local cnt, pos
    local doc, basetopic, baselevel, downlevel
    local thistopic, topiclevel
    local field
    
    if ( passNum == 1 )
        return

    # assume current document

    doc = docFile[ currDoc ]

    # first set working parameters: is there a specification ?

    if ( spec ) {
        cnt = splitfield( spec, field )

        if ( cnt > 2 ) {
            MUTerr( "Too many TOC arguments" )
            return
        }

        # particular doc:branch ?

        if ( cnt > 1 ) {
            basetopic = field[ 2 ]
            if ( (pos = index(basetopic, ":")) ) {
                doc = toupper( substr(basetopic, 1, pos - 1) )
                basetopic = substr( basetopic, pos + 1 )
            }
        }                

        # default doc:branch

        else
            basetopic = workTopic

        # display how many topic levels below starting topic level ?

        downlevel = field[ 1 ]
        if ( downlevel "" !~ /^[0-9]+$/ ) {
            MUTerr( "Bad TOC downlevel" )
            return
        }

    }

    # no specification -> entire document

    else {
        basetopic = ""
        downlevel = 10
    }

    # what is starting topic level ?

    if ( basetopic ) {
        basetopic = toupper( basetopic )
        baselevel = split( basetopic, field, "\\" )
    }
    else
        baselevel = 0

    # generate table of contents

    if ( !(doc in masterToc) ) {
        MUTerr( "Non-existent TOC document" )
        return
    }
    uniqueDocToc = masterToc[ doc ]

    cnt = 0
    FMTcmd_begintoc()
    for ( i in uniqueDocToc ) {
        thistopic = uniqueDocToc[ i ]
        if ( !basetopic || (index(toupper(thistopic), basetopic) == 1) ) {
            cnt = split( thistopic, field, "\\" )
            topiclevel = cnt - baselevel
            if ( (topiclevel > 0) && (topiclevel <= downlevel) ) {
                FMTcmd_toc( currDocFile, thistopic, field[cnt], topiclevel )
            }
        }
    }
    FMTcmd_endtoc()

    # count is at least one if at least one topic for table of contents found

    if ( !cnt )
        MUTerr( "Empty table of contents" )
}

# ------------------------------------------------------

# handle "TEXT" command

local function docmd_text(this) { begincollect( "TEXT", this ) }

# ------------------------------------------------------

# handle "CODE" command

local function docmd_code(this) { begincollect( "CODE", this ) }

# ------------------------------------------------------

# handle "LIST" command

local function docmd_list(this) { begincollect( "LIST", this ) }

# ------------------------------------------------------

# handle "TABLE" command

local function docmd_table(this) { begincollect( "TABLE", this) }

# ------------------------------------------------------

# handle "DIV" command

local function docmd_div() {

    if ( passNum > 1 )
        FMTcmd_div()
}

# ------------------------------------------------------

# handle "EMAIL" command

local function docmd_email(this) { begincollect("EMAIL", this) }

# ------------------------------------------------------

# handle command line

local function do_command(this) {

    local command

    # isolate command

    command = substr( toupper(this), 2 )

    # try to identify

    if ( command == "DOC" )
        docmd_doc( getstr(TRUE) )

    else if ( command == "ENDDOC" )
        docmd_enddoc( getstr(FALSE) )

    else if ( command == "SET" )
        docmd_set( getstr(TRUE) )

    else if ( command == "INCLUDE" )
        docmd_include( getstr(TRUE) )

    else if ( command == "TOPIC" )
        docmd_topic( getstr(TRUE) )

    else if ( command == "ENDTOPIC" )
        docmd_endtopic( TRUE )

    else if ( command == "TOC" )
        docmd_toc( getstr(FALSE) )

    else if ( command == "TEXT" )
        docmd_text( getstr(FALSE) )

    else if ( command == "CODE" )
        docmd_code( getstr(FALSE) )

    else if ( command == "LIST" )
        docmd_list( getstr(FALSE) )

    else if ( command == "TABLE" )
        docmd_table( getstr(FALSE) )

    else if ( command == "DIV" )
        docmd_div()

    else if ( command == "EMAIL" )
        docmd_email( getstr(FALSE) )

    # don't know what it is...

    else
        MUTerr( "Unrecognized command" )
}

# ------------------------------------------------------
# Pass One: Read marked-up text source files
# ------------------------------------------------------

# expand any SET strings in argument

local function exp_setstr(text) {

    local name

    # slower test executed only if quick test passes

    if ( index(text, "%") && match(text, /%[_A-Z][_A-Z0-9]+%/i) ) {
        do {
            name = toupper( substr(text, RSTART, RLENGTH) )
            if ( name in envStr ) {
                name = envStr[ name ]
                substr( text, RSTART, RLENGTH ) = name
            }
            else {
                MUTerr( "Equate name not found", name )
                return( "" )
            }
        } while ( match(text, /%[_A-Z][_A-Z0-9]+%/i, RSTART + length(name)) )
    }

    return( text )
}

# fetch next line

local function nextline() {

    # read until we have a non-blank non-comment line or EOF

    while ( (getline() < currFile) > 0 ) {
        ++currLine

        # blank or comment ?

        if ( ($0 ~ /^[ \t]*$/) || ($1 ~ /^[;#]/) )
            endcollect()

        # expand any SET strings

        else {
            $0 = exp_setstr( $0 )
            return( TRUE )
        }
    }

    return( FALSE )
}

# ------------------------------------------------------

# execute a pass

local function dopass(name, num) {

    passNum = num
    status( "Pass " passNum )

    # try to open file

    if ( setfile(name, 0, 0) ) {

        do {
            while ( nextline() ) {

                # command line ?

                if ( $1 ~ /^[!\.][A-Z][A-Z0-9]*/i )
                    do_command( $1 )

                else if ( passNum > 1 ) {

                    # LIST data line ?

                    if ( collectType == "LIST" )
                        collect_list( getfirst($1) )

                    # TABLE data line ?

                    else if ( collectType == "TABLE" )
                        collect_table( getfirst($1) )

                    # generic data line (default TEXT)

                    else {
                        if ( !collectType )
                            docmd_text( "" )

                        if ( collectType == "TEXT" )
                            FMTcmd_text( do_inline($0) )
                        else if ( collectType == "CODE" )
                            FMTcmd_code( do_inline($0) )
                        else if ( collectType == "EMAIL" )
                            FMTcmd_email( $0 )
                    }
                }
            }
        } while ( popfile() )
    }

    # quit on error

    if ( errCnt ) {
        status( errCnt " errors found" )
        abort()
    }   
}

# control passes

BEGIN {

    # got an input filename ?

    if ( ARGC < 2 )
        fatalerr( "Need a filename" )

    setpath( ARGV[1] )

    # process it

    dopass( ARGV[1], 1 )
    dopass( ARGV[1], 2 )
}

# ------------------------------------------------------
# Pass Two: Translate the marked-up source files
# ------------------------------------------------------

# ------------------------------------------------------
# Output-type Specific Functions
# ------------------------------------------------------

# send data to output file (may buffer here if desired)

local function write(this) {

    MUTwrite(this)
    writeFlag = 1
}

# ------------------------------------------------------

# for table of contents

local tocIndent

local writeFlag = 0

local htmlVal, htmlChar

INIT {

    # HTML tag values

    htmlVal[ "ASIS" ]       = "PRE"
    htmlVal[ "BIG" ]        = "BIG"
    htmlVal[ "BODY" ]       = "BODY"
    htmlVal[ "BODYTITLE" ]  = "H2"
    htmlVal[ "BOLD" ]       = "STRONG"
    htmlVal[ "CAPTION" ]    = "CAPTION"
    htmlVal[ "CENTER" ]     = "CENTER"
    htmlVal[ "CODE" ]       = "CODE"
    htmlVal[ "HEAD" ]       = "HEAD"
    htmlVal[ "HTML" ]       = "HTML"
    htmlVal[ "ITALIC" ]     = "EM"
    htmlVal[ "LISTITEM" ]   = "LI"
    htmlVal[ "LIST" ]       = "UL"
    htmlVal[ "PARA" ]       = "P"
    htmlVal[ "TITLE" ]      = "TITLE"
    htmlVal[ "TCELL" ]      = "TD"
    htmlVal[ "TROW" ]       = "TR ALIGN=CENTER"

    # character replacements

    htmlChar[ "<" ] = "&lt;"
    htmlChar[ ">" ] = "&gt;"
}

# ------------------------------------------------------
# prepare data for output file
# ------------------------------------------------------

# prepare text

local function preptext(this) {

    local ch

    for ( ch in htmlChar ) {
        if ( index(this, ch) > 0 )
            gsubs( ch, htmlChar[ch], this, 0 )
    }

    return( this )
}

# prepare HTML start/end tags

local function prepstart(tag) { return( "<" htmlVal[tag] ">" ) }
local function prepend(tag) {   return( "</" htmlVal[tag] ">" ) }

# send text

local function sendtext(this) {

    if ( this !~ /^[ \t]*$/ )
        write( preptext(this) )
}

# send HTML start/end tags

local function sendstart(tag) { write(prepstart(tag)) }
local function sendend(tag) { write(prepend(tag)) }

# send fully-tagged text

local function sendtagged(type, text) {

    write( prepstart(type) preptext(text) prepend(type) )
}

# new paragraph

local function newpara() { sendstart("PARA") }

# send horizontal rule

local function sendrule(width) {

    write( prepstart("PARA") "<HR WIDTH=" int(width) "%>" )
}

# send DIV line

local function senddiv(level) {

    sendrule( 50 / (level < 6 ? level : 6) )
    newpara()
}

# ------------------------------------------------------

# make a HTML anchor

local function makeanchor(target, text) {

    return( sprintf("<A NAME=\"%s\">%s</A>", tolower(target), text) )
}

# make a HTML link

local function makelink(target, text) {

    return( sprintf("<A HREF=\"%s\">%s</A>", tolower(target), text) )
}

# make a reference to a topic (not including document)

local function makehtmlref(doc, topic) {

    return( sprintf("L%02X", MUTgetrefnum(doc, topic)) )
}

# make link to a topic in a document in a translation set (internal)

local function makeilink(doc, topic, text) {

    local target

    if ( topic )
        target = "#" makehtmlref( doc, topic )
    else
        target = ""

    if ( MUTdiffdoc(doc) )
        target = MUTfnewext( doc, FMTfext() ) target

    return( makelink(target, text) )
}

# ------------------------------------------------------

# traverse text chunk list

local function hurl(chunk) {

    local i
    local type, data
    local topic, show

    i = 1
    while ( i < length(chunk) ) {
        type = chunk[ i++ ]
        data = chunk[ i++ ]
        if ( type == "B" )
            sendtagged( "BOLD", data )
        else if ( type == "I" )
            sendtagged( "ITALIC", data )
        else if ( type == "L" ) {
            topic = chunk[ i++ ]
            show  = chunk[ i++ ]
            write( makeilink(data, topic, show) )
        }
        else if ( type == "N" )
            sendtext( data )
        else if ( type == "X" ) {
            show = chunk[ i++ ]
            write( makelink(tolower(data), show) )
        }
    }
}

# ------------------------------------------------------

# return output filename extension

global function FMTfext() { return("htm") }

# ------------------------------------------------------

# handle "DOC" command

global function FMTcmd_doc(title) {

    # declare HTML 3.2 document 

    write( "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2 Final//EN\">" )

    # begin document

    sendstart( "HTML" )

    # HEAD section

    sendstart( "HEAD" )
    sendtagged( "TITLE", title )
    sendend( "HEAD" )

    # start BODY section

    sendstart( "BODY" )

    # top-of-page anchor

    write( makeanchor("TOP", "") )

    # visible title

    sendstart( "CENTER" )
    sendtagged( "BODYTITLE", title )
    sendend( "CENTER" )

    sendrule( 50 )
}

# ------------------------------------------------------

# handle "ENDDOC" command

global function FMTcmd_enddoc() {

   sendrule( 50 )
   sendtagged( "ITALIC", "Last revised: " ctime() )

   # end body section

   sendend( "BODY" )
   sendend( "HTML" )
}

# ------------------------------------------------------

# handle "TOPIC" command

global function FMTcmd_topic(doc, topic, twig, depth) {

    # make it visually distinct

    newpara()
    senddiv( depth )

    # level 1 and 2 topics are extra-distinguished

    sendstart( "CENTER" )
    sendstart( "BOLD" )
    if ( topicLevel < 3 )
        sendstart( "BIG" )  
    write( makeanchor(makehtmlref(doc, topic), twig) )
    if ( topicLevel < 3 )
        sendend( "BIG" )
    sendend( "BOLD" )
    sendend( "CENTER" )

    writeFlag = 0
}

# ------------------------------------------------------

# handle "ENDTOPIC" command

global function FMTcmd_endtopic() {

    if ( writeFlag ) {
        newpara()
        sendstart( "CENTER" )
        write( makelink("#TOP", "Page Top") )
        sendend( "CENTER" )
        writeFlag = 0
    }
}

# ------------------------------------------------------

# handle "BEGINTOC" command

global function FMTcmd_begintoc() {

    newpara()
    tocIndent = 0
}

# ------------------------------------------------------

# handle "TOC" command

global function FMTcmd_toc(doc, topic, twig, depth) {

    # handle indenting and outdenting as necessary

    while ( tocIndent < depth ) {
        sendstart( "LIST" )
        ++tocIndent
    }

    while ( tocIndent > depth ) {
        sendend( "LIST" )
        --tocIndent
    }

    write( prepstart("LISTITEM") makeilink(doc, topic, twig) )
}

# ------------------------------------------------------

# handle "ENDTOC" command

global function FMTcmd_endtoc() {

    # outdent as much as necessary

    while ( tocIndent ) {
        sendend( "LIST" )
        --tocIndent
    }
}

# ------------------------------------------------------

# handle "BEGINTEXT" command

global function FMTcmd_begintext(this) {

    newpara()
    hurl( this )
}

# ------------------------------------------------------

# handle "TEXT" command

global function FMTcmd_text(this) {

    hurl( this )
}

# ------------------------------------------------------

# handle "ENDTEXT" command

global function FMTcmd_endtext() { }

# ------------------------------------------------------

# handle "BEGINCODE" command

global function FMTcmd_begincode(this) {

    newpara()
    sendtext( this )
    sendstart( "CODE" )
}

# handle "CODE" command

global function FMTcmd_code(this) {

    hurl( this )
}

# handle "ENDCODE" command

global function FMTcmd_endcode() {

    sendend( "CODE" )
}

# handle "BEGINLIST" command

global function FMTcmd_beginlist(this) {

    newpara()
    hurl( this )
    sendstart( "LIST" )
}

# handle "LIST" command

global function FMTcmd_list(first, item) {

    if ( first )
        sendstart( "LISTITEM" )
    hurl( item )
}

# handle "ENDLIST" command

global function FMTcmd_endlist() {

    sendend( "LIST" )
}

# handle "BEGINTABLE" command

global function FMTcmd_begintable(this) {

    newpara()
    write( "<TABLE ALIGN=CENTER BORDER=1 CELLPADDING=2>" )
    if ( this )
        sendtagged( "CAPTION", this )
}

# handle "TABLE" command

global function FMTcmd_table(first, data) {

    if ( first )
        sendstart( "TROW" )
    sendstart( "TCELL" )
    hurl( data )
}

# handle "ENDTABLE" command

global function FMTcmd_endtable() {

    write( "</TABLE>" )
}

# handle "BEGINEMAIL" command

global function FMTcmd_beginemail(this) {

    write( "BEGEMAIL= " this )
}

# handle "EMAIL" command

global function FMTcmd_email(data) {

    write( "EMAIL= " data )
}

# handle "ENDEMAIL" command

global function FMTcmd_endemail() {

    write( "ENDEMAIL" )
}

# handle "DIV" command

global function FMTcmd_div() {

    write( "DIV" )
}
