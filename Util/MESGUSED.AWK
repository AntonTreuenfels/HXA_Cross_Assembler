# a script to check for unused HXA messages

# for Thompson AWK 4.0

# by Anton Treuenfels

# first created: 07/04/09
# last revision: 08/28/13

# ------------------

# output filename

local outFile = "mesgused.txt"

# message indices and default texts

local mesgText, usedCnt, mesgRE

# the count of message indices

local ndxCnt, remainCnt

# ------------------

# print a line to output file

local function out(this) { print this > outFile }

# initialize the test

BEGIN {

    local fd
    local ndx, pos, re

    # get the message indices (descriptions not needed for this)

    ndxCnt = 0
    fd = fopen( "a_mesg.awk", "rt" )
    while ( !feof(fd) ) {
        getline() < fd
        if ( index($1, "=") && match($0, /=.+\+/) ) {           
            ndx = substr( $0, 2, RSTART-2 )
#            re = "\"" ndx "\"|=" ndx " "
            re = "[\"=]" ndx "[\" ]"
            mesgText[ ++ndxCnt ] = ndx
            usedCnt[ ndx ] = 0
            mesgRE[ ndx ] = regex( re )
        }
    }
    close( fd )

    remainCnt = ndxCnt
    print ndxCnt " message indices found"

    # now look at all the source files for those indices

    ARGV[ ARGC++ ] = "a*.awk"
}

# what file are we working on ?

BEGINFILE {

    print "Reading " FILENAME
}

# check an input line to see whether or not it contains a message index

{

    local i
    local ndx

    for ( i = 1; i <= ndxCnt; i++ ) {
        ndx = mesgText[ i ]
        if ( index($0, ndx) && match($0, mesgRE[ndx]) ) {
            if ( usedCnt[ndx] )
                print "Found: " ndx " (again)"
            else {
                print "Found: " ndx " (" remainCnt " left)"
                --remainCnt
            }
            usedCnt[ ndx ] += 1
        }
    }
}

# any message index left wasn't used

END {

    local i
    local ndx
    local cnt

    print "Writing " outFile

    out( "Unused?\n" )
    for ( i = 1; i <= ndxCnt; i++ ) {
        ndx = mesgText[ i ]
        cnt = usedCnt[ ndx ]
        if ( !cnt )
            out( ndx ": " cnt )
    }
            
    out( "\nConsolidate?\n" )
    for ( i = 1; i <= ndxCnt; i++ ) {
        ndx = mesgText[ i ]
        cnt = usedCnt[ ndx ]
        if ( cnt > 2 )
            out( ndx ": " cnt )
    }
            
    out( "\nOthers\n" )
    for ( i = 1; i <= ndxCnt; i++ ) {
        ndx = mesgText[ i ]
        cnt = usedCnt[ ndx ]
        if ( (cnt > 0) && (cnt < 3) )
            out( ndx ": " cnt )
    }           
}
