# a script to compare a previous version of HXA to proposed new one

# for Thompson AWK 4.0

# by Anton Treuenfels

# first created: 07/02/11
# last revision: 07/02/11

# ----------------------------

# prior to running this the previous version of "hxa_t.exe"
# must have been compiled and re-named to "hxa_org.exe"
# - generally the earlier version is likely to be the previous release,
# but it doesn't have to be
# - also might be a good idea to delete "testchk.txt",
# as otherwise the results will just get appended to it

# ----------------------------

local compareEarlier

BEGIN {

    # we want to compare all tests before this date (yyyy,mm,dd)
    # - presumably the earlier version can't handle tests created
    # or changed after this date, so there's no point in checking them

    compareEarlier = time( 2013, 04, 01 )

    # check all the test programs

    ARGV[ ARGC++ ] = "test*.a"
}

# ----------------------------

{

    local fname

    print "Checking " FILENAME

    # get the time and stop processing this file (from this script, anyway)

    fname = substr( FILENAME, 1, length(FILENAME)-2 )
    ftime = filetime( FILENAME )
    close( FILENAME )

    # if it's before the target date (ie., compatible with both versions),
    # test it the easy way (by launching a command shell to process a
    # batch file that does all the real work)

    if ( ftime < compareEarlier ) {
        print "Testing..." fname
        system( "regress1.bat " fname )
    }
}
