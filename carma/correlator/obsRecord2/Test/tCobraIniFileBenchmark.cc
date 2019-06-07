#include <ace/Configuration.h>
#include <ace/Configuration_Import_Export.h>

#include <cobra/CorrelatorConfiguration.h>
#include <cobra/CorrelatorConfigurationIniImporter.h>

#include <algorithm>
#include <iostream>
#include <set>
#include <sstream>
#include <vector>

#include "carma/util/ErrorException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/FileUtils.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/simpleStats.h"

using namespace ::std;
using namespace carma;
using namespace carma::util;


namespace {


void
outputLine( const int      indentLevel,
            const string & t )
{
    const size_t pinnedIndentLevel = ::std::max( 0, indentLevel );

    programLogInfoIfPossible( string( (2 * pinnedIndentLevel), ' ' ) + t );

    cout << string( (2 * (pinnedIndentLevel + 1)), ' ' )
         << t
         << "\n";
}


struct IterResult {
    struct ::timeval startTime;
    struct ::timeval doneTime;
};


double
calcMillis( const IterResult & iterResult )
{
    long long startMicros =
        static_cast< long long >( iterResult.startTime.tv_sec ) *
            1000LL * 1000LL + iterResult.startTime.tv_usec;

    long long doneMicros =
        static_cast< long long >( iterResult.doneTime.tv_sec ) *
            1000LL * 1000LL + iterResult.doneTime.tv_usec;

    return (static_cast< double >( doneMicros - startMicros ) / 1000.0);
}


void
collateAndOutputResults( const int                    indentLevel,
                         const IterResult &           firstResult,
                         const vector< IterResult > & otherResults )
{
    vector< double > otherMillis;
    {
        vector< IterResult >::const_iterator i = otherResults.begin();
        const vector< IterResult >::const_iterator iEnd = otherResults.end();

        for ( ; i != iEnd; ++i )
            otherMillis.push_back( calcMillis( *i ) );
    }

    {
        ostringstream oss;

        oss << "First iteration: " << calcMillis( firstResult ) << " ms";

        outputLine( indentLevel, oss.str() );
    }

    if ( otherMillis.empty() == false ) {
        double minValue, maxValue, medianValue, meanValue, stdDev;

        calcSimpleStatsInplace( otherMillis,
                                &minValue,
                                &maxValue,
                                &medianValue,
                                &meanValue,
                                &stdDev );

        ostringstream oss;

        oss << "Other " << otherMillis.size() << " iterations: ";

        oss << minValue << " ms min, "
            << maxValue << " ms max, "
            << medianValue << " ms median, "
            << meanValue << " ms mean, "
            << stdDev << " ms std dev";
            
        outputLine( indentLevel, oss.str() );
    }
}


void
testAceConfigIniFile( const int                indentLevel,
                      const int                iterations,
                      const string &           inifile,
                      const string &           system,
                      const vector< string > & modeKeys )
{
    IterResult firstResult;
    vector< IterResult > otherResults;

    if ( iterations > 1 )
        otherResults.reserve( iterations - 1 );

    for ( int i = 0; i < iterations; ++i ) {
        IterResult iResult;

        ::gettimeofday( &(iResult.startTime), 0 );
        {
            ACE_Configuration_Heap configHeap;

            const int32_t openStatus = configHeap.open();

            if ( openStatus != 0 )
                throw CARMA_ERROR( "ACE config open failure" );

            ACE_Registry_ImpExp importer( configHeap );

            const int32_t importStatus =
                importer.import_config( inifile.c_str() );

            if ( importStatus != 0 )
                throw CARMA_ERROR( "ACE config import failure" );
        }
        ::gettimeofday( &(iResult.doneTime), 0 );

        if ( i == 0 )
            firstResult = iResult;
        else
            otherResults.push_back( iResult );
    }

    outputLine( indentLevel, "ACE:" );
    collateAndOutputResults( (indentLevel + 1), firstResult, otherResults );
}


void
testCobraConfigIniFile( const int                indentLevel,
                        const int                iterations,
                        const string &           inifile,
                        const string &           system,
                        const vector< string > & modeKeys )
{
    const vector< string >::const_iterator iModeBegin = modeKeys.begin();
    const vector< string >::const_iterator iModeEnd = modeKeys.end();

    if ( iModeBegin == iModeEnd )
        throw CARMA_ERROR( "Empty mode key vector" );

    vector< string >::const_iterator iMode = iModeBegin;

    cobra::CorrelatorConfiguration config;

    IterResult firstResult;
    vector< IterResult > otherResults;

    if ( iterations > 1 )
        otherResults.reserve( iterations - 1 );

    for ( int i = 0; i < iterations; ++i ) {
        IterResult iResult;

        ::gettimeofday( &(iResult.startTime), 0 );
        {
            cobra::CorrelatorConfigurationIniImporter importer;

            const int importStatus =
                importer.import( inifile, system, *iMode, config );

            if ( importStatus != 0 ) {
		ostringstream os;
		os << "Cobra config import failure on file "
		   << inifile;
                throw CARMA_ERROR( os.str() );
	    }
        }
        ::gettimeofday( &(iResult.doneTime), 0 );

        if ( i == 0 )
            firstResult = iResult;
        else
            otherResults.push_back( iResult );

        ++iMode;
        if ( iMode == iModeEnd )
            iMode = iModeBegin;
    }

    outputLine( indentLevel, "Cobra:" );
    collateAndOutputResults( (indentLevel + 1), firstResult, otherResults );
}


}  // namespace < anonymous >

//
// @author Tom Costa
// @version $Revision: 1.13 $
// @description Benchmarks the cobra configuration ini file import code
//              which, at present, gets run on every bandwidth change
//
// @usage use it.
//
// @key aceIters 10 int
//               Number of raw ACE ini file imports to do
//
// @key cobraIters 10 int
//                 Number of cobra ini file imports to do
//
// @key i "correlator/carma15-new.ini,correlator/carma15-2pol.ini" string
//         Whitespace and/or comma separated list of configuration ini file
//         paths
//
// @key s CARMA string
//        configuration system key
//
// @key m "500MHz,250MHz,125MHz,62MHz,31MHz,8MHz,2MHz" string
//        Whitespace and/or comma separated list of configuration mode keys
//
// @logger TEST_FACILITY carma.test.correlator.obsrecord2.tCobraIniFileBenchmark
//


int
Program::main( )
try {
    const int aceIters   = getIntParameter("aceIters");
    const int cobraIters = getIntParameter("cobraIters");
    const string system  = getStringParameter("s");

    vector< string > iniFilePaths;
    {
        const string iParam = getStringParameter("i");
        const char sepChars[] = ", \t\n\r";

        string::size_type beginPos =
            iParam.find_first_not_of( sepChars );

        while ( beginPos != string::npos ) {
            const string::size_type endPos =
                iParam.find_first_of( sepChars, beginPos );

            if ( endPos == string::npos ) {
                const string finalIniFile( iParam, beginPos );

                iniFilePaths.push_back( getConfFile( finalIniFile ) );

                break;
            }

            const string iniFile( iParam, beginPos, (endPos - beginPos) );

            iniFilePaths.push_back( getConfFile( iniFile ) );

            beginPos = iParam.find_first_not_of( sepChars, endPos );
        }
    }

    vector< string > modeKeys;
    {
        const string mParam = getStringParameter("m");
        const char sepChars[] = ", \t\n\r";

        string::size_type beginPos =
            mParam.find_first_not_of( sepChars );

        while ( beginPos != string::npos ) {
            const string::size_type endPos =
                mParam.find_first_of( sepChars, beginPos );

            if ( endPos == string::npos ) {
                const string finalModeKey( mParam, beginPos );

                modeKeys.push_back( finalModeKey );

                break;
            }

            const string modeKey( mParam, beginPos, (endPos - beginPos) );

            modeKeys.push_back( modeKey );

            beginPos = mParam.find_first_not_of( sepChars, endPos );
        }
    }

    vector< string >::const_iterator i = iniFilePaths.begin();
    const vector< string >::const_iterator iEnd = iniFilePaths.end();

    for ( ; i != iEnd; ++i ) {
        if ( FileUtils::exists( *i ) != true )
            throw CARMA_ERROR( "File \"" + (*i) + "\" does not exist" );

        outputLine( 0, (*i + ":") );

        if ( aceIters > 0 )
            testAceConfigIniFile( 1, aceIters, *i, system, modeKeys );

        if ( cobraIters > 0 )
            testCobraConfigIniFile( 1, cobraIters, *i, system, modeKeys );
    }

    return 0;
} catch ( ... ) {
    const string msg =
        "Returning from main on an exception - " + getStringForCaught();

    programLogErrorIfPossible( msg );

    cerr << "ERROR: " << msg << endl;

    throw;
}
